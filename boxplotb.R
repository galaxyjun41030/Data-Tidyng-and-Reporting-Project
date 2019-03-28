require(shiny)
require(tidyverse)
require(shinyjs)
require(fmsb)
library(readr)

########################### CLEANING



seasons_stats <- read_csv("Seasons_Stats.csv", col_types = cols(
  X1 = col_skip(), `3P` = col_double(), 
  `3P%` = col_double(), `3PA` = col_double(), 
  `3PAr` = col_double(), `AST%` = col_double(), 
  BLK = col_double(), `BLK%` = col_double(), 
  BPM = col_double(), DBPM = col_double(), 
  DRB = col_double(), `DRB%` = col_double(), 
  GS = col_double(), MP = col_double(), 
  OBPM = col_double(), ORB = col_double(), 
  `ORB%` = col_double(), PER = col_double(), 
  STL = col_double(), `STL%` = col_double(), 
  TOV = col_double(), `TOV%` = col_double(), 
  TRB = col_double(), `TRB%` = col_double(), 
  `USG%` = col_double(), VORP = col_double(), 
  `WS/48` = col_double(), blank2 = col_double(), 
  blanl = col_double()))

#Remove blank columns
stats=seasons_stats
stats=stats[,-c(21,26)]

#Move name to the front
stats=select(stats, Player, everything())

#Filter to get data only since 2000
stats=stats %>% filter(Year>2000)

#Filter to get data from the 5 main positions
statsC =stats %>% filter(Pos == 'C')
statsPF =stats %>% filter(Pos == 'PF')
statsSF =stats %>% filter(Pos == 'SF')
statsSG =stats %>% filter(Pos == 'SG')
statsPG =stats %>% filter(Pos == 'PG')

stats=rbind(statsC, statsPF, statsSF, statsSG, statsPG)

#Filter players with less than 30 macthes
stats=stats %>% filter(G>30)

### Filtering for the Radar Plot
pstats= stats %>% select(ends_with("%"))
pstats=pstats[,1:8]
rstats=cbind(stats[,c(1:2,5)],pstats)




###################################### SHINY APP


######## INPUTS 

list_variables <-  c(colnames(stats))
list_category <-  unique(c(colnames(stats[6:length(stats[1,])])))
list_year <- unique(stats$Year)[!is.na(unique(stats$Year))]
list_choices <-  unique(msleep$vore)[!is.na(unique(msleep$vore))]


################## ui

ui=navbarPage("NBA features",
              tabPanel("Position Comparision",
                       fluidPage( 
                         sidebarLayout(
                           sidebarPanel(("Control Panel"),
                            selectInput("category", label = h1("Select Category"), 
                                       choices = character(0),
                                       selected = 1),
                            selectInput("year", label = h1("Select Year"), 
                                        choices = character(0),
                                        selected = 1)
                           
                         ), # sidebarPanel
                           mainPanel(("Box Plot (Per-Game)"),
                                     plotOutput(outputId = "boxplot")
                           )# mainPanel
                         )# sidebarLayout
                       )# fluidPage
              ), #  tabPanel
              tabPanel("Description of the variables",
                       fluidPage( 
                         titlePanel("Description of the variables"),
                         sidebarLayout(
                           sidebarPanel(
                             selectInput("name", label = h3("Select Variable"), 
                                         choices = character(0),
                                         selected = 1)
                             
                           ), # sidebarPanel
                           mainPanel(("Description of the variables"),
                           textOutput(outputId = "info")
                              )# mainPanel
                         )# sidebarLayout
                       )# fluidPage
              )#  tabPanel
)

################ server

server <- function(input, output, session) {

  # Select variable for description
  updateSelectInput(session, "name",
                    choices = list_variables,
                    selected = tail(list_variables, 1)
  );
  
  
  # Select category to visualize boxplot
  updateSelectInput(session, "category",
                    choices = list_category,
                    selected = tail(list_category, 1)
  );
  
  # Select year to visualize boxplot
  updateSelectInput(session, "year",
                    choices = list_year,
                    selected = tail(list_year, 1)
  );
  
  #Boxplot
  output$boxplot <- renderPlot({
    # cat(file=stderr(), "input$select:", input$select == "", "\n")
    if(input$category != "" && input$category != ""){
      ggplot(stats %>% filter(Year==input$year), aes(x=Pos, y=PF, colour = Pos)) +
        geom_violin(trim = FALSE,fill="black", alpha=0.1,size = 0.8)+
        geom_jitter(width = 0.15)+
        stat_boxplot(geom ='errorbar', width = 0.1)+
        geom_boxplot(width = 0.4, alpha=0.4)+
        ylab(input$category)
    }
  });
  
  output$info =renderText(input$name)
  
}


# Run the application 
shinyApp(ui = ui, server = server)
