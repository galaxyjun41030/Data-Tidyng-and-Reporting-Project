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


###################################### SHINY APP



ui=navbarPage("NBA features",
              tabPanel("First Page",
                       fluidPage( 
                         sidebarLayout(
                           sidebarPanel("Side Bar panel"),
                           mainPanel("Main panel, Output")
                         )
                       )
              ),
              tabPanel("Second Page",
                       fluidPage( 
                         sidebarLayout(
                           sidebarPanel("Side Bar panel"),
                           mainPanel("Main panel, Output")
                         )
                       )
              )
)


server <- function(input, output, session) {
  
}


# Run the application 
shinyApp(ui = ui, server = server)