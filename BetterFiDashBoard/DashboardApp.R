setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(sf)


load("../data/tennessee/tn_data.RData")

#create variable groupings for maps
tn_tract_vars <- tn_tract %>% 
  select("NAME", "n_lenders", "avg_income", "percent_noncitizen", "total_percent_highschool", "percent_veteran", "mediangrossrent", "Divorced", "Unemployed", "percent_black", "percent_hispaniclat")

#define important variables 
county_names <- c('All', unique(as.character(tn_tract$county)))

tn_vars <- c(as.character(names(tn_tract_vars %>% select(-"NAME"))))

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("BetterFi Interactive Dashboard"),
  
  navlistPanel(
    tabPanel("Introduction",
             h3("Here is our introduction")
    ),
    tabPanel("Interactive Graphs",
             h3("Here is our interactive graph panel"),
             fluidRow(
               column(3, "Variable Selection",
                      selectInput("scale", "Select County",
                                  choices = county_names,
                                  multiple = TRUE,
                                  selected = 'All'
                      ),
                      selectInput("graph_xvar", "Select X Variable",
                                  choices = tn_vars,
                                  selected = "n_lenders"),
                      selectInput("graph_yvar", "Select X Variable",
                                  choices = tn_vars[tn_vars != input$graph_xvar])
               ),
             ),
             column(9, "Graph Output")
             
    ),
    tabPanel("Interactive Maps",
             h3("Here is our interactive map panel")
    ),
    tabPanel("Interactive Vunerability Model",
             h3("Here is our interactive model panel")
    ),
    tabPanel("Meet The Team",
             h3("The BetterFi Team")
    ),
    widths = c(2, 10)
  )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  
}
  


# Run the application 
shinyApp(ui = ui, server = server)
