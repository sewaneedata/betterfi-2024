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
library(stringr)

load("../data/tennessee/tn_data.RData")

#create variable groupings for maps
tn_tract_vars <- tn_tract %>% 
  select("NAME", "n_lenders", "avg_income", "percent_noncitizen", "total_percent_highschool", "percent_veteran", "mediangrossrent", "Divorced", "Unemployed", "percent_black", "percent_hispaniclat")

#define important variables 
county_names <- c("All", sort(unique(as.character(tn_tract$county))))

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
                      selectInput("county", "Select County",
                                  choices = county_names,
                                  multiple = TRUE,
                                  selected = "AndersonCounty"
                      ),
                      selectInput("graph_xvar", "Select X Variable",
                                  choices = tn_vars,
                                  selected = "avg_income"),
                      uiOutput("graph_yvar")
                      # Reactive text example UI element
                      # uiOutput("text")
               ),
             ),
             column(9, "Graph Output",
                    plotOutput("graph")
                    )
             
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
  
  #GRAPHS - UPDATES Y VAR LSIT TO REMOVE SELECTED X VAR
  output$graph_yvar <- renderUI({
    selectInput("graph_yvar", "Select Y Variable",
                choices = tn_vars[tn_vars != input$graph_xvar])
  })
  
  output$graph <- renderPlot({
    
    # Check the values that this graph relies on for debugging
    # print(input$graph_xvar)
    # print(input$graph_yvar)
    # print(input$county)
    
    # Only creating the graph if the required inputs exist
    if(!is.null(input$graph_xvar) && !is.null(input$graph_yvar) && !is.null(input$county)){
      
      #SELECT COUNTY(S)
      if(length(input$county) == 1 && input$county == 'All'){
        this_county <- county_names
      } else {
        this_county <- input$county
      }
      
      #create graph
      ggplot(tn_tract %>% filter(county %in% this_county),
             # aes_string is used since the inputs are stored as a string
             aes_string(x = input$graph_xvar, y = input$graph_yvar ))+
        geom_point(aes(col = county))+
        #this add a linear model line of best fit
        geom_smooth(method = "lm")
    }
    
  })
  # Example of reactive text
  # output$text <- renderText({
  #   paste0("The selected input is ", input$graph_xvar)
  # })
  
}
  


# Run the application 
shinyApp(ui = ui, server = server)
