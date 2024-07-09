setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/

library(shiny)
library(dplyr)
library(ggplot2)
library(sf)
library(stringr)

load("../data/tennessee/tn_tract_dash.RData")

#create variable groupings for maps

#define important variables 
county_names <- c("All", sort(unique(as.character(tn_tract_dash$county))))

map_options <- c()

tn_vars <- c(as.character(names(tn_tract_vars %>% select(-"NAME"))))

# Define UI for application that draws a histogram
ui <- fluidPage(
  #TITLE FOR DASHBOARD
  titlePanel("BetterFi Interactive Dashboard"),
  
  #CREATES SLIDE/PANNELS FOR DATA DISPLAYS
  navlistPanel(
    #INTRO PANNEL===============================================================
    tabPanel("Introduction",
             h3("Here is our introduction")
    ),
    
    #GRAPH PANNEL===============================================================
    tabPanel("Interactive Graphs",
             h3("Here is our interactive graph panel"),
             fluidRow(
               column(3, "Variable Selection",
                      selectizeInput("county", "Select County",
                                     choices = county_names,
                                     multiple = TRUE,
                                     selected = "AndersonCounty"
                      ),
                      selectizeInput("graph_xvar", "Select X Variable",
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
    
    #MAPS PANNEL================================================================
    tabPanel("Interactive Maps",
             h3("Here is our interactive map panel"),
             fluidRow(
               column(3, "Set Map Parameters",
                      selectizeInput("map_var", "Heat Map Variable",
                                     choices = tn_vars,
                                     selected = "n_lenders")),
               column(9, "Map Output",
                      imageOutput("map"))
             )
    ),
    
    #MODEL PANNEL===============================================================
    tabPanel("Interactive Vunerability Model",
             h3("Here is our interactive model panel")
    ),
    
    #TEAM PANNEL================================================================
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
    selectizeInput("graph_yvar", "Select Y Variable",
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
      ggplot(tn_tract_dash %>% filter(county %in% this_county),
             # aes_string is used since the inputs are stored as a string
             aes_string(x = input$graph_xvar, y = input$graph_yvar ))+
        geom_point(aes(col = county))+
        #this add a linear model line of best fit
        geom_smooth(method = "lm", se = FALSE)
    }
    
  })
  # Example of reactive text
  # output$text <- renderText({
  #   paste0("The selected input is ", input$graph_xvar)
  # })
 
  output$map <- renderImage({
    
    #SET SELECTED VARIABLE FOR MAP
    this_map <- input$map_var
    
    #MAKE MAP!-----
    tmap_mode("view")
    tm_shape(tn_tract_dash)+
      tm_polygon(col = this_map, palette = "YlOrRd")
  }) 
}



# Run the application 
shinyApp(ui = ui, server = server)
