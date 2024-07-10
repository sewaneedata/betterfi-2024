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
library(tmap)

load("../data/tennessee/tn_tract_dash.RData")

#create variable groupings for maps

#define important variables 
county_names <- c("All", sort(unique(as.character(tn_tract_dash$county))))

map_options <- c("n_lenders", names(tn_tract_dash %>% select(contains("_group"))))
map_options <- setdiff(map_options, "geometry")

graph_vars <- c(as.character(names(tn_tract_dash %>% 
                                     select(!contains("_group"))%>% 
                                     select(-"NAME") %>%
                                     select(-"county"))))
graph_vars <- setdiff(graph_vars, "geometry")


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
                                     choices = graph_vars,
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
               
               #COLUMN FOR SETTING MAP PARAMETERS
               column(3, "Set Map Parameters",
                      
                      #SELECT MAP VARIABLE
                      selectizeInput("map_var", "Heat Map Variable",
                                     choices = map_options,
                                     selected = "avg_income_group"
                      ),
                      
                      #SELECT COUNTY(S)
                      selectizeInput("county_map", "Which County",
                                     choices = county_names,
                                     selected = "HamiltonCounty")
               ),
               column(9, "Map Output",
                      tmapOutput("map"))
             )
    ),
    
    #MODEL PANNEL===============================================================
    tabPanel("Interactive Vunerability Model",
             h3("Here is our interactive model panel"),
             fluidRow(
               column(3, 
                      selectizeInput(
                        inputId = "vars",
                        label = "Select Desired Variables",
                        choices = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
                        multiple = TRUE
                      ),
                      # This only pops up if variable 1 is selected
                      conditionalPanel(
                        # The condition is in JavaScript hence the weird format
                        condition = "input.vars.includes('1')",
                        uiOutput("weight1"),
                      ),
                      # This only pops up if variable 2 is selected
                      conditionalPanel(
                        condition = "input.vars.includes('2')",
                        uiOutput("weight2"),
                      ),
                      # This only pops up if variable 3 is selected
                      conditionalPanel(
                        condition = "input.vars.includes('3')",
                        uiOutput("weight3"),
                      ),
                      # This only pops up if variable 4 is selected
                      conditionalPanel(
                        condition = "input.vars.includes('4')",
                        uiOutput("weight4"),
                      ),
                      # This only pops up if variable 5 is selected
                      conditionalPanel(
                        condition = "input.vars.includes('5')",
                        uiOutput("weight5"),
                      ),
                      # This only pops up if variable 6 is selected
                      conditionalPanel(
                        condition = "input.vars.includes('6')",
                        uiOutput("weight6"),
                      ),
                      # This only pops up if variable 7 is selected
                      conditionalPanel(
                        condition = "input.vars.includes('7')",
                        uiOutput("weight7"),
                      ),
                      # This only pops up if variable 8 is selected
                      conditionalPanel(
                        condition = "input.vars.includes('8')",
                        uiOutput("weight8"),
                      ),
                      # This only pops up if variable 9 is selected
                      conditionalPanel(
                        condition = "input.vars.includes('9')",
                        uiOutput("weight9"),
                      ),
                      # This only pops up if variable 10 is selected
                      conditionalPanel(
                        condition = "input.vars.includes('10')",
                        uiOutput("weight10"),
                      )
                      ),
               column(9, )
             )
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
                   choices = graph_vars[graph_vars != input$graph_xvar])
  })
  
  #GRAPH SERVER=================================================================
  output$graph <- renderPlot({
    
    # Check the values that this graph relies on for debugging
    # print(input$graph_xvar)
    # print(input$graph_yvar)
    # print(input$county)
    
    # Only creating the graph if the required inputs exist
    if(!is.null(input$graph_xvar) && !is.null(input$graph_yvar) && !is.null(input$county)){
      
      #SELECT COUNTY(S)
      if(length(input$county) == 1 && input$county == 'All'){
        graph_county <- county_names
      } else {
        graph_county <- input$county
      }
      
      #create graph
      ggplot(tn_tract_dash %>% filter(county %in% graph_county),
             # aes_string is used since the inputs are stored as a string
             aes_string(x = input$graph_xvar, y = input$graph_yvar ))+
        geom_point(aes(col = county))+
        #this add a linear model line of best fit
        geom_smooth(method = "lm", se = FALSE)
    }
    
  })
  
  #MAP SERVER===================================================================
  output$map <- renderTmap({
    
    #CREATE COUNTY MAP INPUT
    if(length(input$county_map) == 1 && input$county_map == 'All'){
      map_county <- county_names
    } else {
      map_county <- input$county_map
    }
    
    #CREATE MAP VARIABLE INPUT
    this_map_var = input$map_var
    
    #MAKE MAP!-----
    tmap_mode("view")
    tm_shape(tn_tract_dash %>% filter(county %in% map_county))+
      tm_polygons(col = this_map_var, palette = "YlOrRd")
  }) 
  
  
  #MAP SERVER===================================================================
  # This is a reactive value that will store the sum of the weights and update as input changes
  weight_sum <- reactiveVal(0)
  
  # This updates the weight_sum variable when any weight inputs are changed
  weight_change_observer <- observe({
    # Vector of the weight values
    weights <- c(
      input$weight1,
      input$weight2,
      input$weight3,
      input$weight4,
      input$weight5,
      input$weight6,
      input$weight7,
      input$weight8,
      input$weight9,
      input$weight10
    )
    # Setting the weight_sum variable to the sum of the weights and removing any NAs
    weight_sum(sum(weights, na.rm = TRUE))
  })
  
  output$weight1 <- renderUI({
    sliderInput(
      inputId = "weight1",
      label = "Weight 1",
      min = 0,
      # If the input weight is null because it hasn't been loaded yet, set the max to 1
      # Otherwise, set the max to 1 minus the sum of the other weights
      max = ifelse(is.null(input$weight1), 1, 1 - (weight_sum() - input$weight1)),
      # If the input weight is null because it hasn't been loaded yet or if it is unselected from
      # the variable input, set it equal to 0, otherwise, set it equal to itself so that it doesn't
      # get reset to 0 when other weight inputs change
      value = ifelse(is.null(input$weight1) || !"1" %in% input$vars, 0, input$weight1),
      # Only increase or decrease the value in steps of 0.01 to prevent any horrible fractions and
      # scientific notation
      step = 0.01
    )
  })
  
  output$weight2 <- renderUI({
    sliderInput(
      inputId = "weight2",
      label = "Weight 2",
      min = 0,
      max = ifelse(is.null(input$weight2), 1, 1 - (weight_sum() - input$weight2)),
      value = ifelse(is.null(input$weight2) || !"2" %in% input$vars, 0, input$weight2),
      step = 0.01
    )
  })
  
  output$weight3 <- renderUI({
    sliderInput(
      inputId = "weight3",
      label = "Weight 3",
      min = 0,
      max = ifelse(is.null(input$weight3), 1, 1 - (weight_sum() - input$weight3)),
      value = ifelse(is.null(input$weight3) || !"3" %in% input$vars, 0, input$weight3),
      step = 0.01
    )
  })
  
  output$weight4 <- renderUI({
    sliderInput(
      inputId = "weight4",
      label = "Weight 4",
      min = 0,
      max = ifelse(is.null(input$weight4), 1, 1 - (weight_sum() - input$weight4)),
      value = ifelse(is.null(input$weight4) || !"4" %in% input$vars, 0, input$weight4),
      step = 0.01
    )
  })
  
  output$weight5 <- renderUI({
    sliderInput(
      inputId = "weight5",
      label = "Weight 5",
      min = 0,
      max = ifelse(is.null(input$weight5), 1, 1 - (weight_sum() - input$weight5)),
      value = ifelse(is.null(input$weight5) || !"5" %in% input$vars, 0, input$weight5),
      step = 0.01
    )
  })
  
  output$weight6 <- renderUI({
    sliderInput(
      inputId = "weight6",
      label = "Weight 6",
      min = 0,
      max = ifelse(is.null(input$weight6), 1, 1 - (weight_sum() - input$weight6)),
      value = ifelse(is.null(input$weight6) || !"6" %in% input$vars, 0, input$weight6),
      step = 0.01
    )
  })
  
  output$weight7 <- renderUI({
    sliderInput(
      inputId = "weight7",
      label = "Weight 7",
      min = 0,
      max = ifelse(is.null(input$weight7), 1, 1 - (weight_sum() - input$weight7)),
      value = ifelse(is.null(input$weight7) || !"7" %in% input$vars, 0, input$weight7),
      step = 0.01
    )
  })
  
  output$weight8 <- renderUI({
    sliderInput(
      inputId = "weight8",
      label = "Weight 8",
      min = 0,
      max = ifelse(is.null(input$weight8), 1, 1 - (weight_sum() - input$weight8)),
      value = ifelse(is.null(input$weight8) || !"8" %in% input$vars, 0, input$weight8),
      step = 0.01
    )
  })
  
  output$weight9 <- renderUI({
    sliderInput(
      inputId = "weight9",
      label = "Weight 9",
      min = 0,
      max = ifelse(is.null(input$weight9), 1, 1 - (weight_sum() - input$weight9)),
      value = ifelse(is.null(input$weight9) || !"9" %in% input$vars, 0, input$weight9),
      step = 0.01
    )
  })
  
  output$weight10 <- renderUI({
    sliderInput(
      inputId = "weight10",
      label = "Weight 10",
      min = 0,
      max = ifelse(is.null(input$weight10), 1, 1 - (weight_sum() - input$weight10)),
      value = ifelse(is.null(input$weight10) || !"10" %in% input$vars, 0, input$weight10),
      step = 0.01
    )
  })
  
  #OUTPUT FOR MODEL GIVEN WEIGHT IMPUTS
  output$vul_model
    
}



# Run the application 
shinyApp(ui = ui, server = server)
