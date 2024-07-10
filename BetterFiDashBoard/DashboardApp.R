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
library(DT)

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
                        choices = c("Number Of Lenders", "Average Income", "Percentage Noncitizen", "Highschool Graduation Rate", "Percentage Veteran", "Median Gross Rent", "Percentage Divorced", "Unemployment Rate", "Percantage African American", "Percentage Hispanic/Latino"),
                        multiple = TRUE
                      ),
                      # This only pops up if variable 1 is selected
                      conditionalPanel(
                        # The condition is in JavaScript hence the weird format
                        condition = "input.vars.includes('Number Of Predatory Lenders')",
                        uiOutput("weight_lender"),
                      ),
                      # This only pops up if variable 2 is selected
                      conditionalPanel(
                        condition = "input.vars.includes('Average Income')",
                        uiOutput("weight_income"),
                      ),
                      # This only pops up if variable 3 is selected
                      conditionalPanel(
                        condition = "input.vars.includes('Percentage Noncitizen')",
                        uiOutput("weight_noncitizen"),
                      ),
                      # This only pops up if variable 4 is selected
                      conditionalPanel(
                        condition = "input.vars.includes('Highschool Graduation Rate')",
                        uiOutput("weight_highschool"),
                      ),
                      # This only pops up if variable 5 is selected
                      conditionalPanel(
                        condition = "input.vars.includes('Percentage Veteran')",
                        uiOutput("weight_veteran"),
                      ),
                      # This only pops up if variable 6 is selected
                      conditionalPanel(
                        condition = "input.vars.includes('Median Gross Rent')",
                        uiOutput("weight_mediangrossrent"),
                      ),
                      # This only pops up if variable 7 is selected
                      conditionalPanel(
                        condition = "input.vars.includes('Percentage Divorced')",
                        uiOutput("weight_divorced"),
                      ),
                      # This only pops up if variable 8 is selected
                      conditionalPanel(
                        condition = "input.vars.includes('Unemployment Rate')",
                        uiOutput("weight_unemployed"),
                      ),
                      # This only pops up if variable 9 is selected
                      conditionalPanel(
                        condition = "input.vars.includes('Percantage African American')",
                        uiOutput("weight_black"),
                      ),
                      # This only pops up if variable 10 is selected
                      conditionalPanel(
                        condition = "input.vars.includes('Percentage Hispanic/Latino')",
                        uiOutput("weight_hispaniclat"),
                      )
                      ),
               column(9, 
                      # actionButton("model_button", "Click to Run Model"),
                      # dataTableOutput("tract_vun_ranking_tn")
                      )
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
  #GRAPH SERVER=================================================================
  
  #GRAPHS - UPDATES Y VAR LSIT TO REMOVE SELECTED X VAR
  output$graph_yvar <- renderUI({
    selectizeInput("graph_yvar", "Select Y Variable",
                   choices = graph_vars[graph_vars != input$graph_xvar])
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
  
  
  #MODEL SERVER===================================================================
  # This is a reactive value that will store the sum of the weights and update as input changes
  weight_sum <- reactiveVal(0)
  
  # This updates the weight_sum variable when any weight inputs are changed
  weight_change_observer <- observe({
    # Vector of the weight values
    weights <- c(
      input$weight_lender,
      input$weight_income,
      input$weight_noncitizen,
      input$weight_highschool,
      input$weight_veteran,
      input$weight_mediangrossrent,
      input$weight_divorced,
      input$weight_unemployed,
      input$weight_black,
      input$weight_hispaniclat
    )
    # Setting the weight_sum variable to the sum of the weights and removing any NAs
    weight_sum(sum(weights, na.rm = TRUE))
  })
  
  output$weight_lender <- renderUI({
    sliderInput(
      inputId = "weight_lender",
      label = "Numer Of Predatory Lenders",
      min = 0,
      # If the input weight is null because it hasn't been loaded yet, set the max to 1
      # Otherwise, set the max to 1 minus the sum of the other weights
      max = ifelse(is.null(input$weight_lender), 1, 1 - (weight_sum() - input$weight_lender)),
      # If the input weight is null because it hasn't been loaded yet or if it is unselected from
      # the variable input, set it equal to 0, otherwise, set it equal to itself so that it doesn't
      # get reset to 0 when other weight inputs change
      value = ifelse(is.null(input$weight_lender) || !"1" %in% input$vars, 0, input$weight_lender),
      # Only increase or decrease the value in steps of 0.01 to prevent any horrible fractions and
      # scientific notation
      step = 0.01
    )
  })
  
  output$weight_income <- renderUI({
    sliderInput(
      inputId = "weight_income",
      label = "Average Income",
      min = 0,
      max = ifelse(is.null(input$weight_income), 1, 1 - (weight_sum() - input$weight_income)),
      value = ifelse(is.null(input$weight_income) || !"2" %in% input$vars, 0, input$weight_income),
      step = 0.01
    )
  })
  
  output$weight_noncitizen <- renderUI({
    sliderInput(
      inputId = "weight_noncitizen",
      label = "Percentage Noncitizen",
      min = 0,
      max = ifelse(is.null(input$weight_noncitizen), 1, 1 - (weight_sum() - input$weight_noncitizen)),
      value = ifelse(is.null(input$weight_noncitizen) || !"3" %in% input$vars, 0, input$weight_noncitizen),
      step = 0.01
    )
  })
  
  output$weight_highschool <- renderUI({
    sliderInput(
      inputId = "weight_highschool",
      label = "Highschool Graduation Rate",
      min = 0,
      max = ifelse(is.null(input$weight_highschool), 1, 1 - (weight_sum() - input$weight_highschool)),
      value = ifelse(is.null(input$weight_highschool) || !"4" %in% input$vars, 0, input$weight_highschool),
      step = 0.01
    )
  })
  
  output$weight_veteran <- renderUI({
    sliderInput(
      inputId = "weight_veteran",
      label = "Percentage Of Veterans",
      min = 0,
      max = ifelse(is.null(input$weight_veteran), 1, 1 - (weight_sum() - input$weight_veteran)),
      value = ifelse(is.null(input$weight_veteran) || !"5" %in% input$vars, 0, input$weight_veteran),
      step = 0.01
    )
  })
  
  output$weight_mediangrossrent <- renderUI({
    sliderInput(
      inputId = "weight_mediangrossrent",
      label = "Median Gross Rent",
      min = 0,
      max = ifelse(is.null(input$weight_mediangrossrent), 1, 1 - (weight_sum() - input$weight_mediangrossrent)),
      value = ifelse(is.null(input$weight_mediangrossrent) || !"6" %in% input$vars, 0, input$weight_mediangrossrent),
      step = 0.01
    )
  })
  
  output$weight_divorced <- renderUI({
    sliderInput(
      inputId = "weight_divorced",
      label = "Percentage Divorced ",
      min = 0,
      max = ifelse(is.null(input$weight_divorced), 1, 1 - (weight_sum() - input$weight_divorced)),
      value = ifelse(is.null(input$weight_divorced) || !"7" %in% input$vars, 0, input$weight_divorced),
      step = 0.01
    )
  })
  
  output$weight_unemployed <- renderUI({
    sliderInput(
      inputId = "weight_unemployed",
      label = "Unemployment Rate",
      min = 0,
      max = ifelse(is.null(input$weight_unemployed), 1, 1 - (weight_sum() - input$weight_unemployed)),
      value = ifelse(is.null(input$weight_unemployed) || !"8" %in% input$vars, 0, input$weight_unemployed),
      step = 0.01
    )
  })
  
  output$weight_black <- renderUI({
    sliderInput(
      inputId = "weight_black",
      label = "Percent African American",
      min = 0,
      max = ifelse(is.null(input$weight_black), 1, 1 - (weight_sum() - input$weight_black)),
      value = ifelse(is.null(input$weight_black) || !"9" %in% input$vars, 0, input$weight_black),
      step = 0.01
    )
  })
  
  output$weight_hispaniclat <- renderUI({
    sliderInput(
      inputId = "weight_hispaniclat",
      label = "Percent Hispanic/Latino",
      min = 0,
      max = ifelse(is.null(input$weight_hispaniclat), 1, 1 - (weight_sum() - input$weight_hispaniclat)),
      value = ifelse(is.null(input$weight_hispaniclat) || !"10" %in% input$vars, 0, input$weight_hispaniclat),
      step = 0.01
    )
  })
  
  #OUTPUT FOR MODEL GIVEN WEIGHT IMPUTS
  # observeEvent(input$model_button){
  #   
  #   
  # }
  #   
}



# Run the application 
shinyApp(ui = ui, server = server)
