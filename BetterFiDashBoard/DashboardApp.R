# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/

library(shiny)
library(ggplot2)
library(sf)
library(stringr)
library(tmap)
library(DT)
library(tidyverse)

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


betterfi_model <- function(counties, weight_lender, weight_income, weight_noncitizen,
                           weight_highschool, weight_veteran, weight_mediangrossrent,
                           weight_divorced, weight_unemployed, weight_black,
                           weight_hispaniclat){
  
  # Creating a list of relevant variables to select based on whether or not the weight for the
  # variable is 0
  selected_vars <- c()
  if(weight_lender != 0) selected_vars <- append(selected_vars, "vun_lender")
  if(weight_income != 0) selected_vars <- append(selected_vars, "vun_income")
  if(weight_noncitizen != 0) selected_vars <- append(selected_vars, "vun_noncitizen")
  if(weight_highschool != 0) selected_vars <- append(selected_vars, "vun_highschool")
  if(weight_veteran != 0) selected_vars <- append(selected_vars, "vun_veteran")
  if(weight_mediangrossrent != 0) selected_vars <- append(selected_vars, "vun_mediangrossrent")
  if(weight_divorced != 0) selected_vars <- append(selected_vars, "vun_divorced")
  if(weight_unemployed != 0) selected_vars <- append(selected_vars, "vun_unemployed")
  if(weight_black != 0) selected_vars <- append(selected_vars, "vun_black")
  if(weight_hispaniclat!= 0) selected_vars <- append(selected_vars, "vun_hispaniclat")
  
  # Filter to counties of interest
  tn_tract_filtered <- tn_tract_dash %>% 
    filter(county %in% counties)
  
  # Run the vulnerability scores
  #CREATE VARIABLE BUCKETS
  
  #n_lenders (DONE)
  varlist_lenders <- tn_tract_filtered %>% 
    select("NAME", "n_lenders", "county") %>% 
    mutate(max_lender = max(n_lenders, na.rm = TRUE)) %>% 
    mutate(vun_lender = n_lenders/max_lender) %>% 
    arrange(desc(vun_lender)) %>% 
    select("NAME", "vun_lender", "county") %>% 
    st_drop_geometry()
  
  #avg_income
  varlist_income <- tn_tract_filtered %>% 
    select("NAME", "avg_income") %>% 
    arrange(avg_income) %>% 
    mutate(vun_income = avg_income/max(avg_income, na.rm = TRUE)) %>% 
    mutate(vun_income = 1-vun_income) %>% 
    mutate(vun_income = vun_income/max(vun_income, na.rm = TRUE))
  
  #percent_noncitizen (DONE)
  varlist_noncitizen <- tn_tract_filtered %>% 
    select("NAME", "percent_noncitizen") %>% 
    mutate(max_percent_noncitizen = max(percent_noncitizen, na.rm = TRUE)) %>% 
    mutate(vun_noncitizen = percent_noncitizen/max_percent_noncitizen) %>% 
    arrange(desc(vun_noncitizen))
  
  #total_percent_highschool (ISSUE: VUN SCORE DOES NOT BEGIN AT ZERO)
  varlist_highschool <- tn_tract_filtered %>% 
    select("NAME", "total_percent_highschool") %>% 
    mutate(total_percent_highschool = as.numeric(total_percent_highschool)) %>% 
    mutate(max_percent_highschool = max(total_percent_highschool, na.rm=TRUE)) %>% 
    mutate(vun_highschool = total_percent_highschool/max_percent_highschool) %>% 
    mutate(vun_highschool = 1 - vun_highschool) %>% 
    mutate(vun_highschool = vun_highschool/max(vun_highschool, na.rm=TRUE)) %>% 
    arrange(desc(vun_highschool))
  
  #percent veteran
  varlist_veteran <- tn_tract_filtered %>% 
    select("NAME", "percent_veteran") %>% 
    mutate(percent_veteran = as.numeric(gsub("%", "", percent_veteran))) %>% 
    mutate(percent_veteran = as.numeric(percent_veteran)) %>% 
    mutate(max_percent_veteran = max(percent_veteran, na.rm = TRUE)) %>% 
    mutate(vun_veteran = percent_veteran/max_percent_veteran) %>% 
    arrange(desc(vun_veteran))
  
  #median gross rent 
  varlist_mediangrossrent <- tn_tract_filtered %>% 
    select("NAME", "mediangrossrent") %>% 
    mutate(mediangrossrent = as.numeric(mediangrossrent)) %>% 
    mutate(vun_mediangrossrent = mediangrossrent/max(mediangrossrent, na.rm = TRUE)) %>%
    mutate(vun_mediangrossrent = 1 - vun_mediangrossrent) %>% 
    mutate(vun_mediangrossrent = vun_mediangrossrent/max(vun_mediangrossrent, na.rm=TRUE)) %>% 
    arrange(desc(vun_mediangrossrent))
  
  #percent divorced
  varlist_divorced <- tn_tract_filtered %>% 
    select("NAME", "Divorced") %>%
    mutate(Divorced = as.numeric(gsub("%", "", Divorced))) %>%
    rename(percent_divorced = Divorced) %>% 
    mutate(vun_divorced = percent_divorced/max(percent_divorced, na.rm = TRUE)) %>% 
    arrange(desc(vun_divorced))
  
  #unemployment
  varlist_unemployment <- tn_tract_filtered %>% 
    select("NAME", "Unemployed") %>%
    rename(percent_unemployed = Unemployed) %>%
    mutate(percent_unemployed = as.numeric(gsub("%", "", percent_unemployed))) %>%
    mutate(vun_unemployed = percent_unemployed/max(percent_unemployed, na.rm = TRUE)) %>% 
    arrange(desc(vun_unemployed))
  
  #black_percent
  varlist_black <- tn_tract_filtered %>% 
    select("NAME", "percent_black") %>% 
    mutate(vun_black = percent_black/max(percent_black, na.rm = TRUE)) %>% 
    arrange(desc(vun_black))
  
  #hispaniclat_percent
  varlist_hispaniclat <- tn_tract_filtered %>% 
    select("NAME", "percent_hispaniclat") %>% 
    mutate(vun_hispaniclat = percent_hispaniclat/max(percent_hispaniclat, na.rm = TRUE)) %>% 
    arrange(desc(vun_hispaniclat))
  
  #CREATE DATAFRAM WITH ALL VUN SCORES
  varlist_vun <- varlist_lenders %>% 
    left_join(varlist_income <- st_drop_geometry(varlist_income) %>% select("NAME", "vun_income"), by = "NAME") %>% 
    left_join(varlist_noncitizen <- st_drop_geometry(varlist_noncitizen) %>% select("NAME", "vun_noncitizen"), by = "NAME") %>% 
    left_join(varlist_highschool <- st_drop_geometry(varlist_highschool) %>% select("NAME", "vun_highschool"), by = "NAME") %>% 
    left_join(varlist_veteran <- st_drop_geometry(varlist_veteran) %>% select("NAME", "vun_veteran"), by = "NAME") %>% 
    left_join(varlist_mediangrossrent <- st_drop_geometry(varlist_mediangrossrent) %>% select("NAME", "vun_mediangrossrent"), by = "NAME") %>% 
    left_join(varlist_divorced <- st_drop_geometry(varlist_divorced) %>% select("NAME", "vun_divorced"), by = "NAME") %>% 
    left_join(varlist_unemployment <- st_drop_geometry(varlist_unemployment) %>% select("NAME", "vun_unemployed"), by = "NAME") %>%
    left_join(varlist_black <- st_drop_geometry(varlist_black) %>% select("NAME", "vun_black"), by = "NAME") %>%
    left_join(varlist_hispaniclat <- st_drop_geometry(varlist_hispaniclat) %>% select("NAME", "vun_hispaniclat"), by = "NAME")
  
  # Scale those scores by the weights
  varlist_vun <- varlist_vun %>%
    # Filter out NAs in the relevant weight variables bc they skew the score if treated as 0
    drop_na(any_of(selected_vars)) %>% 
    mutate(weighted_vun = (vun_lender)*(weight_lender) + 
             (vun_income)*(weight_income) +
             (vun_noncitizen)*(weight_noncitizen) +
             (vun_highschool)*(weight_highschool) +
             (vun_veteran)*(weight_veteran) +
             (vun_mediangrossrent)*(weight_mediangrossrent) +
             (vun_divorced)*(weight_divorced) +
             (vun_unemployed)*(weight_unemployed) +
             (vun_black)*(weight_black) +
             (vun_hispaniclat)*(weight_hispaniclat)
    ) %>%
    arrange(desc(weighted_vun))
  
  model_results <- varlist_vun %>% 
    select(NAME, weighted_vun, county, any_of(selected_vars)) %>% 
    arrange(desc(weighted_vun))
  
  
  # Spit out results -- name it model_results
  return(model_results)
}


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
                      selectizeInput("model_counties", "Select Desired Counties",
                                     choices = county_names,
                                     multiple = TRUE,
                                     selected = "All"
                      ),
                      selectizeInput(
                        inputId = "vars",
                        label = "Select Desired Variables",
                        choices = c("All", "Number Of Predatory Lenders", "Average Income", "Percentage Noncitizen", "Highschool Graduation Rate", "Percentage Of Veterans", "Median Gross Rent", "Percentage Divorced", "Unemployment Rate", "Percantage African American", "Percentage Hispanic/Latino"),
                        multiple = TRUE,
                        selected = c("All")
                      ),
                      # This only pops up if variable 1 is selected
                      conditionalPanel(
                        # The condition is in JavaScript hence the weird format
                        condition = "input.vars.includes('Number Of Predatory Lenders') || (input.vars.includes('All') && input.vars.length == 1)",
                        uiOutput("weight_lender"),
                      ),
                      # This only pops up if variable 2 is selected
                      conditionalPanel(
                        condition = "input.vars.includes('Average Income') || (input.vars.includes('All') && input.vars.length == 1)",
                        uiOutput("weight_income"),
                      ),
                      # This only pops up if variable 3 is selected
                      conditionalPanel(
                        condition = "input.vars.includes('Percentage Noncitizen') || (input.vars.includes('All') && input.vars.length == 1)",
                        uiOutput("weight_noncitizen"),
                      ),
                      # This only pops up if variable 4 is selected
                      conditionalPanel(
                        condition = "input.vars.includes('Highschool Graduation Rate') || (input.vars.includes('All') && input.vars.length == 1)",
                        uiOutput("weight_highschool"),
                      ),
                      # This only pops up if variable 5 is selected
                      conditionalPanel(
                        condition = "input.vars.includes('Percentage Veteran') || (input.vars.includes('All') && input.vars.length == 1)",
                        uiOutput("weight_veteran"),
                      ),
                      # This only pops up if variable 6 is selected
                      conditionalPanel(
                        condition = "input.vars.includes('Median Gross Rent') || (input.vars.includes('All') && input.vars.length == 1)",
                        uiOutput("weight_mediangrossrent"),
                      ),
                      # This only pops up if variable 7 is selected
                      conditionalPanel(
                        condition = "input.vars.includes('Percentage Divorced') || (input.vars.includes('All') && input.vars.length == 1)",
                        uiOutput("weight_divorced"),
                      ),
                      # This only pops up if variable 8 is selected
                      conditionalPanel(
                        condition = "input.vars.includes('Unemployment Rate') || (input.vars.includes('All') && input.vars.length == 1)",
                        uiOutput("weight_unemployed"),
                      ),
                      # This only pops up if variable 9 is selected
                      conditionalPanel(
                        condition = "input.vars.includes('Percantage African American') || (input.vars.includes('All') && input.vars.length == 1)",
                        uiOutput("weight_black"),
                      ),
                      # This only pops up if variable 10 is selected
                      conditionalPanel(
                        condition = "input.vars.includes('Percentage Hispanic/Latino') || (input.vars.includes('All') && input.vars.length == 1)",
                        uiOutput("weight_hispaniclat"),
                      )
               ),
               column(9, 
                      actionButton("model_button", "Click to Run Model"))
               
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
      label = "Number Of Predatory Lenders",
      min = 0,
      # If the input weight is null because it hasn't been loaded yet, set the max to 1
      # Otherwise, set the max to 1 minus the sum of the other weights
      max = ifelse(is.null(input$weight_lender), 1, 1 - (weight_sum() - input$weight_lender)),
      # If the input weight is null because it hasn't been loaded yet or if it is unselected from
      # the variable input and the variable input is not just "All", set it equal to 0, otherwise,
      # set it equal to itself so that it doesn't
      # get reset to 0 when other weight inputs change
      value = ifelse(is.null(input$weight_lender) | (!"Number Of Predatory Lenders" %in% input$vars & !"All" %in% input$vars & length(input$vars != 1)), 0, input$weight_lender),
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
      value = ifelse(is.null(input$weight_income) | (!"Average Income" %in% input$vars & !"All" %in% input$vars & length(input$vars != 1)), 0, input$weight_income),
      step = 0.01
    )
  })
  
  output$weight_noncitizen <- renderUI({
    sliderInput(
      inputId = "weight_noncitizen",
      label = "Percentage Noncitizen",
      min = 0,
      max = ifelse(is.null(input$weight_noncitizen), 1, 1 - (weight_sum() - input$weight_noncitizen)),
      value = ifelse(is.null(input$weight_noncitizen) | (!"Percentage Noncitizen" %in% input$vars & !"All" %in% input$vars & length(input$vars != 1)), 0, input$weight_noncitizen),
      step = 0.01
    )
  })
  
  output$weight_highschool <- renderUI({
    sliderInput(
      inputId = "weight_highschool",
      label = "Highschool Graduation Rate",
      min = 0,
      max = ifelse(is.null(input$weight_highschool), 1, 1 - (weight_sum() - input$weight_highschool)),
      value = ifelse(is.null(input$weight_highschool) | (!"Highschool Graduation Rate" %in% input$vars & !"All" %in% input$vars & length(input$vars != 1)), 0, input$weight_highschool),
      step = 0.01
    )
  })
  
  output$weight_veteran <- renderUI({
    sliderInput(
      inputId = "weight_veteran",
      label = "Percentage Of Veterans",
      min = 0,
      max = ifelse(is.null(input$weight_veteran), 1, 1 - (weight_sum() - input$weight_veteran)),
      value = ifelse(is.null(input$weight_veteran) | (!"Percentage Of Veterans" %in% input$vars & !"All" %in% input$vars & length(input$vars != 1)), 0, input$weight_veteran),
      step = 0.01
    )
  })
  
  output$weight_mediangrossrent <- renderUI({
    sliderInput(
      inputId = "weight_mediangrossrent",
      label = "Median Gross Rent",
      min = 0,
      max = ifelse(is.null(input$weight_mediangrossrent), 1, 1 - (weight_sum() - input$weight_mediangrossrent)),
      value = ifelse(is.null(input$weight_mediangrossrent) | (!"Median Gross Rent" %in% input$vars & !"All" %in% input$vars & length(input$vars != 1)), 0, input$weight_mediangrossrent),
      step = 0.01
    )
  })
  
  output$weight_divorced <- renderUI({
    sliderInput(
      inputId = "weight_divorced",
      label = "Percentage Divorced",
      min = 0,
      max = ifelse(is.null(input$weight_divorced), 1, 1 - (weight_sum() - input$weight_divorced)),
      value = ifelse(is.null(input$weight_divorced) | (!"Percentage Divorced" %in% input$vars & !"All" %in% input$vars & length(input$vars != 1)), 0, input$weight_divorced),
      step = 0.01
    )
  })
  
  output$weight_unemployed <- renderUI({
    sliderInput(
      inputId = "weight_unemployed",
      label = "Unemployment Rate",
      min = 0,
      max = ifelse(is.null(input$weight_unemployed), 1, 1 - (weight_sum() - input$weight_unemployed)),
      value = ifelse(is.null(input$weight_unemployed) | (!"Unemployment Rate" %in% input$vars & !"All" %in% input$vars & length(input$vars != 1)), 0, input$weight_unemployed),
      step = 0.01
    )
  })
  
  output$weight_black <- renderUI({
    sliderInput(
      inputId = "weight_black",
      label = "Percent African American",
      min = 0,
      max = ifelse(is.null(input$weight_black), 1, 1 - (weight_sum() - input$weight_black)),
      value = ifelse(is.null(input$weight_black) | (!"Percent African American" %in% input$vars & !"All" %in% input$vars & length(input$vars != 1)), 0, input$weight_black),
      step = 0.01
    )
  })
  
  output$weight_hispaniclat <- renderUI({
    sliderInput(
      inputId = "weight_hispaniclat",
      label = "Percent Hispanic/Latino",
      min = 0,
      max = ifelse(is.null(input$weight_hispaniclat), 1, 1 - (weight_sum() - input$weight_hispaniclat)),
      value = ifelse(is.null(input$weight_hispaniclat) | (!"Percent Hispanic/Latino" %in% input$vars & !"All" %in% input$vars & length(input$vars != 1)), 0, input$weight_hispaniclat),
      step = 0.01
    )
  })
  
  #OUTPUT FOR MODEL GIVEN WEIGHT IMPUTS
  observeEvent(input$model_button, {
    # Creating an empty vector to store county input for model
    counties <- c()
    # If the only selected option is "All", get all of the county names and use
    # them for the model
    if(length(input$model_counties) == 1 && input$model_counties == 'All'){
      # Adding all of the county names to the vector and removing "All"
      counties <- county_names[county_names != 'All']
      # Running the model
      model_results <- betterfi_model(
        counties = counties,
        weight_lender = input$weight_lender,
        weight_income = input$weight_income,
        weight_noncitizen = input$weight_noncitizen,
        weight_highschool = input$weight_highschool,
        weight_veteran = input$weight_veteran,
        weight_mediangrossrent = input$weight_mediangrossrent,
        weight_divorced = input$weight_divorced,
        weight_unemployed = input$weight_unemployed,
        weight_black = input$weight_black,
        weight_hispaniclat = input$weight_hispaniclat 
      )
      # Printing the model results to the console
      print(model_results)
    }
    # If "All" is not the only selected county then only use the individually specified
    # counties that were selected
    else {
      # Getting the selected counties and removing "All" if it is still selected
      counties <- input$model_counties[input$model_counties != 'All']
      # Running the model
      model_results <- betterfi_model(
        counties = counties,
        weight_lender = input$weight_lender,
        weight_income = input$weight_income,
        weight_noncitizen = input$weight_income,
        weight_highschool = input$weight_highschool,
        weight_veteran = input$weight_veteran,
        weight_mediangrossrent = input$weight_mediangrossrent,
        weight_divorced = input$weight_divorced,
        weight_unemployed = input$weight_unemployed,
        weight_black = input$weight_black,
        weight_hispaniclat = input$weight_hispanicla 
      )
      # Printing the model results to the console
      print(model_results)
    }
  })
}



# Run the application 
shinyApp(ui = ui, server = server)
