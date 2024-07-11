#load necessary libraries:
library(shiny)
library(ggplot2)
library(sf)
library(stringr)
library(tmap)
library(DT)
library(tidyverse)

#call Dashboard Data
load("../data/tennessee/tn_tract_dash.RData")

# These NAs actually do represent zero so it should be fine to change them to 0
tn_tract_dash <-tn_tract_dash %>% 
  mutate(n_lenders = ifelse(is.na(n_lenders), 0, n_lenders))

#define important variables 

#creates a vector containing all unique county names, as well an option called All
county_names <- c("All", sort(unique(as.character(tn_tract_dash$county))))

#creates a vector containing all selectable model vars
model_vars <- c("All", "weight_lender", "weight_income", "weight_noncitizen", "weight_highschool",
                "weight_veteran", "weight_mediangrossrent", "weight_divorced", "weight_unemployed",
                "weight_black", "weight_hispaniclat")
names(model_vars) <- c("All", "Number Of Predatory Lenders", "Average Income",
                       "Percentage Noncitizen", "Highschool Graduation Rate",
                       "Percentage Of Veterans", "Median Gross Rent", "Percentage Divorced",
                       "Unemployment Rate", "Percantage African American",
                       "Percentage Hispanic/Latino")

#creates a vector containing all options for the maps panel of the dashboard
map_options <- c("n_lenders", names(tn_tract_dash %>% select(contains("_group"))))
map_options <- setdiff(map_options, "geometry")

#creates a vector containing all the possible variables to be used in the 
#graph panel of the dash board
graph_vars <- c(as.character(names(tn_tract_dash %>% 
                                     select(!contains("_group"))%>% 
                                     select(-"NAME") %>%
                                     select(-"county"))))
graph_vars <- setdiff(graph_vars, "geometry")


#BETTERFI_MODEL=================================================================

#the betterfi_model is a functions that takes county and values for all 
#weights as inputs.
#the model outputs a datatable containing all census tracts that contain NON-NA 
#values for all selected variables included in the model.
#it is EXTREMELY important that we exclude census tracts that are missing variables 
#because the missing values would be counted as a Zero in the vulnerability score, 
#which artifically makes the census tract appear "less vulnerable"

betterfi_model <- function(counties, weight_lender, weight_income, weight_noncitizen,
                           weight_highschool, weight_veteran, weight_mediangrossrent,
                           weight_divorced, weight_unemployed, weight_black,
                           weight_hispaniclat){
  
  # Creating a list of relevant variables to select based on whether or not the weight for the
  # variable is 0
  #selected_vars is used just before the final vulnerability score calculation
  #it is used ensure that any rows containing NAs are dropped before calculating 
  #the relative vulnerability scores
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
  
  # Filter for only counties that the user has selected
  tn_tract_filtered <- tn_tract_dash %>% 
    filter(county %in% counties)
  
  # Calculate all vulnerability scores for all variables for the tracts within 
  # the selected counties
  
  #CREATE VARIABLE BUCKETS
  
  #n_lenders
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
  
  #percent_noncitizen
  varlist_noncitizen <- tn_tract_filtered %>% 
    select("NAME", "percent_noncitizen") %>% 
    mutate(max_percent_noncitizen = max(percent_noncitizen, na.rm = TRUE)) %>% 
    mutate(vun_noncitizen = percent_noncitizen/max_percent_noncitizen) %>% 
    arrange(desc(vun_noncitizen))
  
  #total_percent_highschool
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
  
  #CREATE DATAFRAME WITH ALL VUN SCORES
  varlist_vun <- varlist_lenders %>% 
    left_join(
      varlist_income <- st_drop_geometry(varlist_income) %>%
        select("NAME", "vun_income"), by = "NAME"
    ) %>% 
    left_join(
      varlist_noncitizen <- st_drop_geometry(varlist_noncitizen) %>%
        select("NAME", "vun_noncitizen"), by = "NAME"
    ) %>% 
    left_join(
      varlist_highschool <- st_drop_geometry(varlist_highschool) %>%
        select("NAME", "vun_highschool"), by = "NAME"
    ) %>% 
    left_join(
      varlist_veteran <- st_drop_geometry(varlist_veteran) %>%
        select("NAME", "vun_veteran"), by = "NAME"
    ) %>% 
    left_join(
      varlist_mediangrossrent <- st_drop_geometry(varlist_mediangrossrent) %>%
        select("NAME", "vun_mediangrossrent"), by = "NAME"
    ) %>% 
    left_join(
      varlist_divorced <- st_drop_geometry(varlist_divorced) %>%
        select("NAME", "vun_divorced"), by = "NAME"
    ) %>% 
    left_join(
      varlist_unemployment <- st_drop_geometry(varlist_unemployment) %>%
        select("NAME", "vun_unemployed"), by = "NAME"
    ) %>%
    left_join(
      varlist_black <- st_drop_geometry(varlist_black) %>%
        select("NAME", "vun_black"), by = "NAME"
    ) %>%
    left_join(
      varlist_hispaniclat <- st_drop_geometry(varlist_hispaniclat) %>%
        select("NAME", "vun_hispaniclat"), by = "NAME"
    )
  
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
#END BETTERFI_MODEL=============================================================


# Define UI for application 
ui <- fluidPage(
  #TITLE FOR DASHBOARD
  titlePanel("BetterFi Interactive Dashboard"),
  
  #CREATES SLIDE/PANNELS FOR DATA DISPLAYS
  navlistPanel(
    #INTRO PANNEL UI------------------------------------------------------------
    tabPanel("Introduction",
             h3("Here is our introduction")
    ),
    
    #GRAPH PANNE UI-------------------------------------------------------------
    tabPanel("Interactive Graphs",
             h3("Here is our interactive graph panel"),
             fluidRow(
               column(3, "Variable Selection",
                      
                      #creates drop down for county selection for graph
                      selectizeInput("county", "Select County",
                                     choices = county_names,
                                     multiple = TRUE,
                                     selected = "AndersonCounty"
                      ),
                      
                      #allows user to select x variable in graph
                      selectizeInput("graph_xvar", "Select X Variable",
                                     choices = graph_vars,
                                     selected = "avg_income"),
                      
                      #display for selecting interactive y variable, any selected 
                      #x variable will not appear in this list
                      uiOutput("graph_yvar")
               ),
             ),
             #output the user generated graph
             column(9, "Graph Output",
                    plotOutput("graph")
             )
    ),
    
    #MAPS PANNEL UI-------------------------------------------------------------
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
               #output for user generated map
               column(9, "Map Output",
                      tmapOutput("map"))
             )
    ),
    
    #MODEL PANNEL UI------------------------------------------------------------
    tabPanel("Interactive Vunerability Model",
             h3("Here is our interactive model panel"),
             fluidRow(
               column(3,
                      
                      #select the county for the betterfi_model
                      selectizeInput("model_counties", "Select Desired Counties",
                                     choices = county_names,
                                     multiple = TRUE,
                                     selected = "All"
                      ),
                      #select the variable(s) to include in the betterfi_model
                      selectizeInput(
                        inputId = "vars",
                        label = "Select Desired Variables",
                        choices = model_vars,
                        multiple = TRUE,
                        selected = c("All")
                      ),
                      uiOutput("conditional_weight_sliders"),
               ),
               column(9, 
                      actionButton("model_button", "Click to Run Model")),
               actionButton("reset_button", "Click to Reset the Weights"),
               actionButton("equal_weight_button", "Click to set the Weights Equally"),
               # tableOutput("model_dt"),
               conditionalPanel(
                 condition = "output.model_output_df != NULL",
                 DTOutput("model_dt")
               )
               
             )
             
    ),
    
    #TEAM PANNEL UI-------------------------------------------------------------
    tabPanel("Meet The Team",
             h3("The BetterFi Team")
    ),
    widths = c(2, 10)
  )
)




# Define server logic 
server <- function(input, output, session) {
  #GRAPH SERVER=================================================================
  
  #GRAPHS - UPDATES Y VAR LSIT TO REMOVE SELECTED X VAR
  output$graph_yvar <- renderUI({
    selectizeInput(
      "graph_yvar",
      "Select Y Variable",
      choices = graph_vars[graph_vars != input$graph_xvar],
      selected = input$graph_yvar
    )
  })
  
  output$graph <- renderPlot({
    
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
  
  # Creating the conditionally shown sliders for each variable
  output$conditional_weight_sliders <- renderUI({
    lapply(model_vars[model_vars != "All"], function(variable) {
      conditionalPanel(
        condition = paste0(
          "input.vars.includes('",
          variable,
          "') || (input.vars.includes('All') && input.vars.length == 1)"
        ),
        # uiOutput(variable)
        sliderInput(
          inputId = variable,
          label = names(model_vars)[model_vars == variable],
          min = 0,
          # If the input weight is null because it hasn't been loaded yet, set the max to 1
          # Otherwise, set the max to 1 minus the sum of the other weights
          max = ifelse(
            is.null(input[[variable]]),
            1,
            1 - (weight_sum() - input[[variable]])
          ),
          # If the input weight is null because it hasn't been loaded yet or if the variable input
          # is not just "All", set it equal to 0, otherwise, set it equal to itself so that it
          # doesn't get reset to 0 when other weight inputs change
          value = ifelse(
            is.null(input[[variable]]) | (!variable %in% input$vars & !("All" %in% input$vars & length(input$vars == 1))),
            0,
            input[[variable]]
          ),
          # Only increase or decrease the value in steps of 0.01 to prevent any horrible fractions
          # and scientific notation
          step = 0.0001
        )
      )
    })
  })
  
  
  #MODEL SERVER===================================================================
  # This changes the input weights to 0 if they become unselected
  variable_selection_observer <- observe({
    # If All isn't the only selected option
    if(!(length(input$vars) == 1 && "All" %in% input$vars)) {
      # Iterate through the model variables
      for(variable in model_vars[model_vars != "All"]) {
        # If the variable isn't selected, set it to 0
        if(!variable %in% input$vars) {
          updateSliderInput(session, variable, value = 0)
        }
      }
    }
  })
  
  # observeEvent(weight_sum(), {
  #   print(paste0("Weight Sum = ", weight_sum()))
  # })
  # 
  # observeEvent(
  #   input$weight_lender |
  #     input$weight_income |
  #     input$weight_noncitizen |
  #     input$weight_highschool |
  #     input$weight_veteran |
  #     input$weight_mediangrossrent |
  #     input$weight_divorced |
  #     input$weight_unemployed |
  #     input$weight_black |
  #     input$weight_hispaniclat,
  #   {
  #     print("Input Weight Changes Detected:")
  #     print(paste0("Weight Income = ", input$weight_income))
  #     print(paste0("Weight Lender = ", input$weight_lender))
  #     print(paste0("Weight Noncitizen = ", input$weight_noncitizen))
  #     print(paste0("Weight Highschool = ", input$weight_highschool))
  #     print(paste0("Weight Veteran = ", input$weight_veteran))
  #     print(paste0("Weight Mediangrossrent = ", input$weight_mediangrossrent))
  #     print(paste0("Weight Divorced = ", input$weight_divorced))
  #     print(paste0("Weight Unemployed = ", input$weight_unemployed))
  #     print(paste0("Weight Black = ", input$weight_black))
  #     print(paste0("Weight Hispaniclat = ", input$weight_hispaniclat))
  #   }
  # )
  
  # Equal Weight Button
  observeEvent(input$equal_weight_button, {
    # Set all of the weights to 0 first
    for(variable in model_vars[model_vars != "All"]) {
      updateSliderInput(session, variable, value = 0)
    }
    
    # If "All" is the only option selected, set all values equally
    if(length(input$vars) == 1 && "All" %in% input$vars) {
      # Calculating the equal weight for all variables
      equal_val <- 1/length(model_vars[model_vars != "All"])
      # Setting the weights
      for(variable in model_vars[model_vars != "All"]) {
        updateSliderInput(session, variable, value = equal_val, max = 1)
      }
    } else {
      # Calculating the equal weight based on selected variables
      equal_val <- 1/length(input$vars[input$vars != "All"])
      # Setting the weights
      for(variable in input$vars[input$vars != "All"]) {
        updateSliderInput(session, variable, value = equal_val, max = 1)
      }
    }
  })
  
  # Reset Weight Button
  observeEvent(input$reset_button, {
    # Set all weights to 0
    for(variable in model_vars[model_vars != "All"]) {
      updateSliderInput(session, variable, value = 0)
    }
  })
  
  
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
    
    # Setting the weight_sum variable to the sum of the weights
    weight_sum(sum(weights))
  })
  
  
  # Reactive value to store the data frame (table) for the model outputs
  model_output_df <- reactiveVal()
  
  #OUTPUT FOR MODEL GIVEN WEIGHT IMPUTS
  observeEvent(input$model_button, {
    # Creating an empty vector to store county input for model and an empty var to store the results
    counties <- c()
    model_results <- NULL
    
    # If the only selected option is "All", get all of the county names and use them for the model
    if(length(input$model_counties) == 1 && input$model_counties == 'All'){
      # Adding all of the county names to the vector and removing "All"
      counties <- county_names[county_names != 'All']
    } else {
      # If "All" is not the only selected county then only use the individually specified counties
      # that were selected and remove "All"
      counties <- input$model_counties[input$model_counties != 'All']
    }
    
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
    
    # Rounding the numeric columns for a cleaner display
    model_results <- model_results %>% 
      mutate_if(is.numeric, round, 4)
    
    # Storing the data frame in the reactive model_output_df variable
    model_output_df(model_results)
  })
  
  output$model_dt <- renderDT(
    model_output_df()
  )
}


# Run the application 
shinyApp(ui = ui, server = server)