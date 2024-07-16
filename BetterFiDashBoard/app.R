#load necessary libraries:
library(shiny)
library(ggplot2)
library(sf)
library(stringr)
library(tmap)
library(DT)
library(tidyverse)
library(shinydashboard)
library(shinythemes)
library(readr)
#call Dashboard Data
load("../data/tennessee/tn_tract_dash.RData")

# These NAs actually do represent zero so it should be fine to change them to 0
tn_tract_dash <-tn_tract_dash %>% 
  mutate(num_lender_circles = ifelse(is.na(num_lender_circles), 0, num_lender_circles)) %>% 
  mutate(county = str_replace(county, "County$", " County"))

#define important variables 

#creates a vector containing all unique county names, as well an option called All
county_names <- c("All", sort(unique(as.character(tn_tract_dash$county))))

#creates a vector containing all selectable model vars
model_vars <- c("All", "weight_lender", "weight_income", "weight_noncitizen", "weight_highschool",
                "weight_veteran", "weight_mediangrossrent", "weight_divorced", "weight_unemployed",
                "weight_black", "weight_hispaniclat")
names(model_vars) <- c("All", "Number Of Lenders in 2.5 Mile Radius", "Average Income",
                       "Percentage Noncitizen", "Highschool Graduation Rate",
                       "Percentage Of Veterans", "Median Gross Rent", "Percentage Divorced",
                       "Unemployment Rate", "Percentage African American",
                       "Percentage Hispanic/Latino")

#creates a vector containing all options for the maps panel of the dashboard
map_options <- c(names(tn_tract_dash %>% select(contains("_group"))))
map_options <- setdiff(map_options, "geometry")

#rename map_options so that they are readable on the UI
names(map_options) <- c("Average Income", "Median Gross Rent", "Percentage Noncitizen","Highschool Graduation Rate", "Percentage Divorced", "Percentage Veteran", "Percentage African American", "Percentage Hispanic/Latino", "Unemployment Rate", "Number Of Lenders in 2.5 Mile Radius")

#creates a vector containing all the possible variables to be used in the 
#graph panel of the dash board
graph_vars <- c(as.character(names(tn_tract_dash %>% 
                                     select(!contains("_group"))%>% 
                                     select(-"NAME") %>%
                                     select(-"county"))))
graph_vars <- setdiff(graph_vars, "geometry")

#rename graph_vars so they're readable on the UI
names(graph_vars) <- c("Number Of Lenders in 2.5 Mile Radius", "Total Population", "Average Income", "Percentage Noncitizen","Highschool Graduation Rate", "Percentage Divorced", "Percentage Veteran","Median Gross Rent","Percentage African American", "Percentage Hispanic/Latino", "Unemployment Rate")

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
  #counties <- c("Hamilton County")
  tn_tract_filtered <- tn_tract_dash %>% 
    filter(county %in% counties)
  
  # Calculate all vulnerability scores for all variables for the tracts within 
  # the selected counties
  
  #CREATE VARIABLE BUCKETS
  
  #num_lender_circles
  varlist_lenders <- tn_tract_filtered %>% 
    select("NAME", "num_lender_circles", "county") %>% 
    mutate(vun_lender = num_lender_circles/max(num_lender_circles)) %>% 
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
    arrange(desc(weighted_vun)) %>% 
    rename(`Census Tract` = NAME) %>% 
    rename(`Weighted Vulnerability Score` = weighted_vun) 
  
  if('vun_lender' %in% names(model_results))
  {model_results <- model_results %>% rename(`Lender Vulnerability` = vun_lender)}
  if('vun_noncitizen' %in% names(model_results))
  {model_results <- model_results %>% rename(`Noncitizen Vulnerability` = vun_noncitizen)}
  if('vun_highschool' %in% names(model_results))
  {model_results <- model_results %>% rename(`Highschool Vulnerability` = vun_highschool)}
  if('vun_veteran' %in% names(model_results))
  {model_results <- model_results %>% rename(`Veteran Vulnerability` = vun_veteran)}
  if('vun_mediangrossrent' %in% names(model_results))
  {model_results <- model_results %>% rename(`Median Gross Rent Vulnerability` = vun_mediangrossrent)}
  if('vun_divorced' %in% names(model_results))
  {model_results <- model_results %>% rename(`Divorced Vulnerability` = vun_divorced)}
  if('vun_unemployed' %in% names(model_results))
  {model_results <- model_results %>% rename(`Unemployment Vulnerability` = vun_unemployed)}
  if('vun_black' %in% names(model_results))
  {model_results <- model_results %>% rename(`African American Vulnerability` = vun_black)}
  if('vun_hispaniclat' %in% names(model_results))
  {model_results <- model_results %>% rename(`Hispanic/Latino Vulnerability` = vun_hispaniclat)}
  if('vun_income' %in% names(model_results))
  {model_results <- model_results %>% rename(`Income Vulnerability` = vun_income)}
  
  
  # Spit out results -- name it model_results
  return(model_results)
}
#END BETTERFI_MODEL=============================================================


# Define UI for application 
ui <- fluidPage(
  theme = shinytheme("united"),
  #TITLE FOR DASHBOARD
  titlePanel(title = "BetterFi Interactive Dashboard"),
  
  #CREATES SLIDE/PANNELS FOR DATA DISPLAYS
  navlistPanel(
    widths = c(2,10),
    #INTRO PANNEL UI------------------------------------------------------------
    tabPanel("Introduction",
             fluidRow(
               column(3,
                      tags$figure(
                        tags$img(
                          src = "orangebetterfi.png",
                          width = "100%",
                          alt = "BetterFi Pic"
                        ),
                      ),
               ),
               column(9,
                      br(),
                      h3("What is BetterFi?"),
                      tags$blockquote(
                        em("BetterFi is a non-profit, CDFI that offers affordable installment loans along with coaching to help individuals break free from debt traps and become financially stable.")
                      ),
                      h3(),
                      h3("Where does DataLab come in?"),
                      tags$blockquote(
                        em("We worked with BetterFi to combat the predatory lending crisis in Tennessee by determining the most susceptible areas to predatory lending in Hamilton and Rutherford Counties."),
                      ),
                      ),
             ),
             fluidRow(
               column(10,
                      h3("The Dashboard"),
                      "To maximaize their impact, BetterFi needs the means to uncover where to most vulnerable areas in Tennessee are. The BetterFi Interactive Dashboard provides several comprehensive tools for predicting vulnerability to predatory lending based on our selected population demographics. The dashboard provides three tools: Graphs for Model Variables, Interactive Maps, and the Customizable Model."
               ),
             ),
             hr(),
             fluidRow(
               column(10,
                      h3("Interactive Graphs"),
                      "The Interactive Graph portion of the dashboard allows the user to create custom graphs from the data used in the vulnerability model. Census tracts from a specific county can be selected, or all tracts from every county can also be displayed."
               ),
             ),
             hr(),
             fluidRow(
               column(10,
                      h3("Interactive Maps"),
                      "The Interactive Map portion of the dashboard allows the user to view heat maps of any variable included in the vulnerability model. A specific county or multiple counties can be selected, or a map containing all counties in Tennessee can be displayed."
               ),
             ),
             hr(),
             fluidRow(
               column(10,
                      h3("Interactive Vulnerability Model"),
                      "The Interactive Vulnerability Model portion of the dashboard allows for total customization of the methodology for creating and ranking vulnerability scores. The user may select which counties, variables, and variable weights to include in the model. Having selected all of these inputs, the model will produce an ordered list displaying census tracts ranked from most to least vunerable."
               ),
             ),
             # "Number Of Lenders in 2.5 Mile Radius", "Average Income",
             # "Percentage Noncitizen", "Highschool Graduation Rate",
             # "Percentage Of Veterans", "Median Gross Rent", "Percentage Divorced",
             # "Unemployment Rate", "Percentage African American",
             # "Percentage Hispanic/Latino"
             hr(),
             fluidRow(
               column(11,
                      h3("Variable Descriptions"),
                      tags$li(tags$u("Number Of Lenders in 2.5 Mile Radius"), 'is a variable that represents the number of predatory lending locations that are within 2.5 miles of each census tract.'), br(),
                      tags$li(tags$u('"Average Income"'), 'represents the average median household income for that census tract.'),br(),
                      tags$li(tags$u('Percentage Noncitizen"'), 'represents percentage of population that is not a US Citizen in each census tract.'),br(),
                      tags$li(tags$u('"High School Graduation Rate"'), 'represents the percentage of the population that are high school graduates in each census tract.'), br(),
                      tags$li(tags$u('"Percentage Of Veterans"'), 'represents the percentage of the population that are veterans in each census tract.'), br(), 
                      tags$li(tags$u('"Median Gross Rent"'), 'represents the median gross rent in dollars for that census tract.'), br(),
                      tags$li(tags$u('"Percentage Divorced"'), 'represents the percentage of the population that is divorced in each census tract.'), br(),
                      tags$li(tags$u('"Unemployment Rate"'), 'represents the unemployment rate in each census tract.'), br(),
                      tags$li(tags$u('"Percentage African American"'), 'represents the percentage of the population that is African American in each census tract.'), br(), 
                      tags$li(tags$u('"Percentage Hispanic/Latino"'), 'represents the percentage of the population that is Hispanic or Latino in each census tract.'), hr(),
                      
               ),
             ),
    ),
    
    #GRAPH PANNE UI-------------------------------------------------------------
    tabPanel("Interactive Graphs",
             h3("Interactive Graph Panel"),
             fluidRow(
               column(4, 
                      
                      
                      
                      #creates drop down for county selection for graph
                      selectizeInput("county", "Select County",
                                     choices = county_names,
                                     multiple = TRUE,
                                     selected = "Hamilton County"
                      ),
               ),
               column(4, 
                      #allows user to select x variable in graph
                      selectizeInput("graph_xvar", "Select X Variable",
                                     choices = graph_vars,
                                     selected = "avg_income"),
               ),
               #display for selecting interactive y variable, any selected 
               #x variable will not appear in this list
               column(4, 
                      uiOutput("graph_yvar",
                               selected = "percent_veteran")
               ),
             ),
             #output the user generated graph
             #column(9, "Graph Output",
             plotOutput("graph")
    ),
    
    #MAPS PANNEL UI-------------------------------------------------------------
    tabPanel("Interactive Maps",
             h3("Interactive Map Panel"),
             fluidRow(
               
               #COLUMN FOR SETTING MAP PARAMETERS
               column(4, 
                      
                      #SELECT MAP VARIABLE
                      selectizeInput("map_var", "Select Map Variable",
                                     choices = map_options,
                                     selected = "avg_income_group"
                      ),
               ),
               #SELECT COUNTY(S)
               column(8,
                      selectizeInput("county_map", "Select County",
                                     choices = county_names,
                                     selected = "Hamilton County",
                                     multiple = TRUE)
               ),
             ),
             #output for user generated map
             fluidRow(
               column(12,
                      tmapOutput("map"))
             ),
    ),
    
    #MODEL PANNEL UI------------------------------------------------------------
    tabPanel("Interactive Vunerability Model",
             h3("Interactive Vulnerability Model Panel"),
             fluidRow(
               column(3,
                      
                      #select the county for the betterfi_model
                      selectizeInput("model_counties", "Select Counties",
                                     choices = county_names,
                                     multiple = TRUE,
                                     selected = "All"
                      ),
                      actionButton("reset_county", "Click to Reset County"),
                      br(), hr(), 
                      #select the variable(s) to include in the betterfi_model
                      selectizeInput(
                        inputId = "vars",
                        label = "Select Variables",
                        choices = model_vars,
                        multiple = TRUE,
                        selected = c("All"),
                        width = '100%'
                      ),
                      
                      actionButton("equal_weight_button", "Click to set the Weights Equally"),
                      br(), br(),
                      actionButton("reset_button", "Click to Reset the Weights"),
                      br(), hr(),
                      actionButton("model_button", "Click to Run Model", class="btn-warning")
               ),
               column(9,
                      # tableOutput("model_dt"),
                      conditionalPanel(
                        condition = "output.model_output_df != NULL",
                        DTOutput("model_dt")
                      ))),
             br(),
             hr(),
             br(),
             fluidRow(
               column(12, 
                      uiOutput("conditional_weight_sliders"),
                      br(), br() )),
             
             tags$head(tags$style(".leaflet-top {z-index:999!important;}"))
    ),
    
    
    #TEAM PANNEL UI-------------------------------------------------------------
    tabPanel("About Us",
             hr(),
             fluidRow(
               column(4,
                      tags$figure(
                        tags$img(
                          src = "spike.jpg",
                          width = "100%",
                          alt = "Picture of Betterfi"
                        ),
                      ),
               ),
               column(8,
                      h3("The BetterFi Mission Statement"),
                      tags$blockquote(
                        em("Our mission as a non-profit economic justice enterprise is to provide equitable financial services and programming as a pathway out of dependence on predatory loans.")
                      ),
                      h3("Spike Hosch |", uiOutput("betterfiurl", inline = TRUE)),
                      tags$blockquote(
                        em("Founder and Executive Director")
                      ),
               ),
             ),
             hr(),
             fluidRow(
               column(12, h3("The 2024 DataLab BetterFi Team")),
             ),
             br(),
             fluidRow(
               column(2,
                      tags$figure(
                        tags$img(
                          src = "buck.jpg",
                          width = "100%",
                          alt = "Picture of Buchanan"
                        ),
                      ),
               ),
               column(10,
                      style = 'border-left: 2px solid',
                      h3("W. Buchanan Lindsey |", uiOutput("buckurl", inline = TRUE), "|", a(href = 'mailto:buchananlindsey2002@gmail.com', 'Email Me', inline = TRUE)),
                      h4("C'25 Economics"),
               ),
             ),
             hr(),
             fluidRow(
               column(2,
                      
                      tags$figure(
                        tags$img(
                          src = "ramzy.jpg",
                          width = "100%",
                          alt = "Photo of Ramzy"
                        ),
                      ),
               ),
               column(10,
                      style = 'border-left: 2px solid',
                      h3("Ramzy Maraqa |", uiOutput("ramzyurl", inline = TRUE), "|", a(href = 'mailto:ramzymaraqa02@gmail.com', 'Email Me', inline = TRUE)),
                      h4("C'25 Finance"),
                      
               ),
             ),
             hr(),
             fluidRow(
               column(2,
                      
                      tags$figure(
                        tags$img(
                          src = "kyle.jpg",
                          width = "100%"
                        ),
                      ),
               ),
               column(10,
                      style = 'border-left: 2px solid',
                      h3("Kyle Jones |", uiOutput("kyleurl", inline = TRUE), "|", a(href = 'mailto:kylejonesmlk@gmail.com', 'Email Me', inline = TRUE)),
                      h4("C'25 Finance"),
               ),
             ),
             hr(),
             fluidRow(
               column(2,
                      
                      tags$figure(
                        tags$img(
                          src = "gavin.jpg",
                          width = "100%"
                        ),
                      ),
               ),
               column(10,
                      style = 'border-left: 2px solid',
                      h3("Gavin Clark |", uiOutput("gavinurl", inline = TRUE), "|", a(href = 'mailto:gavincnc@icloud.com', 'Email Me', inline = TRUE)),
                      h4("C'27 History"),
                      
               ),
             ),
             hr(),
    )
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
      
      # c("num_lender_circles", "total_population", "avg_income", "percent_noncitizen", 
      #   "total_percent_highschool", "Divorced", "percent_veteran", "mediangrossrent", 
      #   "percent_black", "percent_hispaniclat", "Unemployed")
      
      # c("Number Of Lenders in 2.5 Mile Radius", "Total Population", "Average Income", "Percentage Noncitizen","Highschool Graduation Rate", "Percentage Divorced", "Percentage Veteran","Median Gross Rent","Percentage African American", "Percentage Hispanic/Latino", "Unemployment Rate")
      
      graph_labels <- c(num_lender_circles = "Number Of Lenders in 2.5 Mile Radius", 
                        total_population = "Total Population", 
                        avg_income = "Average Income", 
                        percent_noncitizen = "Percentage Noncitizen",
                        total_percent_highschool = "Highschool Graduation Rate", 
                        Divorced = "Percentage Divorced", 
                        percent_veteran = "Percentage Veteran", 
                        mediangrossrent = "Median Gross Rent",
                        percent_black = "Percentage African American", 
                        percent_hispaniclat = "Percentage Hispanic/Latino", 
                        Unemployed = "Unemployment Rate")
      
      #create graph
      ggplot(tn_tract_dash %>% filter(county %in% graph_county),
             # aes_string is used since the inputs are stored as a string
             aes_string(x = input$graph_xvar, y = input$graph_yvar ))+
        geom_point(if(length(graph_county) <= 5){ aes(col = county)})+
        #this add a linear model line of best fit
        geom_smooth(#method = "lm", 
                    #se = FALSE
                    )+ 
        labs(x = graph_labels[[input$graph_xvar]], y = graph_labels[[input$graph_yvar]])
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
      tm_polygons(col = this_map_var, palette = "YlOrRd", alpha = 0.8) +
      tm_view( control.position = c("left","bottom"))
    
  }) 
  
  # Creating the conditionally shown sliders for each variable
  output$conditional_weight_sliders <- renderUI({
    
    selected_vars <- input$vars
    if(length(selected_vars) == 1 && selected_vars == 'All'){
      selected_vars = model_vars[model_vars != 'All']
    }else{
      selected_vars <- selected_vars[selected_vars != "All"]
    }
    
    nvar <- length(selected_vars)
    
    
    ncol <- 1 
    column_plan <- list(col1 = c(), 
                        col2 = c(), 
                        col3 = c())
    if(nvar <= 3){
      ncol <- 1
      column_plan[[1]] <- selected_vars[1:length(selected_vars)]
    }
    if(nvar > 3 & nvar <= 6){
      ncol <- 2
      column_plan[[1]] <- selected_vars[1:3]
      column_plan[[2]] <- selected_vars[4:length(selected_vars)]
    }
    if(nvar > 6){
      ncol <- 3
      column_plan[[1]] <- selected_vars[1:3]
      column_plan[[2]] <- selected_vars[4:6]
      column_plan[[3]] <- selected_vars[7:length(selected_vars)]
    }
    
    lapply_funk <- function(model_vars_for_this_column){
      lapply(model_vars_for_this_column, function(variable) {
        #lapply(model_vars[model_vars != "All"], function(variable) {
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
              round(1 - (weight_sum() - input[[variable]]), 4)
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
            round = TRUE,
            step = 0.0001
          )
        )
      })
    }
    
    lapply(1:ncol, function(i){
      if(length(column_plan) >= i){
        column(4, lapply_funk(column_plan[[i]]))
      }
    })
    
    # for(i in 1:ncol){
    #   column(3,
    #                   lapply_funk(column_plan[[i]])
    #   )
    # }
    
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
    model_output_df(),
    options = list(paging = TRUE, 
                   pageLength = 20, 
                   scrollY = '425px', 
                   scrollX = '450px'),
    style = 'bootstrap4'
  )
  
  observeEvent(input$reset_county,{
    updateSelectizeInput(
      session,
      "model_counties", 
      choices = county_names, 
      selected = "All"
    )
  })
  
  #LINKEDIN LINKS FOR MEET THE TEAM
  buckurl <- a("LinkedIn", href="https://www.linkedin.com/in/w-buchanan-lindsey-016b47210/")
  output$buckurl <- renderUI({
    tagList(buckurl)
  })
  ramzyurl <- a("LinkedIn", href="https://www.linkedin.com/in/ramzy-maraqa-1a403328a/")
  output$ramzyurl <- renderUI({
    tagList(ramzyurl)
  })
  kyleurl <- a("LinkedIn", href="https://www.linkedin.com/in/kyle-jones-908772269/")
  output$kyleurl <- renderUI({
    tagList(kyleurl)
  })
  gavinurl <- a("LinkedIn", href="https://www.linkedin.com/in/gavin-clark-268b36303/")
  output$gavinurl <- renderUI({
    tagList(gavinurl)
  })
  betterfiurl <- a("BetterFi Website", href="https://www.betterfi.co/")
  output$betterfiurl <- renderUI({
    tagList(betterfiurl)
  })
  
  
}





# Run the application 
shinyApp(ui = ui, server = server)