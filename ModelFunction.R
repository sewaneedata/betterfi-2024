# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)


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
  
  # Load raw dataset
  load("data/tennessee/tn_tract_dash.RData")
  
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

test_run1 <- betterfi_model(
  counties = c('HamiltonCounty', 'RutherfordCounty'),
  weight_lender = .1,
  weight_income = .1,
  weight_noncitizen = .1,
  weight_highschool = .1,
  weight_veteran = .1,
  weight_mediangrossrent = .1,
  weight_divorced = .1,
  weight_unemployed = .1,
  weight_black = .1,
  weight_hispaniclat = .1
)

test_run2 <- betterfi_model(
  counties = c('HamiltonCounty', 'RutherfordCounty'),
  weight_lender = .9,
  weight_income = .1,
  weight_noncitizen = 0,
  weight_highschool = 0,
  weight_veteran = 0,
  weight_mediangrossrent = 0,
  weight_divorced = 0,
  weight_unemployed = 0,
  weight_black = 0,
  weight_hispaniclat = 0
)
