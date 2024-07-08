#libraries
library(tidycensus)
library(tidyverse)
library(ggplot2)
library(stringr)
library(lubridate)
library(gsheet)
library(readr)
library(readxl)
library(tmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(sf)
library(tidygeocoder)
library(mapsapi)

#-----------------------------DASHBOARD MODELBUILD------------------------------#

#LOAD TN DATA
load("data/tennessee/tn_data.RData")

#mutate county column
tn_tract <- tn_tract %>% 
  rename(Unemployed = "unemployedpercent") %>% 
  mutate(county = str_extract(NAME, "(?<=;)[^;]+(?=;)")) %>% 
  mutate(county = str_replace_all(county, "County", ""))

tn_tract$county <- gsub(" ", "", tn_tract$county)

#ADD GEOGRAPHICAL FILTERS (COUNTY)
tn_tract <- tn_tract %>% filter(county == "")

#CREATE VARIABLE BUCKETS

#n_lenders (DONE)
varlist_lenders <- tn_tract %>% 
  select("NAME", "n_lenders", "county") %>% 
  replace_na(list(n_lenders = 0)) %>% 
  mutate(max_lender = max(n_lenders)) %>% 
  mutate(vun_lender = n_lenders/max_lender) %>% 
  arrange(desc(vun_lender)) %>% 
  select("NAME", "vun_lender", "county") %>% 
  st_drop_geometry()

#avg_income
varlist_income <- tn_tract %>% 
  select("NAME", "avg_income") %>% 
  replace_na(list(avg_income = 0)) %>% 
  filter(avg_income != 0) %>%
  arrange(avg_income) %>% 
  mutate(vun_income = avg_income/max(avg_income)) %>% 
  mutate(vun_income = 1-vun_income) %>% 
  mutate(vun_income = vun_income/max(vun_income))

#percent_noncitizen (DONE)
varlist_noncitizen <- tn_tract %>% 
  select("NAME", "percent_noncitizen") %>% 
  replace_na(list(percent_noncitizen = 0)) %>% 
  mutate(max_percent_noncitizen = max(percent_noncitizen)) %>% 
  mutate(vun_noncitizen = percent_noncitizen/max_percent_noncitizen) %>% 
  arrange(desc(vun_noncitizen))

#total_percent_highschool (ISSUE: VUN SCORE DOES NOT BEGIN AT ZERO)
varlist_highschool <- tn_tract %>% 
  select("NAME", "total_percent_highschool") %>% 
  mutate(total_percent_highschool = as.numeric(total_percent_highschool)) %>% 
  # replace_na(list(total_percent_highschool = 0)) %>% 
  mutate(max_percent_highschool = max(total_percent_highschool, na.rm=TRUE)) %>% 
  mutate(vun_highschool = total_percent_highschool/max_percent_highschool) %>% 
  # filter(vun_highschool != 0) %>%
  mutate(vun_highschool = 1 - vun_highschool) %>% 
  mutate(vun_highschool = vun_highschool/max(vun_highschool, na.rm=TRUE)) %>% 
  arrange(desc(vun_highschool)) %>% 
  drop_na(total_percent_highschool)

#percent veteran
varlist_veteran <- tn_tract %>% 
  select("NAME", "percent_veteran") %>% 
  mutate(percent_veteran = as.numeric(gsub("%", "", percent_veteran))) %>% 
  replace_na(list(percent_veteran = 0)) %>%
  mutate(percent_veteran = as.numeric(percent_veteran)) %>% 
  mutate(max_percent_veteran = max(percent_veteran)) %>% 
  mutate(vun_veteran = percent_veteran/max_percent_veteran) %>% 
  arrange(desc(vun_veteran)) %>% 
  filter(vun_veteran != 0)

#median gross rent 
varlist_mediangrossrent <- tn_tract %>% 
  select("NAME", "mediangrossrent") %>% 
  mutate(mediangrossrent = as.numeric(mediangrossrent)) %>% 
  drop_na(mediangrossrent) %>% 
  mutate(vun_mediangrossrent = mediangrossrent/max(mediangrossrent)) %>%
  mutate(vun_mediangrossrent = 1 - vun_mediangrossrent) %>% 
  mutate(vun_mediangrossrent = vun_mediangrossrent/max(vun_mediangrossrent, na.rm=TRUE)) %>% 
  arrange(desc(vun_mediangrossrent))

#percent divorced
varlist_divorced <- tn_tract %>% 
  select("NAME", "Divorced") %>%
  mutate(Divorced = as.numeric(gsub("%", "", Divorced))) %>%
  drop_na(Divorced) %>% 
  rename(percent_divorced = Divorced) %>% 
  mutate(vun_divorced = percent_divorced/max(percent_divorced)) %>% 
  arrange(desc(vun_divorced))

#unemployment
varlist_unemployment <- tn_tract %>% 
  select("NAME", "Unemployed") %>%
  rename(percent_unemployed = Unemployed) %>%
  mutate(percent_unemployed = as.numeric(gsub("%", "", percent_unemployed))) %>%
  drop_na(percent_unemployed) %>% 
  mutate(vun_unemployed = percent_unemployed/max(percent_unemployed)) %>% 
  arrange(desc(vun_unemployed))

#black_percent
varlist_black <- tn_tract %>% 
  select("NAME", "percent_black") %>% 
  drop_na(percent_black) %>% 
  mutate(vun_black = percent_black/max(percent_black)) %>% 
  arrange(desc(vun_black))

#hispaniclat_percent
varlist_hispaniclat <- tn_tract %>% 
  select("NAME", "percent_hispaniclat") %>% 
  drop_na(percent_hispaniclat) %>%
  mutate(vun_hispaniclat = percent_hispaniclat/max(percent_hispaniclat)) %>% 
  arrange(desc(vun_hispaniclat))


#ASSIGN MODEL WEIGHTS
#for first iteration assign 10% to each weight
weight_lender <- 0.1
weight_income <- 0.1
weight_noncitizen <- 0.1
weight_highschool <- 0.1
weight_veteran <- 0.1
weight_mediangrossrent <- 0.1
weight_divorced <- 0.1
weight_unemployed<- 0.1
weight_black <- 0.1
weight_hispaniclat <- 0.1

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

#FILTER SELECTED VARIABLES
varlist_vun_filtered <- varlist_vun %>% 
  select("")

#calculate avg_vun score
varlist_vun <- varlist_vun %>% 
  mutate(weighted_vun = ((vun_lender)*(weight_lender)) + 
           ((vun_income)*(weight_income)) +
           ((vun_lender)*(weight_lender)) +
           ((vun_noncitizen)*(weight_noncitizen)) +
           ((vun_highschool)*(weight_highschool)) +
           ((vun_veteran)*(weight_veteran)) +
           ((vun_mediangrossrent)*(weight_mediangrossrent)) +
           ((vun_divorced)*(weight_divorced)) +
           ((vun_unemployed)*(weight_unemployed)) +
           ((vun_black)*(weight_black)) +
           ((vun_hispaniclat)*(weight_hispaniclat)) 
  ) %>% 
  drop_na(weighted_vun) %>% 
  arrange(desc(weighted_vun))

tract_vun_ranking_tn <- varlist_vun %>% 
  select("NAME", "weighted_vun", "county") %>% 
  arrange(desc(weighted_vun))

view(tract_vun_ranking_tn)

#-------------------------------------------------------------------------------#