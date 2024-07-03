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

#load hamilton_tract working dataframe
load("data/hamilton_data.RData")

#create meta datafram for hemilton_tract (variable explanation)

#list of variables:
#n_lenders - The number of lenders per census tract
#avg_income - The average income per census tract
#percent_citizeen - The percent of people in a census tract that are US citizens
#percent_noncitizen - The percent of people in a census tract that are NOT US citizens
#total_percent_highschool - The percent of people that have at least graduated highschool, per census tract
#percent_veteran - The percent of people that are veterans per census tract
#mediangrossrent - The median gross rent per census tract
#


#-----------------------------MODEL.BUILD.ITTERATION.1--------------------------#


#CREATE VARIABLE BUCKETS

#n_lenders (DONE)
varlist_lenders <- hamilton_tract %>% 
  select("NAME", "n_lenders") %>% 
  replace_na(list(n_lenders = 0)) %>% 
  mutate(max_lender = max(n_lenders)) %>% 
  mutate(vun_lender = n_lenders/max_lender) %>% 
  arrange(desc(vun_lender))

#avg_income
varlist_income <- hamilton_tract %>% 
  select("NAME", "avg_income") %>% 
  replace_na(list(avg_income = 0)) %>% 
  arrange(avg_income) %>% 
  filter(avg_income != 0) %>%
  mutate( income_percentile = row_number() / n() )

#percent_noncitizen (DONE)
varlist_noncitizen <- hamilton_tract %>% 
  select("NAME", "percent_noncitizen") %>% 
  replace_na(list(percent_noncitizen = 0)) %>% 
  mutate(max_percent_noncitizen = max(percent_noncitizen)) %>% 
  mutate(vun_noncitizen = percent_noncitizen/max_percent_noncitizen) %>% 
  arrange(desc(vun_noncitizen))

#total_percent_highschool (ISSUE: VUN SCORE DOES NOT BEGIN AT ZERO)
varlist_highschool <- hamilton_tract %>% 
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
varlist_veteran <- hamilton_tract %>% 
  select("NAME", "percent_veteran") %>% 
  mutate(percent_veteran = as.numeric(gsub("%", "", percent_veteran))) %>% 
  replace_na(list(percent_veteran = 0)) %>%
  mutate(percent_veteran = as.numeric(percent_veteran)) %>% 
  mutate(max_percent_veteran = max(percent_veteran)) %>% 
  mutate(vun_veteran = percent_veteran/max_percent_veteran) %>% 
  arrange(desc(vun_veteran)) %>% 
  filter(vun_veteran != 0)

#median gross rent 
varlist_mediangrossrent <- hamilton_tract %>% 
  select("NAME", "mediangrossrent") %>% 
  mutate(mediangrossrent = as.numeric(mediangrossrent)) %>% 
  drop_na(mediangrossrent) %>% 
  
  
  

#-------------------------------------------------------------------------------#