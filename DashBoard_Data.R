setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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
library(dplyr)

load("data/tennessee/tn_data.RData")

#REMOVE UNECESSARY VARIABLES
tn_tract_dash <- tn_tract %>% 
  select(-"GEOID", -"variable", -"estimate", -"moe", -"Now married (except separated)", 
         -"Widowed", -"Separated", -"Never married", -"white", -"black", 
         -"nativeamerican", -"asian", -"hawaiianorislander", -"otherrace", 
         -"percent_white", -"percent_nativeamerican", -"percent_hawaiianorislander", 
         -"percent_otherrace", -"hispaniclat", -"nonhispaniclat",
         -"employedpercent", -"percent_citizen")

#REMOVE ALL  % SIGNS FOR NUMERIC CONVERSION
tn_tract_dash$Divorced <- gsub("%", "", tn_tract_dash$Divorced)
tn_tract_dash$percent_veteran <- gsub("%", "", tn_tract_dash$percent_veteran)
tn_tract_dash$Unemployed <- gsub("%", "", tn_tract_dash$Unemployed)
tn_tract_dash$Divorced <- gsub("%", "", tn_tract_dash$Divorced)

#ENSURE ALL COLUMN THAT SHOULD BE NUMERIC ARE NUMERIC
tn_tract_dash <- tn_tract_dash %>% 
  mutate(n_lenders = as.numeric(n_lenders)) %>% 
  mutate(total_population = as.numeric(total_population)) %>% 
  mutate(avg_income = as.numeric(avg_income)) %>% 
  mutate(percent_noncitizen = as.numeric(percent_noncitizen)) %>% 
  mutate(total_percent_highschool = as.numeric(total_percent_highschool)) %>%
  mutate(Divorced = as.numeric(Divorced)) %>% 
  mutate(percent_veteran = as.numeric(percent_veteran)) %>% 
  mutate(mediangrossrent = as.numeric(mediangrossrent)) %>% 
  mutate(percent_black = as.numeric(percent_black)) %>% 
  mutate(percent_hispaniclat = as.numeric(percent_hispaniclat)) %>% 
  mutate(Unemployed = as.numeric(Unemployed)) 

#MUTATE PERCENTILE/GROUPING COLUMNS FOR DASHBOARD MAPS

tn_tract_dash <- tn_tract_dash %>% 
  mutate(noncitizen_group = case_when(
    percent_noncitizen == 0 ~ "0%",
    percent_noncitizen <= 4.99999999 ~ "<5%",
    percent_noncitizen >= 5 & percent_noncitizen < 14.99999999 ~ "5-15%",
    percent_noncitizen >= 15 & percent_noncitizen < 24.99999999 ~ "15-25%",
    percent_noncitizen >= 25 & percent_noncitizen < 34.99999999 ~ "25-35%",
    percent_noncitizen >=35 ~ "35%-45%"
  )) %>% 
  mutate(highschool_group = case_when(
    total_percent_highschool <= 59.99999 ~ "<60%",
    total_percent_highschool >= 60 & total_percent_highschool < 69.99999 ~ "60-70%",
    total_percent_highschool >= 70 & total_percent_highschool < 79.99999 ~ "70-80%",
    total_percent_highschool >= 80 & total_percent_highschool < 89.99999 ~ "80-90%",
    total_percent_highschool >= 90 & total_percent_highschool < 94.99999 ~ "90-95%",
    total_percent_highschool >= 95~ ">95%"
  )) %>% 
  mutate(divorced_group = case_when(
    Divorced < 10 ~ "<10%",
    Divorced >= 10 & Divorced < 19.99 ~ "10-20%",
    Divorced >= 20 & Divorced < 29.99 ~ "20-30%",
    Divorced >= 30 & Divorced < 39.99 ~ "30-40%"
  )) %>% 
  mutate(veteran_group = case_when(
    percent_veteran < 10 ~ "0-10%",
    percent_veteran >= 10 & percent_veteran < 19.99 ~ "10-20%",
    percent_veteran >= 20 & percent_veteran < 29.99 ~ "20-30%",
    percent_veteran >= 30 & percent_veteran < 39.99 ~ "30-40%",
    percent_veteran >= 40 & percent_veteran < 49.99 ~ "40-50%",
    percent_veteran >= 50 & percent_veteran < 59.99 ~ "50-60%",
    percent_veteran >= 60 & percent_veteran < 69.99 ~ "60-70%",
    percent_veteran >= 70 & percent_veteran < 79.99 ~ "70-80%",
    percent_veteran >= 80 & percent_veteran < 89.99 ~ "80-90%",
    percent_veteran >= 90 & percent_veteran <= 100 ~ "90-100%"
  )) %>% 
  mutate(black_group = case_when(
    percent_black < 10 ~ "0-10%",
    percent_black >= 10 & percent_black < 20 ~ "10-20%",
    percent_black >= 20 & percent_black < 30 ~ "20-30%",
    percent_black >= 30 & percent_black < 40 ~ "30-40%",
    percent_black >= 40 & percent_black < 50 ~ "40-50%",
    percent_black >= 50 & percent_black < 60 ~ "50-60%",
    percent_black >= 60 & percent_black < 70 ~ "60-70%",
    percent_black >= 70 & percent_black < 80 ~ "70-80%",
    percent_black >= 80 & percent_black < 90 ~ "80-90%",
    percent_black >= 90 & percent_black <= 100 ~ "90-100%"
  )) %>% 
  mutate(hispaniclat_group = case_when(
    percent_hispaniclat < 10 ~ "0-10%",
    percent_hispaniclat >= 10 & percent_hispaniclat < 20 ~ "10-20%",
    percent_hispaniclat >= 20 & percent_hispaniclat < 30 ~ "20-30%",
    percent_hispaniclat >= 30 & percent_hispaniclat < 40 ~ "30-40%",
    percent_hispaniclat >= 40 & percent_hispaniclat < 50 ~ "40-50%",
    percent_hispaniclat >= 50 & percent_hispaniclat < 60 ~ "50-60%",
    percent_hispaniclat >= 60 & percent_hispaniclat < 70 ~ "60-70%",
    percent_hispaniclat >= 70 & percent_hispaniclat < 80 ~ "70-80%",
    percent_hispaniclat >= 80 & percent_hispaniclat < 90 ~ "80-90%",
    percent_hispaniclat >= 90 & percent_hispaniclat <= 100 ~ "90-100%"
  )) %>% 
  mutate(unemployed_group = case_when(
    Unemployed < 5 ~ "<5%",
    Unemployed >= 5 & Unemployed < 10 ~ "5-10%",
    Unemployed >= 10 & Unemployed < 15 ~ "10-15%",
    Unemployed >= 15 & Unemployed <= 20 ~ "15-20%",
    Unemployed > 20 ~ ">20%"
  ))


#FACTOR REORDER GROUPING COLUMNS FOR BETTER LOOKING MAPS

tn_tract_dash <- tn_tract_dash %>% 
  mutate(black_group=fct_reorder(factor(black_group),(percent_black))) %>% 
  mutate(hispaniclat_group=fct_reorder(factor(hispaniclat_group),(percent_hispaniclat))) %>% 
  mutate(avg_income_group=fct_reorder(factor(avg_income_group),(avg_income))) %>%
  mutate(veteran_group=fct_reorder(factor(veteran_group),(percent_veteran))) %>%
  mutate(noncitizen_group=fct_reorder(factor(noncitizen_group),(percent_noncitizen))) %>%
  mutate(unemployed_group=fct_reorder(factor(unemployed_group),(Unemployed))) %>%
  mutate(highschool_group=fct_reorder(factor(highschool_group),desc(total_percent_highschool))) %>%
  mutate(divorced_group=fct_reorder(factor(divorced_group),(Divorced))) %>%
  mutate(mediangrossrent_group=fct_reorder(factor(mediangrossrent_group),desc(mediangrossrent)))
  

#TEST MAP

tmap_mode("view")
tm_shape(tn_tract_dash %>% filter(county == "ShelbyCounty"))+
  tm_polygons(col = "mediangrossrent_group", palette = "YlOrRd")


  
  
  
  