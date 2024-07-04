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


#set working directory
setwd("/Users/ramzymaraqa/Desktop/ramzy_datalab/betterfi-2024/")

Sys.setenv("VROOM_CONNECTION_SIZE"=5000000)
tn_employment <- read_csv("data/acs_employment_tn.csv")

tn_employed<- tn_employment
tn_unemployed<-tn_employment

#select unemployed and employed for left join 
tn_employed <- tn_employment[5, ] 
tn_unemployed <- tn_employment[6, ]


tn_employed<- tn_employed %>% 
  pivot_longer(cols=starts_with("Census Tract"), names_to = "NAME", values_to = "employed_tn") %>% 
  select()


  
  pivot_longer(cols = starts_with("Census Tract"), names_to = "NAME", values_to = "tn_employment") 

#change census names to match "hamilton_tract"
ACS_income_hamilton$NAME <-  gsub("!.*", "", ACS_income_hamilton$NAME)
ACS_income_hamilton$NAME <- gsub(",", ";", ACS_income_hamilton$NAME)

##employment <- read_csv('data/employment.csv')
employment_cleaning <- employment %>% 
  select(-contains("Margin of Error"))
#selecting row 5 which is number of employed 
employment_cleaning1 <- employment_cleaning[5:6,] %>%  
  # keeping percentages 
  select('Label (Grouping)', contains("Percent")) %>% 
  rename(x = `Tennessee!!Percent`)
# create two columns of unemployed and employed percentages based off census tracks.
employment_cleaning1 <- employment_cleaning1 %>% 
  # making each census track a row while getting the percentages for employment and unemployment
  pivot_longer(cols =  starts_with("Census Tract") & ends_with('Tennessee!!Percent'),
               names_to = 'metric',
               values_to = 'value')  %>% 
  #created new columns of unemployed and employed by using labeling group 
  pivot_wider(id_cols = metric,
              names_from = `Label (Grouping)`,
              values_from = value)

#changing the names of the columns 
ACS_employment_hamilton<- employment_cleaning1 %>% 
  rename( NAME = metric)
#remove the !!Percent from the NAME column
ACS_employment_hamilton$NAME <- gsub( "!!Percent", "", ACS_employment_hamilton$NAME)
ACS_employment_hamilton$NAME <- gsub(",", ";", ACS_employment_hamilton$NAME)

hamilton_tract <- hamilton_tract %>% 
  left_join(ACS_employment_hamilton, by = "NAME")

  

 
