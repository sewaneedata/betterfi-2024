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
# setwd("/Users/buchananlindsey/Desktop/buck_datalab/betterfi-2024/data")

#load data

payday_info <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1x9KSndXG_z2PusNq0ThaQF40VmzJ7G5A/edit?gid=1545321409#gid=1545321409")

flex_info <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1x9KSndXG_z2PusNq0ThaQF40VmzJ7G5A/edit?gid=525051790#gid=525051790")

title_info <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1x9KSndXG_z2PusNq0ThaQF40VmzJ7G5A/edit?gid=1401445265#gid=14014452650")

#merge individual datasets into one

total_info <- rbind(payday_info, title_info, flex_info) 
total_info<-distinct(total_info, Street, .keep_all=TRUE)
total_info <- total_info %>% 
  rename(postal_code = `Postal Code`)

#create full address
total_info <- total_info %>% 
  mutate(full_address = paste0(Street,", ",City)) %>% 
  mutate(full_address = paste0(full_address,", ",State)) %>% 
  mutate(full_address = paste0(full_address," ",postal_code))

#Create Loan Type Column: displays Title, Payday, Flex, or combination of the three depending on if the loan location belongs to one or many of the initial datasets
total_info <- total_info %>% 
  mutate(loan_type = case_when(
    Street %in% flex_info$Street & Street %in% title_info$Street & Street %in% payday_info$Street ~ "Flex, Title, Payday",
    Street %in% flex_info$Street & Street %in% title_info$Street ~ "Flex, Title",
    Street %in% flex_info$Street & Street %in% payday_info$Street ~ "Flex, Payday",
    Street %in% title_info$Street & Street %in% payday_info$Street ~ "Flex, Payday",
    Street %in% flex_info$Street ~ "Flex",
    Street %in% payday_info$Street ~ "Payday",
    Street %in% title_info$Street ~ "Title",
    TRUE ~ NA_character_))

#Convert addresses into dataframe
#coord_data_total <- mp_geocode(addresses = total_info$full_address, key = "AIzaSyAaG4e9IKfLvQBEbyUMY2ko-02UjFHe1LQ")

#add coordinate data to master data file
#  total_lender_info <-  mp_get_points(coord_data_total)

#save new data for coordinates
# save(total_lender_info, file="total_lender_info.RData")

load("data/total_lender_info.RData") 


#load geography information
hamilton_tract <- get_acs(geography = "tract",
                     state = "TN",
                     variables = "B01003_001",
                     county = "Hamilton",
                     year = 2022,
                     geometry = TRUE)


tmap_mode("view")
tm_shape(hamilton_tract)+
  tm_polygons()

#create Hamilton County Polygon
hamilton_county_poly <- st_union(hamilton_tract$geometry)

tm_shape(hamilton_county_poly)+
  tm_polygons()

#check which points are in Hamilton_polygon
points <- total_lender_info$pnt

st_crs(hamilton_county_poly) <- st_crs(points)

hamilton_county_lenders <- total_lender_info %>% 
  mutate(Contained = ifelse(st_within(points, hamilton_county_poly), "Yes", "Not")) %>% 
  filter(Contained == "Yes")

#create map for hamilton county lenders overlayed on Hamilton county limits
tmap_mode("view")

tm_shape(hamilton_county_lenders)+ 
  tm_dots()+
  tm_shape(hamilton_tract)+
  tm_polygons(alpha = 0.5)

#count lenders in each census tract
st_crs(hamilton_tract$geometry) <- st_crs(hamilton_county_lenders$pnt)

# find which census tract contains each lender
m <- st_intersects( hamilton_county_lenders$pnt, hamilton_tract, sparse=FALSE )
# idx provides the row of the census tract for the given lender
idx <- apply(m, 1, which)
# now get the corresponding census tract names 
hamilton_county_lenders$tract <- hamilton_tract$NAME[idx]


#create dataset for lenders per tract
lenders_per_tract <- hamilton_county_lenders %>% 
  st_drop_geometry() %>%
  group_by( tract ) %>% 
  tally( name = "n_lenders") %>%
  arrange( desc(n_lenders) )

#left join lender_per_tract to Hamilton geometry dataset
hamilton_tract <- left_join( hamilton_tract, lenders_per_tract %>% rename( NAME = tract ), by = "NAME" )

#create lender heat map
tm_shape( hamilton_tract ) + tm_polygons( col="n_lenders", id = "NAME")

#create average income for each census tract


#Get ACS Income Data for Income

#Just Hamilton County
ACS_income_hamilton <- read_csv("data/ACS5_hamilton_income.csv")
ACS_income_hamilton <- ACS_income_hamilton %>% 
  select(-contains("Margin of Error")) %>% 
  select(contains("Household")) %>% 
  select(-contains("Nonfamily")) 

#Select just Average Income, Pivot Longer to have just one row per census tract
ACS_income_hamilton <- ACS_income_hamilton[13, ] #select just row 13 which is average income
ACS_income_hamilton <- ACS_income_hamilton %>% 
  pivot_longer(cols = starts_with("Census Tract"), names_to = "NAME", values_to = "avg_income") 

#change census names to match "hamilton_tract"
ACS_income_hamilton$NAME <-  gsub("!.*", "", ACS_income_hamilton$NAME)
ACS_income_hamilton$NAME <- gsub(",", ";", ACS_income_hamilton$NAME)

#join avg_income to Hamilton_tract
hamilton_tract <- hamilton_tract %>% 
  left_join(ACS_income_hamilton, by = "NAME")

hamilton_tract$avg_income <- gsub(",", "", hamilton_tract$avg_income)

#create income level groups
hamilton_tract<- hamilton_tract %>% 
  mutate(avg_income = ifelse(avg_income == '-', NA, avg_income)) %>% 
  mutate(avg_income = as.numeric(avg_income)) %>% 
  mutate(avg_income_group = case_when(
    avg_income < 29999 ~ "<30k",
    avg_income >= 30000 & avg_income < 49999 ~ "30k-50k",
    avg_income >= 50000 & avg_income < 69999 ~ "50k-70k",
    avg_income >= 70000 & avg_income < 89999 ~ "70k-90k",
    avg_income >= 90000 & avg_income < 109999 ~ "90k-110k",
    avg_income >= 110000 & avg_income < 129999 ~ "110k-130k",
    avg_income >= 130000 & avg_income < 149999 ~ "130k-150k",
    avg_income >= 150000 & avg_income < 169999 ~ "150k-170k",
    avg_income >= 170000 & avg_income < 189999 ~ "170k-190k",
    avg_income > 190000 ~ ">190k",
    TRUE~NA
  ) ) %>% 
  mutate(avg_income_group=fct_reorder(factor(avg_income_group), avg_income, .na_rm = TRUE))#factor reorder for viewing ease

#create chloropleth for income
tm_shape(hamilton_tract)+
  tm_polygons( col = "avg_income_group", id="NAME", palette = "Blues")


#load Education Data
hamilton_education <- read_csv("data/education_level.csv")

