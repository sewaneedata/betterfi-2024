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

load("data/hamilton_data.RData")

#set working directory
# setwd("/Users/buchananlindsey/Desktop/buck_datalab/betterfi-2024/data")

#load data for payday, title, and flex lenders
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

#load coordinate data for all lenders
load("data/total_lender_info.RData") 


#load geography information
hamilton_tract <- get_acs(geography = "tract",
                    state = "TN",
                    variables = "B01003_001",
                    county = "Hamilton",
                    year = 2022,
                    geometry = TRUE)


#map for hamilton census tracts
tmap_mode("view")
tm_shape(hamilton_tract)+
  tm_polygons()


#create hamilton Polygon
hamilton_poly <- st_union(hamilton_tract$geometry)

tm_shape(hamilton_poly)+
  tm_polygons()


#check which points are in hamilton_polygon
points <- total_lender_info$pnt

st_crs(hamilton_poly) <- st_crs(points)

hamilton_lenders <- total_lender_info %>% 
  mutate(Contained = ifelse(st_within(points, hamilton_poly), "Yes", "Not")) %>% 
  filter(Contained == "Yes")

#create map for hamilton county lenders overlayed on hamilton county limits
tmap_mode("view")

tm_shape(hamilton_lenders)+ 
  tm_dots()+
  tm_shape(hamilton_tract)+
  tm_polygons(alpha = 0.5)

#count lenders in each census tract
st_crs(hamilton_tract$geometry) <- st_crs(hamilton_lenders$pnt)
# find which census tract contains each lender
m <- st_intersects( hamilton_lenders$pnt, hamilton_tract, sparse=FALSE )
# idx provides the row of the census tract for the given lender
idx <- apply(m, 1, which)
# now get the corresponding census tract names 
hamilton_lenders$tract <- hamilton_tract$NAME[idx]

#create dataset for lenders per tract
lenders_per_tract <- hamilton_lenders %>% 
  st_drop_geometry() %>%
  group_by( tract ) %>% 
  tally( name = "n_lenders") %>%
  arrange( desc(n_lenders) )

#left join lender_per_tract to hamilton geometry dataset
hamilton_tract <- left_join( hamilton_tract, lenders_per_tract %>% rename( NAME = tract ), by = "NAME" )

#create lender heat map
tm_shape( hamilton_tract ) + tm_polygons( col="n_lenders", id = "NAME")

#makes it so you can open our large hamilton state datasets
Sys.setenv("VROOM_CONNECTION_SIZE"=5000000)


tm_shape(hamilton_lenders)+
  tm_dots()+
  tm_shape(hamilton_tract)+
  tm_polygons(alpha = 0.5,hamilton_tract)

tm_shape(hamilton_tract)+
  tm_dots()+
  tm_shape(hamilton_tract)+
  tm_polygons(alpha = 0.5)


ggplot(data=tn_tract,aes(x=Divorced,y=n_lenders,color=))+geom_col()

#create income level groups
tn_tract2 <- tn_tract2 %>% 
  mutate(percent_black = ifelse(percent_black == '-', NA, percent_black)) %>% 
  mutate(percent_black = as.numeric(percent_black)) %>% 
  mutate(percent_black_group = case_when(
    percent_black < 10 ~ "<10%",
    percent_black >= 10.001 & percent_black < 20 ~ "10-20%",
    percent_black >= 20.001 & percent_black < 30 ~ "20-30%",
    percent_black >= 30.001 & percent_black < 40 ~ "30-40%",
    percent_black >= 40.001 & percent_black < 50 ~ "40-50%",
    percent_black >= 50.001 & percent_black < 60 ~ "50-60%",
    percent_black >= 60.001 & percent_black < 70 ~ "60-70%",
    percent_black >= 70.001 & percent_black < 80 ~ "70-80%",
    percent_black >= 80.001 & percent_black < 90 ~ "80-90%",
    percent_black > 90.001 ~ ">90%",
    TRUE~NA
  ) ) %>% 
  mutate(percent_black_group=fct_reorder(factor(percent_black_group), percent_black, .na_rm = TRUE))#factor reorder for viewing ease, so that the key is ordered from lowest to highest

tn_tract2 <- tn_tract2 %>%
  filter(county.x == "HamiltonCounty")

#create chloropleth for income
tm_shape(tn_tract2)+
  tm_polygons( col = "percent_black_group", id="NAME", palette = "Reds")
