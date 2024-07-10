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
ruth_tract <- get_acs(geography = "tract",
                    state = "TN",
                    variables = "B01003_001",
                    county = "Rutherford",
                    year = 2022,
                    geometry = TRUE)


#map for hamilton census tracts
tmap_mode("view")
tm_shape(ruth_tract)+
  tm_polygons()


#create hamilton Polygon
ruth_poly <- st_union(ruth_tract$geometry)

tm_shape(ruth_poly)+
  tm_polygons()


#check which points are in hamilton_polygon
points <- total_lender_info$pnt

st_crs(ruth_poly) <- st_crs(points)

ruth_lenders <- total_lender_info %>% 
  mutate(Contained = ifelse(st_within(points, ruth_poly), "Yes", "Not")) %>% 
  filter(Contained == "Yes")

#create map for hamilton county lenders overlayed on hamilton county limits
tmap_mode("view")

tm_shape(ruth_lenders)+ 
  tm_dots()+
  tm_shape(ruth_tract)+
  tm_polygons(alpha = 0.5)

#count lenders in each census tract
st_crs(ruth_tract$geometry) <- st_crs(ruth_lenders$pnt)
# find which census tract contains each lender
m <- st_intersects( ruth_lenders$pnt, ruth_tract, sparse=FALSE )
# idx provides the row of the census tract for the given lender
idx <- apply(m, 1, which)
# now get the corresponding census tract names 
ruth_lenders$tract <- ruth_tract$NAME[idx]

#create dataset for lenders per tract
lenders_per_tract <- ruth_lenders %>% 
  st_drop_geometry() %>%
  group_by( tract ) %>% 
  tally( name = "n_lenders") %>%
  arrange( desc(n_lenders) )

#left join lender_per_tract to hamilton geometry dataset
ruth_tract <- left_join( ruth_tract, lenders_per_tract %>% rename( NAME = tract ), by = "NAME" )

#create lender heat map
tm_shape( ruth_tract ) + tm_polygons( col="n_lenders", id = "NAME")

#makes it so you can open our large hamilton state datasets
Sys.setenv("VROOM_CONNECTION_SIZE"=5000000)



acs_income_ruth <- read_csv("data/tennessee/acs_income_tn.csv")
acs_income_ruth <- acs_income_ruth %>% 
  select(-contains("Margin of Error")) %>% 
  select(contains("Household")) %>% 
  select(-contains("Nonfamily")) #removes columns with margin of error and nonfamily, keeping only the household income data


#Select just Average Income, Pivot Longer to have just one row per census tract
acs_income_ruth <- acs_income_ruth[13, ] #select just row 13 which is average income
acs_income_ruth <- acs_income_ruth %>% 
  pivot_longer(cols = starts_with("Census Tract"), names_to = "NAME", values_to = "avg_income") 
acs_income_ruth <- acs_income_ruth %>% filter(grepl("Rutherford County", NAME))


#change census names to match "tn_tract"
acs_income_ruth$NAME <-  gsub("!.*", "", acs_income_ruth$NAME) #removing text starting w exclamation mark
acs_income_ruth$NAME <- gsub(",", ";", acs_income_ruth$NAME) #replaces commas w semicolon


#join avg_income to tn_tract
ruth_tract <- ruth_tract %>% 
  left_join(acs_income_ruth, by = "NAME")
ruth_tract$avg_income <- gsub(",", "", ruth_tract$avg_income)


ruth_tract <- ruth_tract %>% 
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
  mutate(avg_income_group=fct_reorder(factor(avg_income_group), avg_income, .na_rm = TRUE))#factor reorder for viewing ease, so that the key is ordered from lowest to highest

tmap_mode("plot")
tm_shape(ruth_tract)+
  tm_polygons( col = "avg_income_group", id="NAME", palette = "Blues")+  tm_layout(legend.position = c("right", "bottom"))

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
  filter(county.x == "RutherfordCounty")

#create chloropleth for income
tm_shape(tn_tract2)+
  tm_polygons( col = "percent_black_group", id="NAME", palette = "Reds")

##################################################################################

#load geography information
counties_tract <- get_acs(geography = "county",
                    state = "TN",
                    variables = "B01003_001",
                    county = c("Hamilton","Rutherford"),
                    year = 2022,
                    geometry = TRUE)


#create tn Polygon
tn_poly <- st_union(tn_tract$geometry)

tm_shape(tn_poly)+
  tm_polygons()

counties_tract <- counties_tract %>% 
  # rename(Unemployed = "unemployedpercent") %>% 
  mutate(County = str_extract(NAME, "(?<=;)[^;]+(?=;)")) %>% 
  mutate(County = str_replace_all(county, "County", ""))

#counties_tract$county <- gsub(" ", "", counties_tract$county)

save(counties_tract, file="data/tennessee/dontpushthis.RData")

#map for tn census tracts
#counties_tract <- counties_tract %>% rename(County=county)

tmap_mode("plot")
#tm_shape(tn_poly)+
tm_shape(counties_tract)+
  tm_polygons(col='County')

tm_shape(tn_poly) + 
  tm_polygons(border.col = "black") +       # Base layer with black borders
  tm_shape(counties_tract) + 
  tm_polygons(col="NAME",          # Overlay layer with grey borders
              alpha = 0.7) +                # Transparency level for overlay
  tm_layout(legend.position = c("right", "bottom"))


#create tn Polygon
counties_poly <- st_union(counties_tract$geometry)

tm_shape(counties_poly)+
  tm_polygons()


#check which points are in tn_polygon
points <- total_lender_info$pnt

st_crs(counties_poly) <- st_crs(points)

tn_lenders <- total_lender_info %>% 
  mutate(Contained = ifelse(st_within(points, counties_poly), "Yes", "Not")) %>% 
  filter(Contained == "Yes")

#create map for tn county lenders overlayed on tn county limits
tmap_mode("view")

tm_shape(tn_lenders)+ 
  tm_dots()+
  tm_shape(tn_tract)+
  tm_polygons(alpha = 0.5)

#count lenders in each census tract
st_crs(tn_tract$geometry) <- st_crs(tn_lenders$pnt)
# find which census tract contains each lender
m <- st_intersects( tn_lenders$pnt, tn_tract, sparse=FALSE )
# idx provides the row of the census tract for the given lender
idx <- apply(m, 1, which)
# now get the corresponding census tract names 
tn_lenders$tract <- tn_tract$NAME[idx]

#create dataset for lenders per tract
lenders_per_tract <- tn_lenders %>% 
  st_drop_geometry() %>%
  group_by( tract ) %>% 
  tally( name = "n_lenders") %>%
  arrange( desc(n_lenders) )

#left join lender_per_tract to tn geometry dataset
tn_tract <- left_join( tn_tract, lenders_per_tract %>% rename( NAME = tract ), by = "NAME" )

#create lender heat map
tm_shape( tn_tract ) + tm_polygons( col="n_lenders", id = "NAME")

#makes it so you can open our large tn state datasets
Sys.setenv("VROOM_CONNECTION_SIZE"=5000000)

summary(tract_vun_ranking_tn$weighted_vun)
