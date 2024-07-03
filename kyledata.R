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
#setwd("/Users/buchananlindsey/Desktop/buck_datalab/betterfi-2024/")

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

load("D:/kyle_datalab/betterfi-2024/data/total_lender_info.RData") 


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

acstotalpophamilton <- read_csv("D:/kyle_datalab/betterfi-2024/data/acshamiltonpop.csv")
acstotalpophamilton <- acstotalpophamilton %>% 
  select(-contains("Margin of Error"))

acstotalpophamilton <- acstotalpophamilton %>% 
  pivot_longer(cols = starts_with("Census Tract"), names_to = "NAME", values_to = "total_population") 

acstotalpophamilton$NAME <-  gsub("!.*", "", acstotalpophamilton$NAME)
acstotalpophamilton$NAME <- gsub(",", ";", acstotalpophamilton$NAME)

acstotalpophamilton <- acstotalpophamilton %>% select(NAME,total_population)

hamilton_tract <- hamilton_tract %>% 
  left_join(acstotalpophamilton, by = "NAME")


#Get ACS Income Data for Income

#Just Hamilton County
ACS_income_hamilton <- read_csv("ACS5_hamilton_income.csv")
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

acs_citizen <- read_csv("D:/kyle_datalab/betterfi-2024/data/acs_nativity_citizenshiphamilton.csv")

acs_citizen <- acs_citizen %>% 
  select(-contains("Margin of Error"))

acs_citizen_hamilton <- acs_citizen[2, ]
acs_noncitizen_hamilton <- acs_citizen[6, ]

acs_citizen_hamilton <- acs_citizen_hamilton %>% 
  pivot_longer(cols = starts_with("Census Tract"), names_to = "NAME", values_to = "citizens") 

acs_noncitizen_hamilton <- acs_noncitizen_hamilton %>% 
  pivot_longer(cols = starts_with("Census Tract"), names_to = "NAME", values_to = "noncitizens") 

#change census names to match "hamilton_tract"
acs_citizen_hamilton$NAME <-  gsub("!.*", "", acs_citizen_hamilton$NAME)
acs_citizen_hamilton$NAME <- gsub(",", ";", acs_citizen_hamilton$NAME)

acs_citizen_hamilton <- acs_citizen_hamilton %>% select(NAME,citizens)

hamilton_tract <- hamilton_tract %>% 
  left_join(acs_citizen_hamilton, by = "NAME")

acs_noncitizen_hamilton$NAME <-  gsub("!.*", "", acs_noncitizen_hamilton$NAME)
acs_noncitizen_hamilton$NAME <- gsub(",", ";", acs_noncitizen_hamilton$NAME)

acs_noncitizen_hamilton <- acs_noncitizen_hamilton %>% select(NAME,noncitizens)

hamilton_tract <- hamilton_tract %>% 
  left_join(acs_noncitizen_hamilton, by = "NAME")




#####

####

acs_hl_hamilton <- read_csv("D:/kyle_datalab/betterfi-2024/data/acs_hispanic_or_latino_hamilton.csv") #reads csv for hispanic or latino individuals in hamilton county
acs_hl_hamilton <- acs_hl_hamilton %>% select(-contains("Margin of Error"))#removes margin of error column
acs_hispanic_hamilton <- acs_hl_hamilton[3, ] #keeps row of hispanic individuals as own dataset
acs_nonhispanic_hamilton <- acs_hl_hamilton[2, ]#makes new dataset of nonhispanic individuals

acs_hispanic_hamilton <- acs_hispanic_hamilton %>% 
  pivot_longer(cols = starts_with("Census Tract"), names_to = "NAME", values_to = "hispaniclat") #pivots longer so it flips it around so census tracts are rows and not columns

acs_nonhispanic_hamilton <- acs_nonhispanic_hamilton %>% 
  pivot_longer(cols = starts_with("Census Tract"), names_to = "NAME", values_to = "nonhispaniclat") 

acs_hispanic_hamilton <- acs_hispanic_hamilton %>% select(NAME,hispaniclat) #removes the column that labels which row is which
acs_nonhispanic_hamilton <- acs_nonhispanic_hamilton %>% select(NAME,nonhispaniclat)
acs_hispanic_hamilton$NAME <-  gsub("!.*", "", acs_hispanic_hamilton$NAME)#makes entries in the census tract column for hispanic individuals a little cleaner
acs_hispanic_hamilton$NAME <- gsub(",", ";", acs_hispanic_hamilton$NAME)

acs_nonhispanic_hamilton$NAME <-  gsub("!.*", "", acs_nonhispanic_hamilton$NAME)
acs_nonhispanic_hamilton$NAME <- gsub(",", ";", acs_nonhispanic_hamilton$NAME)

hamilton_tract <- hamilton_tract %>% 
  left_join(acs_hispanic_hamilton, by = "NAME")#joins hispanic individuals dataset to the main dataset
hamilton_tract <- hamilton_tract %>% 
  left_join(acs_nonhispanic_hamilton, by = "NAME")#joins nonhispanic individuals to the main dataset

hamilton_tract <- hamilton_tract %>% 
  mutate(percent_hispaniclat = (hispaniclat/total_population)*100)

#####

acs_race_hamilton <- acs_race_hamilton <- read_csv("D:/kyle_datalab/betterfi-2024/data/acs_detailedrace_hamilton.csv")#reads race info csv

#This line removes margin of error column
acs_race_hamilton <- acs_race_hamilton %>% 
  select(-contains("Margin of Error"))


#These lines make new datasets of just the row for individuals based on race in hamilton county
acs_white_hamilton <- acs_race_hamilton[3, ]
acs_black_hamilton <- acs_race_hamilton[4, ]
acs_nativeamerican_hamilton <- acs_race_hamilton[5, ]
acs_asian_hamilton <- acs_race_hamilton[6, ]
acs_hawaiian_hamilton <- acs_race_hamilton[7, ]
acs_otherrace_hamilton <- acs_race_hamilton[8, ]


#These lines pivot longer to flip the datasets around to where census tracts are 1 column and the rows are which census tract in particular and there is a column for individuals of race
acs_white_hamilton <- acs_white_hamilton %>% 
  pivot_longer(cols = starts_with("Census Tract"), names_to = "NAME", values_to = "white") 
acs_black_hamilton <- acs_black_hamilton %>% 
  pivot_longer(cols = starts_with("Census Tract"), names_to = "NAME", values_to = "black") 
acs_nativeamerican_hamilton <- acs_nativeamerican_hamilton %>% 
  pivot_longer(cols = starts_with("Census Tract"), names_to = "NAME", values_to = "nativeamerican") 
acs_asian_hamilton <- acs_asian_hamilton %>% 
  pivot_longer(cols = starts_with("Census Tract"), names_to = "NAME", values_to = "asian") 
acs_hawaiian_hamilton <- acs_hawaiian_hamilton %>% 
  pivot_longer(cols = starts_with("Census Tract"), names_to = "NAME", values_to = "hawaiianorislander") 
acs_otherrace_hamilton <- acs_otherrace_hamilton %>% 
  pivot_longer(cols = starts_with("Census Tract"), names_to = "NAME", values_to = "otherrace") 


#these lines make the census tract column entries a little cleaner
acs_white_hamilton$NAME <-  gsub("!.*", "", acs_white_hamilton$NAME)
acs_white_hamilton$NAME <- gsub(",", ";", acs_white_hamilton$NAME)
acs_black_hamilton$NAME <-  gsub("!.*", "", acs_black_hamilton$NAME)
acs_black_hamilton$NAME <- gsub(",", ";", acs_black_hamilton$NAME)
acs_nativeamerican_hamilton$NAME <-  gsub("!.*", "", acs_nativeamerican_hamilton$NAME)
acs_nativeamerican_hamilton$NAME <- gsub(",", ";", acs_nativeamerican_hamilton$NAME)
acs_asian_hamilton$NAME <-  gsub("!.*", "", acs_asian_hamilton$NAME)
acs_asian_hamilton$NAME <- gsub(",", ";", acs_asian_hamilton$NAME)
acs_hawaiian_hamilton$NAME <-  gsub("!.*", "", acs_hawaiian_hamilton$NAME)
acs_hawaiian_hamilton$NAME <- gsub(",", ";", acs_hawaiian_hamilton$NAME)
acs_otherrace_hamilton$NAME <-  gsub("!.*", "", acs_otherrace_hamilton$NAME)
acs_otherrace_hamilton$NAME <- gsub(",", ";", acs_otherrace_hamilton$NAME)


#the next lines drop the label column because it is unnecessary and we need to get rid of it for merging
acs_white_hamilton <- acs_white_hamilton %>% select(NAME,white)
acs_black_hamilton <- acs_black_hamilton %>% select(NAME,black)
acs_nativeamerican_hamilton <- acs_nativeamerican_hamilton %>% select(NAME,nativeamerican)
acs_asian_hamilton <- acs_asian_hamilton %>% select(NAME,asian)
acs_hawaiian_hamilton <- acs_hawaiian_hamilton %>% select(NAME,hawaiianorislander)
acs_otherrace_hamilton <- acs_otherrace_hamilton %>% select(NAME,otherrace)


#next lines merge the datasets into the hamilton tract main set
hamilton_tract <- hamilton_tract %>% 
  left_join(acs_white_hamilton, by = "NAME")
hamilton_tract <- hamilton_tract %>% 
  left_join(acs_black_hamilton, by = "NAME")
hamilton_tract <- hamilton_tract %>% 
  left_join(acs_nativeamerican_hamilton, by = "NAME")
hamilton_tract <- hamilton_tract %>% 
  left_join(acs_asian_hamilton, by = "NAME")
hamilton_tract <- hamilton_tract %>% 
  left_join(acs_hawaiian_hamilton, by = "NAME")
hamilton_tract <- hamilton_tract %>% 
  left_join(acs_otherrace_hamilton, by = "NAME")

#creates new race into percentages
hamilton_tract <- hamilton_tract %>% 
  mutate(percent_white = (white/total_population)*100)
hamilton_tract <- hamilton_tract %>% 
  mutate(percent_black = (black/total_population)*100)
hamilton_tract <- hamilton_tract %>% 
  mutate(percent_nativeamerican = (nativeamerican/total_population)*100)
hamilton_tract <- hamilton_tract %>% 
  mutate(percent_hawaiianorislander = (hawaiianorislander/total_population)*100)
hamilton_tract <- hamilton_tract %>% 
  mutate(percent_otherrace = (otherrace/total_population)*100)


#####

#loads median gross rent
acs_gross_rent <- read_csv("D:/kyle_datalab/betterfi-2024/data/acs_gross_rent.csv")

#clears margins of error columns
acs_gross_rent <- acs_gross_rent %>% 
  select(-contains("Margin of Error"))

#converts columns to as character in order to pivot longer
acs_gross_rent <- acs_gross_rent %>%
  mutate(across(starts_with("Census Tract"), as.character))

#pivots longer flipping data so census tracts are 1 column instead of however many
acs_gross_rent <- acs_gross_rent %>% 
  pivot_longer(cols = starts_with("Census Tract"), 
               names_to = "NAME", 
               values_to = "mediangrossrent")

#cleans census tract column a little
acs_gross_rent$NAME <-  gsub("!.*", "", acs_gross_rent$NAME)
acs_gross_rent$NAME <- gsub(",", ";", acs_gross_rent$NAME)

#removes label column, only keeping rent and census tract name
acs_gross_rent <- acs_gross_rent %>% select(NAME,mediangrossrent)

#joins gross rent data to hamilton_tract dataset
hamilton_tract <- hamilton_tract %>% 
  left_join(acs_gross_rent, by = "NAME")

tm_shape(hamilton_tract)+tm_polygons(col="mediangrossrent")

#create income level groups
hamilton_tract<- hamilton_tract %>% 
  mutate(mediangrossrent = ifelse(mediangrossrent == '-', NA, avg_income)) %>% 
  mutate(mediangrossrent = as.numeric(mediangrossrent)) %>% 
  mutate(mediangrossrent_group = case_when(
    mediangrossrent < 29999 ~ "<30k",
    mediangrossrent >= 30000 & mediangrossrent < 49999 ~ "30k-50k",
    mediangrossrent >= 50000 & mediangrossrent < 69999 ~ "50k-70k",
    mediangrossrent >= 70000 & mediangrossrent < 89999 ~ "70k-90k",
    mediangrossrent >= 90000 & mediangrossrent < 109999 ~ "90k-110k",
    mediangrossrent >= 110000 & mediangrossrent < 129999 ~ "110k-130k",
    mediangrossrent >= 130000 & mediangrossrent < 149999 ~ "130k-150k",
    mediangrossrent >= 150000 & mediangrossrent < 169999 ~ "150k-170k",
    mediangrossrent >= 170000 & mediangrossrent < 189999 ~ "170k-190k",
    mediangrossrent > 190000 ~ ">190k",
    TRUE~NA
  ) ) %>% 
  mutate(mediangrossrent_group=fct_reorder(factor(mediangrossrent_group), mediangrossrent, .na_rm = TRUE))#factor reorder for viewing ease

#create chloropleth for income
tm_shape(hamilton_tract)+
  tm_polygons( col = "mediangrossrent_group", id="NAME", palette = "Blues")