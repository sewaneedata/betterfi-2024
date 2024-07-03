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



####



#load total pop data
acstotalpophamilton <- read_csv("data/acshamiltonpop.csv")

#clean pop data
acstotalpophamilton <- acstotalpophamilton %>% 
  select(-contains("Margin of Error"))

acstotalpophamilton <- acstotalpophamilton %>% 
  pivot_longer(cols = starts_with("Census Tract"), names_to = "NAME", values_to = "total_population") 


#match tract names with hamilton_tract
acstotalpophamilton$NAME <-  gsub("!.*", "", acstotalpophamilton$NAME)
acstotalpophamilton$NAME <- gsub(",", ";", acstotalpophamilton$NAME)

acstotalpophamilton <- acstotalpophamilton %>% select(NAME,total_population)


#left_join pop data to hamilton_tract
hamilton_tract <- hamilton_tract %>% 
  left_join(acstotalpophamilton, by = "NAME") 



####



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



####



#load citizen data
acs_citizen <- read_csv("data/acs_nativity_citizenshiphamilton.csv")

#clean citizen data and names
acs_citizen <- acs_citizen %>% 
  select(-contains("Margin of Error"))

#select citizen rows for left_join
acs_citizen_hamilton <- acs_citizen[2, ]
acs_noncitizen_hamilton <- acs_citizen[6, ]

#pivot for left_join
acs_citizen_hamilton <- acs_citizen_hamilton %>% 
  pivot_longer(cols = starts_with("Census Tract"), names_to = "NAME", values_to = "citizens") 

acs_noncitizen_hamilton <- acs_noncitizen_hamilton %>% 
  pivot_longer(cols = starts_with("Census Tract"), names_to = "NAME", values_to = "noncitizens") 

#change census names to match "hamilton_tract"
acs_citizen_hamilton$NAME <-  gsub("!.*", "", acs_citizen_hamilton$NAME)
acs_citizen_hamilton$NAME <- gsub(",", ";", acs_citizen_hamilton$NAME)

acs_citizen_hamilton <- acs_citizen_hamilton %>% select(NAME,citizens)

acs_noncitizen_hamilton$NAME <-  gsub("!.*", "", acs_noncitizen_hamilton$NAME)
acs_noncitizen_hamilton$NAME <- gsub(",", ";", acs_noncitizen_hamilton$NAME)

acs_noncitizen_hamilton <- acs_noncitizen_hamilton %>% select(NAME,noncitizens)

#left join 
hamilton_tract <- hamilton_tract %>% 
  left_join(acs_citizen_hamilton, by = "NAME")

hamilton_tract <- hamilton_tract %>% 
  left_join(acs_noncitizen_hamilton, by = "NAME") 

#mutate %citizen and %noncitizen
hamilton_tract <- hamilton_tract %>% 
  mutate(percent_citizen = (citizens/total_population)*100) %>% 
  mutate(percent_noncitizen = (noncitizens/total_population)*100)%>% 
  select(-"citizens") %>% 
  select(-"noncitizens")



####



#load education data
hamilton_edu <- read_csv("data/education_level.csv")
#hamilton_edu_meta <- read_csv("data/Hamilton-Education-Column-Metadata.csv")

#select Highschool education data
hamilton_edu <- hamilton_edu %>% 
  select("NAME", "S1501_C02_002E", "S1501_C01_001E", "S1501_C01_007E", "S1501_C01_008E", "S1501_C01_006E")

#remove first row
hamilton_edu <- hamilton_edu[-1, ] 

#rename education columns
names(hamilton_edu) <- c(
    "NAME", #NAME,
    "18NOhighschool", #"S1501_C01_002E",
    "18total", #"S1501_C01_001E"
    "25NO9th",#"S1501_C01_007E", 
    "25NOhighschool", #"S1501_C01_008E"
    "25total" #S1501_C01_006E
    
    )

#mutate columns from character to numeric
hamilton_edu <- hamilton_edu %>% 
  mutate(`18NOhighschool` = as.numeric(`18NOhighschool`)) %>% 
  mutate(`18total` = as.numeric(`18total`)) %>% 
  mutate(`25NO9th` = as.numeric(`25NO9th`)) %>% 
  mutate(`25NOhighschool` = as.numeric(`25NOhighschool`)) %>% 
  mutate(`25total` = as.numeric(`25total`)) 


#mutate columns for 18 and 25 than HAVE GRADUATED HIGHSCHOOL
hamilton_edu <- hamilton_edu %>% 
  mutate(`18highschool` = `18total` - `18NOhighschool`) %>% 
  mutate(`25highschool` = `25total` - `25NOhighschool` - `25NO9th`)


#mutate column for total pop and total highschool graduation pop
hamilton_edu <- hamilton_edu %>% 
  mutate(total_pop = `18total` + `25total`) %>% 
  mutate(total_highschool_pop = `25highschool` + `18highschool`)

hamilton_edu <- hamilton_edu %>% 
  mutate(total_percent_highschool = (total_highschool_pop/total_pop)*100) 

hamilton_edu <- hamilton_edu %>% 
  select("NAME", "total_percent_highschool")


#left_join highschool education to hamilton_tract
hamilton_tract <- hamilton_tract %>% 
  left_join(hamilton_edu, by = "NAME")


#heat map for education
tm_shape(hamilton_tract)+
  tm_polygons(col = "total_percent_highschool")



####



#load marital data
acs_marital <- read_csv("data/acs_marital.csv") 

#pivot acs_marital
acs_marital <- acs_marital %>% 
  pivot_longer(cols = starts_with("Census Tract"), names_to = "NAME", values_to = "values")



####



#load veteran data
acs_veteran <- read_csv("data/acs_veteran.csv")


#keep only the first row
acs_veteran <- acs_veteran[1, ]


#select only percent veteran estimates
acs_veteran <- acs_veteran %>% 
  select(contains("Percent Veterans")) %>% 
  select(-contains("Margin of Error"))


#pivot for left_join
acs_veteran <- acs_veteran %>% 
  pivot_longer(cols = starts_with("Census Tract"), names_to = "NAME", values_to = "percent_veteran")

#clean NAME for left join
acs_veteran$NAME <-  gsub("!.*", "", acs_veteran$NAME)
acs_veteran$NAME <- gsub(",", ";", acs_veteran$NAME)


#left_join veteran data
hamilton_tract <- hamilton_tract %>% 
  left_join(acs_veteran, by = "NAME")



####



#loads median gross rent
acs_gross_rent <- read_csv("data/acs_gross_rent.csv")

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

acs_gross_rent <- acs_gross_rent %>%
  mutate(across(starts_with("Census Tract"), as.character))

#cleans census tract column a little
acs_gross_rent$NAME <-  gsub("!.*", "", acs_gross_rent$NAME)
acs_gross_rent$NAME <- gsub(",", ";", acs_gross_rent$NAME)

#removes label column, only keeping rent and census tract name
acs_gross_rent <- acs_gross_rent %>% select(NAME,mediangrossrent)

#joins gross rent data to hamilton_tract dataset
hamilton_tract <- hamilton_tract %>% 
  left_join(acs_gross_rent, by = "NAME")

#creates mediangrossrent column on hamilton tract for the groups
hamilton_tract <- hamilton_tract %>%
  mutate(
    mediangrossrent = ifelse(mediangrossrent == '-' | is.na(mediangrossrent), NA, mediangrossrent)
  )

#converts the mediangrossrent column to numeric
hamilton_tract <- hamilton_tract %>%
  mutate(mediangrossrent = as.numeric(mediangrossrent))

#groups mediangrossrent by how much each tract pays for rent
hamilton_tract <- hamilton_tract %>%
  mutate(mediangrossrent_group = case_when(
    mediangrossrent < 250 ~ "<250",
    mediangrossrent >= 250 & mediangrossrent < 500 ~ "250-500",
    mediangrossrent >= 500 & mediangrossrent < 750 ~ "500-750",
    mediangrossrent >= 750 & mediangrossrent < 1000 ~ "750-1000",
    mediangrossrent >= 1000 & mediangrossrent < 1250 ~ "1000-1250",
    mediangrossrent >= 1250 & mediangrossrent < 1500 ~ "1250-1500",
    mediangrossrent >= 1500 & mediangrossrent < 1750 ~ "1500-1750",
    mediangrossrent >= 1750 & mediangrossrent < 2000 ~ "1750-2000",
    mediangrossrent >= 2000 & mediangrossrent < 2250 ~ "2000-2250",
    mediangrossrent >= 2250 & mediangrossrent < 2500 ~ "2250-2500",
    mediangrossrent >= 2500 ~ "2500+",
    TRUE ~ NA_character_
  ))

#factor reorders by levels of rent, the exact scale i just made previously
hamilton_tract <- hamilton_tract %>%
  mutate(mediangrossrent_group = factor(
    mediangrossrent_group,
    levels = c("<250", "250-500", "500-750", "750-1000", "1000-1250", 
               "1250-1500", "1500-1750", "1750-2000", "2000-2250", "2250-2500", "2500+")
  ))

#creates the map based on how much each tract pays for median gross rent
tm_shape(hamilton_tract) +
  tm_polygons(
    col = "mediangrossrent_group",
    id = "NAME",
    palette = "Blues",
    title = "Median Gross Rent"
  ) +
  tm_layout(
    legend.title.size = 1,
    legend.text.size = 0.8,
    legend.position = c("left", "bottom")
  ) +
  tm_borders(alpha = 0.5)



####



#load race data
acs_race_hamilton <- read_csv("data/acs_detailedrace_hamilton.csv")#reads race info csv


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


#create percent race columns
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



####


#reads csv for hispanic or latino individuals in hamilton county
acs_hl_hamilton <- read_csv("data/acs_hispanic_or_latino_hamilton.csv") 


#removes margin of error column
acs_hl_hamilton <- acs_hl_hamilton %>% select(-contains("Margin of Error"))


#keeps row of hispanic individuals as own dataset
acs_hispanic_hamilton <- acs_hl_hamilton[3, ] 
#makes new dataset of nonhispanic individuals
acs_nonhispanic_hamilton <- acs_hl_hamilton[2, ]


#pivots longer so it flips it around so census tracts are rows and not columns
acs_hispanic_hamilton <- acs_hispanic_hamilton %>% 
  pivot_longer(cols = starts_with("Census Tract"), names_to = "NAME", values_to = "hispaniclat") 

acs_nonhispanic_hamilton <- acs_nonhispanic_hamilton %>% 
  pivot_longer(cols = starts_with("Census Tract"), names_to = "NAME", values_to = "nonhispaniclat") 
#Create data frames for hispanic and nonhispanic
acs_hispanic_hamilton <- acs_hispanic_hamilton %>% select(NAME,hispaniclat) 

acs_nonhispanic_hamilton <- acs_nonhispanic_hamilton %>% select(NAME,nonhispaniclat)


#clean NAME column for left_join into hamilton_tract
acs_hispanic_hamilton$NAME <-  gsub("!.*", "", acs_hispanic_hamilton$NAME)
acs_hispanic_hamilton$NAME <- gsub(",", ";", acs_hispanic_hamilton$NAME)

acs_nonhispanic_hamilton$NAME <-  gsub("!.*", "", acs_nonhispanic_hamilton$NAME)
acs_nonhispanic_hamilton$NAME <- gsub(",", ";", acs_nonhispanic_hamilton$NAME)


#joins hispanic individuals dataset to the main dataset
hamilton_tract <- hamilton_tract %>% 
  left_join(acs_hispanic_hamilton, by = "NAME")


#joins nonhispanic individuals to the main dataset
hamilton_tract <- hamilton_tract %>% 
  left_join(acs_nonhispanic_hamilton, by = "NAME")

hamilton_tract <- hamilton_tract %>% 
  mutate(percent_hispaniclat = (hispaniclat/total_population)*100)



####



#reads csv for marital status census info
acs_marital_ham <- read_csv("data/acs_marital.csv")

#removes columns with margins of error
acs_marital_ham <- acs_marital_ham %>% 
  select(-contains("Margin of Error"))

#keeps population total row
acs_marital_ham1 <- acs_marital_ham[1, ]

#pivot longer
acs_marital_ham1 <- acs_marital_ham1 %>% 
  pivot_longer(cols = starts_with("Census Tract"), 
               names_to = "NAME")

#filters out columns with Tennessee total estimate based on name variable
acs_marital_ham1 <- acs_marital_ham1 %>% 
  filter(!grepl("Tennessee!!Total!!Estimate",NAME))

#creates censustract column and varname column used for pivoting wider
acs_marital_ham1 <- acs_marital_ham1 %>%
  mutate(censustract=(gsub("!!.*","",NAME)),
         varname=(gsub(".*Tennessee!!","",NAME)),
         varname=(gsub("!!Estimate.*", "", varname)))

#pivots wider with varname and value column
acs_marital_ham2 <- acs_marital_ham1 %>% select(-`Label (Grouping)`,-NAME) %>% 
  pivot_wider(names_from=varname,values_from=value)

#merges marital dataset with hamilton tract by name and censustract variables.
hamilton_tract <- hamilton_tract %>% 
  left_join(acs_marital_ham2, by = c("NAME" = "censustract"))



####



##looking at just in Hamilton county
employment <- read_csv('data/employment.csv')
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

names(hamilton_tract) <- str_replace_all(names(hamilton_tract), "\\s", "")


####



#--------------------------------MISC.CODE-------------------------------------#


#write RData for hamilton tract (main working dataframe)
save(hamilton_tract, file="data/hamilton_data.RData")


#add Company Name to Hamilton Lenders Dataframe
total_info_join <- total_info %>%
  rename(address = full_address) %>%
  select("Company Name", "address")

hamilton_county_lenders <- hamilton_county_lenders %>%
  left_join(total_info_join, by = "address") 
hamilton_county_lenders <- hamilton_county_lenders %>%
  rename(company_name = `Company Name`)

#total tn geography info
tn_geo <- get_acs(geography = "tract",
                  state = "TN",
                  variables = "B01003_001",
                  year = 2022,
                  geometry = TRUE)


#------------------------------------------------------------------------------#