# The goal of this file is to generate a mappable polygon as well as our tn_tract  which is our main data file for both our model and dashboard
#libraries, missing gsheet, readxl and googledrive if needed
library(tidycensus)
library(tidyverse)
library(stringr)
library(readr)
library(tmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(sf)
library(mapsapi)

#load coordinate data for all lenders
load("data/total_lender_info.RData") 

#load geography information
tn_tract <- get_acs(geography = "tract",state = "TN",variables = "B01003_001",year = 2022,geometry = TRUE)

#map for tn census tracts
tmap_mode("view")
tm_shape(tn_tract)+
  tm_polygons()

#create tn Polygon
tn_poly <- st_union(tn_tract$geometry)
tm_shape(tn_poly)+
  tm_polygons()

#check which points are in tn_polygon
points <- total_lender_info$pnt
st_crs(tn_poly) <- st_crs(points)
tn_lenders <- total_lender_info %>% 
  mutate(Contained = ifelse(st_within(points, tn_poly), "Yes", "Not")) %>% 
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

#merge lender_per_tract to tn geometry dataset
tn_tract <- left_join( tn_tract, lenders_per_tract %>% rename( NAME = tract ), by = "NAME")

#create lender heat map
tm_shape( tn_tract ) + tm_polygons( col="n_lenders", id = "NAME")
#makes it so you can open our large tn state datasets
Sys.setenv("VROOM_CONNECTION_SIZE"=5000000)


#### Total Population Dataset
#load total pop data
tn_pop <- read_csv("data/tennessee/acs_totalpop_tn.csv")

#clean pop data
tn_pop <- tn_pop %>% 
  select(-contains("Margin of Error"))

#pivots population longer
tn_pop <- tn_pop %>% 
  pivot_longer(cols = starts_with("Census Tract"), names_to = "NAME", values_to = "total_population") 

#match tract names with tn_tract
tn_pop$NAME <-  gsub("!.*", "", tn_pop$NAME)#removing text starting with exclamation mark
tn_pop$NAME <- gsub(",", ";", tn_pop$NAME) #replaces commas with a semicolon
tn_pop <- tn_pop %>% select(NAME,total_population) #keeps name and total population column

#combine population data to tn_tract
tn_tract <- tn_tract %>% 
  left_join(tn_pop, by = "NAME") 


#### American Community Survey Income Dataset
acs_income_tn <- read_csv("data/tennessee/acs_income_tn.csv")
acs_income_tn <- acs_income_tn %>% 
  select(-contains("Margin of Error")) %>% 
  select(contains("Household")) %>% 
  select(-contains("Nonfamily")) #removes columns with margin of error and nonfamily, keeping only the household income data

#Select just Average Income, Pivot Longer to have just one row per census tract
acs_income_tn <- acs_income_tn[13, ] #select just row 13 which is average income
acs_income_tn <- acs_income_tn %>% 
  pivot_longer(cols = starts_with("Census Tract"), names_to = "NAME", values_to = "avg_income") 

#change census names to match "tn_tract"
acs_income_tn$NAME <-  gsub("!.*", "", acs_income_tn$NAME) #removing text starting w exclamation mark
acs_income_tn$NAME <- gsub(",", ";", acs_income_tn$NAME) #replaces commas w semicolon

#join avg_income to tn_tract
tn_tract <- tn_tract %>% 
  left_join(acs_income_tn, by = "NAME")
tn_tract$avg_income <- gsub(",", "", tn_tract$avg_income)

#create income level groups
tn_tract<- tn_tract %>% 
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

#create map for income
tm_shape(tn_tract)+
  tm_polygons( col = "avg_income_group", id="NAME", palette = "Blues")


#### Citizenship and Nativity Dataset
#load citizen data
acs_citizen_tn <- read_csv("data/tennessee/acs_citizenship_tn.csv")

#clean citizen data and names
acs_citizen_tn <- acs_citizen_tn %>% 
  select(-contains("Margin of Error"))

#select citizen rows for merging
acs_citizen_tn1 <- acs_citizen_tn[2, ]
acs_noncitizen_tn <- acs_citizen_tn[6, ]

#flipping dataset to be formatted properly for merging
acs_citizen_tn1 <- acs_citizen_tn1 %>% 
  pivot_longer(cols = starts_with("Census Tract"), names_to = "NAME", values_to = "citizens") 
acs_noncitizen_tn <- acs_noncitizen_tn %>% 
  pivot_longer(cols = starts_with("Census Tract"), names_to = "NAME", values_to = "noncitizens") 

#change census names to match "tn_tract"
acs_citizen_tn1$NAME <-  gsub("!.*", "", acs_citizen_tn1$NAME)
acs_citizen_tn1$NAME <- gsub(",", ";", acs_citizen_tn1$NAME)
acs_citizen_tn1 <- acs_citizen_tn1 %>% select(NAME,citizens)
acs_noncitizen_tn$NAME <-  gsub("!.*", "", acs_noncitizen_tn$NAME)
acs_noncitizen_tn$NAME <- gsub(",", ";", acs_noncitizen_tn$NAME)
acs_noncitizen_tn <- acs_noncitizen_tn %>% select(NAME,noncitizens)

#merging citizen and noncitizen data to main tn_tract dataset
tn_tract <- tn_tract %>% 
  left_join(acs_citizen_tn1, by = "NAME")
tn_tract <- tn_tract %>% 
  left_join(acs_noncitizen_tn, by = "NAME") 

#create %citizen and %noncitizen columns
tn_tract <- tn_tract %>% 
  mutate(percent_citizen = (citizens/total_population)*100) %>% 
  mutate(percent_noncitizen = (noncitizens/total_population)*100)%>% 
  select(-"citizens") %>% 
  select(-"noncitizens") #creates citizen and noncitizen percent columns 


#### Educational Attainment Dataset
#load education csv
acs_edu_tn <- read_csv("data/tennessee/acs_edu_tn.csv")

#removes margin of error column
acs_edu_tn <- acs_edu_tn %>% select("Label (Grouping)", contains("Total!!Estimate"))
edu_rows <- c(2, 3, 7, 8, 9)
acs_edu_tn <- acs_edu_tn[edu_rows, ]
acs_edu_tn <- acs_edu_tn %>% 
  pivot_longer(cols = starts_with("Census Tract"), names_to = "NAME") 

#creates censustract column and varname column used for pivoting wider
acs_edu_tn <- acs_edu_tn  %>%
  mutate(censustract=(gsub("!!.*","",NAME)),
         varname=(gsub(".*Tennessee!!","",NAME)),
         varname=(gsub("!!Estimate.*", "", varname)))

#pivots wider with varname and value column
acs_edu_tn  <- acs_edu_tn  %>% select(-NAME) %>% 
  pivot_wider(names_from= `Label (Grouping)` ,values_from=value) %>% 
  select(-"varname") 

names(acs_edu_tn) <- str_replace_all(names(acs_edu_tn), "\\s", "")

#rename columns
names(acs_edu_tn) <- c(
  "NAME", #"censustract", 
  "18total", #"Population18to24years", 
  "18NOhighschool", #"Lessthanhighschoolgraduate", 
  "25total", #"Population25yearsandover", 
  "25NO9th", #"Lessthan9thgrade", 
  "25NOhighschool" #"9thto12thgradenodiploma"
)

#remove commas
acs_edu_tn$`25total` <- gsub(",", "", acs_edu_tn$`25total`)
acs_edu_tn$`18total` <- gsub(",", "", acs_edu_tn$`18total`)
acs_edu_tn$`18NOhighschool` <- gsub(",", "", acs_edu_tn$`18NOhighschool`)
acs_edu_tn$`25NOhighschool` <- gsub(",", "", acs_edu_tn$`25NOhighschool`)
acs_edu_tn$`25NO9th` <- gsub(",", "", acs_edu_tn$`25NO9th`)

#convert character string to numeric
acs_edu_tn <- acs_edu_tn %>% 
  mutate(`18NOhighschool` = as.numeric(`18NOhighschool`)) %>% 
  mutate(`18total` = as.numeric(`18total`)) %>% 
  mutate(`25NO9th` = as.numeric(`25NO9th`)) %>% 
  mutate(`25NOhighschool` = as.numeric(`25NOhighschool`)) %>% 
  mutate(`25total` = as.numeric(`25total`)) 

#create columns for 18 and 25 than HAVE GRADUATED HIGHSCHOOL
acs_edu_tn <-acs_edu_tn %>% 
  mutate(`18highschool` = `18total` - `18NOhighschool`) %>% 
  mutate(`25highschool` = `25total` - `25NOhighschool` - `25NO9th`)

#create column for total pop and total highschool graduation pop
acs_edu_tn <- acs_edu_tn %>% 
  mutate(total_pop = `18total` + `25total`) %>% 
  mutate(total_highschool_pop = `25highschool` + `18highschool`)
acs_edu_tn <- acs_edu_tn %>% 
  mutate(total_percent_highschool = (total_highschool_pop/total_pop)*100) 
acs_edu_tn <- acs_edu_tn %>% 
  select("NAME", "total_percent_highschool")

#merges educational dataset with tn tract by name and censustract variables.
tn_tract <- tn_tract %>% 
  left_join(acs_edu_tn, by = "NAME")


#### Marital Status Dataset
#load marital data
acs_marital_tn <- read_csv("data/tennessee/acs_marital_tn.csv") 

#removes columns with margins of error
acs_marital_tn <- acs_marital_tn %>% 
  select(-contains("Margin of Error"))

#keeps population total row
acs_marital_tn1 <- acs_marital_tn[1, ]

#pivot acs_marital
acs_marital_tn1 <- acs_marital_tn1 %>% 
  pivot_longer(cols = starts_with("Census Tract"), names_to = "NAME", values_to = "values")

#filters out columns with Tennessee total estimate based on name variable
acs_marital_tn1 <- acs_marital_tn1 %>% filter(!grepl("Tennessee!!Total!!Estimate",NAME))

#creates censustract column and varname column used for pivoting wider
acs_marital_tn1 <- acs_marital_tn1 %>%
  mutate(censustract=(gsub("!!.*","",NAME)),
         varname=(gsub(".*Tennessee!!","",NAME)),
         varname=(gsub("!!Estimate.*", "", varname)))

#pivots wider with varname and value column
acs_marital_tn2 <- acs_marital_tn1 %>% select(-`Label (Grouping)`,-NAME) %>% 
  pivot_wider(names_from=varname,values_from=values)

#merges marital dataset with tn tract by name and censustract variables.
tn_tract <- tn_tract %>% 
  left_join(acs_marital_tn2, by = c("NAME" = "censustract"))


#### Veteran Status Dataset
#load veteran data
acs_veteran_tn <- read_csv("data/tennessee/acs_veteran_tn.csv")

#keep only the first row
acs_veteran_tn <- acs_veteran_tn[1, ]

#select only percent veteran estimates
acs_veteran_tn <- acs_veteran_tn %>% 
  select(contains("Percent Veterans")) %>% 
  select(-contains("Margin of Error"))

#flipping dataset around to format for merging
acs_veteran_tn <- acs_veteran_tn %>% 
  pivot_longer(cols = starts_with("Census Tract"), names_to = "NAME", values_to = "percent_veteran")

#clean NAME column for merge
acs_veteran_tn$NAME <-  gsub("!.*", "", acs_veteran_tn$NAME)
acs_veteran_tn$NAME <- gsub(",", ";", acs_veteran_tn$NAME)

#merge veteran data to main dataset
tn_tract <- tn_tract %>% 
  left_join(acs_veteran_tn, by = "NAME")


#### Median Gross Rent Dataset
#loads median gross rent
acs_gross_rent_tn <- read_csv("data/tennessee/acs_grossrent_tn.csv")

#clears margins of error columns
acs_gross_rent_tn <- acs_gross_rent_tn %>% 
  select(-contains("Margin of Error"))

#converts columns to as character in order to pivot longer
acs_gross_rent_tn <- acs_gross_rent_tn %>%
  mutate(across(starts_with("Census Tract"), as.character))

#pivots longer flipping data so census tracts are 1 column instead of however many
acs_gross_rent_tn <- acs_gross_rent_tn %>% 
  pivot_longer(cols = starts_with("Census Tract"), 
               names_to = "NAME", 
               values_to = "mediangrossrent")

acs_gross_rent_tn <- acs_gross_rent_tn %>%
  mutate(across(starts_with("Census Tract"), as.character))

#cleans census tract column a little
acs_gross_rent_tn$NAME <-  gsub("!.*", "", acs_gross_rent_tn$NAME)
acs_gross_rent_tn$NAME <- gsub(",", ";", acs_gross_rent_tn$NAME)

#removes label column, only keeping rent and census tract name
acs_gross_rent_tn <- acs_gross_rent_tn %>% select(NAME,mediangrossrent)

#joins gross rent data to tn_tract dataset
tn_tract <- tn_tract %>% 
  left_join(acs_gross_rent_tn, by = "NAME")

#creates mediangrossrent column on tn tract for the groups
tn_tract <- tn_tract %>%
  mutate(
    mediangrossrent = ifelse(mediangrossrent == '-' | is.na(mediangrossrent), NA, mediangrossrent))

#converts the mediangrossrent column to numeric
tn_tract <- tn_tract %>%
  mutate(mediangrossrent = as.numeric(mediangrossrent))

#groups mediangrossrent by how much each tract pays for rent
tn_tract <- tn_tract %>%
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
    TRUE ~ NA_character_))

#factor reorders by levels of rent, the exact scale i just made previously
tn_tract <- tn_tract %>%
  mutate(mediangrossrent_group = factor(
    mediangrossrent_group,
    levels = c("<250", "250-500", "500-750", "750-1000", "1000-1250", 
               "1250-1500", "1500-1750", "1750-2000", "2000-2250", "2250-2500", "2500+")))

#creates the map based on how much each tract pays for median gross rent
tm_shape(tn_tract) +
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


#### Race Dataset
#load race data
acs_race_tn <- read_csv("data/tennessee/acs_detailedrace_tn.csv")#reads race info csv

#This line removes margin of error column
acs_race_tn <- acs_race_tn %>% 
  select(-contains("Margin of Error"))

#These lines make new datasets of just the row for individuals based on race in tn county
acs_white_tn <- acs_race_tn[3, ]
acs_black_tn <- acs_race_tn[4, ]
acs_nativeamerican_tn <- acs_race_tn[5, ]
acs_asian_tn <- acs_race_tn[6, ]
acs_hawaiian_tn <- acs_race_tn[7, ]
acs_otherrace_tn <- acs_race_tn[8, ]

#These lines pivot longer to flip the datasets around to where census tracts are 1 column and the rows are which census tract in particular and there is a column for individuals of race
acs_white_tn <- acs_white_tn %>% 
  pivot_longer(cols = starts_with("Census Tract"), names_to = "NAME", values_to = "white") 
acs_black_tn <- acs_black_tn %>% 
  pivot_longer(cols = starts_with("Census Tract"), names_to = "NAME", values_to = "black") 
acs_nativeamerican_tn <- acs_nativeamerican_tn %>% 
  pivot_longer(cols = starts_with("Census Tract"), names_to = "NAME", values_to = "nativeamerican") 
acs_asian_tn <- acs_asian_tn %>% 
  pivot_longer(cols = starts_with("Census Tract"), names_to = "NAME", values_to = "asian") 
acs_hawaiian_tn <- acs_hawaiian_tn %>% 
  pivot_longer(cols = starts_with("Census Tract"), names_to = "NAME", values_to = "hawaiianorislander") 
acs_otherrace_tn <- acs_otherrace_tn %>% 
  pivot_longer(cols = starts_with("Census Tract"), names_to = "NAME", values_to = "otherrace") 

#these lines make the census tract column entries a little cleaner
acs_white_tn$NAME <-  gsub("!.*", "", acs_white_tn$NAME)
acs_white_tn$NAME <- gsub(",", ";", acs_white_tn$NAME)
acs_black_tn$NAME <-  gsub("!.*", "", acs_black_tn$NAME)
acs_black_tn$NAME <- gsub(",", ";", acs_black_tn$NAME)
acs_nativeamerican_tn$NAME <-  gsub("!.*", "", acs_nativeamerican_tn$NAME)
acs_nativeamerican_tn$NAME <- gsub(",", ";", acs_nativeamerican_tn$NAME)
acs_asian_tn$NAME <-  gsub("!.*", "", acs_asian_tn$NAME)
acs_asian_tn$NAME <- gsub(",", ";", acs_asian_tn$NAME)
acs_hawaiian_tn$NAME <-  gsub("!.*", "", acs_hawaiian_tn$NAME)
acs_hawaiian_tn$NAME <- gsub(",", ";", acs_hawaiian_tn$NAME)
acs_otherrace_tn$NAME <-  gsub("!.*", "", acs_otherrace_tn$NAME)
acs_otherrace_tn$NAME <- gsub(",", ";", acs_otherrace_tn$NAME)

#the next lines drop the label column because it is unnecessary and we need to get rid of it for merging
acs_white_tn <- acs_white_tn %>% select(NAME,white)
acs_black_tn <- acs_black_tn %>% select(NAME,black)
acs_nativeamerican_tn <- acs_nativeamerican_tn %>% select(NAME,nativeamerican)
acs_asian_tn <- acs_asian_tn %>% select(NAME,asian)
acs_hawaiian_tn <- acs_hawaiian_tn %>% select(NAME,hawaiianorislander)
acs_otherrace_tn <- acs_otherrace_tn %>% select(NAME,otherrace)

#next lines merge the datasets into the tn tract main set
tn_tract <- tn_tract %>% 
  left_join(acs_white_tn, by = "NAME")
tn_tract <- tn_tract %>% 
  left_join(acs_black_tn, by = "NAME")
tn_tract <- tn_tract %>% 
  left_join(acs_nativeamerican_tn, by = "NAME")
tn_tract <- tn_tract %>% 
  left_join(acs_asian_tn, by = "NAME")
tn_tract <- tn_tract %>% 
  left_join(acs_hawaiian_tn, by = "NAME")
tn_tract <- tn_tract %>% 
  left_join(acs_otherrace_tn, by = "NAME")

#create percent race columns
tn_tract <- tn_tract %>% 
  mutate(percent_white = (white/total_population)*100)
tn_tract <- tn_tract %>% 
  mutate(percent_black = (black/total_population)*100)
tn_tract <- tn_tract %>% 
  mutate(percent_nativeamerican = (nativeamerican/total_population)*100)
tn_tract <- tn_tract %>% 
  mutate(percent_hawaiianorislander = (hawaiianorislander/total_population)*100)
tn_tract <- tn_tract %>% 
  mutate(percent_otherrace = (otherrace/total_population)*100)


#### Hispanic or Latino Data
#reads csv for hispanic or latino individuals in tn county
acs_hl_tn <- read_csv("data/tennessee/acs_hispaniclatino_tn.csv") 

#removes margin of error column
acs_hl_tn <- acs_hl_tn %>% select(-contains("Margin of Error"))

#keeps row of hispanic individuals as own dataset
acs_hispanic_tn <- acs_hl_tn[3, ] 
#makes new dataset of nonhispanic individuals
acs_nonhispanic_tn <- acs_hl_tn[2, ]

#pivots longer so it flips it around so census tracts are rows and not columns
acs_hispanic_tn <- acs_hispanic_tn %>% 
  pivot_longer(cols = starts_with("Census Tract"), names_to = "NAME", values_to = "hispaniclat") 
acs_nonhispanic_tn <- acs_nonhispanic_tn %>% 
  pivot_longer(cols = starts_with("Census Tract"), names_to = "NAME", values_to = "nonhispaniclat") 

#Create data frames for hispanic and nonhispanic
acs_hispanic_tn <- acs_hispanic_tn %>% select(NAME,hispaniclat) 
acs_nonhispanic_tn <- acs_nonhispanic_tn %>% select(NAME,nonhispaniclat)

#clean NAME column for merging into tn_tract
acs_hispanic_tn$NAME <-  gsub("!.*", "", acs_hispanic_tn$NAME)
acs_hispanic_tn$NAME <- gsub(",", ";", acs_hispanic_tn$NAME)
acs_nonhispanic_tn$NAME <-  gsub("!.*", "", acs_nonhispanic_tn$NAME)
acs_nonhispanic_tn$NAME <- gsub(",", ";", acs_nonhispanic_tn$NAME)

#joins hispanic or latino dataset to the main dataset
tn_tract <- tn_tract %>% 
  left_join(acs_hispanic_tn, by = "NAME")

#joins nonhispanic individuals to the main dataset
tn_tract <- tn_tract %>% 
  left_join(acs_nonhispanic_tn, by = "NAME")

tn_tract <- tn_tract %>% 
  mutate(percent_hispaniclat = (hispaniclat/total_population)*100)


##### Employment Data next
acs_emp_tn <- read_csv("data/tennessee/acs_employment_tn.csv")

#removes columns with margins of error
acs_emp_tn <- acs_emp_tn %>% 
  select(-contains("Margin of Error"))

#keeps population total row
acs_emp_tn1 <- acs_emp_tn[5, ]
acs_emp_tn2 <- acs_emp_tn[6, ]

#pivots longer to organize by census tracts as 1 column
acs_emp_tn1 <- acs_emp_tn1 %>% 
  pivot_longer(cols = starts_with("Census Tract"), names_to = "NAME", values_to = "values")
acs_emp_tn2 <- acs_emp_tn2 %>% 
  pivot_longer(cols = starts_with("Census Tract"), names_to = "NAME", values_to = "values")

#these two lines filter out rows that have tennessee estimate in the name
acs_emp_tn1 <- acs_emp_tn1 %>% filter(!str_detect(NAME, regex("County; Tennessee!!Estimate", ignore_case = TRUE)))
acs_emp_tn2 <- acs_emp_tn2 %>% filter(!str_detect(NAME, regex("County; Tennessee!!Estimate", ignore_case = TRUE)))

#creates censustract column and varname column used for pivoting wider
acs_emp_tn1 <- acs_emp_tn1 %>%
  mutate(censustract=(gsub("!!.*","",NAME)),
         varname=(gsub(".*Tennessee!!","",NAME)),
         varname=(gsub("!!Estimate.*", "", varname)))
acs_emp_tn2 <- acs_emp_tn2 %>%
  mutate(censustract=(gsub("!!.*","",NAME)),
         varname=(gsub(".*Tennessee!!","",NAME)),
         varname=(gsub("!!Estimate.*", "", varname)))

#pivots wider with varname and value column
acs_emp_tn1 <- acs_emp_tn1 %>% select(-`Label (Grouping)`,-NAME) %>% 
  pivot_wider(names_from=varname,values_from=values)

#merges employment dataset with tn tract by name and censustract variables.
tn_tract <- tn_tract %>% 
  left_join(acs_emp_tn1, by = c("NAME" = "censustract"))

#renames the column added by the employment data to employedpercent
tn_tract <- tn_tract %>% rename(employedpercent = Percent)

#pivots wider with varname and value column
acs_emp_tn2 <- acs_emp_tn2 %>% select(-`Label (Grouping)`,-NAME) %>% 
  pivot_wider(names_from=varname,values_from=values)

#merges marital dataset with tn tract by name and censustract variables.
tn_tract <- tn_tract %>% 
  left_join(acs_emp_tn2, by = c("NAME" = "censustract"))

#renames the column added by the unemployment dataset from percent to unemployedpercent
tn_tract <- tn_tract %>% rename(Unemployed = Percent)

#adds a county column
tn_tract <- tn_tract %>% 
  # rename(Unemployed = "unemployedpercent") %>% 
  mutate(county = str_extract(NAME, "(?<=;)[^;]+(?=;)")) 
# %>%
#   mutate(county = str_replace_all(county, "County", ""))

tn_tract$county <- gsub(" ", "", tn_tract$county)

save(tn_tract, file="data/tennessee/tn_data.RData")

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

#create PERCENTILE/GROUPING COLUMNS FOR DASHBOARD MAPS
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
    Unemployed > 20 ~ ">20%"))

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
  tm_polygons(col = "geometry", palette = "YlOrRd")

#SAVE DASHBOARD DATA
save(tn_tract_dash, file = "data/tennessee/tn_tract_dash.RData")
