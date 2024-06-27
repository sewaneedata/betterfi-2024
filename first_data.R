#load libraries
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

#set working directory
# setwd("/Users/buchananlindsey/Desktop/buck_datalab/betterfi-2024/data")


#Api key(opetional if needed)
# api_key <- readLines("data/census_api_key.txt")
# census_api_key(api_key)

#call data for lender locations from GOOGLE DRIVE
payday_info <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1x9KSndXG_z2PusNq0ThaQF40VmzJ7G5A/edit?gid=1545321409#gid=1545321409")

flex_info <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1x9KSndXG_z2PusNq0ThaQF40VmzJ7G5A/edit?gid=525051790#gid=525051790")

title_info <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1x9KSndXG_z2PusNq0ThaQF40VmzJ7G5A/edit?gid=1401445265#gid=14014452650")

#merge data 

total_info <- rbind(payday_info, title_info, flex_info) 
total_info<-distinct(total_info, Street, .keep_all=TRUE)
total_info <- total_info %>% 
  rename(postal_code = `Postal Code`)
#which company has the most locations?
lender_info %>% 
  group_by(`Company Name`) %>% 
  tally() %>% 
  arrange(desc(n))

#call all variables from ACS
ACS_2022_vars <- read_xlsx("data/ACS2022_Table_Shells.xlsx")

#call ACS variables from 2022
acs_2022 <- load_variables(year = 2022, dataset = "acs1", cache = TRUE)


#create dataset with several different variavles from ACS
first_acs <- get_acs(geography = "tract",
                 state = "TN",
                 county = "Hamilton",
                 variables = c(
                   gross_rent1 = "B25063_001",
                   gross_rent2 = "B25063_002",
                   gross_rent3 = "B25063_003",
                   contract_rent = "B25056_001",
                   median_gross_rent_bedroom = "B25064_001",
                   poverty_income = "B23024_002",
                   # white = "B02003_003", #detailed race codes start here
                   # afr_amr = "B02003_004",
                   # nativeamr = "B02003_005",
                   # asian = "B02003_006",
                   # pac_isl = "B02003_007",
                   # otherrace = "B02003_008",
                   lessthan10k = "B25122_002", #household income last 12 months start here
                   btwn10kand19999 = "B25122_019",
                   btwn20kand34999 = "B25122_036",
                   btwn35kand49999 = "B25122_053",
                   btwn50kand74999 = "B25122_070",
                   btwn75kand99999 = "B25122_087",
                   over100k = "B25122_104",
                   nevermarried = "B26104_002", #group quarters type by marital status starts here
                   nowmarried = "B26104_003",
                   seperated = "B26104_004",
                   widowed = "B26104_005",
                   divorced = "B26104_006",
                   inlf = "B23025_002", #employment status 16 and over
                   civlf = "B23025_003",
                   employed = "B23025_004",
                   unemployed = "B23025_005",
                   armedforces = "B23025_006",
                   notinlf = "B23025_007",
                   allocatedveteran = "B99211_002", #allocation of veteran status
                   notallocatedveteranstatus = "B99211_003",
                   noschooling = "B15003_002", #educational attainment for population 25 yrs and over
                   nurseryschool = "B15003_003",
                   kindergarten = "B15003_004",
                   firstgrade = "B15003_005",
                   secondgrade = "B15003_006",
                   thirdgrade = "B15003_007",
                   fourthgrade = "B15003_008",
                   fifthgrade = "B15003_009",
                   sixthgrade = "B15003_010",
                   seventhgrade = "B15003_011",
                   eighthgrade = "B15003_012",
                   ninthgrade = "B15003_013",
                   tenthgrade = "B15003_014",
                   eleventhgrade = "B15003_015",
                   twelthgradenodiploma = "B15003_016",
                   diploma = "B15003_017",
                   ged = "B15003_018",
                   somecollegelt1yr = "B15003_019",
                   oneyrnodegree = "B15003_020",
                   associates = "B15003_021",
                   bachelors = "B15003_022",
                   mastersdegree = "B15003_023",
                   professionalschooldegree = "B15003_024",
                   doctorate = "B15003_025"
                 ),
                 summary_var = "B01003_001",
                 year = 2022,
                 geometry = TRUE)

#PIVOT TO CREATE COLUMNS WITH EACH VARIABLE FOR EACH UNIQUE CENSUS TRACT
pivot_first_acs <- first_acs %>% 
  select(-moe) %>% 
  pivot_wider(names_from = variable, values_from = estimate)

# #map mode
# tmap_mode("plot")
tmap_mode("view")

#heat map for income level below poverty line and unployed population 
tm_shape(pivot_first_acs) +
  tm_polygons(alpha = 0.8, col = c('poverty_income', 'unemployed'), id = "NAME") +
  tm_facets(as.layers = TRUE) #makes several layered maps that you can toggle between


#tidygeocoder work

# lat_longs <- lender_info %>%
#   geocode(street = "Street", city = "City", postalcode = "Postal Code", state = "State", , country = "Country", method = 'osm', lat = latitude , long = longitude)

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

# clean STREET column
# problems: format: FIRST LINE (PROBLEM) SECOND LINE (SOLUTION)

# 1203A Scottsville Highway, Lafayette, TN 37083 <- 
# 1203A Scottsville Road, Lafayette, TN 37083, ROAD NOT HIGHWAY THANK YOU WRONG DATA!!!!

# 811 N.W. Broad Street, Murfreesboro, TN 37129
# 811 NW Broad Street, Murfreesboro, TN 37129, NO PERIODS IN DIRECTIONS
# BUT GEO DOESNT CARE ABOUT PERDIOS WHEN THERE IS ONLY ONE DIRECTION EG: N. VS N

# 101 Reed Street, Portland, TN 37148
# ???

# 321 Dexter Woods Road Suite 3, Waynesboro, TN 38485
# 321 Dexter L Woods Memorial Blvd, Waynesboro, TN 38485, WRONG ADDRESS AWESOME!

# 1600 Carmack Blvd. Ste A, Columbia, TN 38401
# 1600 Carmack Blvd., Columbia, TN 38401, NO SUITE
### SUITE ABBR VARIATIONS: Suite, SUITE, Ste, STE, STE., Suites ###

# 214 Gallatin Pike South, Madison, TN 37115
# ???

# 1640 Hwy. 46 South, Dickson, TN 37055
# ???

# 909 New Hwy 68, Sweetwater, TN 37874
# ???

# 19798 Alberta Street B-8, Oneida, TN 37841
# 19798 Alberta Street, Oneida, TN 37841, NO STRANGE LETTERS/SYMBOL AFTER STREET

# 6729 Cumberland Gap Pkwy.#2, Harrogate, TN 37752
# 6729 Cumberland Gap Parkway, Harrogate, TN 37752, NO NUMBERS/SYMBOL AFTER PKWY

#16952A Rankin Ave, Dunlap, TN 37327
#16952 Rankin Ave, Dunlap, TN 37327, NO LETTER AFTER STREET NUMBER


# 718 Gallatin Road, Nashville, TN 37206
# 718 Gallatin Ave, Nashville, TN 37206

# 1641 N. Memorial Blvd., Murfreesboro, TN 37129
# 1641 N. Memorial Blvd, Murfreesboro, TN 37129, NO PERIOD AFTER BLVD

# 1016 Gallatin Pike South, Madison, TN 37115
# 1016 Gallatin Pike, Madison, TN 37115, GEO DIDNT LIKE SOUTH IN THIS ADDRESS

# 310 Kimball Crossing Drive Unit #5, Kimball, TN 37347
# 310 Kimball Crossing Drive, Kimball, TN 37347, NO "UNIT"/"UNITS"

# 113 Clairborne Lane, Jacksboro, TN 37757
# ???
 
# 151 FORKS OF THE RIVER PKWY, SEVIERVILLE, TN 37862-3457
# 151 FORKS OF THE RIVER PKWY, SEVIERVILLE, TN 37862, NO ZIP GREATER THAN 5 DIGITS

# 3397 Main St, Jasper (Kimball), TN 37347
# 3397 Main St, Jasper, TN 37347, NO PARENTHESES

# 
# 

# heres the sample data
test_coords <- read_csv("test_coords") %>% 
  filter(is.na(lat) | is.na(long))

geo(address = '909 NW Broad St, Murfreesboro, TN 37130', method = 'osm') %>% print()



#create address column for lender_info
total_info <- total_info %>% 
  mutate(full_address = paste0(Street,", ",City)) %>% 
  mutate(full_address = paste0(full_address,", ",State)) %>% 
  mutate(full_address = paste0(full_address," ",postal_code))

# Create small test dataset to see which address cannot convert to coordinates
total_info_test <- total_info %>% 
  head(275)
#lat_long <- geo(address = total_info_test$full_address, method = 'osm')

#write csv for test file so we do not waste time
write.csv(lat_long, file = "test_coords", row.names = TRUE)

#test cleaning address for coord reading
test_cleaning2 <- test %>% 
  filter(is.na(lat) | is.na(long)) %>% 
  mutate(address = gsub(" N.", " North ", address, fixed = TRUE)) %>%  #replace N. with North
  mutate(address = gsub(" N ", " North ", address, fixed = TRUE)) %>% #replace " N " with North
  mutate(address = gsub(" S.", " South ", address, fixed = TRUE)) %>%  #replace S. with South
  mutate(address = gsub(" E.", " East ", address, fixed = TRUE)) %>%   #replace E. with East
  mutate(address = gsub(" E ", " East ", address, fixed = TRUE)) %>% #replace E with East
  mutate(address = gsub(" W.|W.", " West ", address)) %>%  #replace W. with West
  mutate(address = gsub("Hwy|HWY ", "Highway ", address)) %>%  #replace Hwy and HWY with Highway
  mutate(address = gsub("Unit [^,]+|Unit \\d+", " ", address)) %>% #get rid of unit
  mutate(address = gsub("Suite \\d+|Ste \\d+|Ste. \\d+|STE \\d+|Ste.|STE.|Suite [^,]+|Ste [^,]+", "", address))  %>% #get rid of suits
  mutate(address = gsub(" ,", ",", address)) %>% #Handle double space before comma
  mutate(address = gsub("  ", " ", address))  # Handle any double spaces


lat_long2 <- geo(address = test_cleaning$address, method = 'osm')
test_cleaning2 <- lat_long2 %>% 
  filter(is.na(lat) | is.na(long))
# missing_lat_long <- lat_long %>% 
test_cleaning %>% filter(is.na(lat) | is.na(long)) %>% print(n = 112)



