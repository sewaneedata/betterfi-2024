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

#Api key(opetional if needed)
# api_key <- readLines("data/census_api_key.txt")
# census_api_key(api_key)

#call data for lender locations
lender_info <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1x9KSndXG_z2PusNq0ThaQF40VmzJ7G5A/edit?usp=sharing&ouid=113284526452189132173&rtpof=true&sd=true")

#which company has the most locations?
lender_info %>% 
  group_by(`Company Name`) %>% 
  tally() %>% 
  arrange(desc(n))

#call all variables from ACS
ACS_2022_vars <- read_xlsx("data/ACS2022_Table_Shells.xlsx")

#call ACS variables from 2022
acs_2022 <- load_variables(year = 2022, dataset = "acs1", cache = TRUE)

# #map mode
# tmap_mode("plot")
# tmap_mode("view")



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

#heat map for income level below poverty line and unployed population 
tm_shape(pivot_first_acs) +
  tm_polygons(alpha = 0.8, col = c('poverty_income', 'unemployed')) +
  tm_facets(as.layers = TRUE) #makes several layered maps that you can toggle between