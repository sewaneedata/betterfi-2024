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

api_key <- readLines("data/census_api_key.txt")
census_api_key(api_key)

#call data
lender_info <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1x9KSndXG_z2PusNq0ThaQF40VmzJ7G5A/edit?usp=sharing&ouid=113284526452189132173&rtpof=true&sd=true")

#which company has the most locations
lender_info %>% 
  group_by(`Company Name`) %>% 
  tally() %>% 
  arrange(desc(n))


test <- read_xlsx("data/ACS2022_Table_Shells.xlsx")


test_acs <- load_variables(year = 2022, dataset = "acs1", cache = TRUE)

tn_acs <- get_acs(geography = "tract", 
                  state = "TN",
                  county = "Hamilton",
                  variables = c(
                    gross_rent1 = "B25063_001",
                    gross_rent2 = "B25063_002",
                    gross_rent3 = "B25063_003",
                    contract_rent = "B25056_001",
                    median_gross_rent_bedroom = "B25064_001",
                    poverty_income = "B23024_002"
                  ),
                  summary_var = "B01003_001",
                  year = 2022,
                  geometry = TRUE) 


tmap_mode("view")

poverty_level <- tn_acs %>% filter(variable == "poverty_income")

tm_shape(poverty_level) +
  tm_polygons(alpha = 0.8, col = 'estimate')


#kyle
n_acs <- get_acs(geography = "tract",
                 state = "TN",
                 county = "Hamilton",
                 variables = c(
                   gross_rent1 = "B25063_001",
                   gross_rent2 = "B25063_002",
                   gross_rent3 = "B25063_003",
                   contract_rent = "B25056_001",
                   median_gross_rent_bedroom = "B25064_001",
                   poverty_income = "B23024_002",
                   white = "C02003_003", #detailed race codes start here
                   afr_amr = "C02003_004",
                   nativeamr = "C02003_005",
                   asian = "C02003_006",
                   pac_isl = "C02003_007",
                   otherrace = "C02003_008",
                   lessthan500 = "C25122_003", #household income last 12 months start here
                   btwn500and999 = "C25122_004",
                   over1000 = "C25122_005",
                   btwn10kand19999 = "C25122_007",
                   btwn20kand34999 = "C25122_012",
                   btwn35kand49999 = "C25122_017",
                   btwn50kand74999 = "C25122_022",
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
                   noschooling = "C15003_002", #educational attainment for population 25 yrs and over
                   nurseryto4thgr = "C15003_003",
                   fifthand6thgr = "C15003_004",
                   seventhand8thgr = "C15003_005",
                   ninthgrade = "C15003_006",
                   tenthgrade = "C15003_007",
                   eleventhgrade = "C15003_008",
                   twelthgradenodiploma = "C15003_009",
                   diploma = "C15003_010",
                   ged = "C15003_011",
                   somecollegelt1yr = "C15003_012",
                   oneyrnodegree = "C15003_013",
                   associates = "C15003_014",
                   bachelors = "C15003_015",
                   mastersdegree = "C15003_016",
                   professionalschooldegree = "C15003_017",
                   doctorate = "C15003_018",
                 ),
                 summary_var = "B01003_001",
                 year = 2022,
                 geometry = TRUE)

