#load libraries
library(tidycensus)
library(tidyverse)
library(ggplot2)
library(stringr)
library(lubridate)
library(gsheet)
library(readr)
library(readxl)

#call data
lender_info <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1x9KSndXG_z2PusNq0ThaQF40VmzJ7G5A/edit?usp=sharing&ouid=113284526452189132173&rtpof=true&sd=true")

#which company has the most locations
lender_info %>% 
  group_by(`Company Name`) %>% 
  tally() %>% 
  arrange(desc(n))


test <- read_xlsx("ACS2022_Table_Shells.xlsx")


test_acs <- load_variables(year = 2022, dataset = "acs1", cache = TRUE)

tn_acs <- get_acs(geography = "tract", 
                  state = "TN",
                  variables = c(
                    gross_rent1 = "B25063_001",
                    gross_rent2 = "B25063_002",
                    gross_rent3 = "B25063_003",
                    contract_rent = "B25056_001",
                    median_gross_rent_bedroom = "B25064_001",
                    poverty_income = "B23024_002"
                  ),
                  year = 2022,
                  geometry = TRUE)
tn_acs <- tn_acs %>% 
  filter(NAME == "Hamilton County, Tennessee") #does not work anymore bc census tracts included in tidycensus
