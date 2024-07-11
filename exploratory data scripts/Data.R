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
coord_data_total <- mp_geocode(addresses = total_info$full_address, key = "AIzaSyAaG4e9IKfLvQBEbyUMY2ko-02UjFHe1LQ")

#add coordinate data to master data file
  total_lender_info <-  mp_get_points(coord_data_total)

#save new data for coordinates
 save(total_lender_info, file="total_lender_info.RData")

load("data/total_lender_info.RData") 