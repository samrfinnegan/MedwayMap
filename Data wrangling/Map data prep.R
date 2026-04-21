#------------------------------------------------------------------------------#
# Map of Medway - Data cleaning
# Author: Sam Finnegan
# Date: 20-04-2026
#------------------------------------------------------------------------------#

# Clean up and load libraries --------------------------------------------------
rm(list=ls())
library(tidyverse)
library(janitor)
library(qs2)
library(openxlsx)

# Read in shape files and lookups ----------------------------------------------


# Read shape files
Wards <- read_sf("./Data/Shapefiles/Wards_May_2025_v2_BGC") |>
  clean_names() |>
  st_transform("WGS84") |>
  filter(lad25nm == "Medway")

LSOAIMD <- read_sf("./Data/Shapefiles/LSOA_IMD25") |>
  clean_names() |>
  st_transform("WGS84")

# Read lookups
LSOALookup <- read.csv("C:/RProjects/MedwayMap/Data/Lookups/LSOA2021toEW2025toLAD2025_Best_Fit_v2.csv") |>
  clean_names() |>
  filter(lad25nm == "Medway")

# Join lookups to shape files
LSOA <- LSOAIMD |>
  left_join(LSOALookup, by = c("lsoa21cd", "lsoa21nm")) |>
  filter(lad25nm == "Medway")

# Save 
rm(LSOALookup, LSOAIMD)
qs_savem(Wards,
         LSOA,
         file = "C:/RProjects/MedwayMap/app/data/MapData.qs2")


# Read in data from previous election ------------------------------------------

SheetNames <- getSheetNames("./Data/Stats/2023 results and national trend model.xlsx")

Election23Results <- data.frame()
# Cleaning data
for (i in 1:24) {
  
  # Read in most data
  Sheet <- read.xlsx("./Data/Stats/2023 results and national trend model.xlsx",
                     sheet = i,
                     startRow = 6) |>
    clean_names()
  
  # Read in number of electors from cell B1
  Electors <- read.xlsx("./Data/Stats/2023 results and national trend model.xlsx",
                        sheet = i,
                        rows = 1,
                        cols = 2) |>
    colnames() |>
    as.numeric()
  
  # Read in turnout from cell B3
  Turnout <- read.xlsx("./Data/Stats/2023 results and national trend model.xlsx",
                       sheet = i,
                       rows = 3,
                       cols = 2) |>
    colnames() |>
    as.numeric()
  
  # Combine Sheet with electors and turnout, 
  Sheet <- Sheet |>
    mutate(electors = Electors,
           turnout = Turnout,
           wd25nm = SheetNames[i])
  
  Election23Results <- Election23Results |>
    rbind(Sheet)
  
}

# Make a couple of names match
Election23Results <- Election23Results |>
  mutate(party = case_when(party == "Labour and Co-operative Party" ~ "Labour Party",
                           party == "Conservative Party candidate" ~ "Conservative Party",
                           party == "Local Conservatives" ~ "Conservative Party",
                           TRUE ~ party),
         wd25nm = case_when(wd25nm == "Hoo St W & High Halstow" ~ "Hoo St Werburgh & High Halstow",
                            wd25nm == "Strood N & Frindsbury" ~ "Strood North & Frindsbury",
                            TRUE ~ wd25nm),
         colour = case_when(party == "Labour Party" ~ "#E4003B",
                            party == "Conservative Party" ~ "#0087DC",
                            party == "Green Party" ~ "#00643B",
                            party == "Liberal Democrats" ~ "#FF6400",
                            party == "Reform UK" ~ "#12B6CF",
                            party == "Independent" ~ "#ABABAB",
                            party == "Social Democratic Party" ~ "#6C2f56",
                            party == "Christian Peoples Alliance" ~ "#6142A3",
                            party == "Heritage Party" ~ "#0A00A5"),
         percent_of_votes = percent_of_votes/100,
         percent_of_electors = percent_of_electors/100,
         percent_of_electorate = percent_of_electorate/100,
         text_colour = "#000000")

# Save
qs_savem(Election23Results,
         file = "./app/data/Election23Results.qs2")
