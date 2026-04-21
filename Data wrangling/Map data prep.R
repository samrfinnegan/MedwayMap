#------------------------------------------------------------------------------#
# Map of Medway - Data cleaning
# Author: Sam Finnegan
# Date: 20-04-2026
#------------------------------------------------------------------------------#

# Clean up and load libraries --------------------------------------------------
rm(list=ls())
library(tidyverse)
library(janitor)
library(leaflet)
library(sf)
library(qs2)

# Read in some data ------------------------------------------------------------

# Want to create two possible maps:
# - Ward
# - LSOA

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


rm(LSOALookup, LSOAIMD)
qs_savem(Wards,
         LSOA,
         file = "C:/RProjects/MedwayMap/app/data/MapData.qs2")



leaflet(Wards) |>
  addProviderTiles("CartoDB", group = "CartoDB") |>
  addPolygons(weight = 1,
              color = "#ffffff",
              fillColor = "#00643b",
              fillOpacity = 0.7)


leaflet(LSOA) |>
  addProviderTiles("CartoDB", group = "CartoDB") |>
  addPolygons(weight = 1,
              color = "#ffffff",
              fillColor = "#00643b",
              fillOpacity = 0.7)
