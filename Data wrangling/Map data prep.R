#------------------------------------------------------------------------------#
# Map of Medway - Data cleaning
# Author: Sam Finnegan
# Date: 20-04-2026
#------------------------------------------------------------------------------#

# Clean up and load libraries --------------------------------------------------
rm(list=ls())
library(tidyverse)
library(janitor)
library(sf)
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

# Read in data from previous election ------------------------------------------

# Note needed anymore because there's a summary sheet. Derp
# SheetNames <- getSheetNames("./Data/Stats/2023 results and national trend model.xlsx")
# 
# Election23Results <- data.frame()
# # Cleaning data
# for (i in 1:24) {
#   
#   # Read in most data
#   Sheet <- read.xlsx("./Data/Stats/2023 results and national trend model.xlsx",
#                      sheet = i,
#                      startRow = 6) |>
#     clean_names()
#   
#   # Read in number of electors from cell B1
#   Electors <- read.xlsx("./Data/Stats/2023 results and national trend model.xlsx",
#                         sheet = i,
#                         rows = 1,
#                         cols = 2) |>
#     colnames() |>
#     as.numeric()
#   
#   # Read in turnout from cell B3
#   Turnout <- read.xlsx("./Data/Stats/2023 results and national trend model.xlsx",
#                        sheet = i,
#                        rows = 3,
#                        cols = 2) |>
#     colnames() |>
#     as.numeric()
#   
#   # Combine Sheet with electors and turnout, 
#   Sheet <- Sheet |>
#     mutate(electors = Electors,
#            turnout = Turnout,
#            wd25nm = SheetNames[i])
#   
#   Election23Results <- Election23Results |>
#     rbind(Sheet)
#   
# }
# 
# # Make a couple of names match
# Election23Results <- Election23Results |>
#   mutate(party = case_when(party == "Labour and Co-operative Party" ~ "Labour Party",
#                            party == "Conservative Party candidate" ~ "Conservative Party",
#                            party == "Local Conservatives" ~ "Conservative Party",
#                            TRUE ~ party),
#          wd25nm = case_when(wd25nm == "Hoo St W & High Halstow" ~ "Hoo St Werburgh & High Halstow",
#                             wd25nm == "Strood N & Frindsbury" ~ "Strood North & Frindsbury",
#                             TRUE ~ wd25nm),
#          colour = case_when(party == "Labour Party" ~ "#E4003B",
#                             party == "Conservative Party" ~ "#0087DC",
#                             party == "Green Party" ~ "#00643B",
#                             party == "Liberal Democrats" ~ "#FF6400",
#                             party == "Reform UK" ~ "#12B6CF",
#                             party == "Independent" ~ "#ABABAB",
#                             party == "Social Democratic Party" ~ "#6C2f56",
#                             party == "Christian Peoples Alliance" ~ "#6142A3",
#                             party == "Heritage Party" ~ "#0A00A5"),
#          percent_of_votes = percent_of_votes/100,
#          percent_of_electors = percent_of_electors/100,
#          percent_of_electorate = percent_of_electorate/100,
#          text_colour = "#000000")

# Election data cleaning -------------------------------------------------------
Election23Results <- read.xlsx("./Data/Stats/2023 results and national trend model.xlsx",
                               sheet = "Summary") |>
  clean_names() |>
  rename(wd25nm = ward,
         electors = registered_electors) |>
  mutate(party = case_when(party == "Liberal Democrats" ~ "Lib Dems",
                           TRUE ~ party)) |>
  mutate(colour = case_when(party == "Independent" ~ "#ABABAB",
                            party == "Conservative" ~ "#0087DC",
                            party == "Labour" ~ "#E4003B",
                            party == "Reform" ~ "#12B6CF",
                            party == "Green" ~ "#00643B",
                            party == "Lib Dems" ~ "#FF6400",
                            party == "Labour Co-op" ~ "#E4003B",
                            party == "SDP" ~ "#6C2F56",
                            party == "CPA" ~ "#6142A3",
                            party == "Heritage" ~ "#0A00A5"),
         text_colour = "#000000") 

# Read in the number of councillors in each ward
# This can come from each sheet, so read in a loop
SheetNames <- getSheetNames("./Data/Stats/2023 results and national trend model.xlsx")

NumCouncillorsLookup <- data.frame()

# Loop over ward sheets, avoiding national and summary
for (i in 1:24) {
  
  # Ward
  wd25nm <- SheetNames[i]
  
  # Councillor number always in cell B2
  number_councillors <- read.xlsx("./Data/Stats/2023 results and national trend model.xlsx",
                                  sheet = i,
                                  rows = 2,
                                  cols = 2) |>
    colnames() |>
    as.numeric()
  
  sheet <- c(wd25nm, number_councillors)
  
  NumCouncillorsLookup <- NumCouncillorsLookup |> rbind(sheet)
  rm(wd25nm, number_councillors, sheet)
  
}

# Some tidying up before join
colnames(NumCouncillorsLookup) <- c("wd25nm", "number_councillors")
NumCouncillorsLookup <- NumCouncillorsLookup |>
  mutate(wd25nm = case_when(wd25nm == "Hoo St W & High Halstow" ~ "Hoo St Werburgh & High Halstow",
                            wd25nm == "Strood N & Frindsbury" ~ "Strood North & Frindsbury",
                            TRUE ~ wd25nm))

# Join
Election23Results <- Election23Results |> left_join(NumCouncillorsLookup)


# Member to ward analysis cleaning ---------------------------------------------

M2W <- read.xlsx("./Data/Stats/Member to Ward Target Analysis - 190326.xlsx",
                 sheet = 1,
                 startRow = 4) |>
  clean_names() |>
  select(wd25nm, average_residents_per_member) |>
  mutate(average_residents_per_member = case_when(average_residents_per_member == 0 ~ NA,
                                                  TRUE ~ average_residents_per_member),
         rank = rank(average_residents_per_member,
                     na.last = TRUE,
                     ties.method = "min"),
         rank_word = case_when(is.na(average_residents_per_member)~ "Not targeting",
                               TRUE ~ paste0(
                                 as.character(rank),
                                 case_when(rank %in% c("1", "21") ~ "st",
                                           rank %in% c("2", "22") ~ "nd",
                                           rank %in% c("3", "23") ~ "rd",
                                           TRUE ~ "th")
                               )),
         rank_word = case_when(rank_word == "Not targeting" ~ "Not targeting",
                               rank_word == "1st" ~ "Most",
                               rank_word == "18th" ~ "Least",
                               rank_word == "17th" ~ "2nd least",
                               TRUE ~ paste0(rank_word, " most")))


# Read in canvassing data ------------------------------------------------------

Canvas <- read.csv("./Data/Stats/Canvasing ratings 260426.csv") |>
  clean_names() |>
  select(ward, revised_average) |>
  rename(wd25nm = ward) |>
  right_join(NumCouncillorsLookup) |>
  select(-number_councillors)

# Projecting 2026 vote based on national polling -------------------------------

# YouGov polling data from https://yougov.com/en-gb/articles/53923-how-would-britain-vote-at-the-start-of-2026
YouGov <- read.csv("./Data/Stats/YouGovGenderAge.csv") |>
  mutate(across(3:8, ~ . / 100))

# British Election Survey has turnout data by age and gender
# https://www.britishelectionstudy.com/data-object/wave-25-of-the-2014-2023-british-election-study-internet-panel/
BES <- haven::read_dta("./Data/Stats/BES2019_W25_v30.1.dta") |>
  select(age, gender, localTurnoutRetro) |>
  mutate(gender = case_when(gender == 1 ~ "Male",
                            gender == 2 ~ "Female"),
         localTurnoutRetro = case_when(is.na(localTurnoutRetro) ~ 0,
                                       localTurnoutRetro == 1 ~ 1,
                                       TRUE ~ NA),
         ageGroup = case_when(age <= 24 ~ "18-24",
                              age >= 25 & age <= 49 ~ "25-49",
                              age >= 50 & age <= 64 ~ "50-64",
                              age >= 65 ~ "65+")) |>
  filter(!is.na(localTurnoutRetro)) |>
  group_by(ageGroup, gender) |>
  summarise(turnout = mean(localTurnoutRetro))

# Now we need population by gender and age, which we got from nomis
# This is 2024 population, based on 2021-based population projections
# Males
popsM <- read.xlsx("./Data/Stats/nomis_2026_04_26_014535.xlsx",
                   rows = c(7:98)) |> 
  pivot_longer(cols = 2:25,
               names_to = "ward",
               values_to = "population") |>
  mutate(age = as.numeric(gsub("[^0-9]", "", Age)),
         ageGroup = case_when(age <= 24 ~ "18-24",
                              age >= 25 & age <= 49 ~ "25-49",
                              age >= 50 & age <= 64 ~ "50-64",
                              age >= 65 ~ "65+"),
         ward = gsub("\\.", " ", ward),
         gender = "Male") |>
  group_by(ward, gender, ageGroup) |>
  summarise(population = sum(population)) |>
  ungroup() |>
  mutate(wd25nm = case_when(ward == "All Saints (Medway)" ~ "All Saints",
                            ward == "Princes Park (Medway)" ~ "Princes Park",
                            ward == "Watling (Medway)" ~ "Watling",
                            TRUE ~ ward)) |>
  select(-ward)

# Females
popsF <- read.xlsx("./Data/Stats/nomis_2026_04_26_014535.xlsx",
                   rows = 108:199) |>
  pivot_longer(cols = 2:25,
               names_to = "ward",
               values_to = "population") |>
  mutate(age = as.numeric(gsub("[^0-9]", "", Age)),
         ageGroup = case_when(age <= 24 ~ "18-24",
                              age >= 25 & age <= 49 ~ "25-49",
                              age >= 50 & age <= 64 ~ "50-64",
                              age >= 65 ~ "65+"),
         ward = gsub("\\.", " ", ward),
         gender = "Female") |>
  group_by(ward, gender, ageGroup) |>
  summarise(population = sum(population)) |>
  ungroup() |>
  mutate(wd25nm = case_when(ward == "All Saints (Medway)" ~ "All Saints",
                            ward == "Princes Park (Medway)" ~ "Princes Park",
                            ward == "Watling (Medway)" ~ "Watling",
                            TRUE ~ ward)) |>
  select(-ward)


# Join pops
Pops <- rbind(popsF, popsM)
rm(popsM, popsF)

# Join BES, YouGov, and Pops and calculate projection
ProjectionSF <- Pops |>
  left_join(BES, by = c("gender", "ageGroup")) |>
  left_join(YouGov, by = c("gender", "ageGroup")) |>
  mutate(voters = population * turnout,
         Green_votes = voters * Green,
         Labour_votes = voters * Labour,
         `Lib Dems_votes` = voters * `Liberal.Democrat`,
         Other_votes = voters * Other,
         Conservative_votes = voters * Conservative,
         Reform_votes = voters * `Reform.UK`) |>
  select(c(4, 1, 2, 13:18)) |>
  pivot_longer(cols = 4:9,
               names_to ="party",
               names_transform = ~gsub("_votes", "", .),
               values_to = "votes") |>
  group_by(wd25nm, party) |>
  summarise(votes = sum(votes)) |>
  group_by(wd25nm) |>
  mutate(votes_percent = votes/sum(votes)) |>
  ungroup() |>
  mutate(colour = case_when(party == "Other" ~ "#ABABAB",
                            party == "Conservative" ~ "#0087DC",
                            party == "Labour" ~ "#E4003B",
                            party == "Reform" ~ "#12B6CF",
                            party == "Green" ~ "#00643B",
                            party == "Lib Dems" ~ "#FF6400"),
         text_colour = "#000000")


# Before saving, I want to add colours columns to the map data
# If we have canvassing data then we'll use "#00643b" and "#00a85a" on click
# If not, we'll use "#333333" and "#999999" on click
Wards <- Wards |>
  left_join(Canvas) |>
  mutate(fill_colour = case_when(is.na(revised_average) ~ "#999999",
                                 TRUE ~ "#00a85a"),
         fill_click_colour = case_when(is.na(revised_average) ~ "#333333",
                                       TRUE ~ "#00643b")) |>
  select(-revised_average)


# Save data
qs_savem(Wards,
         LSOA,
         Election23Results,
         M2W,
         Canvas,
         ProjectionSF,
         file = "C:/RProjects/MedwayMap/app/data/appData.qs2")
