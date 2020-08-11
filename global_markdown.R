#
#
#
#
#
#
#
#     Everything here can be shared among any annual report .Rmd file
#
#
#
#
#
#
#

# Save this file after changing the years 

Year_to_Filter_Data_by <- 2019 # <== CHANGE ME TO FILTER DATA <==

Export_END_Year <- 2019 # <== CHANGE ME TO REFLECT ENDING YEAR OF DATA TEXT FILE <==

One_Meter_Quad_Appendix_Letter <- "E"

Five_Meter_Quad_Appendix_Letter <- "F"

Band_Transect_Appendix_Letter <- "G"

RPC_Appendix_Letter <- "H"

Fish_Transect_Appendix_Letter <- "I"

Nat_Hab_Appendix_Letter <- "J"

Temperature_Appendix_Letter <- "N"

# DO NOT CHANGE the following unless you would like to re-factor the levels or add additional libraries

# UNCOMMENT AND RUN ONCE PER COMPUTER, RECOMMENT AFTER 

# CTRL + SHIFT + C  will comment lines out

# install.packages('tidyverse')
# install.packages('ggpubr')
# install.packages('glue')
# install.packages('lubridate')
# install.packages('rmarkdown')
# install.packages('zoo')
# install.packages('MASS')
# install.packages('vegan')
# install.packages('labdsv')
# install.packages('lme4')
# install.packages('car')
# install.packages('knitr')
# install.packages('tinytex')
# install.packages('Cairo')

library(tidyverse)
library(ggpubr)
library(glue)
library(lubridate)
library(rmarkdown)
library(zoo)
library(MASS)
library(vegan)
library(labdsv)
library(lme4)
library(car)
library(knitr)
library(tinytex)
library(Cairo)
library(ggnewscale)
library(randomForest)
library(pdp)


is.even <- function(x) x %% 2 == 0
is.odd <- function(x) x %% 2 != 0

siteInfo1 <- read_csv("Meta_Data/Site_info.csv")
Species_Info <- read_csv("Meta_Data/SpeciesComplete.csv") 
Fish_Trophic_Levels <- readr::read_csv("Meta_Data/KFM_Fish_Trophic_Levels.csv")

SiteColor <- as.character(siteInfo1$Color)
names(SiteColor) <- siteInfo1$SiteName

SiteLine <- siteInfo1$LineType
names(SiteLine) <- siteInfo1$SiteName

SpeciesColor <- c(as.character(Species_Info$Color))
names(SpeciesColor) <- c(Species_Info$CommonName)
SpeciesColor <- SpeciesColor[!is.na(SpeciesColor)]

BenthicBiomassColor <- c(
  'Lithopoma gibberosa' = "deeppink", 
  'Megastraea undosa' = "grey40", 
  'Patiria miniata' = "orange2", 
  'Strongylocentrotus franciscanus' = "red2", 
  'Strongylocentrotus purpuratus' = "darkorchid2", 
  'Tegula regina' = "yellow", 
  'Pisaster giganteus' = "deepskyblue2", 
  'Crassedoma giganteum' = "gold", 
  'Haliotis rufescens' = "firebrick1",             
  'Kelletia kelletii' = "black",
  'Lytechinus anamesus' = "grey80", 
  'Megathura crenulata' = "aquamarine2", 
  'Pycnopodia helianthoides' = "aquamarine2",       
  'Tethya aurantia' = "gold1", 
  'Macrocystis pyrifera' = "forestgreen")

Fish_Colors <- Fish_Trophic_Levels$Color_R
names(Fish_Colors) <- Fish_Trophic_Levels$CommonName

Island_Colors <- c("San Miguel" = "darkmagenta", "SM" = "darkmagenta", 
                   "Santa Rosa" = "dodgerblue4", "SR" = "dodgerblue4", 
                   "Santa Cruz" = "forestgreen", "SC" = "forestgreen", 
                   "Anacapa" = "darkorange", "AN" = "darkorange", 
                   "Santa Barbara" = "firebrick2","SB" = "firebrick2", 
                   "Inside" = "green", "Outside" = "red") 

IslandLevels <- c("San Miguel", "Santa Rosa", "Santa Cruz", "Anacapa",  "Santa Barbara")
IslandLevelsFull <- c("San Miguel Island", "Santa Rosa Island", "Santa Cruz Island", "Anacapa Island",  "Santa Barbara Island")
MPA_Levels <- c("Santa Rosa", "Santa Cruz", "Anacapa",  "Santa Barbara")
multipatt_island_list <- c(rep("AN", 3), rep("SB", 3), rep("SC", 3), rep("SR", 3))

SiteLevels <- c(
  # San Miguel
  "Wyckoff Ledge", "Miracle Mile", "Hare Rock",
  # Santa Rosa
  "Johnson's Lee North", "Johnson's Lee South", "Rodes Reef", "Cluster Point", "Trancion Canyon", "Chickasaw", "South Point",
  # Santa Cruz
  "Fry's Harbor", "Pelican Bay", "Yellow Banks", "Devil's Peak Member", "Pedro Reef", "Little Scorpion",
  "Gull Island South", "Scorpion Anchorage", "Potato Pasture", "Cavern Point", 
  # Anacapa
  "Admiral's Reef", "East Fish Camp", "Lighthouse", "Cathedral Cove" , "Landing Cove", "Black Sea Bass Reef", "Keyhole", 
  # Santa Barbara
  "Arch Point", "Cat Canyon", "Webster's Arch", "SE Sea Lion Rookery", "Graveyard Canyon", "Southeast Reef")

MonthLevels <- c('May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar', 'Apr')
season_months <- c("May", "June", "July", "August", "September", "October") 

oneM_Biomass_Species <- c(
  "Megastraea undosa",  #  All years
  "Lithopoma gibberosa",  #  All years
  "Tegula regina", # 2006 to present (when they were added as a species)
  "Patiria miniata",  #  All years               
  "Pisaster giganteus", # 1982 - 1995 then 5 m to 2013
  "Strongylocentrotus franciscanus",  #  All years
  "Strongylocentrotus purpuratus")  #  All years
fiveM_Biomass_Species <- c("Pisaster giganteus") # 1996 - 2013 then to Band Transects
bands_Biomass_Species <- c(
  "Tethya aurantia", #  All years 
  "Haliotis rufescens",  #  All years
  "Kelletia kelletii",  #  All years
  "Megathura crenulata",  #  All years
  "Crassedoma giganteum", #  All years
  "Pisaster giganteus",  #  2014 - present 
  "Pycnopodia helianthoides",  #  All years
  "Lytechinus anamesus")  #  All years





