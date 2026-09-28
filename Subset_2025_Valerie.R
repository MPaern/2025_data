# Subset for Valerie for manual analysis
# data from only 2025
# from lakes in Lista that no one has looked at - CM-14 and CM-05

# first subset of PPYG and PNAT for classifyers from CM-56 and CM-05 spring 2026

# Sara's Masters thesis data prep and innitial overview

library(tidyverse)
library(lubridate)

# library(data.table)
# library(beepr)
# library(purrr)
# library(janitor)
# library(renv)
# library(stringr)
# library(beepr)
# library(randomcoloR)
# library(wesanderson)
# library(leaflegend)
# library(osmdata)
# library(MetBrewer)
# library(colorBlindness)
# library(colorblindcheck)
# library(MoMAColors)


# output directory 
output <-"C:/Users/mapa/OneDrive - Norwegian University of Life Sciences/Desktop/Sound examples"

file.name <- "PPYG"

todays_date <- Sys.Date()

dir.name <- str_c(output,"/", file.name, "_", todays_date)
dir.name

output_today <- dir.name
output_today

dir.create(output_today)
output_today


# Read in the processed data and look for potential errors 

# Sites CM-56 and CM-05

inputCM56 <- read_csv("P:/SW_CoastalMonitoring/Data_collection_2026/CM-56/ID/23.04.2026_CM-56/id.csv") 
# 6173 obs of 44 vars
# Add a site column 
inputCM56$site <- "CM-56"

inputCM05 <- read_csv("P:/SW_CoastalMonitoring/Data_collection_2026/CM-05/ID/23.04.2026_CM-05/id.csv") 
# 20131 obs of 44 vars
# Add a site column 
inputCM05$site <- "CM-05"


# Randomly select 100 PPYG files

ppyg_sub <- inputCM56 %>% 
  filter(`MANUAL ID` == "PPYG") %>% 
  droplevels() %>% 
  group_by(site) %>% slice_sample(n=100) %>% 
  ungroup() 
dim(ppyg_sub)
# 100   45

# Select all PNAT files

pnat_sub <- inputCM56 %>% 
  filter(`MANUAL ID` == "PNAT") %>% 
  droplevels() %>% 
  ungroup() 
dim(pnat_sub)
# 67   45

# copy paste those to the file

file.copy(
  from = file.path("P:/SW_CoastalMonitoring/Data_collection_2026/CM-56/WAV/KPRO_V1_23.04.2026_CM-56/Data", ppyg_sub$`OUT FILE FS`),
  to   = file.path("C:/Users/mapa/OneDrive - Norwegian University of Life Sciences/Desktop/Sound examples/PPYG_2026-09-28", ppyg_sub$`OUT FILE FS`),
  overwrite = TRUE
)

file.copy(
  from = file.path("P:/SW_CoastalMonitoring/Data_collection_2026/CM-56/WAV/KPRO_V1_23.04.2026_CM-56/Data", pnat_sub$`OUT FILE FS`),
  to   = file.path("C:/Users/mapa/OneDrive - Norwegian University of Life Sciences/Desktop/Sound examples/PNAT_2026-09-28", pnat_sub$`OUT FILE FS`),
  overwrite = TRUE
)

