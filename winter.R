# winter data 2025-2026

#version 2 of 2024 data--------
library(tidyverse)
library(ggplot2)
library(lubridate)
library(dplyr)
library(fs)

# make directories of all the sites and add the merged id files so when there is a problem with one of the sites, it can be fixed on it's own -----------

# source and destination directories 
source_dir <- "P:/SW_CoastalMonitoring/Data_collection_winter"
destination_dir <- "data/IDfileswinter"

# list of all 50 site folders in source_dir
site_folders <- list.dirs(source_dir, recursive = FALSE)

# Find id.csv files in every site and merge them into one all_id.csv file

# Loop through each site folder
for (site in site_folders) {
  
  site_wav <- path(site, "WAV")
  site_name <- path_file(site)
  
  cat("Processing:", site_name, "\n")
  
  # Find id.csv recursively
  csv_files <- dir_ls(
    site_wav,
    recurse = TRUE,
    regexp = "id\\.csv$",
    type = "file",
    fail = FALSE
  )
  
  csv_files <- csv_files[!grepl("[/\\\\]Data[/\\\\]", csv_files, ignore.case = TRUE)]
  
  if (length(csv_files) > 0) {
    
    merged_data <- bind_rows(lapply(csv_files, read_csv)) # Efficient merging 
    
    # Create a corresponding site folder in "data" 
    site_output_folder <- file.path(destination_dir, site_name) 
    if (!dir.exists(site_output_folder)) 
      dir.create(site_output_folder, recursive = TRUE) 
    
    # Save merged file 
    
    write_csv(merged_data, file.path(site_output_folder, "all_id.csv")) }
}

# list files

files <- list.files(
  path = "data/IDfileswinter",
  pattern = "\\.csv$",   # use double backslash for regex
  recursive = TRUE,      # search subfolders
  full.names = TRUE      # return full path
)

# merge files adding a Site column

all <- files |>
  lapply(function(f) {
    read_csv(f, ) |>
      mutate(Site = basename(dirname(f)))
  }) |>
  bind_rows()

# get rid of extra columns --------------- # not done yet

all <- all %>%
  mutate(id = coalesce(`MANUAL ID`, `MANUAL ID*`)) %>%
  select(-`MANUAL ID`, -`MANUAL ID*`)

short <- all %>%
  select(id, Site, DATE)

str(short)
short$DATE <- as.Date(short$DATE, format: "%d-%m-%Y")
str(cm) 
summary(cm) 
summary(cm$autoid) 
colSums(is.na(short))

short <- na.omit(short)

short2<- filter(short, id!="Noise")

ggplot(
  short2 %>%
    filter(DATE >= as.Date("2025-11-09")) %>% 
    droplevels(),
  aes(x = DATE, color = taxa)
) + 
  stat_count(geom = "point", size = 4, alpha = 0.90, color = "royalblue") +
  ylab("Recordings per night") + 
  xlab("Month") +
  ggtitle("Overall amount of calls") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_minimal()+
  theme(text = element_text(size = 20))

short2$DATE <- as.Date(short2$DATE, format= "%d/%m/%Y")









any(is.na(cm_2025$IN_FILE))

cm_2025 <- cm_2025 %>%
  mutate(OUT_FILE_FS = coalesce(`OUT FILE FS`, OUT.FILE.FS)) %>%
  relocate(OUT_FILE_FS, .after = DURATION) %>%
  select(-`OUT FILE FS`, -OUT.FILE.FS)

any(is.na(cm_2025$OUT_FILE_FS))
