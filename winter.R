# winter data 2025-2026

#2025 winter data setup--------
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
  mutate(
    id = coalesce(`MANUAL ID`, `MANUAL ID*`),
    INDIR = coalesce(INDIR, INDIR...1, INDIR...45)
  ) %>%
  select(
    -`MANUAL ID`, -`MANUAL ID*`,
    -INDIR...1, -INDIR...45
  )

all$INDIR_new <- paste(all[[1]], all[[44]])

colSums(is.na(all))

all[1] <- NULL
all[43] <- NULL

all <- all %>%
  mutate(
    TIME = coalesce(TIME, `TIME*`),
    autoid = coalesce("AUTO ID*", "AUTO ID")
  ) %>%
  select(
    -`TIME*`,
    -"AUTO ID*", -"AUTO ID"
  ) %>%
  rename(
    INDIR = "INDIR_new",
    filename = "OUT FILE FS"
  ) %>%
  relocate(INDIR, .before = OUTDIR)

all <- all %>% 
  mutate(filename = coalesce(filename, "IN FILE"))

write.csv(all, "cm_winter_total.csv")

# smaller dataset to work with---------------------------------

cm <- all %>% 
  rename(
    IN_FILE = "IN FILE",
    DATE_12 = "DATE-12",
    TIME_12 = "TIME-12",
    HOUR_12 = "HOUR-12",
    MATCH_RATIO = "MATCH RATIO",
    ALTERNATE_1 = "ALTERNATE 1",
    ALTERNATE_2 = "ALTERNATE 2") %>% 
  mutate(autoid = factor(autoid),
         DATE_12 = as.Date(DATE_12, format = "%d/%m/%Y"),
         DATE = as.Date(DATE, format = "%d/%m/%Y")) %>% 
  dplyr::select(OUTDIR, FOLDER, IN_FILE, filename, DURATION, 
                DATE, TIME, HOUR,
                DATE_12, TIME_12, HOUR_12,
                autoid, PULSES, MATCH_RATIO, ALTERNATE_1, ALTERNATE_2, Site, id
  )

write.csv(cm, "cm_winter.csv")

# overview of data preparing the dataset------------------------------------













# plots----------------

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

