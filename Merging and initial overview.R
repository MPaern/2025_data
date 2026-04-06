# This Rscript is made mainly to put together the data from all sites into 1 csv file
# This script also gives a simple overview of the data.
# 2025 data

# setup -------------------------------------------------------------------
# libraries used in this project
library(tidyverse)
library(ggplot2)
library(dplyr)
#library(suncalc)
#library(suntools)
#library(lubridate)
#library(readr)
#library(esquisse)
#library(MetBrewer)
#library(purrr)
#library(sp)
#library(sf)
#library(hms)
#library(maptools)
#library(ggcorrplot)


# read location overview data in ------------------------------------------------------------

deployment <-  read_csv("data/survey_deployment.csv")
maintenance <-  read_csv("data/survey_maintenance.csv")
overview_data <-  read_csv("data/overview_table_25.csv")
coordinates <- read_csv("data/GPS_2025.csv")
water <- read_csv("data/distance_from_water_2025.csv")


# make directories of all the sites and add the merged id files so when there is a problem with one of the sites, it can be fixed on it's own -----------

# source and destination directories 
source_dir <- "P:/SW_CoastalMonitoring/Data_collection_2025"
destination_dir <- "data/IDfiles"

# list of all 50 site folders in source_dir
site_folders <- list.dirs(source_dir, recursive = FALSE)

# Find id.csv files in every site and merge them into one all_id.csv file

# Loop through each site folder
for (site in site_folders[5]) {
  
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


# Correcting mistakes for 2025 ----------------------------------------------------------
#CM-05 mingi jama autoidga
# df <- read.csv("data/IDfiles/CM-05/all_id.csv")
#
# df$"AUTO ID*" <- coalesce(df[[16]], df[[45]])
#
# df <- df[, append(setdiff(seq_along(df), 46), 46, after = 15)]
# df <- df[-17]
# df <- df[-45]
#
# write.csv(df, "data/IDfiles/CM-05/all_id.csv", row.names = FALSE)

#CM-06 kuupäevaga mingi jama
#
# df <- read.csv("data/IDfiles/CM-06/all_id.csv")
#
# df[[10]] <- as.Date(df[[10]])
# df[[13]] <- as.Date(df[[13]])

# write.csv(df, "data/IDfiles/CM-06/all_id.csv", row.names = FALSE)

# Combine all into one file for 2025  --------------------------------------------------------------

# I have parsing problems, how to get over them?

files_dir <- "data/IDfiles"
site_folders <- list.dirs(files_dir, recursive = FALSE)

# read in all the csv-s

# Loop through each site folder
for (site in site_folders) {
  
  # extract site name
  site_name <- basename(site)
  
  # clean name
  site_name_clean <- str_replace_all(site_name, "-", "")
  
  # construct file path using site_name
  csv_file <- file.path(files_dir, site_name, "all_id.csv")
  
  # Read the CSV file
  input_data <- read_csv(csv_file)
  
  # Dynamically assign it to a variable
  assign(paste0(site_name_clean), input_data, envir = .GlobalEnv)
}

# check what is different in them and fix it

# change column type
# 
# CM01$"ALTERNATE 2" <- as.character(CM01$"ALTERNATE 2")
# CM02$"ALTERNATE 2" <- as.character(CM02$"ALTERNATE 2")
# 
# 
# CM07$DATE <- as.Date(CM07$DATE)
# CM08$DATE <- as.Date(CM08$DATE)
# CM26$DATE <- as.Date(CM26$DATE)
# CM45$DATE <- as.Date(CM45$DATE)
# 
# CM07$`DATE-12` <- as.Date(CM07$`DATE-12`) #this did not work, they are not in date form, make sure they are before moving forward!
# CM08$`DATE-12` <- as.Date(CM08$`DATE-12`)
# CM26$`DATE-12` <- as.Date(CM26$`DATE-12`)
# CM45$`DATE-12` <- as.Date(CM45$`DATE-12`)

# merge csv

# get all CM
dfs <- mget(ls(pattern = "^CM[0-9]{2}$"))

# merge and create Site column
cm_2025 <- bind_rows(dfs, .id = "Site") %>%
  mutate(Site = sub("CM([0-9]{2})", "CM-\\1", Site))

# get rid of extra columns

cm_2025 <- cm_2025 %>%
  mutate(IN_FILE = coalesce(`IN FILE`, IN.FILE)) %>%
  relocate(IN_FILE, .after = FOLDER) %>%
  select(-`IN FILE`, -IN.FILE)

any(is.na(cm_2025$IN_FILE))

cm_2025 <- cm_2025 %>%
  mutate(OUT_FILE_FS = coalesce(`OUT FILE FS`, OUT.FILE.FS)) %>%
  relocate(OUT_FILE_FS, .after = DURATION) %>%
  select(-`OUT FILE FS`, -OUT.FILE.FS)

any(is.na(cm_2025$OUT_FILE_FS))

# delete extra columns that are not needed

cm_2025 <- cm_2025 %>%
  select(-OUT.FILE.ZC, -REVIEW.ORGID, -REVIEW.USERID, -MANUAL.ID)

cm_2025 <- cm_2025 %>%
  mutate(autoid = coalesce(`AUTO ID*`, AUTO.ID., `AUTO ID`)) %>%
  relocate(autoid, .after = `HOUR-12`) %>%
  select(-`AUTO ID*`, -AUTO.ID., -`AUTO ID`)
any(is.na(cm_2025$autoid))

cm_2025 <- cm_2025 %>%
  mutate(ALTERNATE_2 = coalesce(`ALTERNATE 2`, ALTERNATE.2)) %>%
  relocate(ALTERNATE_2, .after = `ALTERNATE 1`) %>%
  select(-`ALTERNATE 2`, -ALTERNATE.2)

cm_2025 <- cm_2025 %>%
  mutate(ALTERNATE_1 = coalesce(`ALTERNATE 1`, ALTERNATE.1)) %>%
  relocate(ALTERNATE_1, .after = MARGIN) %>%
  select(-`ALTERNATE 1`, -ALTERNATE.1)

cm_2025 <- cm_2025 %>%
  mutate(MATCH_RATIO = coalesce(`MATCH RATIO`, MATCH.RATIO)) %>%
  relocate(MATCH_RATIO, .after = MATCHING) %>%
  select(-`MATCH RATIO`, -MATCH.RATIO)

cm_2025 <- cm_2025 %>%
  mutate(HOUR_12 = coalesce(`HOUR-12`, HOUR.12)) %>%
  relocate(HOUR_12, .after = `TIME-12`) %>%
  select(-`HOUR-12`, -HOUR.12)

cm_2025 <- cm_2025 %>%
  mutate(TIME_12 = coalesce(`TIME-12`, TIME.12)) %>%
  relocate(TIME_12, .after = `DATE-12`) %>%
  select(-`TIME-12`, -TIME.12)

cm_2025 <- cm_2025 %>%
  mutate(DATE_12 = coalesce(`DATE-12`, DATE.12)) %>%
  relocate(DATE_12, .after = `HOUR`) %>%
  select(-`DATE-12`, -DATE.12)


write.csv(cm_2025, "cm_2025_total.csv")


# one file for 2025 -------------------------------
cm <- read.csv("cm_2025_total.csv")

# check data

str(cm) 
summary(cm) 
summary(cm$autoid) 
colSums(is.na(cm))

na_rows <- cm %>% filter(is.na(DATE))

#choose variables for dataset
cm <- cm %>% 
  rename(
    filename = "OUT_FILE_FS") %>% 
  mutate(autoid = factor(autoid)) %>% 
  dplyr::select(OUTDIR, FOLDER, IN_FILE, filename, DURATION, 
                DATE, TIME, HOUR,
                DATE_12, TIME_12, HOUR_12,
                autoid, PULSES, MATCH_RATIO, ALTERNATE_1, Site
  )

# rows that don't have a file name, but have autoid
na_rows <- cm %>% filter(is.na(filename)) 
# delete Noise rows and those that last too little
na_rows <- na_rows[na_rows$autoid != "Noise", ]
na_rows <- na_rows[na_rows$DURATION > 1, ]
# why is there 57 rows of data without filenames?

# 4 locations have the DATE columns in wrong format (CM-07, CM-08, CM-26, CM-45)

# new date column
cm$DATE_clean <- as.Date(NA)

# 2 formats
idx_iso <- grepl("^\\d{4}-\\d{2}-\\d{2}$", cm$DATE)
idx_short <- grepl("^\\d{1,2}-\\d{1,2}-\\d{2}$", cm$DATE)

cm$DATE_clean[idx_iso] <- ymd(cm$DATE[idx_iso])
cm$DATE_clean[idx_short] <- dmy(cm$DATE[idx_short])

# Fix year (all short ones should be 2025)
cm$DATE_clean[idx_short] <- update(cm$DATE_clean[idx_short], year = 2025)

# Final format
cm$DATE_clean <- format(cm$DATE_clean, "%Y-%m-%d")

# Same for DATE_12 column

# new column
cm$DATE_12_clean <- as.Date(NA)

# 2 formats
idx_iso <- grepl("^\\d{4}-\\d{2}-\\d{2}$", cm$DATE_12)
idx_short <- grepl("^\\d{1,2}-\\d{1,2}-\\d{2}$", cm$DATE_12)

# Parse
cm$DATE_12_clean[idx_iso] <- ymd(cm$DATE_12[idx_iso])
cm$DATE_12_clean[idx_short] <- dmy(cm$DATE_12[idx_short])

# Fix year (all short ones should be 2025)
cm$DATE_12_clean[idx_short] <- update(cm$DATE_12_clean[idx_short], year = 2025)

# Final format
cm$DATE_12_clean <- format(cm$DATE_12_clean, "%Y-%m-%d")

# make dates
cm$DATE_clean <- as.Date(cm$DATE_clean)
cm$DATE_12_clean <- as.Date(cm$DATE_12_clean)

# remove other date columns and replace them with new ones

cm <- cm %>%
  select(-DATE) %>% 
  relocate(DATE_clean, .after = DURATION) %>%
  rename (DATE = DATE_clean) %>% 
  select(-DATE_12) %>% 
  relocate(DATE_12_clean, .after = HOUR) %>%
  rename (DATE_12 = DATE_12_clean)
  

# find PIPPIP and change them to PIPPYG

cm$autoid[cm$autoid == "PIPPIP"] <- "PIPPYG"

# Overview table for 2025--------------------------

# new table called overview_summary

overview_summary <- cm %>%
  group_by(Site) %>%
  summarise(
    total = n(),
    noise = sum(autoid == "Noise"),
    noise_pct = 100 * noise / total,
    pnatcalls = sum(autoid == "PIPNAT"),
    batcalls = sum(autoid!="Noise"),
    pnatcalls_pct = 100 * pnatcalls/ batcalls,
    days_active = sum(n_distinct(DATE)),
    earliest_active = min(DATE),
    last_active = max(DATE),
  )

# add info from other tables
# Make date more readable
deployment <- deployment %>%
  mutate(`Date and Time Deployed` = parse_date_time(`Date and Time Deployed`,
                                                    orders = "m/d/Y H:M"))

retrieval <- maintenance %>% filter(`Type of maintenance` == "Retrival") %>%
  mutate(
    `Date and Time` = parse_date_time(`Date and Time`,
                                      orders = c("m/d/Y H:M:S", "d/m/Y H:M")))

# Join onto deployment
deployment <- deployment %>%
  left_join(retrieval, by = "Site Name")

deployment <- deployment %>% 
  rename(
    Site = "Site Name")

coordinates <- coordinates %>% 
  rename(
    Site = "Name")

overview_data <- overview_data %>% 
  rename(
    Site = Location)

overview_summary <- overview_summary %>%
  left_join(
    deployment %>%
      select(
        Site,
        date_deployed = `Date and Time Deployed`,
        date_retrieved = `Date and Time`,
      ),
    by = "Site"
  )

overview_summary <- overview_summary %>%
  left_join(
    overview_data %>%
      select(Site,
             type = type),
    by= "Site"
  )

overview_summary <- overview_summary %>%
  left_join(
    coordinates %>%
      select(Site,
             latitude = Latitude,
             longitude = Longitude),
    by= "Site"
  )

# distance to water bodies

overview_summary <- overview_summary %>%
  left_join(
    water %>%
      select(Site,
             m_to_freshwater = "distance to freshwater (m)",
             m_to_coast = "distance to coast (m)"),
    by= "Site"
  )


# one working file for 2025--------------


# found 2 gaps (CM-35, CM-44) that are not explained, ran Kaleidoscope for those timeframes again to get an id.csv file

df35 <- read.csv("P:/SW_CoastalMonitoring/Data_collection_2025/CM-35/WAV/KPRO_V1/New_25.09.2025/id.csv")
df44 <- read.csv("P:/SW_CoastalMonitoring/Data_collection_2025/CM-44/WAV/KPRO_V1/New_30.09.2025/id.csv")

df35$Site <- "CM-35"
df44$Site <- "CM-44"

df35$filename <- df35$IN.FILE
df44$filename <- df44$IN.FILE

df35 <- df35 %>% 
  rename(
    IN_FILE = "IN.FILE",
    DATE_12 = "DATE.12",
    TIME_12 = "TIME.12",
    HOUR_12 = "HOUR.12",
    autoid = "AUTO.ID.",
    MATCH_RATIO = "MATCH.RATIO",
    ALTERNATE_1 = "ALTERNATE.1") %>% 
  mutate(autoid = factor(autoid),
         DATE_12 = as.Date(DATE_12, format = "%d/%m/%Y"),
         DATE = as.Date(DATE, format = "%d/%m/%Y")) %>% 
  dplyr::select(OUTDIR, FOLDER, IN_FILE, filename, DURATION, 
                DATE, TIME, HOUR,
                DATE_12, TIME_12, HOUR_12,
                autoid, PULSES, MATCH_RATIO, ALTERNATE_1, Site
  )

df44 <- df44 %>% 
  rename(
    IN_FILE = "IN.FILE",
    DATE_12 = "DATE.12",
    TIME_12 = "TIME.12",
    HOUR_12 = "HOUR.12",
    autoid = "AUTO.ID.",
    MATCH_RATIO = "MATCH.RATIO",
    ALTERNATE_1 = "ALTERNATE.1") %>% 
  mutate(autoid = factor(autoid),
         DATE_12 = as.Date(DATE_12, format = "%d/%m/%Y"),
         DATE = as.Date(DATE, format = "%d/%m/%Y")) %>% 
  dplyr::select(OUTDIR, FOLDER, IN_FILE, filename, DURATION, 
                DATE, TIME, HOUR,
                DATE_12, TIME_12, HOUR_12,
                autoid, PULSES, MATCH_RATIO, ALTERNATE_1, Site
  )

# find what files are not in cm and add those there

new_from_35 <- df35 %>%
  anti_join(cm, by = "filename")

new_from_44 <- df44 %>%
  anti_join(cm, by = "filename")

cm <- cm %>%
  bind_rows(new_from_35, new_from_44)


# full file

write.csv(cm, "cm_2025.csv")

cm <- read.csv("cm_2025.csv")

# Where is the most noise, visualisation  -------------------------------------------------------------------

# plot of noise and nr of bat calls
ggplot(overview_summary) +
  aes(x = `noise_pct`, y = `batcalls`, colour = type) +
  geom_point(size = 3.35, 
             shape = "diamond") +
  scale_color_viridis_d(option = "cividis", direction = 1) +
  labs(x = "Percentage of noise files in recordings", 
       y = "Number of recordings with bat calls", title = "Correlation between noise and bat calls", subtitle = "By location") +
  theme_classic() +
  theme(legend.text = element_text(face = "bold"), legend.title = element_text(face = "bold"))

barnoise <- ggplot(cm) + 
  geom_bar(aes(x= Site, fill = autoid), position = "fill") +
  scale_fill_manual(name = "AutoID", values = met.brewer("Signac", n = 14)) + 
  ylab("Proportion of recordings") + 
  xlab("Site") +
  theme(text = element_text(size = 10)) 

barnoise


# visualize recording period


ggplot(cm) + 
  geom_bin2d(aes(x = DATE_12, y = Site), bins = 100) +  # Adjust bins for detail
  scale_fill_viridis_c() +  # Better color scale for density
  xlab("Month") + ylab("Site") +
  scale_x_date(date_breaks = "1 month", , date_labels = "%b") +
  ggtitle("Recording period 2025") + 
  theme_minimal()


# gaps in dataset, more info for overview ---------------------------------------------------------
# DATE_12 is the start of the night eg. the start date. 

# all dates all sites should be active on
complete_dates <- overview_summary %>% 
  mutate(
    BeginDate = as.Date(date_deployed),
    EndDate   = coalesce(as.Date(date_retrieved), as.Date("2025-10-22")) - 1,
    full_dates = map2(BeginDate, EndDate, ~ seq.Date(from = .x, to = .y, by = "day"))
  ) %>%
  select(Site, full_dates) %>%
  unnest(full_dates) %>%
  rename(ExpectedDate = full_dates)

# all dates I have in the dataset for each site
available_dates <- cm %>%
  mutate(ActualDate = as.Date(DATE_12)) %>%
  reframe(ActualDate = unique(ActualDate), .by = "Site")

# missing nights for each site
missing_dates <- complete_dates %>%
  anti_join(available_dates, by = c("Site", "ExpectedDate" = "ActualDate"))

# View missing nights by site
ggplot(missing_dates) + 
  geom_point(aes(x = ExpectedDate, y = Site)) +  
  scale_fill_viridis_c() + 
  xlab("Date in season") + ylab(" ") +
  ggtitle("Missing dates") + 
  theme_minimal()

# add missing nights to overview_summary

n_missing_days <- missing_dates %>%
  group_by(Site) %>%
  summarise(
    missing_days = n(),
  )

overview_summary <- overview_summary %>%
  left_join(
    n_missing_days %>%
      select(Site,
             missing_days = missing_days),
    by= "Site"
  )

# change NA to 0 in missing_days

overview_summary <- overview_summary %>% 
  mutate(missing_days = ifelse(is.na(missing_days), 0, missing_days))

# some types still wrong

overview_summary[1,13] = "inland" 
overview_summary[29,13] = "coast" 
overview_summary[34,13] = "coast" 

# change missing retrival dates to 22-10-2025

overview_summary <- overview_summary %>% 
  mutate(date_retrieved = if_else(
    is.na(date_retrieved),
    as.Date("2025-10-22"),
    date_retrieved
  ))

#final product ----------------------------------------------------------------------------

write.csv(overview_summary, "overview_2025.csv")

# going of from here --------------------

overview_summary <- read.csv("overview_2025.csv")

# write.csv(missing_dates, "Missing_dates.csv")

write.csv(missing_dates, "missing_dates_2025.csv")


#working with cm

cm <- read.csv("cm_2025_total.csv")
cm <- read.csv("cm_2025.csv")

cm$DATE <- as.Date(cm$DATE)
cm$DATE_12 <- as.Date(cm$DATE_12)


str(cm) 
summary(cm) 
summary(cm$autoid) 
colSums(is.na(cm))
