# winter data 2025-2026

#2025 winter data setup--------
library(tidyverse)
library(ggplot2)
library(lubridate)
library(dplyr)
library(fs)
library(esquisse)
library(hms)

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
  mutate( id = coalesce(`MANUAL ID`, `MANUAL ID*`), 
          INDIR = coalesce(INDIR, INDIR...1, INDIR...45) ) %>% 
  select( -`MANUAL ID`, -`MANUAL ID*`, -INDIR...1, -INDIR...45 )

all$INDIR_new <- coalesce(all[[1]], all[[44]])

colSums(is.na(all))

all[1] <- NULL
all[43] <- NULL

all <- all %>%
  mutate(
    TIME = coalesce(TIME, `TIME*`),
    autoid = coalesce(`AUTO ID*`, `AUTO ID`)
  ) %>%
  select(
    -`TIME*`,
    -`AUTO ID*`, -`AUTO ID`
  ) %>%
  rename(
    INDIR = INDIR_new,
    filename = `OUT FILE FS`
  ) %>%
  relocate(INDIR, .before = OUTDIR)

all <- all %>% 
  mutate(filename = coalesce(filename, `IN FILE`))

# check file

colSums(is.na(all))
unique(all$INDIR)
unique(all$OUTDIR)
unique(all$autoid)
unique(all$id)
unique(all$`DATE-12`)

# fix mistakes

all$INDIR[all$INDIR == "P:\\SW_CoastalMonitoring\\Data_collection_winter\\CM-06\\DATA"] <- "P:\\SW_CoastalMonitoring\\Data_collection_winter\\CM-06\\DATA\\25.11.2025_CM-06"
all$autoid[all$autoid == "PIPPIP"] <- "PIPPYG"

write.csv(all, "cm_winter_total.csv")

# all <- read.csv("cm_winter_total.csv")

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

colSums(is.na(cm))
unique(cm$OUTDIR)
unique(cm$autoid)
unique(cm$id)
unique(cm$`DATE_12`)

# fix mistakes

cm$id[cm$id == "PIPNAT"] <- "PNAT"

write.csv(cm, "cm_winter.csv")

# manual id-s ------------------------------------

cm <- read.csv("cm_winter.csv")

cm[1]<- NULL

unique(cm$id)

cm <- cm %>%
  mutate(manual = id) %>%
  separate_rows(manual, sep = "_")

unique(cm$manual)

# new column with Noise as NA value

cm$manualwnoise <- cm$manual
cm$manualwnoise[is.na(cm$manualwnoise)] <- "Noise"


# plots----------------

cm$DATE <- as.Date(cm$DATE)
cm$DATE_12 <- as.Date(cm$DATE_12)

cm$TIME <- as_hms(cm$TIME)
cm$TIME_12 <- as_hms(cm$TIME_12)


ggplot(
  cm %>%
    filter(manualwnoise != "Noise") %>% 
    droplevels(),
  aes(x = DATE_12, color = taxa)
) + 
  stat_count(geom = "point", size = 4, alpha = 0.90, color = "royalblue") +
  ylab("Recordings per night") + 
  xlab("Month") +
  ggtitle("Overall amount of calls") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_minimal()+
  theme(text = element_text(size = 20))

ggplot(cm %>% filter(manualwnoise != "Noise"), aes(x = DATE_12)) + 
  stat_count(geom = "point", size = 4, alpha = 0.7, color = "royalblue4") +
  ylab("Number of bat passes per night") + xlab("Month") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_minimal()

# activity levels per site
ggplot(cm) +
  aes(x = HOUR_12) +
  geom_histogram(bins = 30L, fill = "royalblue4") +
  theme_minimal() +
  facet_wrap(vars(Site))

esquisser()

# species 

cm %>%
  filter(!(manualwnoise %in% c("Noise", "PISP", "NYCT", "MYSP"))) %>%
  count(DATE_12, manualwnoise) %>%
  ggplot() +
  aes(x = DATE_12, y = manualwnoise, size = n) +
  geom_point(colour = "green4", alpha = 0.8) +
  labs(
    x = "Month",
    y = "Species",
    title = "Winter bat calls 2025/2026"
  ) +
  scale_size_continuous(range = c(4, 12))+
  theme_linedraw()+
  theme(
    plot.title = element_text(size = 20L),
    axis.title.y = element_text(size = 16L),
    axis.title.x = element_text(size = 16L)
  )

# sites 

cm %>%
  filter(!(manualwnoise %in% c("Noise", "PISP", "NYCT", "MYSP"))) %>%
  count(DATE_12, manualwnoise, Site) %>%
  ggplot(aes(x = DATE_12, y = manualwnoise, size = n)) +
  geom_point(colour= "blue3", alpha = 0.5) +
  facet_wrap(~ Site) +
  labs(
    x = "Month",
    y = "Species",
    title = "Winter bat calls 2025/2026"
  ) +
  scale_size_continuous(range = c(4, 12)) +
  theme_linedraw() +
  theme(
    plot.title = element_text(size = 20),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16)
  )


# match ratio calculations------------------
# pnat

pnat_data <- cm %>%
  filter(autoid == "PIPNAT") %>%
  mutate(
    MATCH_RATIO = as.numeric(MATCH_RATIO),
    correct = ifelse(manualwnoise == "PNAT", 1, 0)) %>%
  filter(!is.na(MATCH_RATIO))


pnat_glm <- glm(
  correct ~ MATCH_RATIO,
  family = binomial,
  data = pnat_data)

summary(pnat_glm)

# Create values
x <- seq(0, 1, length.out = 100)

# Logistic function
y <- 1 / (1 + exp(-(-3.2591 + 5.9644 * x)))

# Plot
plot(x, y, type = "l", lwd = 2,
     xlab = "MATCH_RATIO",
     ylab = "Predicted Probability (correct)",
     main = "Logistic Regression Curve")

# calculates the match ratio cut-off by inverting the logistic regression
get_cutoff <- function(p, model) {
  beta0 <- coef(model)[1]
  beta1 <- coef(model)[2]
  (log(p / (1 - p)) - beta0) / beta1
}

# target probabilities for which we want to know the match ratio
target_probs <- c(0.5, 0.6, 0.7, 0.8, 0.9)

# create table with probability column and fill with values from get-cutoff + lost and retained calls + percentage
loss_table <- data.frame(Probability = target_probs) %>%
  mutate(
    Cutoff_Ratio = pmin(
      sapply(Probability, get_cutoff, model = pnat_glm),
      1),
    Calls_retained = sapply(Cutoff_Ratio, function(c) 
      sum(pnat_data$MATCH_RATIO >= c)),
    Calls_lost = sapply(Cutoff_Ratio, function(c) 
      sum(pnat_data$MATCH_RATIO < c)),
    Percent_loss = round(
      (Calls_lost / nrow(pnat_data)) * 100, 2 ) )

print(loss_table)

#enil
enil_data <- cm %>%
  filter(autoid == "EPTNIL") %>%
  mutate(
    MATCH_RATIO = as.numeric(MATCH_RATIO),
    correct = ifelse(manualwnoise == "ENIL", 1, 0)) %>%
  filter(!is.na(MATCH_RATIO))


enil_glm <- glm(
  correct ~ MATCH_RATIO,
  family = binomial,
  data = enil_data)

summary(enil_glm)

#ppyg
ppyg_data <- cm %>%
  filter(autoid == "PIPPYG") %>%
  mutate(
    MATCH_RATIO = as.numeric(MATCH_RATIO),
    correct = ifelse(manualwnoise == "PPYG", 1, 0)) %>%
  filter(!is.na(MATCH_RATIO))


ppyg_glm <- glm(
  correct ~ MATCH_RATIO,
  family = binomial,
  data = ppyg_data)

summary(ppyg_glm)

# from some point in the year, the match ratio for Enil is not reliable at all and should be basically discarded

# activity minutes and further data handling----------------------------------------



















