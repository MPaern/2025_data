# Subset for Valerie for manual analysis
# data from only 2025
# Everything from lakes in Lista that no one has looked at - CM-14

# first subset of PPYG and PNAT for classifyers from CM-56 and CM-05 spring 2026--------------

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

# subset from several lakes to Valerie----------------------------------
# For example CM-06, CM-05?, CM-32 and CM-52
# Available 130GB

library(data.table)
library(tidyverse)
library(lubridate)
library(purrr)
library(janitor)
library(beepr)
library(renv)
library(stringr)
library(RColorBrewer)
library(scales)
#library(randomcoloR)
#library(wesanderson)
#library(leaflegend)
#library(osmdata)
#library(MetBrewer)
#library(colorBlindness)
#library(colorblindcheck)
#library(stringdist)


## Setup output directory 
output <-"C:/Users/mapa/OneDrive - Norwegian University of Life Sciences/BatLab Norway/Projects/CoastalMonitoring/Analyses/Outputs/Maris/Subset_2025" # where you want to save your data

file.name <- "Subset2025"

todays_date <- Sys.Date()

dir.name <- str_c(output,"/", file.name, "_", todays_date)
dir.name

output_today <- dir.name
output_today

dir.create(output_today)
output_today
# "C:/Users/mapa/OneDrive - Norwegian University of Life Sciences/BatLab Norway/Projects/CoastalMonitoring/Analyses/Outputs/Maris/Subset_2025/Subset2025_2026-10-07"


# Subset dataset 
#Sites: CM-06, CM-32, CM-52

cm <- read_csv("cm_2025.csv")

cm[1] <- NULL

cm_sub <- cm %>%
  filter(Site %in% c("CM-06", "CM-32", "CM-52"))

# Housekeeping
df1 <- cm_sub %>% 
  mutate(autoid = factor(autoid), 
         site = factor(Site)) %>% 
  dplyr::select(OUTDIR, FOLDER, filename, 
                DATE, TIME, HOUR,
                DATE_12, TIME_12, HOUR_12,
                autoid, site
  ) %>% droplevels()
dim(df1)
# 721118     11

auto_overview <- df1 %>% 
  group_by(autoid) %>%
  summarise(percentage = n() / nrow(df1) * 100) %>% 
  mutate(percentage = as.integer(percentage)) 
## 24% noise

summary(df1)

summary(df1$autoid)
# BARBAR EPTNIL MYOBRA MYODAU MYOMYS MYONAT   NoID  Noise NYCNOC PIPNAT PIPPYG PLEAUR VESMUR 
# 5163 304254    108  11059    675    118  59209 179467   1826  23562 132121   2850    706 


#What is the percentage of noise recorded across sites?
cm06 <- df1 %>% filter(site == "CM-06")  
cm32 <- df1 %>% filter(site == "CM-32")  
cm52 <- df1 %>% filter(site == "CM-52")  
 

# noiserat_cm06 <- cm06 %>%
#   group_by(autoid) %>%
#   summarise(percentage = n() / nrow(cm06) * 100) %>%
#   mutate(percentage = as.integer(percentage))
# # 24% noise

# noiserat_cm32 <- cm32 %>%
#   group_by(autoid) %>%
#   summarise(percentage = n() / nrow(cm32) * 100) %>%
#   mutate(percentage = as.integer(percentage))
# # 25% noise

# noiserat_cm52 <- cm52 %>%
#   group_by(autoid) %>%
#   summarise(percentage = n() / nrow(cm52) * 100) %>%
#   mutate(percentage = as.integer(percentage))
# # 26% noise


## Look at overall recording period 

ggplot(df1) +
  geom_point(aes(x = DATE_12, y = site),
             color = "orange", alpha = 0.5, size = 3) +
  xlab("Date in season") + ylab(" ") +
  ggtitle("Recording activity") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 25),
        axis.line = element_line(colour = "black"))

# CM-52 had a hole in september, others are ok


## Remove noise and take another look 

levels(df1$autoid)
summary(df1$autoid)
# 179467 noise files that we will remove. 

df2 <- df1 %>% dplyr::filter(autoid != "Noise") %>% droplevels()
dim(df2)
# 541651  obs of  11 vars 

summary(df2$autoid)

table(df1$site, df1$autoid)
# Randomly select 500 Noise files from each site for her to validate


df1$month <-  as.factor(format(df1$DATE, "%B") ) # Full month name (e.g., "January")
summary(df1$month)


table(df1$site, df1$month)
#         April August   July   June  March  May October September
# CM-06  71832 110044  67317  82046  17423  71039   15580     50837
# CM-32   2168  26467  17950  20618      0   9873    2004     14083
# CM-52  22349  42390  25109   5762   3575  31976    8095      2581

## The only month that we don't have recordings for all sites is April. 
DF <- df1 %>% 
  dplyr::filter(month %in% c("April", "May", "June" , "July", "August", "September", "October")) %>% 
  droplevels()
dim(DF)
# 700120 obs 12 vars 

## Now with all BBAR, PAUR, NNOC, VMUR, PNAT and Myotis species combined 
levels(df1$autoid)

sub1 <- DF %>% filter(
  autoid %in% c(
    "BARBAR", "PLEAUR", "VESMUR", "NYCNOC", 
    "MYOBRA", "MYODAU", "MYOMYS", "MYONAT",
    "PIPNAT"))
dim(sub1)
# 45818 obs of 12 vars 

# sub 1 is too big, try to cut some things out like PNAT

table(sub1$site, sub1$autoid)
#       BARBAR EPTNIL MYOBRA MYODAU MYOMYS MYONAT  NoID Noise NYCNOC PIPNAT PIPPYG PLEAUR VESMUR
# CM-06   2843      0      0    550      2      5     0     0    576  15500      0   1400    523
# CM-32    329      0     81   1239    564     87     0     0   1076   1566      0   1211     54
# CM-52   1973      0     27   9247    109     25     0     0    105   6424      0    239     63

sub1_1 <- DF %>% filter(
  autoid %in% c(
    "BARBAR", "PLEAUR", "VESMUR", "NYCNOC", 
    "MYOBRA", "MYODAU", "MYOMYS", "MYONAT"))
dim(sub1_1)
# 22328 obs of 12 vars 

# sub 1_1 is still too big, try to cut some things out like MDAU

sub1_2 <- DF %>% filter(
  autoid %in% c(
    "BARBAR", "PLEAUR", "VESMUR", "NYCNOC", 
    "MYOBRA", "MYOMYS", "MYONAT"))
dim(sub1_2)
# 11292 obs of 12 vars 


## Overview of soprano pip autoid recordings
ppyg <- DF %>% 
  dplyr::filter(autoid %in% c("PIPPYG")) %>% 
  dplyr::filter(month %in% c("April", "May", "June" , "July", "August", "September", "October")) %>%
  droplevels()

ggplot(ppyg) + 
  geom_bar(aes(x = month), stat = "count", fill = "orange") + 
  facet_wrap(~site) + ggtitle("Soprano pips autoid")

## Overview of northern bat autoid recordings
epni <- DF %>% 
  dplyr::filter(autoid %in% c("EPTNIL")) %>% 
  dplyr::filter(month %in% c("April", "May", "June" , "July", "August", "September", "October")) %>%
  droplevels() 

ggplot(epni) + 
  geom_bar(aes(x = month), stat = "count", fill = "turquoise") +
  facet_wrap(~site) + ggtitle("Northern bats autoid")

# potential subset of northern bats 
sub_EPTNIL <- DF %>% 
  dplyr::filter(autoid == "EPTNIL") %>%
  droplevels() %>%
  group_by(site, month) %>% slice_sample(n = 200) %>%
  ungroup() %>%
  arrange(site, month)
dim(sub_EPTNIL)
# 3324   12

# potential subset of nathusii 
sub_PIPNAT <- DF %>% 
  dplyr::filter(autoid == "PIPNAT") %>%
  droplevels() %>%
  group_by(site, month) %>% slice_sample(n = 100) %>%
  ungroup() %>%
  arrange(site, month)
dim(sub_PIPNAT)
# 1480   12

# potential subset of daubentons 
sub_MYODAU <- DF %>% 
  dplyr::filter(autoid == "MYODAU") %>%
  droplevels() %>%
  group_by(site, month) %>% slice_sample(n = 100) %>%
  ungroup() %>%
  arrange(site, month)
dim(sub_MYODAU)
# 1544   12

# potential subset of soprano pips 
sub_PIPPYG <- DF %>% 
  dplyr::filter(autoid == "PIPPYG") %>%
  droplevels() %>%
  group_by(site, month) %>% slice_sample(n = 150) %>%
  ungroup() %>%
  arrange(site, month)
dim(sub_PIPPYG)
# 3133   12    

## NoID subset 
noid_sub <- DF %>% 
  filter(autoid == "NoID") %>% 
  droplevels() %>% 
  group_by(site, month) %>% slice_sample(n=150) %>% 
  ungroup() 
dim(noid_sub)
# 2810   12

## Noise subset 
noise_sub <- DF %>% 
  filter(autoid == "Noise") %>% 
  droplevels() %>% 
  group_by(site, month) %>% slice_sample(n=50) %>% 
  ungroup() 
dim(noise_sub)
# 1050   12

# How many observations all together? 
dim(sub1_2) + dim(sub_PIPPYG) + dim(sub_PIPNAT) +  dim(sub_MYODAU) +
  dim(sub_EPTNIL) + dim(noid_sub) + dim(noise_sub) 
# 24633 observations
# for Judith 40 000 

# With amounts Judith made in a month, the result is 63973, which  is too much, should have less for ca 140GB, cut all the amounts in half and took a subset also from nathusii and daubentons.

## Now add together so you can re-create the file paths 
pt <- dplyr::bind_rows(
  sub1_2,
  sub_PIPPYG,
  sub_EPTNIL,
  sub_PIPNAT,
  sub_MYODAU,
  noid_sub,
  noise_sub
)
 # checks out! 


summary(pt)

#check for duplicats
duplicates <- pt %>%
  group_by(filename) %>%
  filter(n() > 1) %>%
  ungroup() # none - good! 


summary(pt$autoid)
# BARBAR EPTNIL MYOBRA MYODAU MYOMYS MYONAT NYCNOC   NoID  Noise 
#   2436   7200    142   3929    374    123   2052   5400   1800 
# PIPNAT PIPPYG PLEAUR VESMUR 
#   6239   3600   2897    987 

summary(pt$site)
# CM-04 CM-05 CM-06 CM-21 CM-23 CM-42 
#  3732  3961  8022  7878  4981  8605 

table(pt$site, pt$month)
#       August July September
# CM-04   1382 1074      1276
# CM-05   1294 1250      1417
# CM-06   3004 1886      3132
# CM-21   2427 1563      3888
# CM-23   1662 1462      1857
# CM-42   4556 2169      1880

# The numbers of rows add up! 
## Explore what this looks like across the sites and months 


ggplot(pt) + 
  geom_bar(aes(x = month, fill = autoid), stat = "count") + 
  facet_wrap(~site) + 
  scale_fill_manual(
    name = "AutoID",
    values = hue_pal()(13)
  ) + 
  ylab("N recordings per month") + 
  xlab("Site") +
  theme_minimal(base_size = 16) + 
  ggtitle("2025")


dim(pt)
# 24633    12

dir.create(output_today, recursive = TRUE, showWarnings = FALSE)

write.csv(
  pt,
  file = file.path(output_today, "ValerieSubset_attempt1.csv"),
  row.names = FALSE
)



## Recreate file paths 
```{r}
fp <- read_csv("C:/Users/apmc/OneDrive - Norwegian University of Life Sciences/BatLab Norway/Projects/CoastalMonitoring/Analyses/Outputs/Reed/FixingFilepaths_2025-10-07/cleaned_output_filepaths_CM2024.csv",)


## Combine back filepath info 


fp$path <- paste0(fp$OUTDIR, "/", fp$FOLDER, "/")

fp$path1 <- gsub("\\\\", "/", fp$path)

fp$path1 <- factor(fp$path1)
levels(fp$path1)


## Copying files 
# not sure about noise subdirectories

### CM-06 ### 

cm06 <- fp %>% filter(site == "CM-06") %>% droplevels()
levels(cm06$path1)


# I'm here












cm06.noise <- cm06 %>% filter(autoid == "Noise") %>% droplevels()
# 300 noise files, 25 vars

cm06.noise$fullpath <- paste0(cm06.noise$path12, "/NOISE/", cm06.noise$filename)
head(cm06.noise$fullpath)

cm06.bats<- cm06 %>% filter(autoid != "Noise") %>% droplevels()
summary(cm06.bats$autoid)
# BARBAR EPTNIL MYODAU MYONAT NYCNOC   NoID PIPNAT PIPPYG PLEAUR VESMUR 
#    469   1200     36     64    568    900   2644    600    947    294 
# 7722 files 

cm06.bats$fullpath <- paste0(cm06.bats$path12, "/", cm06.bats$filename)
head(cm06.bats$fullpath)

cm06.subset <- full_join(cm06.noise, cm06.bats)
# 8022 obs of 26 vars - good!
summary(cm06.subset)

cm06.subset1 <- cm06.subset %>% 
  select(filename, fullpath, 
         DATE, TIME, HOUR, 
         DATE_12, TIME.12, HOUR.12, 
         autoid, site, month) %>% droplevels()
summary(cm06.subset1)
# 8022 obs of 11 vars

test <- unique(cm06.subset1$fullpath)
#8022 unique filepaths - good!

## Now try copying over CM-06 files... 
cm06_files <- as.list(cm06.subset1$fullpath)
cm06_files_list <- unlist(cm06_files)

dir.create("P:/SW_CoastalMonitoring/Data_analysis_2024/MSc_Theses/Judith/CM-06_subset1")

## This will take a while: 
file.copy(from = cm06_files_list,
          to = "P:/SW_CoastalMonitoring/Data_analysis_2024/MSc_Theses/Judith/CM-06_subset1")
beep()

## attach metadata
write.csv(cm42.subset1, 
          "P:/SW_CoastalMonitoring/Data_analysis_2024/MSc_Theses/Judith/CM-42_subset1/CM42_JudithSubset1_meta.csv" )




