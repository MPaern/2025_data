# 2025 autumn data exploration for mist netting
# find out what pnat and enil did last year
# pnat what did happen around Rennesøy, NMBU and Stavanger, coastal area etc. (potential mist netting sites)
# where was Enil later, what are the most promising locations for that species
# Look what happened to pnat in the suth and in Grødem area- potential for mist netting?


# library -----------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(dplyr)
library(suncalc)
#library(suntools)
library(lubridate)
library(leaflet.minicharts)
library(leaflet)



# read data ---------------------------------------------------------------

cm <- read.csv("cm_2025.csv")
overview_summary <- read.csv("overview_2025.csv")
head(cm)

# cut data from July

cm$DATE <- as.Date(cm$DATE)
cm$DATE_12 <- as.Date(cm$DATE_12)

cm <- cm %>%
  filter(DATE >= "2025-07-01")

# add enil calls to overview and change pnat calls to only autumn too

enilcalls <- cm %>%
  group_by(Site) %>%
  summarise(
    enilcall = sum(autoid == "EPTNIL"))

enilsept <- cm %>%
  filter(DATE >= "2025-09-01") %>%
  group_by(Site) %>%
  summarise(
    enilsept = sum(autoid == "EPTNIL"))

pnata2 <- cm %>%
  filter(DATE >= "2025-08-15" & DATE <= "2025-08-31") %>%
  group_by(Site) %>%
  summarise(
    pnataug = sum(autoid == "PIPNAT"))

pnats1 <- cm %>%
  filter(DATE >= "2025-09-01" & DATE <= "2025-09-15") %>%
  group_by(Site) %>%
  summarise(
    pnatsept1 = sum(autoid == "PIPNAT"))

pnats2 <- cm %>%
  filter(DATE >= "2025-09-15" & DATE <= "2025-09-30") %>%
  group_by(Site) %>%
  summarise(
    pnatsept2 = sum(autoid == "PIPNAT"))

pnatcalls <- cm %>%
  group_by(Site) %>%
  summarise(
    pnatcall = sum(autoid == "PIPNAT"))

 new <- left_join(overview_summary, enilcalls, by = "Site")

overview_summary <- left_join(new, pnatcalls, by= "Site")

overview_summary <- left_join(overview_summary, enilsept, by= "Site")

# enil --------------------------------------------------------------------

enil <- cm %>%
  filter(autoid=="EPTNIL")

ggplot(enil %>% 
         filter(Site=="CM-05") %>%
         droplevels(), aes(x= DATE_12, color = taxa)) + 
  stat_count( geom = "point", size= 4, alpha = 0.90, color = "royalblue") +
  ylab("Recordings per night") + xlab("Month") +
  ggtitle("Autumn activity 2025") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_minimal()

# enil map

tilesURL <- "http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"

basemap <- leaflet(width = "100%", height = "400px") %>%
  addTiles(tilesURL)

basemap %>%
  addMinicharts(
    overview_summary$longitude, overview_summary$latitude,
    chartdata = overview_summary$enilsept, 
    showLabels = TRUE,
    width = 45
  )

# pnat --------------------------------------------------------------------

pnat <- cm %>%
  filter(autoid=="PIPNAT")

ggplot(pnat %>% 
         filter(Site=="CM-44") %>%
         droplevels(), aes(x= DATE_12, color = taxa)) + 
  stat_count( geom = "point", size= 4, alpha = 0.90, color = "royalblue") +
  ylab("Recordings per night") + xlab("Month") +
  ggtitle("Autumn activity 2025") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_minimal()

# Campus time of bats

cm41 <- cm %>%
  filter(Site=="CM-41")

ggplot(cm41 %>% 
         droplevels(), aes(x= DATE_12, color = taxa)) + 
  stat_count( geom = "point", size= 4, alpha = 0.90, color = "royalblue") +
  ylab("Recordings per night") + xlab("Month") +
  ggtitle("Autumn activity 2025") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_minimal()

cm41 %>%
  filter(DATE >= "2025-07-01") %>%
  ggplot() +
  aes(x = HOUR_12) +
  geom_histogram(bins = 30L, fill = "#112446") +
  theme_minimal() +
  facet_wrap(vars(DATE))

cm41 %>%
  filter(DATE >= "2025-07-01", autoid == "EPTNIL") %>%
  mutate(
    half_month = paste0(
      format(DATE, "%Y-%m"),
      if_else(as.integer(format(DATE, "%d")) <= 15, "-1", "-2")
    )
  ) %>%
  ggplot() +
  aes(x = HOUR_12) +
  geom_histogram(bins = 30L, fill = "#112446") +
  theme_minimal() +
  facet_wrap(vars(half_month))
