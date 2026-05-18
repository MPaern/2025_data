#plots
library(tidyverse)
library(ggplot2)
library(dplyr)
library(suncalc)
library(suntools)
library(lubridate)
library(readr)
library(esquisse)
library(MetBrewer)
library(purrr)
library(sp)
library(sf)
library(hms)
library(maptools)
library(ggcorrplot)


cm25 <- read.csv("cm_2025.csv")
cm24 <- read.csv("C:/Users/mapa/OneDrive - Norwegian University of Life Sciences/Documents/R for data/Overview of sites/cm_2024.csv")
ov25 <- read.csv("overview_2025.csv")
ov24 <- read.csv("C:/Users/mapa/OneDrive - Norwegian University of Life Sciences/Documents/R for data/Overview of sites/overview_2024.csv")

# 2025 plot all

pnat25 <- cm25 %>% filter(autoid == "PIPNAT" & MATCH_RATIO>= 0.5) %>% droplevels()
pnat25$type <- ov25$type[match(pnat25$Site, ov25$Site)]
pnat25$DATE <- as.Date(pnat25$DATE)

pnat.plot.25 <- ggplot(pnat25, aes(x = DATE)) + 
  stat_count(geom = "point", aes(y = ..count..),
             color = "cyan3", size = 4) +
  scale_x_date(
    limits = as.Date(c("2025-03-11", "2025-10-16"))) + 
  ylab("N recordings across all sites 2025") + 
  xlab("") + 
  ggtitle("Nathusius' pipistrelle (Pipistrellus nathusii) activity 2025") + 
  theme_minimal() 

pnat.plot.25

# 2025 plot coast
pnat25c <- pnat25 %>% filter(type == "coast")


pnat.plot.25.c <- ggplot(pnat25c, aes(x = DATE)) + 
  stat_count(geom = "point", aes(y = ..count..),
             color = "cyan3", size = 4) +
  scale_x_date(
    limits = as.Date(c("2025-03-11", "2025-10-16"))) + 
  ylab("N recordings across all sites 2025") + 
  xlab("") + 
  ggtitle("Nathusius' pipistrelle (Pipistrellus nathusii) activity on the coast 2025") + 
  theme_minimal() 

pnat.plot.25.c

# 2024 plot all

pnat24 <- cm24 %>% filter(autoid == "PIPNAT" & MATCH.RATIO>= 0.5) %>% droplevels()
pnat24$type <- ov24$type[match(pnat24$Site, ov24$Site)]
pnat24$DATE <- as.Date(pnat24$DATE)

pnat.plot.24 <- ggplot(pnat24, aes(x = DATE)) + 
  stat_count(geom = "point", aes(y = ..count..),
             color = "turquoise", size = 4) +
  scale_x_date(
    limits = as.Date(c("2024-03-11", "2024-10-17"))) + 
  ylab("N recordings across all sites 2024") + 
  xlab("") + 
  ggtitle("Nathusius' pipistrelle (Pipistrellus nathusii) activity 2024") + 
  theme_minimal() 

pnat.plot.24


# 2024 plot coast

pnat24c <- pnat24 %>% filter(type == "coast")

pnat.plot.24.c <- ggplot(pnat24c, aes(x = DATE)) + 
  stat_count(geom = "point", aes(y = ..count..),
             color = "turquoise", size = 4) +
  scale_x_date(
    limits = as.Date(c("2024-03-11", "2024-10-17"))) + 
  ylab("N recordings across all sites 2024") + 
  xlab("") + 
  ggtitle("Nathusius' pipistrelle (Pipistrellus nathusii) activity on the coast 2024") + 
  theme_minimal() 

pnat.plot.24.c


cowplot::plot_grid(pnat.plot.24.c, pnat.plot.25.c, nrow = 2)
cowplot::plot_grid(pnat.plot.24, pnat.plot.25, nrow = 2)
