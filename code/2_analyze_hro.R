####
# Project: Regionaldatenbank der Statistischen Aemter
# Purpose: Analyze Rostock
# Author: Henrik-Alexander Schubert
# Date: 08.05.2025
###

library(sf)
library(tidyverse)


# Load the map -------------------------------------

# Source: 
# https://www.zensus2022.de/DE/Presse/Grafik/shapefile.html

# Load the map data
map_reg <- read_sf(list.files("EPSG_25832", pattern="KRS.shp", full.names=T))

# Keep only thei mportant variables
map_reg <- map_reg[, c("ARS", "GEN", "BEZ")]

# Rename the columns of the map data
names(map_reg) <- c("region", "region_name", "level", "geometry")

# Create the kreis code data
reg_codes <- st_drop_geometry(map_reg)

# What is the region code for rostock
region_rostock <- reg_codes$region[reg_codes$region_name == "Rostock" & reg_codes$level == "Kreisfreie Stadt"]

# Combine map with population data -----------------

# Load the population data
load("data/pop_age.Rda")

# Filter the data
pop_hro <- pop_df[pop_df$region %in% region_rostock, ]

# Pivot wider
ggplot(subset(pop_hro, year == 2023 & age<75), aes(x=age+0.5, y = ifelse(sex=="M", -pop, pop), fill=sex)) +
  geom_col() +
  #geom_step(data=subset(pop_hro, year == 2003 & age<75), aes(x = age-0.5, linetype=sex), color="black", linewidth=1.5) +
  geom_hline(yintercept = 0, color="white") +
  coord_flip() +
  scale_linetype_manual("Geschlecht:", values = c("dotted", "dashed")) +
  scale_fill_manual("Geschlecht:", values = c("darkblue", "darkred")) +
  scale_x_continuous("Alter", expand = c(0, 0), limits=c(0, 75), n.breaks = 20, sec.axis = sec_axis(name="Geburtsjahrgang", transform =~2023-., breaks = seq(1950, 2020, by=10))) +
  scale_y_continuous("Bevölkerung", n.breaks = 10, label=abs) +
  theme_bw(base_size=14, base_family="serif") +
  theme(
    legend.position = c(0.15, 0.1),
    axis.title = element_text(face="bold")
  )
ggsave(filename="figures/pop_pyramide_rostock.pdf",
       height=20, width=19, unit="cm")

### END ##############################################