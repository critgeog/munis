# count_munis.R
# counts number of properties in each municipality
# created: March 31, 2019
# last updated: May 20, 2021

# load libraries
library(tidyverse)
library(tidycensus)
library(sf)
library(tmap)
library(tigris)
options(tigris_use_cache = TRUE)
options(tigris_class = 'sf')

# read in Georiga municipalities. Downloaded from: 
ga_munis <- read_sf("Cities_Georgia/Cities_Georgia-shp/Cities_Georgia.shp")
ga_munis <- ga_munis %>%
  st_transform(26916)       # UTM 16 projection

# read in geocoded address level data for INVH properties
ih_dots <- read_sf("../../invh/metro/data/ih_points_ct.geojson")

ih_dots <- ih_dots %>%
  st_transform(26916) #UTM 16

# Atlanta metropolitan area sf and project
atl_sf <- core_based_statistical_areas(cb = TRUE) %>%
  filter(GEOID %in% c("12060")) %>%   # ATL metro FIPS code
  st_transform(26916)


# Crop all ga_munis to Atlanta metropolitan area
atl_munis <- st_crop(ga_munis,atl_sf)

##

# 19 counties where Invitation Homes is active
metro_counties <- c("Gwinnett", "Barrow", "Bartow", "Carroll", "Cherokee","Clayton","Coweta", 
                    "DeKalb", "Douglas", "Fayette","Forsyth", "Gwinnett", 
                    "Hall","Henry", "Newton", "Paulding", "Rockdale", "Walton", "Cobb", "Fulton")


# county boundaries for metro area
counties_sf <- counties("GA") %>%
  filter(NAME %in% metro_counties)
  
# project
counties_sf <- counties_sf %>%
  st_transform(26916)

# primary roads for study are
rd <- primary_roads()
rd <- rd %>%
  st_transform(26916)
rd <- st_crop(rd,counties_sf)

atl_munis <- st_crop(ga_munis,counties_sf)

tmap_mode('plot')

tm_shape(ih_dots) +
  tm_dots(size = .02) +
  tm_shape(atl_munis) + 
  tm_borders(alpha = .5)

tm_shape(ga_munis) + 
  tm_borders(alpha = .5)

# map incorporated v unincorporated
# Intersection between polygon and points ---------------------------------
muni_intersection <- st_intersection(x = atl_munis, y = ih_dots)

# using dplyr
int_result <- muni_intersection %>% 
  group_by(Name) %>% 
  count()

# munis_n is the municipalities with INVH properties
munis_n <- int_result %>%
  rename(munis = n)

munis_n %>%
  summarize(total = sum(munis)
  )
munis_n %>%
  arrange(desc(munis))

# combine munis_n and ga_munis 
# is there a better approach?
munis2 <- st_join(atl_munis,munis_n)

munis2 <- munis2 %>%
  replace_na(list(munis = 0)) 

munis_n %>%
  arrange(desc(munis)) %>%
  print(n = 21
        )

tmap_mode('plot')
# messing around
tm_shape(munis2) +
  tm_polygons( col ='munis', style = 'jenks', palette = 'viridis', lwd = 0)




# 29% of ih market is in incorporated spaces
# I think this will be surprising to people in both directions
# 7/10 in unincorporated space is pretty striking. 

# Beyond the State
# one of the oldest, and newest (Allums, Connor) technologies
# of a rescaled (Brenner) state space 