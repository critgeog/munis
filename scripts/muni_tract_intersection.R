# muni_tract_intersection.R
# counts number of properties in each municipality                               #
# created: December 10, 2020                                                     #
####### copied from count_munis.R as a base                                      #
# last updated: May 20, 2021 for Vinson project                                  #
# did not change code, but did take some to update vinson/count_munis.r and      #
# the script that ultimately became, muni_analysis.r                             #
# we can treat this script as Deprecated at this point. It should only be        #
# used as a reference and not a updated or relied on any further                 #
# all muni related data and scripts are now in /inProgress/vinson. (5.20.2021)   #
##################################################################################

library(tidyverse)
library(sf)
library(tmap)
library(tigris)
options(tigris_use_cache = TRUE)
options(tigris_class = 'sf')

ga_munis <- read_sf("Cities_Georgia//Cities_Georgia-shp/Cities_Georgia.shp")
# ih_dots <- read_sf("ih_points_ct_class.geojson") %>%
#   st_transform(26916)

atl_county20_munis <- ga_munis %>%
  filter(County20 == "yes")

# tigris for geometry, Atlanta
# atl_sf <- core_based_statistical_areas(cb = TRUE) %>%
#   filter(GEOID %in% c("12060")) %>%
#   st_transform(26916)

ga_munis <- ga_munis %>%
  st_transform(26916)

# atl_metro_munis <- st_crop(ga_munis,atl_sf)

##
metro_counties <- c("Gwinnett", "Barrow", "Bartow", "Carroll", "Cherokee","Clayton","Coweta", 
                    "DeKalb", "Douglas", "Fayette","Forsyth", "Gwinnett", 
                    "Hall","Henry", "Newton", "Paulding", "Rockdale", "Walton", "Cobb", "Fulton")

metro_counties_sf <- counties("GA", year = 2016) %>%
  filter(NAME %in% metro_counties) %>%
  st_transform(26916)


tmap_mode('plot')


tm_shape(atl_county20_munis) + 
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


