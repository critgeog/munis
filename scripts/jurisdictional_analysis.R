###------------------------------------------------------------------
## jurisdictional_analysis.R
## This script analyzes the Geography of Atlanta's largest landlord
## across the various jurisdictional boundaries in metropolitan Atlanta
## copied from count_munis.R. count_munis.R shows more data verification
# last updated: May 22, 2021
###------------------------------------------------------------------

# load libraries
library(tidyverse)
library(tidycensus)
library(sf)
library(tmap)
library(tigris)
options(tigris_use_cache = TRUE)
options(tigris_class = 'sf')
library(hrbrthemes)
library(ggplot2)
library(kableExtra)
library(areal)
th_api_acs <- '45544f0d114cfaa037a5566745d18bb8d4778cfa'


####-----------------------------------------------------------------------
## Retrieve data from census bureau using tigris package
## counties boundary
## roads (for cartographic representation)
####-----------------------------------------------------------------------

# create list of 19 counties where Invitation Homes owns housing
metro_counties <- c("Gwinnett", "Barrow", "Bartow", "Carroll", "Cherokee","Clayton","Coweta", 
                    "DeKalb", "Douglas", "Fayette","Forsyth", "Gwinnett", 
                    "Hall","Henry", "Newton", "Paulding", "Rockdale", "Walton", "Cobb", "Fulton"
                    )


# bring in county boundaries where INVH is active via tigris
counties_sf <- counties("GA") %>%      # Georgia counties
  filter(NAME %in% metro_counties)     # filter to metro

# project
counties_sf <- counties_sf %>%
  st_transform(26916)

# primary roads for study area
rd <- primary_roads()
rd <- rd %>%
  st_transform(26916)
rd <- st_crop(rd,counties_sf) #crop roads to study area


###-------------------------------------------------
# read in georgia municipalites from Atlanta Regional Commission open data
ga_cities_api = read_sf("https://opendata.arcgis.com/datasets/34520575dfc34b8cac783caff702b8cc_58.geojson")
st_is_valid(ga_cities_api)

# correct geometry errors
ga_cities_api <- st_make_valid(ga_cities_api)
st_is_valid(ga_cities_api)
# good to go. all geoms fixed.

# project
ga_cities_api <- ga_cities_api %>%
  st_transform(26916) #EPSG 26916 is projected: NAD83 / UTM zone 16N # EPSG:26916 Projected coordinate system for North America

# cut to 20 metropolitan counties, and delete municipalities in Spalding County, 
# Invitation Homes does not own property in Spalding County
spalding_munis <- c("Griffin", "Sunny Side", 'Orchard Hill')

atl_munis <- ga_cities_api %>%
  filter(County20 == "yes") %>% #per metadata, County20 == Within ARC 20 County region
  filter(!(Name %in% spalding_munis)) # drop Spalding

# 119 municipalities in 19 county study area


####-----------------------------------------------------------------------
## Read in Invitation Homes geolocated data
## these data were retrieved from public facing tax parcel data
## cleaned, geocoded using google api, and joined together in a separate project
## I would prefer to keep this data private as this is unpublished research
## If you need it, I will make it available on github so that the script will run
## 11,633 addresses
####-----------------------------------------------------------------------

# read in geocoded address level data for INVH properties
ih_dots <- read_sf("../../invh/metro/data/ih_points_ct.geojson") # data I would prefer not to share. 

ih_dots <- ih_dots %>%
  st_transform(26916) #EPSG 26916 is projected: NAD83 / UTM zone 16N # EPSG:26916 Projected coordinate system for North America


####-----------------------------------------------------------------------
## test data

# set tmap mode
tmap_mode('plot')

# test map with municipal boundaries, county boundaries, and geolocation data
tm_shape(atl_munis) + 
  tm_borders("red", lwd = .9) +
  tm_shape(ih_dots) +
  tm_dots("gray",size = .01) +
  tm_shape(counties_sf) +
  tm_borders(lwd = 2) +
  tm_shape(rd) +
  tm_lines()
# good to go!

# clean up this subsection
rm(ga_cities_api)


####-----------------------------------------------------------------------
## Identify if geolocated address data are in a municipality
## Count how many locations are in each municipality
## st_intersection help: https://github.com/r-spatial/sf/issues/347
####-----------------------------------------------------------------------

## This identifies and returns the municipality each address is in
muni_intersection <- st_intersection(x = atl_munis, y = ih_dots)

####-----------------------------------------------------------------------
## We could create a file from this.
## create point file (i.e. 'metro' that includes GEOID for each location)
## write_csv(muni_intersection, "ih_dots_muni.csv")
####------------------------------------------------------------------------

# Check to see if any address was assigned to more than one municipality
muni_intersection %>%
  as_tibble() %>%
  select(-(geometry)) %>%   # this line makes the code run faster
  group_by(Parcel.ID) %>%   # parcel uniqe to each 
  summarise(N = n()) %>%
  filter(!(N == 1))         # return any parcels identified more than once

# There are two records with two parcel IDs. This is because the geolocation overlapped with two city boundaries
# these are the two parcels:
# 1 09F020200131418 # Fairborn and South Fulton -- Fairborn, per Fulton County website: https://iaspublicaccess.fultoncountyga.gov/search/CommonSearch.aspx?mode=PARID
# 2 09F280001111122 # Union City and South Fulton -- Union City, per Fulton County website: https://iaspublicaccess.fultoncountyga.gov/search/CommonSearch.aspx?mode=PARID

# to correct, remove the two rows/parcels identified as in 'South Fulton'
# incorp_dots is all addresses within incorporated municipalities
incorp_dots <- muni_intersection %>%
  as_tibble() %>%
  select(-(geometry)) %>%
  filter(!(Parcel.ID == "09F020200131418" & Name == 'South Fulton' | Parcel.ID == "09F280001111122" & Name == 'South Fulton'))

# group all addresses by (muni) name, then sum, and create new tbl 
munis_n <- incorp_dots %>% 
  group_by(Name) %>%  # municipality name
  summarise(ihsfrs = n()) # ihsfrs = the number of addresses/points in each municipality

# INVH owns housing in 83 of the 119 municipalities

# how many properties are in the municipalities? 
munis_n %>%
  summarize(total = sum(ihsfrs)
  )
#3382, correct.

# arrange municipalities, in descending order, by number of properties Ivntiation Homes owns.
# munis_n %>%
#   arrange(desc(ihprops))

# join ihprops count to atl_munis sf in order to create a 
# file with all 119 municipalities and IH totals
munis2 <- left_join(atl_munis, munis_n, by = "Name")

# replace NAs (munis where INVH owns 0 SFRs) with 0
munis2 <- munis2 %>%
  replace_na(list(ihsfrs = 0)) %>%
  select(OBJECTID, Name, ihsfrs, Acres, Sq_Miles) # clean up sf. don't need County20, County10, GlobalID, and 'last_edited'

# looking at data
munis2 %>%
  filter(ihsfrs > 20) %>%
  select(Name, ihsfrs) %>%
  arrange(desc(ihsfrs)) %>%
  print(n = 30)

munis2 %>%
  filter(ihsfrs > 50) %>%
  group_by(county)
  summarise(total = sum(ihsfrs))
# testing
munis2 %>%
  summarise(total = sum(ihsfrs))

# 3382/11632
# 29.0% of ih market is in incorporated spaces
# 7/10 in unincorporated space is pretty striking. 

# map it
tm_shape(munis2) +
  tm_polygons("ihsfrs", palette = 'viridis', breaks = c(0,50,100,185,416), lwd = 0.1) +
  tm_shape(counties_sf) +
  tm_borders()

munis2

# clean up this section
rm(munis_n, atl_munis)
# muni_intersection?

####-----------------------------------------------------------------------
## county analysis. In the next series of steps, I calculate the number of IH-owned
## SFRs in each county, within a municipality in each county, and outside of a municipality
## in each county
## 
####-----------------------------------------------------------------------
# create county tbl calculating sum of ih owned sfrs in incorporated areas in each county
# based on addresses identified in line 213
incorp_county <- incorp_dots %>%
  mutate(county_fips = str_sub(GEOID,1,5)) %>%  # create county fips column from tract
  group_by(county_fips) %>%
  summarise(
    incorp_ihsfrs = sum(N = n()) # unincorp_ihsfrs == sum of ih owned sfrs in unincorporated areas
  )

incorp_county %>%
  summarise(N = sum(incorp_ihsfrs))
#3,382 == correct

####-----------------------------------------------------------------------
# create sf of addresses outside of municipalities
# using anti-join of all address and incorporated address
unincorp_dots <- anti_join(ih_dots, incorp_dots, by = 'Parcel.ID')

# create county tbl calculating sum of ih owned sfrs in unincorporated areas in each county
# based on addresses identified in line 213
unincorp_county <- unincorp_dots %>%
  as_tibble() %>%
  select(-(geometry)) %>%
  mutate(county_fips = str_sub(GEOID,1,5)) %>%  # create county fips column from tract
  group_by(county_fips) %>%
  summarise(
    unincorp_ihsfrs = sum(N = n()) # unincorp_ihsfrs == sum of ih owned sfrs in unincorporated areas
  )

unincorp_county %>%
  summarise(N = sum(unincorp_ihsfrs))
# 8251(unincorporated)+ 3382(incorp_dots) == 11633
# all good.

####-----------------------------------------------------------------------
# create county tbl calculating sum of ih owned sfrs in each county
# based on all addresses
total_county <- ih_dots %>%
  as_tibble() %>%
  select(-(geometry)) %>%
  mutate(county_fips = str_sub(GEOID,1,5)) %>%
  group_by(county_fips) %>%
  summarise(
    county_ihsfrs = sum(N = n())
  )

# join 
ih_counties <- left_join(counties_sf, total_county, by = c("GEOID" = "county_fips")) %>%
  left_join(., unincorp_county, by = c("GEOID" = "county_fips")) %>%
  left_join(., incorp_county, by = c("GEOID" = "county_fips")
            )

# check
ih_counties %>%
  summarize(check = county_ihsfrs -(incorp_ihsfrs + unincorp_ihsfrs)
            ) %>%
  print(n = 19)

# project
ih_counties<- ih_counties %>%
  st_transform(26916)

# compare distribution across counties
tm_shape(ih_counties) +
  tm_polygons(c("unincorp_ihsfrs","incorp_ihsfrs","county_ihsfrs"), style = 'jenks')

# create shorter county name in preparation of figures
ih_counties <- ih_counties %>%
  mutate(short_name = '',
         short_name = sub(" .*", "", NAME)) %>%
  select(short_name, everything()) 



####-----------------------------------------------------------------------
## read in census data. this section of code was adapted from :
## https://github.com/jshannon75/awp_tidycensus/blob/master/ACS_walkthrough.md
####-----------------------------------------------------------------------

# existing .csv of variables of interest to function as a lookup table
metadata <- read_csv("acs_vars_metadata.csv")

# variable codes of interest
acs_vars <- metadata$variable
acs_vars

# using tidycensus, pull in census tract data from 2014-18 ACS
atl_ct <- get_acs(geography = "tract", variables = acs_vars, state = "GA", 
                  county = metro_counties, year = 2018, 
                  geometry = FALSE, key = th_api_acs) %>% # no geometry to make data modifications for MOE run smoothly. Will join to spatial in line 328
  mutate(GEOID=as.character(GEOID)
         )

atl_ct2 <- atl_ct %>%
  left_join(metadata) %>%
  group_by(GEOID,var_group) %>% #var_name or var_group? #shannon says group, i think name
  summarise(
    est=sum(estimate),
    moe=round(sqrt(sum(moe^2)),0)
  )

# make wide to join with sf and prepare for areal package
atl_wide <- atl_ct2 %>%
  pivot_wider(
    names_from = var_group, 
    values_from = c(est,moe),
    names_glue = "{var_group}_{.value}"
  )

# get tract geographies
atl_sf <- get_acs(geography = "tract", variables = "B19001_001", state = "GA",
                  county = metro_counties, year = 2018,
                  geometry = TRUE, key = th_api_acs) %>%
  mutate(GEOID=as.character(GEOID)) %>%
  select(-c("variable", "estimate", "moe")) # don't need the variable

# join sf to correct census data
atl_wide <- left_join(atl_sf,atl_wide)

# project
atl_wide <- atl_wide %>%
  st_transform(26916)

# remove missing data in ATL metro
atl_wide <- atl_wide %>%
  filter(!(GEOID %in% c(13089023115, 13121006801, 13063980000,13089980000,13121980000)))

# clean up section
rm(atl_ct,atl_ct2,atl_sf)

####-----------------------------------------------------------------------------
# Area  Interpolation with 'areal' package
####-----------------------------------------------------------------------------

# area interopolation
# take tract geography and take some of listed variables for each census tract to municipality
muni_tenure <- aw_interpolate(munis2, tid = OBJECTID, source = atl_wide, sid = "GEOID",
                              weight = "sum", output = "sf", extensive = c("rntocc_sf_est","tot_sf_est"))


# create ratios of Invitation Homes' ownership to normalize based on single-family rentals and single-family units
muni_tenure <- muni_tenure %>%
  mutate(pct_ih = round(ihsfrs/tot_sf_est*100000,1),
         pct_ihsfr = round(ihsfrs/rntocc_sf_est*100000,1))

muni_tenure

####-----------------------------------------------------------------------
## figures
####-----------------------------------------------------------------------

# before we make figures, if we wanted to export any of the tbls or sfs, that is possible. 
# write_sf(muni_tenure, "muni_tenure.geojson") # save muni_tenure sf as geojson in working directory
# write_sf(counties_sf, "metro_atl_counties_ih.geojson") # save counties_sf as geojson named 'metro_atl_counties_ih' in working directory
# write_csv(counties_sf, "metro_atl_counties_ih.csv") # save same file as csv. 

tm_shape(muni_tenure) +
  tm_polygons("pct_ihsfr", palette = 'viridis', style = 'jenks', lwd = 0.1) +
  tm_shape(counties_sf) +
  tm_borders()

tmap_mode('view')
tm_shape(muni_tenure) +
  tm_polygons("pct_ih", palette = 'viridis', style = 'jenks', lwd = 0.1, id = 'Name') +
  tm_shape(counties_sf) +
  tm_borders()

library(ggrepel)
ggplot(data = muni_tenure, mapping = aes(x = ihsfrs, y = pct_ihsfr)) +
  geom_point() +
  geom_smooth() +
  geom_text_repel(data = subset(muni_tenure, ihsfrs > 150 | pct_ihsfr >6000), 
                  mapping = aes(label = Name)) +
  # arrow = arrow(length = unit(0.02, "npc")),
  # box.padding = 1) +
  theme_ipsum()


muni_tenure %>%
  select(OBJECTID, Name, ihsfrs, pct_ihsfr, pct_ih, rntocc_sf_est) %>%
  arrange(desc(pct_ihsfr)) %>%
  print(n=15)


####-----------------------------------------------------------------------
# munis figures
####-----------------------------------------------------------------------

quantile(munis2$ihsfrs, probs = seq(0, 1, by= 0.1)) # decile
summary(munis2$ihsfrs)
# 70th percentile is 23.8; median is 5 SFRs
# max is 418

# create figure of municipalities where Invitation Homes owns >50 single-family units.
# 51.4 is 80th percentile; top 25 municipalities
munis2 %>%
  filter(ihsfrs >50) %>%
  ggplot(munis2, mapping = aes(x = reorder(Name,ihsfrs), y = ihsfrs, label = ihsfrs)) +
  geom_col(fill = "#41912E", position = 'dodge') +
  labs(x = NULL, y = "Total SFRs",
       title = "Invitation Homes SFRs, by Municipality") +
  geom_text(size = 2.5, position = position_stack(vjust = 0.5)) +
  theme_ipsum(axis_text_size = 9) +
  coord_flip()

# create tbl of key variables: ihprops, all sfs, all sfrs 
munis2 %>%
  as_tibble() %>%
  select(-(geometry)) %>%
  select(Name, ihsfrs) %>%
  arrange(Name) %>%
  kable(caption = "Inviitation Homes, by SFR", #, escape = F, booktabs =T,
        # format.args = list(big.mark = ",", scientic = FALSE),
        col.names = c("Municipality",
                      "INVH SFRs"))
#       %>%
# kable_styling(latex_options = c("striped", "hold_position","scale_down")) %>%
# # column_spec(3:16, width = "6em") %>%

ggplot(ih_counties, aes(x = reorder(short_name,unincorp_ihsfrs), y = unincorp_ihsfrs, label = unincorp_ihsfrs)) +
  geom_col(fill = "#AADE55", position = 'dodge') +
  labs(x = NULL, y = "Total SFRs",
       title = "Invitation Homes SFRs, by County, Unincorporated Area") +
  geom_text(size = 2.5, position = position_stack(vjust = 0.5)) +
  scale_y_continuous(labels = scales::comma) +
  # theme(axis.text.x=element_text(angle=60, hjust=1)) +
  theme_ipsum(axis_text_size = 9) +
  coord_flip()

ih_counties_long <- ih_counties %>%
  pivot_longer(cols = c(unincorp_ihsfrs,incorp_ihsfrs), names_to = "var", values_to = 'count')

ggplot(ih_counties_long, aes(x = reorder(short_name,count), y = count, fill = var, label = count )) +
  geom_col(position = 'stack') +
  scale_fill_manual(values=c("#AADE55","#41912E"), labels = c("Unincorporated SFRs", "Incorporated SFRs")) +
  labs(x = NULL, y = "SFRs",
       title = "Atlanta's Largest Land owner concentrated outside Municipalities",
       subtitle = "Invitation Homes' SFRs, by jurisdiction",
       fill = "") +
  # geom_text(size = 2.5, position = position_stack(vjust = 0.5)) +
  # theme(axis.text.x=element_text(angle=60, hjust=1)) +
  theme_ipsum(axis_text_size = 9) +
  theme(legend.position  = "bottom",
        legend.text = element_text(size = rel(0.9)),
        legend.key = element_rect(size = 1)) +
  coord_flip()


# this for a map.
# create an sf of incorporated geolocations
incorp_dots_sf <- muni_intersection %>%
  filter(!(Parcel.ID == "09F020200131418" & Name == 'South Fulton' | Parcel.ID == "09F280001111122" & Name == 'South Fulton'))
# create a munis2 tbl
munis2_tbl <- munis2 %>%
  as_tibble %>%
  select(-(geometry))
# join this munis info th incoprorate dots
incorp_dots_info <- left_join(incorp_dots_sf, munis2_tbl, by = "Name")

# leg_col <- c(tmaptools::get_brewer_pal("YlOrRd", 4, plot = FALSE)

# map it
tm_shape(counties_sf) +
  tm_borders(lwd = .5, col='#BEBEBE') +
  # tm_shape(munis2) +
  # tm_borders(lty = 5, lwd = .5, col='#BEBEBE') +
  tm_shape(incorp_dots_info) +
  tm_dots("ihsfrs", palette = 'YlOrRd', breaks = c(0,26,50,100,185,417), border.lwd =0, legend.show = FALSE) +
  tm_add_legend(type = c("symbol"),size = .2,border.lwd = 0,
                col = c(tmaptools::get_brewer_pal("YlOrRd", 6, plot = FALSE)),
                labels = c("0 to 25", "26 to 50", "51 to 100", "101 to 185", "185 to 416"),
                title = "Only INVH SFRs\nwithin in Incorporated Areas")+
  tm_legend(position = c(0.76, 0.02),
            bg.color = "black",
            frame = TRUE,
            legend.text.size = .7,
            legend.title.size = .9) +
  tm_layout(frame = FALSE,
            outer.margins=c(0,0,0,0),
            inner.margins=c(0,0,0,0), asp=0,
            bg.color = 'Black',
            legend.text.col = '#ffffff',
            legend.title.col = '#ffffff')
  
  



??lty
# county level comparison
tmap_mode('view')
tm_shape(ih_counties) +
  tm_polygons(c("unincorp_ihsfrs","incorp_ihsfrs","county_ihsfrs"), style = 'jenks') +
  tm_shape(muni_tenure) +
  tm_polygons('pct_ihsfr', palette = "viridis", style = 'jenks')

####-----------------------------------------------------------------------
# questions
# 1. what county has the most non-incorporated SFRs?
# answered, Gwinnett holds
# big change is Fulton
# 2. at the municipality, what is IH's share of the SF stock and SFR stock?
# see below

# push back on low percentages?
# 1. this is a single entity, which gives them considerable ownership of the housing stock
# available in this municipality. Two other publicly-listed SFRs operating in metropolitan Atlanta,
# own, in tandem, about the number of houses equivalent to Invitation Homes.

# why it matters
# 2. these findings are relevent to public and elected officials of these jurisdictions as they
# discuss issues related to zoning, housing affordability, housing supply, and homeownership
####-----------------------------------------------------------------------



###---------------------------------------------------------------
### Extra
###---------------------------------------------------------------


muni_tenure %>%
  as_tibble() %>%
  select(-(geometry)) %>%
  select(Name, ihprops, rntocc_sf_est) %>%
  filter(ihprops > 50) %>%
  pivot_longer(cols = c(ihprops, rntocc_sf_est), names_to = "var", values_to = "value") %>%
  group_by(Name,var) %>%
  summarize(total = sum(value)) %>%
  mutate(freq = total / sum(total),
         pct = round((freq*100), 1)) %>%
  ggplot(mapping = aes(x = reorder(Name, desc(pct)), y = pct, fill = as.factor(var))) +
  geom_col(position = 'stack', width = .7) +
  # ggplot(mapping = aes(x = urb_yr, y = pct, fill = urb_yr)) +
  # geom_col(position = 'dodge2', show.legend = FALSE) +
  theme(legend.position = "bottom") +
  coord_flip() +
  theme_ipsum()









# if needed, after bringing in Census Data

# create tbl of key variables: ihprops, all sfs, all sfrs 
munis2 %>%
  as_tibble() %>%
  select(-(geometry)) %>%
  group_by(Name) %>%
  # filter(!(is.na(is_hms18))) %>%
  # filter(!(is.na(mm18))) %>%
  select(Name, ihprops) %>%
  pivot_longer(!Name) %>%
  group_by(name,is_hms18) %>%
  summarize(total = sum(value)) %>%
  mutate(freq = total / sum(total),
         pct = round((freq*100), 1)) %>%
  # select(-(freq))  %>%                # remove freq to make smaller table
  pivot_wider(names_from = name, values_from = c(total:pct))->ih_hms18

# table of above
formattable(ih_hms18)