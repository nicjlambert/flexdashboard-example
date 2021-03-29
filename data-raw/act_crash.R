## code to prepare `act_crash` dataset goes here

#-------------------------------------------------------------------------------
### ACT crash data sourced from ################################################
### https://www.data.act.gov.au/Transport/ACT-Road-Crash-Data/6jn4-m8rx ########

library(here)
library(janitor)
library(sf)
library(rgeos)

#-------------------------------------------------------------------------------
### point data #################################################################

point <- read.csv(here::here("data-raw/ACT_Road_Crash_Data.csv")) %>%
  clean_names() %>%
  mutate(crash_hour = as.integer(substr(crash_time, 1, 2)))
glimpse(point)

#-------------------------------------------------------------------------------
### polygon data (shapefile) ###################################################

shp <- st_read(here::here("data-raw/SA2_2016_AUST.shp")) %>%
  clean_names() %>%
  filter(ste_name16 == "Australian Capital Territory") %>%
  st_transform(4283)

# find centroid of each county
shp$cent_lng <- st_coordinates(st_centroid(shp))[,1] # add longitude to sf
shp$cent_lat <- st_coordinates(st_centroid(shp))[,2] # add latitude to sf
# checking coordinate reference system of object
st_crs(shp)

#-------------------------------------------------------------------------------
### join point and polygon data ################################################

act_crash <- point %>%
  filter(!is.na(latitude),!is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4283) %>%
  st_join(shp["sa2_name16"])

# count unique crashes per sa2 in ACT
count <- act_crash %>%
  as_tibble() %>%
  group_by(sa2_name16) %>%
  summarise(
    crashes = n_distinct(crash_id)) %>%
  print()

# join  count with sa2 in ACT, calc crash density
act_crashes_sa2 <- left_join(shp, count, by = c("sa2_name16" = "sa2_name16")) %>%
  mutate(crashes_sq_km = round(crashes / areasqkm16, 2),
         decile = ntile(crashes_sq_km, 10),
         color = ifelse(decile == 1, "blue", ifelse(decile == 10, "red", "black")),
         bins = ifelse(decile == 1 ,"Area in bottom 10% for accidents", ifelse(decile == 10, "Area in top 10% for accidents", NA)))
  #filter(!is.na(crashes)) %>%
glimpse(act_crashes_sa2)

#-------------------------------------------------------------------------------
### save data ##################################################################

usethis::use_data(act_crashes_sa2, overwrite = TRUE)

rm(point, shp, act_crash, count)
