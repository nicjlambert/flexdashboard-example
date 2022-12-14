#! /usr/bin/R
## code to prepare `act_crash` dataset goes here

Sys.setenv("TZ"="Australia/Sydney")

#-------------------------------------------------------------------------------
### ACT crash data sourced from ################################################
### https://www.data.act.gov.au/Transport/ACT-Road-Crash-Data/6jn4-m8rx ########

# Check if packages are installed and install them if necessary
# Set the CRAN mirror to use
CRAN <- "https://cran.csiro.au"

# Define the packages to check
packages <- c("janitor", "rgeos", "sf")

# Check if packages are installed and install them if necessary
lapply(packages, function(x) {
  # Check if the package is installed
  if (!require(x, character.only = TRUE)) {
    # Install the package with dependencies
    install.packages(x, dependencies = TRUE, repos = CRAN)
  }
  # Load the package into the current R session
  library(x, character.only = TRUE)
})

#-------------------------------------------------------------------------------
### point data #################################################################

tf <- tempfile()

download.file("https://www.data.act.gov.au/api/views/6jn4-m8rx/rows.csv?accessType=DOWNLOAD", tf)

point <- read.csv(tf) 

print(head(point))

# Delete the temporary file
unlink(tf)
#%>%
#  clean_names() %>%
#  mutate(crash_hour = as.integer(substr(crash_time, 1, 2)))
#glimpse(point)

#-------------------------------------------------------------------------------
### polygon data (shapefile) ###################################################
#
#hp <- st_read(here::here("data-raw/SA2_2016_AUST.shp")) %>%
# clean_names() %>%
# filter(ste_name16 == "Australian Capital Territory") %>%
# st_transform(4283)
#
# find centroid of each county
#hp$cent_lng <- st_coordinates(st_centroid(shp))[,1] # add longitude to sf
#hp$cent_lat <- st_coordinates(st_centroid(shp))[,2] # add latitude to sf
# checking coordinate reference system of object
#t_crs(shp)
#
#-------------------------------------------------------------------------------
### join point and polygon data ################################################
#
#ct_crash <- point %>%
# filter(!is.na(latitude),!is.na(longitude)) %>%
# st_as_sf(coords = c("longitude", "latitude"), crs = 4283) %>%
# st_join(shp["sa2_name16"])
#
# count unique crashes per sa2 in ACT
#ount <- act_crash %>%
# as_tibble() %>%
# group_by(sa2_name16) %>%
# summarise(
#   crashes = n_distinct(crash_id)) %>%
# print()
#
# join  count with sa2 in ACT, calc crash density
#ct_crashes_sa2 <- left_join(shp, count, by = c("sa2_name16" = "sa2_name16")) %>%
# mutate(crashes_sq_km = round(crashes / areasqkm16, 2),
#        decile = ntile(crashes_sq_km, 10),
#        color = ifelse(decile == 1, "blue", ifelse(decile == 10, "red", "black")),
#        bins = ifelse(decile == 1 ,"Area in bottom 10% for accidents", ifelse(decile == 10, "Area in top 10% for accidents", NA)))
# #filter(!is.na(crashes)) %>%
#limpse(act_crashes_sa2)
#
#-------------------------------------------------------------------------------
### save data ##################################################################
#
#sethis::use_data(act_crashes_sa2, overwrite = TRUE)
#
#m(point, shp, act_crash, count)
