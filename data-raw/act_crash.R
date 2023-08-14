# Set the time zone to "Australia/Sydney"
Sys.setenv("TZ" = "Australia/Sydney")

# Define a list of packages to check
required_packages <- c("janitor", "rgeos", "sf", "dplyr", "stringr")

repos <- "https://cran.csiro.au"

install_required_packages <- function(pkgs) {
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, dependencies = TRUE, repos = repos)
    }
    library(pkg, character.only = TRUE)
  }
}

install_required_packages(required_packages)

csv_url <- "https://www.data.act.gov.au/api/views/6jn4-m8rx/rows.csv?accessType=DOWNLOAD"

download_and_read_csv <- function(url) {
  temp_file <- tempfile()
  download.file(url, temp_file)
  df <- read.csv(temp_file)
  unlink(temp_file)
  return(df)
}

point_data <- download_and_read_csv(csv_url)

process_data <- function(df) {
  df %>%
    clean_names() %>%
    mutate(year = substr(crash_date, 7, 10))
}

processed_data <- process_data(point_data)

create_act_crash_dataset <- function(df) {
  df %>%
    select(year, crash_id, latitude, longitude, crash_severity) %>%
    group_by(year, crash_id, latitude, longitude, crash_severity) %>%
    summarize(count = n())
}

act_crash <- create_act_crash_dataset(processed_data)

sa2_shp <- st_read("SA2_2016_AUST.shp") %>%
  clean_names() %>%
  filter(ste_name16 == "Australian Capital Territory") %>%
  st_transform(4283) %>%
  mutate(cent_lng = st_coordinates(st_centroid(sa2_shp))[, 1],
         cent_lat = st_coordinates(st_centroid(sa2_shp))[, 2])

act_crash <- act_crash %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4283) %>%
  st_join(sa2_shp["sa2_name16"])

count <- act_crash %>%
  as_tibble() %>%
  group_by(sa2_name16) %>%
  summarise(crashes = n_distinct(crash_id))

act_crashes_sa2 <- left_join(sa2_shp, count, by = c("sa2_name16" = "sa2_name16")) %>%
  mutate(crashes_sq_km = round(crashes / areasqkm16, 2),
         decile = ntile(crashes_sq_km, 10),
         color = c("blue", "black", "red")[findInterval(decile, c(1, 9, 10))],
         bins = c("Area in bottom 10% for accidents", NA, "Area in top 10% for accidents")[findInterval(decile, c(1, 9, 10))]) %>%
  filter(!is.na(crashes))

save(act_crashes_sa2, file = "/data/processed/act_crashes_sa2.RData")
