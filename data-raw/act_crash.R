#! /usr/bin/R

# This script downloads and processes crash data from the ACT.

# Set the time zone to "Australia/Sydney"
Sys.setenv("TZ" = "Australia/Sydney")

# Define a list of packages to check
required_packages <- c("janitor", "rgeos", "sf", "dplyr", "stringr")

repos <- "https://cran.csiro.au"

# Check if the required packages are installed and install them if necessary
install_required_packages <- function(pkgs) {
  for (pkg in pkgs) {
    # Check if the package is installed
    if (!require(pkg, character.only = TRUE)) {
      # Install the package with dependencies
      install.packages(pkg,
      dependencies = TRUE,
      repos = repos
)
    }
    # Load the package into the current R session
    library(pkg, character.only = TRUE)
  }
}

# Install the required packages
install_required_packages(required_packages)

# Define the URL of the CSV file to download
csv_url <- "https://www.data.act.gov.au/api/views/6jn4-m8rx/rows.csv?accessType=DOWNLOAD"

# Define a function to download and read the CSV file
download_and_read_csv <- function(url) {
  # Create a temporary file
  temp_file <- tempfile()

  # Download the CSV file to the temporary file
  download.file(url, temp_file)

  # Read the CSV file into an R data frame
  df <- read.csv(temp_file)
  print(head(df))

  # Delete the temporary file
  unlink(temp_file)

  # Return the data frame
  return(df)
}

# Download and read the CSV file
point_data <- download_and_read_csv(csv_url)

## Define a function to process the data
process_data <- function(df) {
  # Clean and transform the data as needed
  cleaned_df <- df %>%
    clean_names() %>%
    mutate(year = substr(crash_date, 7, 10))

  # Return the cleaned data frame
  return(cleaned_df)
}

# Process the data
processed_data <- process_data(point_data)

# Define a function to create the `act_crash` dataset
create_act_crash_dataset <- function(df) {
   # Create the `act_crash` dataset from the processed data
  act_crash <- df %>%
   select(year, crash_id, latitude, longitude, crash_severity) %>%
    group_by(year, crash_id, latitude, longitude, crash_severity) %>%
    summarize(count = n())

  # Return the `act_crash` dataset
  return(act_crash)
}

# Create the `act_crash` dataset
act_crash <- create_act_crash_dataset(processed_data)

# Read features from shape file
sa2_shp <- st_read("SA2_2016_AUST.shp") %>%
  clean_names() %>%
  filter(ste_name16 == "Australian Capital Territory") %>%
  st_transform(4283)

# Find centroid of each county and add columns to data frame
sa2_shp <- sa2_shp %>%
  mutate(cent_lng = st_coordinates(st_centroid(sa2_shp))[, 1],
         cent_lat = st_coordinates(st_centroid(sa2_shp))[, 2])

# Check the coordinate reference system (CRS) of object
st_crs(sa2_shp)

# Join point and polygon data 

act_crash <- act_crash %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4283) %>%
  st_join(sa2_shp["sa2_name16"])

# Count unique crashes per SA2 in ACT
count <- act_crash %>%
  as_tibble() %>%
  group_by(sa2_name16) %>%
  summarise(crashes = n_distinct(crash_id))

# Join  count with sa2 in ACT, calc crash density
act_crashes_sa2 <- left_join(sa2_shp, count, 
  by = c("sa2_name16" = "sa2_name16"))  %>%
  mutate(crashes_sq_km = round(crashes / areasqkm16, 2),
         decile = ntile(crashes_sq_km, 10),
         color = ifelse(decile == 1, "blue",
                 ifelse(decile == 10, "red",
                "black")),
         bins = ifelse(decile == 1 ,"Area in bottom 10% for accidents", 
                ifelse(decile == 10, "Area in top 10% for accidents", NA))
         ) %>%
 filter(!is.na(crashes))

 save(act_crashes_sa2, "/data/processed/act_crashes_sa2.RData")