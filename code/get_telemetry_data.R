# reading in telemetry data from csv
# need to fix datetimes


# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(sf)
library(janitor) # to clean names
library(lubridate) # for working with datetimes


# Import Data -------------------------------------------------------------

dinga <- read_csv("data/Argos_Dinga.csv")

# access a column with spaces or weird characters with back ticks ``
dinga$`Error radius`

# we can rename to clean names
dinga_clean <- dinga %>% janitor::clean_names()
str(dinga_clean)


# Clean the Datetimes -----------------------------------------------------

# format one of the datetimes types
dinga_dates <- dinga_clean %>%
  mutate(datetime1 = lubridate::dmy_hm(date), .after=date) %>%
  separate(col=date, into = c("HMS", "date2"), sep = " ", remove = FALSE)

# separate and switch the date and times from weird format
dinga_dates <- dinga_dates %>%
  mutate(datetime2 = case_when(
    is.na(datetime1) ~ paste0(date2, " ", HMS),
    TRUE ~ NA_character_
    ), .after=datetime1)

# format and merge the datetimes
dinga_datetimes <- dinga_dates %>%
  mutate(datetime2= lubridate::dmy_hms(datetime2),
         datetime = case_when(
           is.na(datetime1) ~ datetime2,
           is.na(datetime2) ~ datetime1
         ), .after=date) %>%
  select(-c(datetime2, datetime1, HMS, date2))


# Make a Mapview Map --------------------------------------------------------

# use sf to make a quick map of data
dinga_sf <- dinga_datetimes %>%
  select(deploy_id, ptt, datetime, type, quality, latitude, longitude) %>%
  sf::st_as_sf(coords=c("longitude", "latitude"), remove=FALSE, crs=4326) %>%
  # add day for plotting purpose:
  mutate(days=yday(datetime))

plot(dinga_sf$geometry)


library(mapview)
mapviewOptions(fgb = FALSE)
mapview(dinga_sf, zcol="days")



