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

# read multiple files in


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(purrr) # for looping through files
library(janitor) # to clean names
library(lubridate) # for working with datetimes
library(fs)
library(glue)

# List files and Read Data ------------------------------------------------

# make our file list
file_list <- list.files(path = "data", pattern = "Argos*", full.names = TRUE)
file_list

# loop through and read files
df <- purrr::map(file_list, ~read_csv(.x)) %>%
  bind_rows() %>%
  # clean names
  janitor::clean_names()


# Clean Datetime ----------------------------------------------------------

df_clean <- df %>%
  mutate(datetime1 = lubridate::dmy_hm(date), .after=date) %>%
  separate(col=date, into = c("HMS", "date2"), sep = " ", remove = FALSE)



df_clean <- df_clean %>%
  mutate(datetime2 = case_when(
    is.na(datetime1) ~ paste0(date2, " ", HMS),
    TRUE ~ NA_character_ # must match the same class of data, which is character here
  ), .after=datetime1)

# merge together
# first format the second datetime column properly (as POSIX)
df_all <- df_clean %>%
  mutate(datetime2= lubridate::dmy_hms(datetime2),
         # now use case when to say if NA in one col, use the other
         datetime = case_when(
           is.na(datetime1) ~ datetime2,
           is.na(datetime2) ~ datetime1
         ), .after=date) %>%
  # drop the old temporary columns
  select(-c(datetime2, datetime1, HMS, date2))


# Split Data --------------------------------------------------------------

df_list <- df_all %>%
  split(.$deploy_id)


# Save out as separate files ----------------------------------------------

file_list
(out_files <- path_ext_remove(path_file(file_list)))

map2(df_list, out_files, ~write_csv(.x, file = glue("data_output/cleaned_{.y}_{Sys.Date()}.csv")))

# Make a Mapview Map --------------------------------------------------------

# use sf to make a quick map of data
dinga_sf <- df_all %>%
  select(deploy_id, ptt, datetime, type, quality, latitude, longitude) %>%
  sf::st_as_sf(coords=c("longitude", "latitude"), remove=FALSE, crs=4326) %>%
  # add day for plotting purpose:
  mutate(days=yday(datetime))

plot(dinga_sf$geometry)


library(mapview)
mapviewOptions(fgb = FALSE)
mapview(dinga_sf, zcol="days")



