# 2020-11-05 DeBugging Buddies --------------------------------------------

# today we'll go over how to set up git on your computer, and how to pull/commit/push your code & data.
# we'll also clean some datetime data and talk about iteration.

# github: sign up online
# make a new repository, add readme file.
# once created, click on green button and clone via HTTPS. Copy link to your clipboard.
# open RStudio and make a new project from github repo
# paste link and voila!
# then create data and scripts folders

# github w usethis is easier...can set up a PAT token or add your email/username
#https://usethis.r-lib.org/articles/articles/usethis-setup.html
library(usethis)

git_sitrep() # situation report! check how things are doing
usethis::use_git_config() # add username/email

# remove/add files to your .gitignore so they don't get added to your public repository
git_vaccinate()

# then ADD
# then COMMIT (useful message required!)
# then PUSH!

# going forward, always PULL then ADD-COMMIT-PUSH

# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(janitor) # to clean column names
library(lubridate) # for working with datetimes
library(purrr) # for looping through and applying a function
library(janitor) # to clean names
library(fs) # make/access filenames/folders
library(glue) # paste things together for filenames, titles, etc


# Import Data -------------------------------------------------------------

dinga <- read_csv("data/Argos_Dinga.csv")

# weird date formats

# List files and Read Data ------------------------------------------------

# make our file list
file_list <- list.files(path = "data", pattern = "Argos*", full.names = TRUE)
file_list

# loop through and read files
df <- purrr::map(file_list, ~read_csv(.x)) %>%
  bind_rows() %>% # data are same format so can squish together
  # clean column names
  janitor::clean_names()


# need to fix the datetime that has time first...
df_clean <- df %>%
  mutate(datetime1 = lubridate::dmy_hm(date), .after=date) %>%
  separate(col=date, into = c("HMS", "date2"), sep = " ", remove = FALSE)


# Clean Datetime ----------------------------------------------------------

# first clean and format one datetime type, fill evertyhing else in column with NA
df_clean <- df_clean %>%
  mutate(datetime2 = case_when(
    is.na(datetime1) ~ paste0(date2, " ", HMS),
    TRUE ~ NA_character_ # must match the same class of data, which is character here
  ), .after=datetime1)

# merge together
# format the second datetime column properly (as POSIXct)
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

# split into dataframes by the animal id (deploy id)
df_list <- df_all %>%
  split(.$deploy_id)


# Save out as separate files ----------------------------------------------

# make a new list of just the names we want to use for saving things out
file_list

# use fs package to remove extensions and file path so we just have the names
(out_files <- path_ext_remove(path_file(file_list)))

# use map2 to map a function over 2 lists at once, here items from .x=df_list, and items .y=out_files
map2(.x = df_list, .y = out_files,
     ~write_csv(.x, file = glue("data_output/cleaned_{.y}_{Sys.Date()}.csv")))

