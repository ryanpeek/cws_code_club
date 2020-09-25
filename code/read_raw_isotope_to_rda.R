
# Overview ----------------------------------------------------------------

# data from Isotope Facility:
## Sheet 1 = "Summary" has info about the submission data
## Sheet 2 = "Samples" has the results
## Sheet 3 = "References" reference info
## Sheet 4 = "Sample List" original sample list submitted


# Load Libraries ----------------------------------------------------------

library(here) # WHERE AM I!!?
library(readxl) # for working with spreadsheets
library(dplyr) # tidying data!
library(janitor) # great for renaming columns and tidying

# Read In Data ------------------------------------------------------------

# read in just columns A:G
iso <- read_xlsx(here("data", "raw", "TilcockM SutterCNTray1 CN 0520 (1).xlsx"), 
                 sheet = "Samples", 
                 range = cell_cols("A:J"))

# this is the same but without "here", still using relative link though!
iso <- read_xlsx("data/raw/TilcockM SutterCNTray1 CN 0520 (1).xlsx",
                 sheet = "Samples", 
                 range = cell_cols("A:J"))


# Read in Metadata --------------------------------------------------------

# this is the same but without "here", still using relative link though!
iso_meta <- read_xlsx("data/raw/TilcockM SutterCNTray1 CN 0520 (1).xlsx",
                 sheet = "Summary", 
                 range = "A22:C23", 
                 col_names = c("submission", "NA", "date")) %>% 
  #reformat the date
  mutate(date=as.Date(date)) %>% 
  as.data.frame()
  

# Tidy --------------------------------------------------------------------

# the col names have spaces and weird characters. Not great for coding. Fix with janitor!
iso_clean <- janitor::clean_names(iso) %>% 
  # rename a few columns
  rename(d13C=d13cvpdb, d15N=d15n_air, tissue=type_of_material) %>% 
  # drop unneeded cols
  select(-ends_with("comment"), -tray_name, -well_id) %>% 
  # relocate tissue column to right after sample_id
  relocate(tissue, .after=sample_id)


# Save out ----------------------------------------------------------------

# use glue to add submission date to filename
library(glue)

save(iso_clean, file = glue("data/{Sys.Date()}_iso_tidy_submitted_{iso_meta[1,3]}.rda"))
