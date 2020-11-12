
# 2020-10-22 DeBugging Buddies ---------------------------------

## * Loading Libraries -------------------------------------------------------

library(tidyverse) # everything
library(readxl) # reading xlsx data
library(sf) # spatial package for reading/writing/analyzing spatial data
library(rio) # R import & export

# install with:
#install.packages("sf")

# make a break with: hold down together (Ctrl + Shift + R)


## * Read in XLSX ------------------------------------------------------------

#  with readxl

xslx_readxl <- readxl::read_excel("data/DC_cagedata_06_29_20.xlsx")

xslx_messy <- readxl::read_excel("~/Downloads/code_wrangling/All_Sites_Synthesis.xlsx", "percent change", skip = 1)

# with rio package (works with rdata (rda), csv, excel files)


xlsx_rio <- rio::import("data/DC_cagedata_06_29_20.xlsx")



## * Read CSV ----------------------------------------------------------------

# with rio

csv_rio <- rio::import("data/DC_2020_Zoop.csv")

# with tidyverse readr package
csv_readr <- readr::read_csv("data/DC_2020_Zoop.csv")


## * Read in Messy Spreadsheet & Tidy ----------------------------------------


# data from Isotope Facility:
## Sheet 1 = "Summary" has info about the submission data
## Sheet 2 = "Samples" has the results
## Sheet 3 = "References" reference info
## Sheet 4 = "Sample List" original sample list submitted

library(janitor) # for renaming column names

iso <- readxl::read_xlsx("data/TilcockM SutterCNTray1 CN 0520 (1).xlsx", sheet="Samples",
                         range=cell_cols("A:J"))


## CLEAN NAMES

iso_clean <- janitor::clean_names(iso) %>%
  # rename a few columns with rename, new column = old column
  rename(d13C=d13cvpdb, d15N=d15n_air, tissue=type_of_material) %>%
  select(-ends_with("comment"), -tray_name, -well_id) %>%
  relocate(tissue, .after=sample_id) %>%
  separate(col = "sample_id", into=c("siteID","fishID", "tissueID"), sep = "-", remove = FALSE)


library(glue)

submission <- "2020-05-19"

# use glue to add submission date to the filename and save out
rio::export(iso_clean, file = glue::glue("data_output/{Sys.Date()}_iso_tidy_submitted_{submission}.csv"))


