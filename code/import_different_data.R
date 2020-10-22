# Read in Different Data!


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(rio)
library(readxl)
library(sf)


# Read in Excel -----------------------------------------------------------

# with rio
xlsx_rio <- rio::import("data/DC_cagedata_06_29_20.xlsx")

# with readxl
xlsx_readxl <- readxl::read_excel("data/DC_cagedata_06_29_20.xlsx", sheet = 1)

# Read in csv -------------------------------------------------------------

# with rio
csv_rio <- rio::import("data/DC_2020_Zoop.csv")

# with readr
csv_readr <- readr::read_csv("data/DC_2020_Zoop.csv")


# Read a .dbf file (mainly for Eric) --------------------------------------

library(foreign)
dbf <- read.dbf(system.file("files/sids.dbf", package="foreign"))



# Read in Shapefile -------------------------------------------------------


# finally, read in shapefile with sf package
library(sf)

# glacial extent last 18-20k years ago
glaciers <- st_read("data/glacial_extent_18kaBP.shp")

# quick look with mapview
library(mapview)
mapview(glaciers)
