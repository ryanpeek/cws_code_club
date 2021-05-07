# FUNCTIONS


# Ways to load functions
cms_to_cfs <- function(discharge) {
  cfs_out <- discharge * 35.3146662
  return(cfs_out)
}

# some long script to read in data and convert to cfs

source("code/f_cms_to_cfs.R")

cms_to_cfs(10)
data_piece <- cms_to_cfs(10)


library(tidyverse)
library(dataRetrieval) # a spellbook of water data functions

# use a function to download data:
flow_site <- readNWISdata(site = "11427000", # USGS Station ID
                          service = "iv", # instantaneous measurements
                          parameterCd = "00060", # discharge in cfs
                          startDate = "2021-04-01",
                          endDate = "2021-05-06",
                          tz = "America/Los_Angeles")

# fix column names so we can use a more informative parameter name
flow_site <- dataRetrieval::renameNWISColumns(flow_site)

# plot with ggplot
# visualize:
ggplot(data = flow_site) +
  geom_line(aes(x = dateTime, y = Flow_Inst), color = "darkblue") +
  theme_classic() +
  labs(x = "", y = "Flow (cfs)",
       title = "Discharge from USGS Station 11427000",
       caption = "data pulled using {dataRetrieval} package in R")


# apply function to our data:
# add a new column to existing dataset with mutate
flow_site <- flow_site %>%
  mutate(Flow_cms = cms_to_cfs(Flow_Inst))


# new function to plot a single week of our choice from flow data

plot_flow_week <- function(data, wk) {
  # filters to the week we want
  data_wk <- data %>% filter( lubridate::week(dateTime) == wk)
  # plots the week we want
  ggplot(data=data_wk) +
    geom_line(aes(x = dateTime, y = Flow_Inst), color = "darkblue") +
    theme_classic() +
    labs(x = "", y = "Flow (cfs)",
         title = glue::glue("Discharge from USGS Station 11427000: from Week {wk}"))
}

plot_flow_week(flow_site, 13)
plot_flow_week(flow_site, 15)

## custom function to download, unzip, plot shp from R
library(sf)
library(tidyverse)


# Get Data ----------------------------------------------------------------

durl <- "https://water.usgs.gov/GIS/dsdl/gagesII_9322_point_shapefile.zip"
webpath <- durl

get_shp_zip <- function(webpath){

  dest_dir <- tempdir() # make temp dir
  temp_shp <- tempfile(tmpdir = dest_dir) # make tempfile for shp
  download.file(webpath,temp_shp) # download the zipped file
  # now unzip the file depending on extension
  if( grepl('.tgz$|.tar.gz$', webpath) ){
    utils::untar(temp_shp, exdir = dest_dir)
  } else if(grepl('.zip$', webpath)){
    utils::unzip(temp_shp, exdir = dest_dir)
  } else{
    stop('not *.zip or *.tgz or *.tar.gz!')
  }
  # now get shp name with full temp path
  shp_file <- list.files(dest_dir, pattern = ".shp$", full.names=TRUE)
  # read it in!
  st_read(shp_file)

  # rm temp files

}

gages2 <- get_shp_zip(durl)

mapview::mapview(gages2)
