# dates and times in R

# working with date times
# read in some data
library(tidyverse)

# need to look at file first to see why it didn't read in
tempdata <- read_csv("https://raw.githubusercontent.com/gge-ucd/R-DAVIS/master/data/2015_NFY_solinst.csv", skip=12)

# DATETIMES package
library(lubridate)

# POSIXct -----------------------------------------------------------------

# known as "Calendar time"

# first need to make a datetime column
tempdata <- tempdata %>%
  mutate(datetime = ymd_hms(paste0(Date, " ", Time)))

# how paste0 works: everything to character
class(paste0(tempdata$Date, " ", tempdata$Time)) # makes character

# look at class of data
class(tempdata$Date)
class(tempdata$Time)
class(tempdata$datetime)
unclass(tempdata$datetime[1])

# convert to just date:
tempdata$datetime[1]
as.Date(tempdata$datetime)[1]
class(as.Date(tempdata$datetime)[1])


# POSIXlt -----------------------------------------------------------------

# "local" time...stores information in a list

# pull out a single row or cell
(p1 <- tempdata[1,"datetime"]) # this is a single cell
(p1 <- tempdata[1,]) # this is a single row

# only problem is it defaults to number of seconds since Jan 1 1970
as.character(tempdata[1,6])

# make POSIXlt
plt <- as.POSIXlt(paste0(p1$Date, " ", p1$Time))
str(plt)
unclass(plt)

# note, this format is zero indexed, so Jan = 0, Feb = 1, etc. Same with days.
# years are relative to 1900, so 2015 would be 115

# Exporting funky formats ----------------------------------------------

# if trying to export to specific date or datetime format, create new columns, and then paste together:
newcolumn <- paste0(year(tempdata$datetime), "/", month(tempdata$datetime), "/", day(tempdata$datetime))

# Working with Formatting Funky Formats -----------------------------------

# strptime:
ex1 <- "2011/05A01/T09:00"
ex1
ymd_hm(ex1) # uses default UTC time

as.POSIXct(ex1, format="%Y/%mA%d/T%H:%M") # timezone of my computer


# set a specific time zone (see time zones here: https://en.wikipedia.org/wiki/List_of_tz_database_time_zones)
as.POSIXct(ex1, format="%Y/%mA%d/T%H:%M", tz = "America/New_York")
as.POSIXct(ex1, format="%Y/%mA%d/T%H:%M", tz = "America/Los_Angeles")
as.POSIXct(ex1, format="%Y/%mA%d/T%H:%M", tz = "America/Buenos_Aires")
as.POSIXct(ex1, format="%Y/%mA%d/T%H:%M", tz = "Europe/Paris")
as.POSIXct(ex1, format="%Y/%mA%d/T%H:%M", tz = "Europe/Berlin")
as.POSIXct(ex1, format="%Y/%mA%d/T%H:%M", tz = "Pacific/Honolulu")


# Other packages that are good for times & dates --------------------------

# see {chron}: https://cran.r-project.org/web/packages/chron/index.html
# see {zoo}: https://cran.r-project.org/web/packages/zoo/vignettes/zoo-quickref.pdf

# a lesson I wrote a few years ago:
# https://mikoontz.github.io/data-carpentry-week/lesson_lubridate.html
