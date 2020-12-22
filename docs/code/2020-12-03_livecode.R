##This is a data exploration script working with data from the Zooper R package created by folks at the IEP.
##More details on the Zooper package can be found here: https://github.com/InteragencyEcologicalProgram/zooper
##In this script we will practice:
##    1) binning zooplankton taxa into operational taxanomic units (OTUs)
##    2) plotting zooplankton data in a time series, binning by day of year, and by month
##    3) joining zooplankton data with water year type and flow data covariates
##    4) plotting the sampling sites on various map types (using base and leaflet)
##    5) spatial interpolation of zooplankton CPUE to a raster
##    6) constructing a loop to output annual delta cladocera spatial distribution
##
##Written for debug buddies code club meeting 2020-12-03 by Eric Holmes

# Load libraries ----------------------------------------------------------

##Run these two lines (without the '#' sign) to install the zooper package. Only needs to be done once.
# install.packages("devtools")
# devtools::install_github("InteragencyEcologicalProgram/zooper")

library(zooper) # this package is not available from CRAN, if installing for the first time use the code above
library(dplyr) # useful package for manipulating and sumarizing dataframes
library(ggplot2) # plotting package which excels with many data types
library(leaflet) # Create interactive maps
library(sf) # simple features spatial package
library(gstat) # spatial modeling, prediction and simulation package
library(raster) # package for working with spatial raster data
library(sp) # package for working with spatial data

# Download zoop data ------------------------------------------------------

##Function from Zooper package to download and synthesize zoop data from multiple surveys
MyZoops <- Zoopsynther(Data_type = "Taxa", 
                       Sources = c("EMP", "FRP", "FMWT"), 
                       Size_class = "Meso", 
                       Date_range = c("1990-10-01", "2020-09-30"))

##Save copy of downloaded zoop data
zoop <- MyZoops

##Load zoop taxa OTU classifications from csv file on Box
taxalookup <- read.csv("https://ucdavis.box.com/shared/static/m96xeicraj9ba5hnhhe13s7lrtrrga5g.csv", stringsAsFactors = F)

# Join and aggregate zoop data with OTUs -----------------------------------------------

##Join zoop and taxalookup data which holds the OTU classifications
zoop <- merge(zoop, taxalookup[, c("Taxname", "OTU")], by = "Taxname", all.x = T)

##Quick check on counts by OTU class
table(zoop$OTU)

##Use dplyr to group and summarise data
zooply <- zoop %>% group_by(Station, Latitude, Longitude, Date, OTU) %>% 
  summarise(sumcatch = sum(CPUE), n = length(Station))

##Add julien day, year, month and water year to summarized data
zooply$jday <- as.numeric(format(as.Date(zooply$Date), format = "%j"))
zooply$year <- as.integer(format(zooply$Date, format = "%Y"))
zooply$month <- as.integer(format(zooply$Date, format = "%m"))
zooply$wy <- ifelse(zooply$month %in% c(10:12), zooply$year + 1, zooply$year)

##Calculate unique sampling events per site
siteN <- zooply %>% group_by(Station, Date) %>% 
  summarize(sumtot = sum(sumcatch, na.rm = T), lati = mean(Latitude, na.rm = T), long = mean(Longitude, na.rm = T)) %>% 
  group_by(Station) %>% summarize(n = length(sumtot), lat = mean(lati, na.rm = T), lon = mean(long, na.rm = T), 
                                  logzoop = log(mean(sumtot + 1)))

##Plot histogram of site sample sizes
ggplot(siteN, aes(x = Station, y = n)) + geom_bar(stat = "identity") + 
  theme_bw() + theme(axis.text.x = element_text(angle = 90)) + geom_hline(yintercept = 50)

##Plot subset of sites with more the X sampling events
ggplot(zooply[zooply$Station %in% unlist(siteN[siteN$n > 50,"Station"]),], aes(x = Date, y = log(sumcatch + 1))) + 
  geom_point() + facet_wrap(Station ~ .)

##Plot time series of single site
ggplot(zooply[zooply$Station == "NZ028",], aes(x = Date, y = sumcatch)) + geom_point() + theme_bw()

##Plot CPUE by julien day, grouped by year
ggplot(zooply[zooply$Station == "NZ028",], aes(x = jday, y = log10(sumcatch + 1))) + 
  geom_point() + stat_smooth(aes(color = wy), se = F) + scale_color_viridis_d() + theme_bw() +
  facet_grid(OTU ~ ., scales = "free")

##Plot log CPUE distribution by month
ggplot(zooply[zooply$Station == "NZ028",], aes(x = month, y = log10(sumcatch + 1))) + 
  geom_violin(aes(group = month), fill = "black") +
  facet_grid(OTU ~ ., scales = "free") + stat_smooth(se = F, color = "brown") + theme_bw()

# Join zoop data with CDEC water year type --------------------------------

##Load CA water year type data originally from CDEC but stored in a csv file on Box
wytype <- read.csv("https://ucdavis.box.com/shared/static/g0bizu3u7kqdhulaxugneg0wvaoorljg.csv", stringsAsFactors = F)

##Join zooply and wytype data
zooply <- merge(zooply, wytype, by = "wy", all.x = T)

##Plot single station response to water year type
ggplot(zooply[zooply$Station == "NZ054" & zooply$wy < 2020,], aes(x = Type, y = log10(sumcatch + 1))) + geom_boxplot() +
  facet_grid(OTU ~ ., scales = "free")

##Plot log cladocera CPUE versus Oct-mar flow index for a single station
ggplot(zooply[zooply$Station == "NZ054" & zooply$wy < 2020 &
                     zooply$OTU %in% "Branchiopoda" & zooply$month %in% c(10:12, 1:3),], aes(x = OctMar, y = log10(sumcatch + 1))) + geom_point() +
  facet_grid(OTU ~ ., scales = "free") + stat_smooth(se = F) + labs(x = "Oct-Mar flow index")

##Plot log cladocera CPUE versus Oct-mar flow index faceted by station
ggplot(zooply[zooply$Station %in% unlist(siteN[siteN$n > 50, "Station"]) & zooply$wy < 2020 &
                zooply$OTU %in% "Branchiopoda" & 
                zooply$month %in% c(10:12, 1:3),], aes(x = OctMar, y = log10(sumcatch + 1))) + geom_point() +
  facet_wrap(Station ~ ., scales = "free") + stat_smooth(se = F, color = "red", method = "lm") + 
  labs(x = "Oct-Mar flow index")

# Join flow data ----------------------------------------------------------

##Set parameters for cdec flow download
sensor_num <- 41
start <- "1980-10-1"
end <- "2019-9-30"
station <- "VON"

##Download cdec flow data
von <- read.table(paste("http://cdec.water.ca.gov/dynamicapp/req/CSVDataServlet?Stations=", station, "&SensorNums=", 
                           sensor_num, "&dur_code=", "D", "&Start=", start, "&End=", end, sep=""),
                     header=FALSE, sep=",", skip=1, stringsAsFactors = F)
##Select only Datetime and Flow fields
von <- von[,c(5,7)]
##Set column names
colnames(von) <- c("Datetime", "Flow")
##Convert flow to numeric
von$Flow <- as.numeric(von$Flow)
##Format datetime column
von$Datetime <- as.POSIXct(von$Datetime, format = "%Y%m%d %H%M")
##Format date for future merge
von$Date <- as.Date(von$Datetime)

##add month, year, and wy fields to von
von$month <- as.integer(format(von$Datetime, format = "%m"))
von$year <- as.integer(format(von$Datetime, format = "%Y"))
von$wy <- ifelse(von$month %in% c(10:12), von$year + 1, von$year)

##Calculate mean flow for the jan-may period for each wy
vonply <- von[von$month %in% 1:5,] %>% group_by(wy) %>% summarize(meanflow = mean(Flow))

##Convert date formats to be compatible with the flow data
zooply$Date <- as.Date(zooply$Date)

##extract flow values for each sampling event
zooply <- merge(zooply, von[,c("Date", "Flow")], by = "Date", all.x = T)

##Plot log cladocera CPUE versus flow
ggplot(zooply[zooply$Station %in% unlist(siteN[siteN$n > 50, "Station"]) & zooply$wy < 2020 &
                zooply$OTU %in% "Branchiopoda" & 
                zooply$month %in% c(10:12, 1:3),], aes(x = Flow, y = log10(sumcatch + 1))) + geom_point() +
  facet_wrap(Station ~ .) + stat_smooth(se = F, color = "red")

# Plot sites zoop sampling sites on a map ---------------------------------

##Plot the sampling sites
ggplot(siteN, aes(x = lon, y = lat)) + geom_point() + theme_bw()

##Plot sampling sites in an interactive map
leaflet(siteN) %>% addCircles(lng = ~lon, lat = ~lat,
                                label = ~Station,
                                labelOptions = labelOptions(noHide = F, textOnly = F)) %>% addTiles()

##Spruce up the interactive map with persistent labels, subsetted sites, and a basemap toggle
leaflet(siteN[siteN$n>50,]) %>% addCircles(lng = ~lon, lat = ~lat,
                                label = ~Station,
                                labelOptions = labelOptions(noHide = T, textOnly = F)) %>%
  addTiles(options = providerTileOptions(noWrap = TRUE), group="Base") %>%
  addProviderTiles("Esri.WorldImagery", group="Imagery") %>%
  addLayersControl(baseGroups = c("Base","Imagery"), options = layersControlOptions(collapsed = FALSE))

##Load delta open water polygon for later use as an analysis mask - stored in memory as an sf object
delta <- st_read("https://ucdavis.box.com/shared/static/4mc7e3eb0i77kumbuadhw4l1ug3zcgsr.geojson")
##Transform delta to WGS84 geographic coordinate system
deltawgs84 <- st_transform(delta, CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))
##Convert deltawgs84 object from sf to sp - spatial points dataframe
deltasp <- as(deltawgs84, "Spatial")

##Convert siteN from data.frame to sf object
zoopsf <- st_as_sf(siteN, coords = c("lon", "lat"))
##Set coordinate reference system to wgs84
zoopsf <- st_set_crs(zoopsf, CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))
##Convert object from sf to sp
zoopsp <- as(zoopsf, "Spatial")

##Check alignment with plot of zoop points on delta polygon 
plot(deltasp, col = "blue", border = NA)
points(zoopsp, pch = 18, col = "brown")

# Spatial interpolation ---------------------------------------------------
##adapted from this example: https://mgimond.github.io/Spatial/interpolation-in-r.html

##Create an empty grid where n is the total number of cells
grd <- as.data.frame(spsample(zoopsp, "regular", n=100000))
names(grd) <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd) <- TRUE  # Create SpatialPixel object
fullgrid(grd) <- TRUE  # Create SpatialGrid object

##Add pojection information to the empty grid
proj4string(grd) <- proj4string(zoopsp)
zoopsp@bbox <- deltasp@bbox

##Interpolate the grid cells using a power value of 2 (idp=2.0)
zoopidw <- gstat::idw(logzoop ~ 1, zoopsp, newdata = grd, idp=2.0)

##Convert spatial grid dataframe to raster object
zoopras <- raster(zoopidw)
plot(zoopras)

##clip raster to delta open water geometry polygon
zooprasdelta <- mask(zoopras, deltasp)
plot(zooprasdelta)

##Spruce it up with a better color ramp
plot(zooprasdelta,
     breaks = seq(zoopras@data@min, zoopras@data@max, length.out = 10), 
     col = hcl.colors(10, "Lajolla"))

# Create a loop to visualize annual cladocera spatial distributions --------

##Subset to cladocera class and convert dataframe to simple features spatial object
cladsf <- st_as_sf(zooply[zooply$OTU == "Branchiopoda" & is.na(zooply$Longitude) == F,], 
                   coords = c("Longitude", "Latitude"))

##Set CRS to WGS geographic coordinate system
cladsf <- st_set_crs(cladsf, CRS("+ellps=WGS84 +proj=longlat +datum=WGS84 +no_defs"))

##Calculate log total CPUE
cladsf$logzoop <- log(cladsf$sumcatch + 1)

##Convert sf object to spatial points dataframe
cladsp <- as(cladsf, "Spatial")

##Set standard breaks for symbolizing the interpolated CPUE values
brks <- seq(0, max(cladsf$logzoop), .5)

##Loop
for(i in unique(cladsp$wy)){
  ##Print the water year the console for progress update
  print(i)
  ##Interpolation
  cladidw <- gstat::idw(logzoop ~ 1, cladsp[cladsp$wy == i & cladsp$month %in% c(1:5), ], newdata=grd, idp=2.0)
  ##Mask by delta open water polygon
  cladrasdelta <- mask(raster(cladidw), deltasp)
  
  ##Save plot of interpolated values for each water year
  ##NOTE: will fail if you do not have an output/Cladocera_annual folder in your working directory
  png(paste("output/Cladocera_annual/Cladocera_", i, ".png", sep = ""), 
      height = 7, width = 9, unit = "in", res = 300)
  plot(cladrasdelta, breaks = brks, col = hcl.colors(length(brks), "Lajolla"), 
       main = paste(i, ": Jan-May Cladocera log CPUE", sep = ""))
  points(cladsp[cladsp$year == i & cladsp$month %in% c(1:5), ])
  dev.off()
}

print("THE END")
