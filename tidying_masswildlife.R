#tidy MassWildlife Fish data
library(readxl)
library(lubridate)
library(tidyverse)
library(sf)

dat <- read_excel("raw_data/NeponsetWatershed_Fish Data_fromCaleb.xlsx")
range(dat$sample_date)
table(dat$sample_date)


dat$year <- year(dat$sample_date)
dat$month <- month(dat$sample_date)
table(dat$year)
unique(dat$month)
unique(dat$method)
unique(dat$waterbody)

#make shapefile

#read in the edna results so we can make the two the same crs
edna <- st_read("eDNA_results.shp")
st_crs(edna)

geo <- st_crs(edna)
class(geo)

dat2 <- st_as_sf(dat, coords = c("longitude", "latitude"), crs=geo)

st_crs(dat2)
ggplot() +
  geom_sf(data = dat2) +
  ggtitle("Map of Plot Locations")

final <- as(dat2, "Spatial")
shapefile(final, file = "MassWildlife_results.shp", overwrite = TRUE)





