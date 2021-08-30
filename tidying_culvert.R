library(readxl)
library(tidyverse)
library(sf)
library(sp)
library(rgdal)
library(raster)


dat <- read_excel("raw_data/Culvert Scoring.xlsx", sheet= 2) %>% 
  select(1:32)

#make this into a shapefile
#read in the edna results so we can make the two the same crs
edna <- st_read("eDNA_results.shp")
st_crs(edna)

geo <- st_crs(edna)
class(geo)

dat2 <- st_as_sf(dat, coords = c("x", "y"), crs=geo)

st_crs(dat2)
ggplot() +
  geom_sf(data = dat2) +
  ggtitle("Map of Plot Locations")

final <- as(dat2, "Spatial")
shapefile(final, file = "culvert.shp", overwrite = TRUE)
