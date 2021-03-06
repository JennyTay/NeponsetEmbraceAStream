library(readxl)
library(tidyverse)
library(sf)
library(sp)
library(rgdal)
library(raster)


dat <- read_excel("raw_data/CulvertScoring.xlsx", sheet= 2) %>% 
  dplyr::select(1:32)

#edit a couple
dat$`Aquatic Passability Score`[dat$`Local ID` == "PUB005"] <- "Significant Barrier" #the score did not account for the very large inlet drop

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
