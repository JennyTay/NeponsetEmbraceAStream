#tidying dam data

#read in Neprwa dam survey data and MassGIS Office of dam safety dam data

library(sf)
library(raster)



ods <- st_read("raw_data/dams/DAMS_PT_Neponset_Clip.shp")
ods$source <- "ODS"

nep <- st_read("raw_data/dams/NepRWA_Dams.shp")
nep$source <- "neprwa"

names(ods)
names(nep)

#remove the dams in the neponset list that also occur in the ODS dataset

nep <- nep %>% 
  filter(!MAStructur %in% c(ods$NATID)) %>% 
  dplyr::select(-DataSheetN, -MAStructur) 

names(nep) <- tolower(names(nep))
names(ods) <- tolower(names(ods))
  
dams <- bind_rows(ods, nep)

dams <- as(dams, "Spatial")
shapefile(dams, file = "dams_ODS_nep_COMBINED.shp", overwrite = TRUE)

#then in GIS I did a spatial join of this file to the NHD file, so that we could have stream names on each dam

dams <- st_read("raw_data/dams/Neprwa_ODS_COMBINED_NHD.shp")
dams <- dams %>% 
  dplyr::select(-(15:31)) %>% 
  filter(Distance <300) #remove ones that are not on a NHD stream line
dams <- as(dams, "Spatial")
shapefile(dams, file = "dams_ODS_nep_COMBINED.shp", overwrite = TRUE)

