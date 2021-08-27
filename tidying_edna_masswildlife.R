#we combine the two sets of species data
#we want the date, species, occurrence, and location (either waterbody or site)
#in the masswildlife data we call the count pos_new because in the eDNA data pos_new is how many replicates were positive.  THey are both meant to describe the population size observed



edna <- st_read("eDNA_results.shp")
edna <- edna %>% 
  dplyr::select(site, date, species, pos_new, occurrence) %>% 
  mutate(source = "EAS")



masswildlife <- st_read("MassWildlife_results.shp")


#filter for less than 150mm because caleb said in general they stock trout 6in (152mm), which less than that would be natural reproduction (greater could have been stocked)
test <- masswildlife %>% 
  filter(cmmn_nm == "Brook Trout",
         length <=150) %>% 
  dplyr::select(cmmn_nm, smpl_dt, watrbdy, lctn_ds) %>% 
  rename(site = "watrbdy",
         species = "cmmn_nm",
         date = "smpl_dt") 

test$lctn_ds[137:155]<- "Off Moose Hill Rd2" #need to edit location description becaseu two areas with the same location descritpion have different geometries

test <- test%>%  
  group_by(species, site,date, lctn_ds) %>% 
  summarise(pos_new = n()) %>% 
  ungroup() %>% 
  mutate(occurrence = 1,
         site = paste(site, lctn_ds, sep = " "),
         source = "MassWildlife") %>% 
  dplyr::select(-lctn_ds)

d <- rbind(test, edna)
d <- d %>% 
  mutate(season = ifelse(month(date) %in% c(3, 4,5), "spring", "summer"),
         species = tolower(species))



ggplot() +
  geom_sf(data = d) +
  ggtitle("Map of Plot Locations")


st_crs(d)
final <- as(d, "Spatial")
shapefile(final, file = "MassWildlife_EAS_combined_results.shp", overwrite = TRUE)

bio <- st_read("MassWildlife_EAS_combined_results.shp")
