library(sf)
library(readxl)
library(raster)
library(rgdal)
library(tidyverse)
library(lubridate)


dat <- read_excel("raw_data/edna_samplingschedule.xlsx")
dat <- dat %>% 
  filter(!str_detect(site, "Control")) %>% 
  mutate(year = year(date))
#site list
sites <- unique(dat$site)

#prepare results data
dna <- read_excel("raw_data/eDNA_results_SpringSampling_2020.xlsx")
names(dna)
names(dna)[1:2] <- c("site", "pos")
dna$meanCq <- NA
dna$year <- 2020

dna2 <- read_excel("raw_data/eDNA_results_SummerSampling_2021.xlsx")
names(dna2)
names(dna2)[1] <- "site"
names(dna2)[4] <- "pos"
names(dna2)[2] <- "species"
names(dna2)[5] <- "meanCq"
dna2$year <- 2021
dna2 <- dna2 %>% 
 dplyr:: select(site, pos, species, meanCq, year)
dna2$site <- ifelse(dna2$site == "PTB_t01", "PTB_T01",
                    ifelse(dna2$site == "PTB_t02", "PTB_T02", 
                           ifelse(dna2$site == "THB_t01", "THB_T01", dna2$site)))
 

dna <- rbind(dna, dna2)


rm(dna2)

dna <- dna %>% 
  mutate(occurrence = 1)
dna$site <- gsub("_", "", dna$site)

dna_all <- left_join(dat, dna, by = c("site", "year"))





#first add all the other eDNA sites to the dna dataframe with 0 for occurrence
dna_all$occurrence[is.na(dna_all$occurrence)] <- 0
dna_all$species <- "brook trout"
dna_all$pos_new <- ifelse(dna_all$pos == "4/4", 1.0,
                      ifelse(dna_all$pos == "3/4", 0.75,
                             ifelse(dna_all$pos == "2/4", 0.50,
                                    ifelse(dna_all$pos == "1/4", 0.25, 0))))

dna_all$pos[is.na(dna_all$pos)] <- 0
dna_all$pos_new[is.na(dna_all$pos_new)] <- 0





#fix file with the spatial information
geo <- st_read("raw_data/EDNA.shp")
geo <- geo %>% 
  dplyr:: select(-1) %>% 
  rename("site" = "Site_Type")
geo$site <- ifelse(geo$site == "MLB01", "MLB001",
                          ifelse(geo$site == "MLB02", "MLB002",
                                 ifelse(geo$site == "MLB03", "MLB003",
                                        ifelse(geo$site == "MLB04", "MLB004",
                                               ifelse(geo$site == "MLB05", "MLB005", geo$site)))))

#test to see which sites do not have corresponding spatial information
test <- dna_all %>% 
  filter(!site %in% geo$site) #THBT03




#first add the date 2020 to the spatial data so that we can join the notes in the spatial data to the correct site
geo$year <- 2020
t <- geo %>% 
  dplyr:: select(site, Notes, year)
dna_all <- left_join(dna_all, t, by = c("site", "year"))
#notes in THB001 is incorrect
dna_all[dna_all$site == "THB001", 11] <- NA

#combine the notes column and remove the geometry
dna_all$notes <- ifelse(is.na(dna_all$notes), dna_all$Notes, dna_all$notes)
dna_all <- dna_all %>% 
  dplyr:: select(-Notes, -geometry) %>% 
  dplyr:: select(site, date, year, species, pos, pos_new, occurrence, meanCq, samples, notes)





#add spatial data to dna_all
geo <- geo %>% 
  dplyr:: select(-Notes, -year) 
dna_all <- left_join(geo, dna_all, by = "site")






# NOTE: THBT03 was deleted from data because there was no spatial information - we could go in and add this manually (it was negative for brook trout)






#write shapefile
final <- as(dna_all, "Spatial")
shapefile(final, file = "eDNA_results.shp", overwrite = TRUE)

edna <- st_read("eDNA_spring2020_neponset.shp")
