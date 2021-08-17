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
  select(site, pos, species, meanCq, year)
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




#join dna_all with dat for spatial information
dat <- left_join(dat, dna_all, by = "site")


dat <- dat %>% 
  select(site, species, pos, pos_new, occurrence, Notes, geometry) %>% 
  rename("replicates" = "pos")


#notes in THB001 is incorrect
dat[dat$site == "THB001", 6] <- NA

#write shapefile
dat <- as(dat, "Spatial")
shapefile(dat, file = "eDNA_spring2020_neponset.shp")

edna <- st_read("eDNA_spring2020_neponset.shp")
