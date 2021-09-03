#tidying habitat assessment data

library(tidyverse)
library(lubridate)

df <- read.csv("raw_data/HA_HabUnit_0811201.csv")

#remove test rows
dat <- df %>% 
  filter(!Creator == "neponset",
         !Editor == "neponset",
         !Crew_HU == "test2") %>% 
  dplyr::select(5:23) %>% 
  separate(CreationDate, into = c("date", "time"), sep = " ") %>% 
  mutate(date = mdy(date)) %>% 
  dplyr::select(-c(18,20)) %>% 
  mutate(depth_cm = ifelse(Depth.Max..nearest.0.01m.<5, Depth.Max..nearest.0.01m.*100, Depth.Max..nearest.0.01m.),
         depth_cm = ifelse(depth_cm<0, abs(depth_cm), depth_cm),
         SiteID_HU = toupper(SiteID_HU),
         month = month(date)) %>% 
  dplyr::select(-Depth.Max..nearest.0.01m.)

names(dat)[1:17] <- c("site", "crew", "habitat", "start_position_m", "end_position_m", "length_m", "split_channel", "vel", 
                "wetted_width_m", "pool_feature", "LWD", "dom_sub", "subdom_sub", "canopy_cov_perc", "instream_cov", "notes", "date")

#reorder columns so that site and date come first
dat <- dat %>% 
  dplyr::select(site, date, month, depth_cm, habitat:instream_cov, crew, notes )

#fix site typos
dat$site[dat$site == "MOB007.5"] <- "MMB007.5"
dat$site[dat$site == "THB 005"] <- "THB005"
dat$site[dat$site == "PBT004"] <- "PTB004"
dat$site[dat$site == "POB001"] <- "BEB001" #this was figured out because the reach end poisiitons are the same
dat$wetted_width_m[dat$wetted_width_m == 99] <- NA

test <- dat %>% 
  dplyr::select(site, month) %>% 
  unique()

table(dat$vel)
hist(dat$depth_cm, breaks = 40)
hist(dat$LWD)
hist(dat$canopy_cov_perc)
hist(dat$wetted_width_m)
table(dat$dom_sub)
table(dat$subdom_sub)
table(dat$site)
