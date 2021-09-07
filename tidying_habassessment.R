#tidying habitat assessment data

library(tidyverse)
library(lubridate)


###############  habitat unit data ########################



df <- read.csv("raw_data/HA_HabUnit_0811201.csv")

#remove test rows
dat <- df %>% 
  filter(!Creator == "neponset",
         !Crew_HU == "test2") %>% 
  dplyr::select(5:23) %>% 
  separate(CreationDate, into = c("date", "time"), sep = " ") %>% 
  mutate(date = mdy(date)) %>% 
  dplyr::select(-c(18,20)) %>% 
  mutate(depth_cm = ifelse(Depth.Max..nearest.0.01m.<=1.5, Depth.Max..nearest.0.01m.*100, 
                           ifelse(Depth.Max..nearest.0.01m. >1.5 & Depth.Max..nearest.0.01m. <=7, Depth.Max..nearest.0.01m.*10,
                                  Depth.Max..nearest.0.01m.)),
         depth_cm = ifelse(depth_cm<0, abs(depth_cm), depth_cm),
         SiteID_HU = toupper(SiteID_HU),
         month = month(date)) %>% 
  dplyr::select(-Depth.Max..nearest.0.01m.) %>% 
  filter(-site == "GEB001") #only one visit and they only recorded one habitat unit - erroneously 

names(dat)[1:17] <- c("site", "crew", "habitat", "start_position_m", "end_position_m", "length_m", "split_channel", "vel", 
                "wetted_width_m", "pool_feature", "LWD", "dom_sub", "subdom_sub", "canopy_cov_perc", "instream_cov", "notes", "date")

#reorder columns so that site and date come first
dat <- dat %>% 
  dplyr::select(site, date, month, depth_cm, habitat:instream_cov, crew, notes )

#fix site typos
dat$site[dat$site == "MOB007.5"] <- "MMB007.5"
dat$site[dat$site == "THB 005"] <- "THB005"
dat$site[dat$site == "PBT004"] <- "PTB004"
dat$site[dat$site == "POB001"] <- "BEB001" #this was figured out because the reach end positions are the same
dat$site[dat$site == "Bev001"] <- "BEB001"
dat$site[dat$site == "Beb 1"] <- "BEB001"
dat$site <- ifelse(dat$date == mdy(06-21-2021) & dat$site == PTB005, PTB004, dat$site)



dat$wetted_width_m[dat$wetted_width_m == 99] <- NA

#issues: 2 100's for end positin on GEB003 in June
#what is the deepest depth we consider to be in meters? what about the depths of 2.7 and 3.1?  in THB006 are these m or cm?

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

ggplot(data = dat, mapping = aes(x = as.factor(month), y = depth_cm))+
  geom_boxplot()

ggplot(data = dat, mapping = aes(x = as.factor(site), y = depth_cm))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90))

ggplot(data = dat, mapping = aes(x = as.factor(site), y = canopy_cov_perc))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90))

ggplot(data = dat, mapping = aes(x = depth_cm))+
  geom_bar()+
  facet_wrap(~month)+
  theme(axis.text.x = element_text(angle = 90))




###############  site data ########################


df <- read.csv("raw_data/HA_dat_0811201.csv")


dat <- df %>% 
  filter(!Creator == "neponset",
         !Editor == "neponset",
         !Crew.Names == "test2",
         !Crew.Names == "Test")
