#tidying habitat assessment data

library(tidyverse)
library(lubridate)


###############  habitat unit data ########################



df <- read.csv("raw_data/HabitatUnitInfo_1.csv")

#remove test rows
dat <- df %>% 
  filter(!Creator == "neponset",
         !Crew_HU == "test2",
         !HU.Comment..free.text. == "training, incomplete",
         !SiteID_HU == "GEB001") %>%  #only one visit and they only recorded one habitat unit - erroneously 
  dplyr::select(c(3:21)) %>% 
  separate(Date_HU, into = c("date", "time"), sep = " ") %>% 
  mutate(date = mdy(date),
         SiteID_HU = str_trim(SiteID_HU, "both")) %>% 
  dplyr::select(-time) %>% 
  mutate(depth_cm = ifelse(Depth.Max..nearest.0.01m.<=1.5, Depth.Max..nearest.0.01m.*100, 
                           ifelse(Depth.Max..nearest.0.01m. >1.5 & Depth.Max..nearest.0.01m. <=7, Depth.Max..nearest.0.01m.*10,
                                  Depth.Max..nearest.0.01m.)),
         depth_cm = ifelse(depth_cm<0, abs(depth_cm), depth_cm),
         SiteID_HU = toupper(SiteID_HU),
         month = month(date)) %>% 
  dplyr::select(-Depth.Max..nearest.0.01m.) 

names(dat)[2:18] <- c("time", "site", "crew", "habitat", "start_position_m", "end_position_m", "length_m", "split_channel", "vel", 
                "wetted_width_m", "pool_feature", "LWD", "dom_sub", "subdom_sub", "canopy_cov_perc", "instream_cov", "notes")

#reorder columns so that site and date come first
dat <- dat %>% 
  dplyr::select(site, date, time, month, depth_cm, habitat:instream_cov, crew, notes )

#fix site typos
dat$site[dat$site == "MOB007.5"] <- "MMB007.5"
dat$site[dat$site == "THB 005"] <- "THB005"
dat$site[dat$site == "PBT004"] <- "PTB004"
dat$site[dat$site == "POB001"] <- "BEB001" #this was figured out because the reach end positions are the same
dat$site[dat$site == "BEV001"] <- "BEB001"
dat$site[dat$site == "Beb 1"] <- "BEB001"
dat$site[dat$site == "PTB 001"] <- "PTB001"
dat$site[dat$site == "PTB 004"] <- "PTB004"
dat$site[dat$site == "BEB1"] <- "BEB001"
dat$site[dat$site == "GEB 002"] <- "GEB002"

dat$site <- ifelse(dat$date == mdy('6-21-2021') & dat$site == "PTB005", "PTB004", dat$site)
dat$site <- ifelse(dat$date == mdy('8-24-2021') & dat$site == "PTB005" & dat$time %in% c("10:49", "10:52", "10:57"), "PTB007", dat$site)

#fix date typos as described by Declan
dat$date <- as.Date(ifelse(dat$site == "PTB001" & dat$date == mdy("6/9/2021"), mdy("4/30/2021"), dat$date), origin = "1970-01-01")
dat$date <- as.Date(ifelse(dat$site == "PTB004" & dat$date == mdy("6/9/2021"), mdy("4/30/2021"), dat$date), origin = "1970-01-01")
dat$date <- as.Date(ifelse(dat$site == "PTB007" & dat$date == mdy("6/9/2021"), mdy("4/30/2021"), dat$date), origin = "1970-01-01")
dat$date <- as.Date(ifelse(dat$site == "BEB001" & dat$date == mdy("6/9/2021"), mdy("4/30/2021"), dat$date), origin = "1970-01-01")
dat$date <- as.Date(ifelse(dat$site == "POB003" & dat$date == mdy("6/9/2021"), mdy("5/4/2021"), dat$date), origin = "1970-01-01")

#need t0 update month column with new dates
dat$month <- month(dat$date)

#delete duplicated entries for BEB003 on Jun 22 - delete the one started at 9:45

dat$wetted_width_m[dat$wetted_width_m == 99] <- NA


#save habitat data
habitat <- dat
save(habitat, file = "habitat.RData")





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

ggplot(data = dat, mapping = aes(x = as.factor(site), y = complexity_num))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90))


ggplot(data = dat, mapping = aes(x = as.factor(site), y = LWD))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90))

###############  site data ########################


df <- read.csv("raw_data/Form_1_0.csv")

#delete duplicate BEB001 entry on Jun 9 (the entry on 4/30 is the correct one)
df <- df[-26,]
#delete duplicate GEB003 on June 22 (delete the one at 9:45)
df <- df[-46,]

dat <- df %>% 
  filter(!Creator == "neponset",
         !Crew.Names == "test2",
         !Crew.Names == "Test") %>% 
  mutate(site =  toupper(Site.ID),
         site = str_trim(site, "both")) %>% 
  separate(Date, into = c("date", "timedelete"), sep = " ") %>% 
  mutate(date = mdy(date)) %>% 
  dplyr::select(-timedelete, -Site.ID, - ï..ObjectID, -GlobalID) 

names(dat)[1:13] <- c("date", "time", "stream", "location_desc", "crew", "start_position_m", "end_position_m", "temp_logger_position",
                "DO_mgl", "DO_perc_sat", "Temp_C", "DO_calibration", "notes")

dat <- dat %>% 
  dplyr::select(site, location_desc, date, time, 5:20) %>% 
  mutate(stream = substr(site,1,3)) 



#fix site typos
dat$site[dat$site == "MOB007.5"] <- "MMB007.5"
dat$site[dat$site == "THB 005"] <- "THB005"
dat$site[dat$site == "PBT004"] <- "PTB004"
dat$site[dat$site == "POB001"] <- "BEB001" #this was figured out because the reach end positions are the same
dat$site[dat$site == "BEV001"] <- "BEB001"
dat$site[dat$site == "BEB1"] <- "BEB001"
dat$site[dat$site == "PTB 001"] <- "PTB001"
dat$site[dat$site == "PTB 004"] <- "PTB004"
dat$site[dat$site == "GEB 002"] <- "GEB002"

dat$site <- ifelse(dat$date == mdy('6-21-2021') & dat$site == "PTB005", "PTB004", dat$site)
dat$site <- ifelse(dat$date == mdy('8-24-2021') & dat$site == "PTB005" & dat$time == "10:48", "PTB007", dat$site)


#fix date typos as described by Declan
dat$date <- as.Date(ifelse(dat$site == "PTB001" & dat$date == mdy("6/9/2021"), mdy("4/30/2021"), dat$date), origin = "1970-01-01")
dat$date <- as.Date(ifelse(dat$site == "PTB004" & dat$date == mdy("6/9/2021"), mdy("4/30/2021"), dat$date), origin = "1970-01-01")
dat$date <- as.Date(ifelse(dat$site == "PTB007" & dat$date == mdy("6/9/2021"), mdy("4/30/2021"), dat$date), origin = "1970-01-01")
dat$date <- as.Date(ifelse(dat$site == "BEB001" & dat$date == mdy("6/9/2021"), mdy("4/30/2021"), dat$date), origin = "1970-01-01")
dat$date <- as.Date(ifelse(dat$site == "POB003" & dat$date == mdy("6/9/2021"), mdy("5/4/2021"), dat$date), origin = "1970-01-01")

#need t0 update month column with new dates
dat$month <- month(dat$date)

DissOxy <- dat
save(DissOxy, file = "dissolvedO2.RData")
