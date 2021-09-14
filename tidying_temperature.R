

library(readxl)
library(tidyverse)
library(stringr)
library(lubridate)
library(sf)
library(sp)
library(raster)


#read in data



#temperature 2020
#need to add in Purgatory brook - there was no 'final' file that had combined the months so I skipped it for now
# also need to confirm what data is erroneous and what is correct - i removed the first couple rows but not sure about the last


#in 2020 the temperature logger ID's do not align with the eDNA IDs so i did a spatial join in ArcMAP and renamed the 2020 temp loggers to have corresponding eDNA IDs
#read in the new file and save a conversion table (loggerID with siteID)
dat <- st_read("raw_data/2020TempLogger_correctID.shp")
dat <- dat %>% 
  dplyr::select(LogID, site_1) %>% 
  rename(site = "site_1") %>% 
  filter(LogID != "PTB004") #remove this one becaus the closest eDNA site has a different logger that is closer to it.
final <- as(dat, "Spatial")
shapefile(final, file = "2020TempLogID_eDNAsite_convsersion.shp", overwrite = TRUE)



tmpfiles <- list.files(paste(getwd(),"raw_data/temperature2020", sep = "/"), recursive = T, full.names = T)
logid <- substr(tmpfiles, 142, 147)
logid <- gsub(" ", "", logid)


#this loop isnt working because the column names in the files starting at 18 are different....

temps <- NULL

for (i in 1:length(tmpfiles)){
  df <- read_excel(tmpfiles[i], skip = 2, sheet = 3, col_names = FALSE) %>%  #first row is notes, second row is meter error
    dplyr::select(2:3) %>%
    mutate(LogID = logid[i])
  
  names(df)[1:2] <-  c("Date", "Temp_F")
  
  df <- df %>% 
    mutate(Temp_C = (Temp_F - 32)*(5/9))
  
  temps <- rbind(temps, df)
  
  print(i)
}

temps$LogID[temps$LogID == "THB01"] <- "THB001"
temps$LogID[temps$LogID == "THB02"] <- "THB002"
temps$LogID[temps$LogID == "THB03"] <- "THB003"
temps$LogID[temps$LogID == "THB04"] <- "THB004"
temps$LogID[temps$LogID == "THB06"] <- "THB006"
temps$LogID[temps$LogID == "BEB01"] <- "BEB001"
temps$LogID[temps$LogID == "BEB02"] <- "BEB002"
temps$LogID[temps$LogID == "BEB03"] <- "BEB003"
temps$LogID[temps$LogID == "BEB04"] <- "BEB004"

#join to conversion table to get the proper 'site

temps <- temps %>% 
  left_join(dat, by = "LogID") %>% 
  filter(!is.na(site)) %>% 
  dplyr::select(Date, Temp_C, site)


###### temperature 2021 #######

#BEB001
dat <- read_excel("raw_data/temperature2021/BEB001 2021-09-08 13_44_49 -0400.xlsx", skip = 96)%>% 
  dplyr::select(2:3)
names(dat) <- c("date", "tempF")
write.csv(dat, file = "raw_data/temperature2021/final/BEB001.csv")

#BEB005
dat <- read_excel("raw_data/temperature2021/BEB005b 2021-08-21 12_21_03 -0400.xlsx", skip = 96)%>% 
  dplyr::select(2:3)
names(dat) <- c("date", "tempF")
write.csv(dat, file = "raw_data/temperature2021/final/BEB005.csv")

#POB002
jun <- read.csv("raw_data/temperature2021/POB002 2021-06-18 10_13_49 -0400.csv", skip = 96) %>% 
  dplyr::select(1:2)
names(jun) <- c("date", "tempF")
jun$date <- ymd_hms(jun$date)
sep <- read_excel("raw_data/temperature2021/POB002 2021-09-03 15_06_29 -0400.xlsx", skip = 96) %>% 
  dplyr::select(2:3)
names(sep) <- c("date", "tempF")
POB002 <- rbind(jun, sep)
write.csv(POB002, file = "raw_data/temperature2021/final/POB002.csv")

#POB003
jun <- read.csv("raw_data/temperature2021/POB003 2021-06-18 11_49_21 -0400.csv", skip = 96)%>% 
  dplyr::select(1:2)
names(jun) <- c("date", "tempF")
jun$date <- ymd_hms(jun$date)
sep <- read_excel("raw_data/temperature2021/POB003 2021-09-03 09_20_21 -0400.xlsx", skip = 96) %>% 
  dplyr::select(2:3)
names(sep) <- c("date", "tempF")
POB003 <- rbind(jun, sep)
write.csv(POB003, file = "raw_data/temperature2021/final/POB003.csv")

#POB005
dat <- read_excel("raw_data/temperature2021/POB005 2021-09-01 08_38_42 -0400.xlsx", skip = 96)%>% 
  dplyr::select(2:3)
names(dat) <- c("date", "tempF")
write.csv(dat, file = "raw_data/temperature2021/final/POB005.csv")

#POB006 - remove the first week
dat <- read_excel("raw_data/temperature2021/POB006 2021-08-30 12_16_01 -0400.xlsx", skip = 416)%>% 
  dplyr::select(2:3)
names(dat) <- c("date", "tempF")
write.csv(dat, file = "raw_data/temperature2021/final/POB006.csv")

#MMB007.5
jun <- read.csv("raw_data/temperature2021/MMB007.5 2021-06-18 14_26_58 -0400.csv", skip = 96)%>% 
  dplyr::select(1:2)
names(jun) <- c("date", "tempF")
jun$date <- ymd_hms(jun$date)
sep <- read_excel("raw_data/temperature2021/MMB007.5 2021-08-24 12_23_22 -0400.xlsx", skip = 96)%>% 
  dplyr::select(2:3)
names(sep) <- c("date", "tempF")
dat <- rbind(jun, sep)
write.csv(dat, file = "raw_data/temperature2021/final/MMB007.csv")

#PUB004
jul <- read.csv("raw_data/temperature2021/PUB004 2021-07-17 11_51_33 -0400.csv", skip = 96)%>% 
  dplyr::select(1:2)
names(jul) <- c("date", "tempF")
jul$date <- ymd_hms(jul$date)
aug <- read_excel("raw_data/temperature2021/PUB004 2021-08-24 16_01_00 -0400.xlsx", skip = 96)%>% 
  dplyr::select(2:3)
names(aug) <- c("date", "tempF")
dat <- rbind(jul, aug)
write.csv(dat, file = "raw_data/temperature2021/final/PUB004.csv")

#PUB005
jul <- read.csv("raw_data/temperature2021/PUB005 2021-07-17 09_55_23 -0400.csv", skip = 96)%>% 
  dplyr::select(1:2)
names(jul) <- c("date", "tempF")
jul$date <- ymd_hms(jul$date)
aug <- read_excel("raw_data/temperature2021/PUB005 2021-08-24 13_41_46 -0400.xlsx", skip = 96)%>% 
  dplyr::select(2:3)
names(aug) <- c("date", "tempF")
dat <- rbind(jul, aug)
write.csv(dat, file = "raw_data/temperature2021/final/PUB005.csv")

#GEB002
dat <- read_excel("raw_data/temperature2021/GEB002 2021-08-31 11_23_11 -0400.xlsx", skip = 96)%>% 
  dplyr::select(2:3)
names(dat) <- c("date", "tempF")
write.csv(dat, file = "raw_data/temperature2021/final/GEB002.csv")

#GEB003
dat <- read_excel("raw_data/temperature2021/GEB003 replacement 2021-08-31 13_05_12 -0400.xlsx", skip = 96)%>% 
  dplyr::select(2:3)
names(dat) <- c("date", "tempF")
write.csv(dat, file = "raw_data/temperature2021/final/GEB003.csv")

#MLB001
dat <- read_excel("raw_data/temperature2021/MLB001 2021-08-30 11_15_36 -0400.xlsx", skip = 96)%>% 
  dplyr::select(2:3)
names(dat) <- c("date", "tempF")
write.csv(dat, file = "raw_data/temperature2021/final/MLB001.csv")

#MLB005
dat <- read_excel("raw_data/temperature2021/MLB005 2021-08-30 10_37_09 -0400.xlsx", skip = 96)%>% 
  dplyr::select(2:3)
names(dat) <- c("date", "tempF")
write.csv(dat, file = "raw_data/temperature2021/final/MLB005.csv")

#THB001
jun <- read_excel("raw_data/temperature2021/THB001 2021-06-19 11_10_04 -0400.xlsx", skip = 96)%>% 
  dplyr::select(2:3)
names(jun) <- c("date", "tempF")
jul <- read_excel("raw_data/temperature2021/THB001 2021-07-17 11_03_38 -0400.xlsx", skip = 96)%>% 
  dplyr::select(2:3)
names(jul) <- c("date", "tempF")
sep <- read_excel("raw_data/temperature2021/THB001 2021-09-03 12_13_25 -0400.xlsx", skip = 96)%>% 
  dplyr::select(2:3)
names(sep) <- c("date", "tempF")
dat <- rbind(jun, jul, sep)
write.csv(dat, file = "raw_data/temperature2021/final/THB001.csv")

#THBT01
jun <- read_excel("raw_data/temperature2021/THBT01 2021-06-19 12_06_23 -0400.xlsx", skip = 96)%>% 
  dplyr::select(2:3)
names(jun) <- c("date", "tempF")
jul <- read_excel("raw_data/temperature2021/THBT01 2021-07-17 12_01_17 -0400.xlsx", skip = 96)%>% 
  dplyr::select(2:3)
names(jul) <- c("date", "tempF")
sep <- read_excel("raw_data/temperature2021/THBT01 2021-09-03 12_50_49 -0400.xlsx", skip = 96)%>% 
  dplyr::select(2:3)
names(sep) <- c("date", "tempF")
dat <- rbind(jun, jul, sep)
write.csv(dat, file = "raw_data/temperature2021/final/THBT01.csv")

#THB005 #this one is actually stored as THB006
dat <- read_excel("raw_data/temperature2021/THB006 2021-08-20 12_56_32 -0400.xlsx", skip = 96)%>% 
  dplyr::select(2:3)
names(dat) <- c("date", "tempF")
write.csv(dat, file = "raw_data/temperature2021/final/THB005.csv")

#THB006 #this one is actually stored as THB005
dat <- read_excel("raw_data/temperature2021/THB005 2021-08-20 11_32_00 -0400.xlsx", skip = 96)%>% 
  dplyr::select(2:3)
names(dat) <- c("date", "tempF")
write.csv(dat, file = "raw_data/temperature2021/final/THB006.csv")

#PTB001 
dat <- read_excel("raw_data/temperature2021/PTB001 2021-08-31 15_31_01 -0400.xlsx", skip = 96)%>% 
  dplyr::select(2:3)
names(dat) <- c("date", "tempF")
write.csv(dat, file = "raw_data/temperature2021/final/PTB001.csv")

#PTB004 
dat <- read_excel("raw_data/temperature2021/PTB004 2021-08-30 10_42_28 -0400.xlsx", skip = 96)%>% 
  dplyr::select(2:3)
names(dat) <- c("date", "tempF")
write.csv(dat, file = "raw_data/temperature2021/final/PTB004.csv")

#PTB005
jun <- read.csv("raw_data/temperature2021/PTB005 2021-06-19 10_26_20 -0400.csv", skip = 96)%>% 
  dplyr::select(1:2)
names(jun) <- c("date", "tempF")
jun$date <- ymd_hms(jun$date)
jul <- read.csv("raw_data/temperature2021/PTB005 2021-07-16 10_26_38 -0400.csv", skip = 96)%>% 
  dplyr::select(1:2)
names(jul) <- c("date", "tempF")
jul$date <- ymd_hms(jul$date)
sep <- read_excel("raw_data/temperature2021/PTB005 2021-08-24 09_25_49 -0400.xlsx", skip = 96)%>% 
  dplyr::select(2:3)
names(sep) <- c("date", "tempF")
dat <- rbind(jun, jul, sep)
write.csv(dat, file = "raw_data/temperature2021/final/PTB005.csv")

#PTB007
jun <- read.csv("raw_data/temperature2021/PTB007 2021-06-19 11_06_23 -0400.csv", skip = 96)%>% 
  dplyr::select(1:2)
names(jun) <- c("date", "tempF")
jun$date <- ymd_hms(jun$date)
jul <- read.csv("raw_data/temperature2021/PTB007 2021-07-16 11_16_41 -0400.csv", skip = 96)%>% 
  dplyr::select(1:2)
names(jul) <- c("date", "tempF")
jul$date <- ymd_hms(jul$date)
dat <- rbind(jun, jul, sep)
write.csv(dat, file = "raw_data/temperature2021/final/PTB007.csv")




#read in new files and combine

tmpfiles <- list.files(paste(getwd(),"raw_data/temperature2021/final", sep = "/"), recursive = T, full.names = T)
sites <- substr(tmpfiles, 148, 153)


temps21 <- NULL

for (i in 1:length(tmpfiles)){
  df <- read.csv(tmpfiles[i]) %>% 
    mutate(site = sites[i]) %>% 
    dplyr::select(site, date, tempF)
  
  
  df <- df %>% 
    mutate(Temp_C = (tempF - 32)*(5/9)) %>% 
    dplyr::select(-tempF) %>% 
    dplyr::select(date, Temp_C, site) %>% 
    rename(Date = date)
  
  temps21 <- rbind(temps21, df)
  
  print(i)
}




# Rbind temps and temps21 so we all the temperature data together

names(temps)
names(temps21)

temps <- rbind(temps, temps21)




ggplot(data = temps, mapping = aes(x = Date, y = Temp_C))+
  geom_line()+
  facet_wrap( ~ site, nrow = 5)


temps <- temps %>% 
  filter(!is.na(Temp_C))


#calculate daily max, mean, and min
metrics <- temps %>% 
  mutate(year = year(Date),
         month = month(Date),
         day = day(Date),
         season = ifelse(month(temps$Date)%in% c(3:5), "spring", "summer")) %>% 
  group_by(site, year, month, day, season) %>% 
  summarise(dailyMax = max(Temp_C),
            dailyAvg = mean(Temp_C),
            dailyMin = min(Temp_C)) %>% 
  ungroup()


#calculate the 7 day running mean, max, min wat temp

metric_mean_wat<- function(df){
  #calculate the 7 day averages of the water
  c<-df$dailyAvg
  n<- 1:(length(c)-6)
  wat<-NULL
  for (i in n){
    q <- mean(c(c[i],c[i+1],c[i+2],c[i+3],c[i+4],c[i+5],c[i+6]))
    wat <- rbind(wat, q)
  }
  df$SevDayMeanWat <- c(wat,NA,NA,NA,NA,NA,NA)
  return(df)
}

metric_max_wat<- function(df){
  #calculate the 7 day max of the water
  c<-df$dailyMax
  n<- 1:(length(c)-6)
  wat_max<-NULL
  for (i in n){
    q <- max(c(c[i],c[i+1],c[i+2],c[i+3],c[i+4],c[i+5],c[i+6]))
    wat_max <- rbind(wat_max, q)
  }
  df$SevDayMaxWat <- c(wat_max,NA,NA,NA,NA,NA,NA)
  return(df)
}  

metric_min_wat<- function(df){
  #calculate the 7 day min of the water
  c<-df$dailyMin
  n<- 1:(length(c)-6)
  wat_min<-NULL
  for (i in n){
    q <- min(c(c[i],c[i+1],c[i+2],c[i+3],c[i+4],c[i+5],c[i+6]))
    wat_min <- rbind(wat_min, q)
  }
  df$SevDayMinWat <- c(wat_min,NA,NA,NA,NA,NA,NA)
  return(df)
}  





test<-metrics %>% 
  group_by(site, season) %>% 
  nest() %>% 
  mutate(
    mean_wat = map(data, metric_mean_wat)
  )
test <- test %>% 
  dplyr::select(- data) %>% 
  unnest(cols = c(mean_wat))

test<-test %>% 
  group_by(site, season) %>% 
  nest() %>% 
  mutate(
    max_wat = map(data, metric_max_wat)
  )
test <- test %>% 
  dplyr::select(- data) %>% 
  unnest(cols = c(max_wat))

test<-test %>% 
  group_by(site, season) %>% 
  nest() %>% 
  mutate(
    min_wat = map(data, metric_min_wat)
  )
test <- test %>% 
  dplyr::select(- data) %>% 
  unnest(cols = c(min_wat))




#calculate time interval for each site
int <- temps %>% 
  mutate(year = year(Date)) %>% 
  unite(yearsite, year, site, sep = "_") %>% 
  group_by(yearsite) %>% 
  summarize(int = (Date[4] - Date[3]),
            int2 = (Date[400] - Date[399]))

min15 <- int$yearsite[int$int %in% c(0,15)]
min10 <- int$yearsite[int$int == 10]


#calculate duration of time (hrs) that water T is greater than 20, 25, and 28.3



duration <- temps  %>% 
  mutate(year = year(Date),
         season = ifelse(month(temps$Date)%in% c(3:5), "spring", "summer")) %>%  
  unite(yearsite, year, site, sep = "_") %>%  
  group_by(yearsite, season) %>% 
  mutate(
    wat_grt20 = ifelse(yearsite %in% min15, (sum(Temp_C>20)*15/60), sum(Temp_C>20)*10/60)
  )


duration <- duration %>% 
  group_by(yearsite, season) %>% 
  mutate(
    wat_grt15 = ifelse(yearsite %in% min15, (sum(Temp_C>15)*15/60), sum(Temp_C>15)*10/60)
  )

duration <- duration %>% 
  group_by(yearsite, season) %>% 
  mutate(
    wat_grt25 = ifelse(yearsite %in% min15, (sum(Temp_C>25)*15/60), sum(Temp_C>25)*10/60)
  )
duration <- duration %>% 
  group_by(yearsite, season) %>% 
  mutate(
    wat_grt283 = ifelse(yearsite %in% min15, (sum(Temp_C>28.3)*15/60), sum(Temp_C>28.3)*10/60)
  )


#bind duration to test

duration <- duration %>% 
  mutate(year = year(Date),
         month = month(Date),
         day = day(Date)) %>% 
  separate(yearsite, into = c("yeardelete", "site"), sep = "_") %>% 
  dplyr::select(-yeardelete) %>% 
  filter(!is.na(Temp_C)) 


test <- test %>% 
  left_join(duration, by = c("site", "season", "year", "month", "day"))




#calculate the max 7-day max, mean 7-day mean, min 7-day min, max 7-day mean, max 7-day range

metrics <- test %>% 
  group_by(year, site, season) %>% 
  summarise(max7max = max(SevDayMaxWat, na.rm = T),
            avg7max = mean(SevDayMaxWat, na.rm = T),
            min7min = min(SevDayMinWat, na.rm = T),
            max7avg = max(SevDayMeanWat, na.rm = T),
            grt15 = unique(wat_grt15),
            grt20 = unique(wat_grt20),
            grt25 = unique(wat_grt25),
            grt283 = unique(wat_grt283)
  )


#save
save(metrics, file = 'StrmTempMetrics_2020_2021.RData')
load('StrmTempMetrics_2020_2021.RData')

save(temps, file = "StrnTemp_EAS_2020_2021.RData")
load('StrnTemp_EAS_2020_2021.RData')
