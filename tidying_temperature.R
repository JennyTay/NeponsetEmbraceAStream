
library(readxl)
library(tidyverse)
library(stringr)
library(lubridate)
library(sf)
library(sp)

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


#temperature 2021 - I manually edited some of the excel spread sheets so that they had the same number of rows and columns to skip, select out.


#first combine the THB001 and THBT01 files for june and july (they are currently separate) - only need to do this again If I put new versions of these files in the folder
# jun <- read_excel("raw_data/temperature2021/THBT01 2021-06-19 12_06_23 -0400.xlsx", skip = 1)
# jul <-read_excel("raw_data/temperature2021/THBT01 2021-07-17 12_01_17 -0400.xlsx", skip = 1)
# 
# comb <- rbind(jun, jul)
# write.csv(comb, file = "raw_data/temperature2021/THBT01 2021-07-17 12_01_17 -0400.csv")
# 
# #then I went in and manually deleted the original files
# 
# jun <- read_excel("raw_data/temperature2021/THB001 2021-06-19 11_10_04 -0400.xlsx", skip = 1)
# jul <- read_excel("raw_data/temperature2021/THB001 2021-07-17 11_03_38 -0400.xlsx", skip = 1)
# 
# comb <- rbind(jun, jul)
# write.csv(comb, file = "raw_data/temperature2021/THB001 2021-07-17 11_03_38 -0400.csv")

#then I went in and manually deleted the original files
#manually re save the csv's as xlsx so all the same file types

#manually renamed THB005 and THB006 in the 2021 files- these two labels need to be switched( I only did this is my temperature folder, I did not do this in the orginal files uploaded by declan)
#I didnt rename in the file itself because I skip this line when reading in the files


#manunally removed the erroneisou data at the start of THB005, THB006, BEB001, MLB001

tmpfiles <- list.files(paste(getwd(),"raw_data/temperature2021", sep = "/"), recursive = T, full.names = T)
sites <- substr(tmpfiles, 142, 147)


#this loop isnt working because the column names in the files starting at 18 are different....

temps21 <- NULL

for (i in 1:length(tmpfiles)){
  df <- read_excel(tmpfiles[i], skip = 1, sheet=1, col_names = TRUE) %>%  #well will skip the first three rows and just get the data
    dplyr::select(contains(c("Date", "Temp"))) %>%
    mutate(site = sites[i])
  
  names(df)[1:2] <-  c("Date", "Temp_F")
  
  df <- df %>% 
    mutate(Temp_C = (Temp_F - 32)*(5/9)) %>% 
    dplyr::select(-Temp_F)
  
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


temps$season <- ifelse(month(temps$Date)%in% c(3:5), "spring", "summer")

temps <- temps %>% 
  filter(!is.na(Temp_C))


#calculate daily max, mean, and min
metrics <- temps %>% 
  mutate(year = year(Date),
         month = month(Date),
         day = day(Date)) %>% 
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
  summarize(int = (Date[4] - Date[3])/60,
            int2 = (Date[400] - Date[399])/60)

min15 <- int$yearsite[int$int %in% c(0,15)]
min10 <- int$yearsite[int$int == 10]


#calculate duration of time (hrs) that water T is greater than 20, 25, and 28.3



duration <- temps  %>% 
  mutate(year = year(Date)) %>%  
  unite(yearsite, year, site, sep = "_") %>%  
  group_by(yearsite, season) %>% 
  mutate(
    wat_grt20 = ifelse(yearsite %in% min15, (sum(Temp_C>20)*15/60),
                       ifelse (yearsite %in% min10, (sum(Temp_C>20)*10/60),
                               (sum(Temp_C>20)/60)))
  )


duration <- duration %>% 
  group_by(yearsite, season) %>% 
  mutate(
    wat_grt15 = ifelse(yearsite %in% min15, (sum(Temp_C>15)*15/60),
                       ifelse (yearsite %in% min10, (sum(Temp_C>15)*10/60),
                               (sum(Temp_C>15)/60)))
  )

duration <- duration %>% 
  group_by(yearsite, season) %>% 
  mutate(
    wat_grt25 = ifelse(yearsite %in% min15, (sum(Temp_C>25)*15/60),
                       ifelse (yearsite %in% min10, (sum(Temp_C>25)*10/60),
                               (sum(Temp_C>25)/60)))
  )
duration <- duration %>% 
  group_by(yearsite, season) %>% 
  mutate(
    wat_grt283 = ifelse(yearsite %in% min15, (sum(Temp_C>28.3)*15/60),
                        ifelse (yearsite %in% min10, (sum(Temp_C>28.3)*10/60),
                                (sum(Temp_C>28.3)/60)))
  )


#bind duration to test

duration <- duration %>% 
  mutate(year = year(Date),
         month = month(Date),
         day = day(Date)) %>% 
  separate(yearsite, into = c("yeardelete", "site"), sep = "_") %>% 
  select(-yeardelete) %>% 
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

save(temps, file = "StrnTemp_EAS_2020.RData")