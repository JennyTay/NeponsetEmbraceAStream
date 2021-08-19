
library(readxl)
library(tidyverse)
library(stringr)
library(lubridate)
library(ggplot2)



#temperature

#first combine the THB001 and THBT01 files for june and july (they are currently separate)
jun <- read_excel("raw_data/temperature/THBT01 2021-06-19 12_06_23 -0400.xlsx", skip = 1)
jul <-read_excel("raw_data/temperature/THBT01 2021-07-17 12_01_17 -0400.xlsx", skip = 1)

comb <- rbind(jun, jul)
write.csv(comb, file = "raw_data/temperature/THBT01 2021-07-17 12_01_17 -0400.csv")

#then I went in and manually deleted the original files

jun <- read_excel("raw_data/temperature/THB001 2021-06-19 11_10_04 -0400.xlsx", skip = 1)
jul <- read_excel("raw_data/temperature/THB001 2021-07-17 11_03_38 -0400.xlsx", skip = 1)

comb <- rbind(jun, jul)
write.csv(comb, file = "raw_data/temperature/THB001 2021-07-17 11_03_38 -0400.csv")

#then I went in and manually deleted the original files


#then i manunally renamed THB005 and THB006 in the 2021 files- these two labels need to be switched( I only did this is my temperature folder, I did not do this in the orginal files uploaded by declan)
#I didnt rename in the file itself because I skip this line when reading in the files



#mad a second folder with the csv's because some files were saved as .xlsx and others were .csv so will need to run the follow list.files and loop twice for each folder.

#notes
#need to add in Purgatory brook - there was no 'final' file that had combined the months so I skipped it for now
# also need to confirm what data is erroneous and what is correct - i removed the first couple rows but not sure about the last



tmpfiles <- list.files(paste(getwd(),"raw_data/temperature2020", sep = "/"), recursive = T, full.names = T)
sites <- substr(tmpfiles, 142, 147)
sites <- gsub(" ", "", sites)


#this loop isnt working because the column names in the files starting at 18 are different....

temps <- NULL

for (i in 1:length(tmpfiles)){
  df <- read_excel(tmpfiles[i], skip = 2, sheet = 3, col_names = FALSE) %>%  #first row is notes, second row is meter error
    dplyr::select(2:3) %>%
    mutate(site = sites[i])
  
  names(df)[1:2] <-  c("Date", "Temp_F")
  
  df <- df %>% 
    mutate(Temp_C = (Temp_F - 32)*(5/9))
  
  temps <- rbind(temps, df)
  
  print(i)
}


ggplot(data = temps, mapping = aes(x = Date, y = Temp_C))+
  geom_line()+
  facet_wrap( ~ site, nrow = 5)


temps$season <- ifelse(month(temps$Date)%in% c(6,7,8), "warm", "cool")


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
  group_by(site) %>% 
  summarize(int = Date[4] - Date[3],
            int2 = Date[400] - Date[399])

min15 <- int$site[int$int >=15]
min10 <- int$site[int$int ==10]
min1 <- int$site[int$int ==1]

#calculate duration of time (hrs) that water T is greater than 20, 25, and 28.3



duration <- temps %>% 
  group_by(site, season) %>% 
  mutate(
    wat_grt20 = ifelse(site %in% min15, (sum(Temp_C>20)*15/60),
                       ifelse (site %in% min10, (sum(Temp_C>20)*10/60),
                               (sum(Temp_C>20)/60)))
  )
duration <- duration %>% 
  group_by(site, season) %>% 
  mutate(
    wat_grt25 = ifelse(site %in% min15, (sum(Temp_C>25)*15/60),
                       ifelse (site %in% min10, (sum(Temp_C>25)*10/60),
                               (sum(Temp_C>25)/60)))
  )
duration <- duration %>% 
  group_by(site, season) %>% 
  mutate(
    wat_grt283 = ifelse(site %in% min15, (sum(Temp_C>28.3)*15/60),
                        ifelse (site %in% min10, (sum(Temp_C>28.3)*10/60),
                                (sum(Temp_C>28.3)/60)))
  )


#bind duration to test

duration <- duration %>% 
  mutate(year = year(Date),
         month = month(Date),
         day = day(Date))


test <- test %>% 
  left_join(duration, by = c("site", "season", "year", "month", "day"))




#calculate the max 7-day max, mean 7-day mean, min 7-day min, max 7-day mean, max 7-day range

metrics <- test %>% 
  group_by(site, season) %>% 
  summarise(max7max = max(SevDayMaxWat, na.rm = T),
            avg7max = mean(SevDayMaxWat, na.rm = T),
            min7min = min(SevDayMinWat, na.rm = T),
            max7avg = max(SevDayMeanWat, na.rm = T),
            grt20 = unique(wat_grt20),
            grt25 = unique(wat_grt25),
            grt283 = unique(wat_grt283)
  )


#save
save(metrics, file = 'StrmTempMetrics_EAS_2020.RData')
load('StrmTempMetrics_EAS_2020.RData')

save(temps, file = "StrnTemp_EAS_2020.RData")