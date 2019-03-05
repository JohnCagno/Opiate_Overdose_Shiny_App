library(dplyr)
library(readr)
library(stringr)
library(data.table)
library(microbenchmark)
library(lubridate)
library(ggplot2)
library(tidyr)

setwd("C:/Users/Johnny/Documents/UNH Notes and Documents 2018/Fall 2018/Data 901 R/Shiny Assignment")

crime <- read.csv("MN_Crime.csv")


#subset different time formats
crime_time_1 <- crime%>%
  filter(str_detect(Time.of.Offense, "AM|PM")) #subset format 1

crime_time_2 <- crime%>%
  filter(str_detect(ReportedDate, "T")) #subset format 2 

#split time column on T

  #clean format 2

crime_time_2 <- crime_time_2 %>% separate(ReportedDate, into = c('ReportedDate', 'ReportedTime'), sep = "T") #seperate date and time

crime_time_2$ReportedTime = substr(crime_time_2$ReportedTime,1,nchar(crime_time_2$ReportedTime)-5) #remove decimals and Z

crime_time_2$ReportedTime <- format(strptime(crime_time_2$ReportedTime, "%H:%M:%S"), format="%H:%M:%S") #format time as time

  #clean format 1 

crime_time_1$ReportedDate <- format(as.Date(crime_time_1$ReportedDate, '%m/%d/%Y'), "%Y-%m-%d") #reformat date

crime_time_1$Time.of.Offense <- format(strptime(crime_time_1$Time.of.Offense, "%I:%M:%S%p"), format="%H:%M:%S") #format time as time

crime_time_1$ReportedTime <- NA #create column of NA to match format 1 DF

#rejoin

crime_fixed <- rbind(crime_time_2, crime_time_1) #rebind DFs with cleaned times

#elapsed

crime_fixed$ReportedDateTime <- ymd_hms(str_c(crime_fixed$ReportedDate, crime_fixed$ReportedTime, sep = " ")) #column of reported datetime

crime_fixed$DateTimeOffense <- ymd_hms(str_c(crime_fixed$ReportedDate, crime_fixed$Time.of.Offense, sep = " ")) #column of offense datetime

crime_fixed$MinsElapsed <- difftime(crime_fixed$ReportedDateTime,crime_fixed$DateTimeOffense, units = "mins") #elapsed time

#if resonse was next day, add 24 hours to negative values 

crime_fixed$MinsElapsed <- ifelse(crime_fixed$MinsElapsed>0,crime_fixed$MinsElapsed, crime_fixed$MinsElapsed+1440) #add 1440 (one day) to neg. values

crime_fixed$MinsElapsed <- as.integer(crime_fixed$MinsElapsed) #round minutes

crime_final <- crime_fixed[c(2,6,7,8,9,10,11,5,13)] #subset wanted columns

crime_final <- unique(crime_final[, 1:9]) #remove duplicates

write.csv(crime_final, file = "Crime_Cleaned2.csv") #export csv


