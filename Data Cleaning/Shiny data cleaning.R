library(dplyr)
library(readr)
library(stringr)
library(data.table)
library(microbenchmark)
library(lubridate)
library(ggplot2)
library(tidyr)

setwd("C:/Users/Johnny/Documents/UNH Notes and Documents 2018/Fall 2018/Data 901 R/Shiny Assignment")

overdose <- read.csv("Multiple Cause of Death, 1999-2014 v1.1.csv")

#subset wanted columns
overdose <- overdose[c(1,2,3,4,5,8)] 

#remove incomplete data
overdose <- overdose %>%
  filter(Deaths != "Suppressed")

overdose <- overdose %>%
  filter(!Crude.Rate %in% c('Unreliable','Suppressed'))


#rename columns
colnames(overdose)[6] = "Prescriptions Per Million"

colnames(overdose)[5] = "Deaths Per 100000"

#total prescriptions 
overdose$`Number of Prescriptions` <- (overdose$`Prescriptions Per Million`*1000000)

#export csv
write.csv(overdose, file = "Overdose_Cleaned.csv") 


#sample plots for Shiny App

alabama <- overdose %>%
  filter(State == "Alabama")

ggplot(alabama, aes(Year, Population)) + geom_bar()

ggplot(alabama, aes(as.character(Year), Deaths, fill = Deaths)) + 
  geom_bar(stat="identity", position="dodge") + ggtitle("Overdose Deaths Per Year") + xlab("Year") + ylab("Deaths")


grouped <- overdose %>% 
  group_by(Year) %>% 
  summarise(Deaths = sum(Deaths.Per.100.000), Prescriptions = sum(Prescriptions.Per.Million)) 

state <- overdose %>%
  group_by(State) %>%
  summarise(Deaths = sum(Deaths)) %>%
  arrange(desc(Deaths))



grouped <- grouped %>% mutate_each_(funs(scale(.) %>% as.vector), 
                             vars=c("Deaths","Prescriptions"))
breaks1 = 1999:2014

z <- ggplot(grouped, aes(Year)) + 
  geom_line(aes(y = Deaths, colour = "Deaths")) + 
  geom_line(aes(y = Prescriptions, colour = "Prescriptions")) +
  ggtitle("Scaled Overdoses Deaths and Opiate Prescriptons") + xlab("Year") + ylab("Scaled Values") 
z <- ggplotly(z)
z

