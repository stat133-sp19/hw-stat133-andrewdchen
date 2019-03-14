##################################################
## Title: Make Shots Data Script
## Description:
## Inputs:
## Outpus:
##################################################

library(utils)
library(dplyr)

#Read data into R
coltypes <- c("character", "character", "numeric", "numeric", "numeric", "numeric", "character", "character", "character", "numeric", "character", "numeric", "numeric")
curry <- read.csv("../data/stephen-curry.csv", header = T, stringsAsFactors = F, colClasses = coltypes)
green <- read.csv("../data/draymond-green.csv", header = T, stringsAsFactors = F, colClasses = coltypes)
iguodala <- read.csv("../data/andre-iguodala.csv", header = T, stringsAsFactors = F, colClasses = coltypes)
durant <- read.csv("../data/kevin-durant.csv", header = T, stringsAsFactors = F, colClasses = coltypes)
thompson <- read.csv("../data/klay-thompson.csv", header = T, stringsAsFactors = F, colClasses = coltypes)

#Add name column
curry <- curry %>% mutate(name = "Stephen Curry")
green <- green %>% mutate(name = "Draymond Green")
iguodala <- iguodala %>% mutate(name = "Andre Iguodala")
durant <- durant %>% mutate(name = "Kevin Durant")
thompson <- thompson %>% mutate(name = "Klay Thompson")

#Modify shot made flag values
curry$shot_made_flag[curry$shot_made_flag == "y"] = "shot_yes"
curry$shot_made_flag[curry$shot_made_flag == "n"] = "shot_no"
durant$shot_made_flag[durant$shot_made_flag == "y"] = "shot_yes"
durant$shot_made_flag[durant$shot_made_flag == "n"] = "shot_no"
thompson$shot_made_flag[thompson$shot_made_flag == "y"] = "shot_yes"
thompson$shot_made_flag[thompson$shot_made_flag == "n"] = "shot_no"
iguodala$shot_made_flag[iguodala$shot_made_flag == "y"] = "shot_yes"
iguodala$shot_made_flag[iguodala$shot_made_flag == "n"] = "shot_no"
green$shot_made_flag[green$shot_made_flag == "y"] = "shot_yes"
green$shot_made_flag[green$shot_made_flag == "n"] = "shot_no"

#Add minute column
curry <- curry %>% mutate(minute = 12*period - minutes_remaining)
durant <- durant %>% mutate(minute = 12*period - minutes_remaining)
green <- curry %>% mutate(minute = 12*period - minutes_remaining)
thompson <- thompson %>% mutate(minute = 12*period - minutes_remaining)
iguodala <- iguodala %>% mutate(minute = 12*period - minutes_remaining)

#Exporting Data
sink(file = '../output/stephen-curry-summary.txt')
summary(curry)
sink()
sink(file = '../output/andre-iguodala-summary.txt')
summary(iguodala)
sink()
sink(file = '../output/klay-thompson-summary.txt')
summary(thompson)
sink()
sink(file = '../output/draymond-green-summary.txt')
summary(green)
sink()
sink(file = '../output/kevin-durant-summary.txt')
summary(durant)
sink()

#Combining all dataframes into one
combined <- rbind(curry, durant, thompson, green, iguodala)
write.csv(
  x = combined,
  file = '../data/shots-data.csv'
)

sink(file = '../output/shots-data-summary.txt')
summary(combined)
sink()
