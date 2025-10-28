# Intro to Data Science - Assignment 1
# 10/25/2025
# Daniel Toro and Ben Radoslovich 

rm(list=ls())
library(ggplot2) 

# Read csv and create data frame
readLines("data/student-mat.csv")
df <- read.csv("data/student-mat.csv",
               sep = ";",         
               dec = ".",         
               header = TRUE,    
               stringsAsFactors = TRUE)


ggplot(data=df) +
  geom_histogram(aes(x= G3), fill = 'red', bins = 20, color = "red", alpha = 0.3)+
  geom_histogram(aes(x= G1), fill = 'blue', bins = 20, color = "blue", alpha = 0.3)+
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  annotate("text", x=2, y=45, label = "failed students", color= "red")

