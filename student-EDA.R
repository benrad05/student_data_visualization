# Intro to Data Science - Assignment 1
# 10/25/2025
# Daniel Toro and Ben Radoslovich 

rm(list=ls())

library(ggplot2) 

readLines("data/student-mat.csv")

df <- read.csv("data/student-mat.csv",
               sep = ";",         # semicolon separates fields
               dec = ".",         # decimal point
               header = TRUE,     # first row contains column names
               stringsAsFactors = TRUE)  # categorical columns as factors

# Quick check
head(df)
str(df)


#3 
#What feature most strongly correlates with a student's absences? (Ben)
#   - Heatmap / Bar Chart --> absences vs. numeric vars
