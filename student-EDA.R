# Intro to Data Science - Assignment 1
# 10/25/2025
# Daniel Toro and Ben Radoslovich 

rm(list=ls())
source("./functions.R")

# Read csv and create data frame
readLines("data/student-mat.csv")
df <- read.csv("data/student-mat.csv",
               sep = ";",         
               dec = ".",         
               header = TRUE,    
               stringsAsFactors = TRUE)


grade_distribution(df)
grade_distribution_facet(df)
