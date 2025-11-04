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

# Bar chart for no. students by school & higher education plan
higher_education_chart(df)


# Histogram chart for distribution of grades accross Q1,Q2, & Q3
grade_distribution(df)

# Box plot for correlation between family relationship & student absences 
famrel_box(df)
study_box(df)
