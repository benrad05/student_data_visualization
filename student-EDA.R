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
#   Scatter plots--> absences vs. numeric vars


ggplot(df, aes(x = age, y = absences)) +
  geom_point() +
  labs(
    title = "Age vs Absences",
    x = "Age",
    y = "Number of Absences"
  )


######      Avg#Absences for every age Bar Chart #################### Doesn't appear to be big pattern 
ages <- sort(unique(df$age))
mean_absences <- numeric(length(ages))  # empty vector
for(i in 1:length(ages)) {
  mean_absences[i] <- mean(df$absences[df$age == ages[i]])
}
avg_df <- data.frame(age = ages, mean_absences = mean_absences)
ggplot(data = avg_df, aes(x = factor(age), y = mean_absences)) +
  geom_col(fill = "steelblue") +
  labs(title = "Average Absences by Age", x = "Age (years)", y = "Average Number of Absences")


######      Avg#Absences for every Mother education Bar Chart #################### 
Medus <- sort(unique(df$Medu))
mean_absences <- numeric(length(Medus))  # empty vector
for(i in 1:length(Medus)) {
  mean_absences[i] <- mean(df$absences[df$Medu == Medus[i]])
}
avg_df <- data.frame(Medu = Medus, mean_absences = mean_absences)
ggplot(data = avg_df, aes(x = factor(Medu), y = mean_absences)) +
  geom_col(fill = "steelblue") +
  labs(title = "Average Absences by Mother Education level", x = "Mother Education level", y = "Average Number of Absences")

######      Avg#Absences for every Father education Bar Chart ####################
Fedus <- sort(unique(df$Fedu))
mean_absences <- numeric(length(Fedus))  # empty vector
for(i in 1:length(Fedus)) {
  mean_absences[i] <- mean(df$absences[df$Fedu == Fedus[i]])
}
avg_df <- data.frame(Fedu = Fedus, mean_absences = mean_absences)
ggplot(data = avg_df, aes(x = factor(Fedu), y = mean_absences)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Average Absences by Father Education Level",
    x = "Father Education Level",
    y = "Average Number of Absences"
  )

######      Avg#Absences for every Travel Time Bar Chart ####################
traveltimes <- sort(unique(df$traveltime))
mean_absences <- numeric(length(traveltimes))
for(i in 1:length(traveltimes)) {
  mean_absences[i] <- mean(df$absences[df$traveltime == traveltimes[i]])
}
avg_df <- data.frame(traveltime = traveltimes, mean_absences = mean_absences)
ggplot(data = avg_df, aes(x = factor(traveltime), y = mean_absences)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Average Absences by Travel Time",
    x = "Travel Time (1=<15min, 2=15-30min, 3=30-60min, 4=>1h)",
    y = "Average Number of Absences"
  )

######      Avg#Absences for every StudyTime Bar Chart #################### As study time increases absences decrease
studytimes <- sort(unique(df$studytime))
mean_absences <- numeric(length(studytimes))
for(i in 1:length(studytimes)) {
  mean_absences[i] <- mean(df$absences[df$studytime == studytimes[i]])
}
avg_df <- data.frame(studytime = studytimes, mean_absences = mean_absences)
ggplot(data = avg_df, aes(x = factor(studytime), y = mean_absences)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Average Absences by Weekly Study Time",
    x = "Weekly Study Time (1=<2h, 2=2-5h, 3=5-10h, 4=>10h)",
    y = "Average Number of Absences"
  )

######      Avg#Absences for every Past Failures Bar Chart ####################
failures_levels <- sort(unique(df$failures))
mean_absences <- numeric(length(failures_levels))
for(i in 1:length(failures_levels)) {
  mean_absences[i] <- mean(df$absences[df$failures == failures_levels[i]])
}
avg_df <- data.frame(failures = failures_levels, mean_absences = mean_absences)
ggplot(data = avg_df, aes(x = factor(failures), y = mean_absences)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Average Absences by Number of Past Failures",
    x = "Number of Past Failures",
    y = "Average Number of Absences"
  )

######      Avg#Absences for every FamRel Bar Chart ######### Absences decrease as quality increases
famrel_levels <- sort(unique(df$famrel))
mean_absences <- numeric(length(famrel_levels))
for(i in 1:length(famrel_levels)) {
  mean_absences[i] <- mean(df$absences[df$famrel == famrel_levels[i]])
}
avg_df <- data.frame(famrel = famrel_levels, mean_absences = mean_absences)
ggplot(data = avg_df, aes(x = factor(famrel), y = mean_absences)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Average Absences by Family Relationship Quality",
    x = "Family Relationship Quality (1=very bad, 5=excellent)",
    y = "Average Number of Absences"
  )


######      Avg#Absences for every Freetime Bar Chart ######### Very low free time more absences 
freetime_levels <- sort(unique(df$freetime))
mean_absences <- numeric(length(freetime_levels))
for(i in 1:length(freetime_levels)) {
  mean_absences[i] <- mean(df$absences[df$freetime == freetime_levels[i]])
}
avg_df <- data.frame(freetime = freetime_levels, mean_absences = mean_absences)
ggplot(data = avg_df, aes(x = factor(freetime), y = mean_absences)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Average Absences by Free Time After School",
    x = "Free Time (1=very low, 5=very high)",
    y = "Average Number of Absences"
  )

######      Avg#Absences for every Go out level Bar Chart ######### Take a second look
goout_levels <- sort(unique(df$goout))
mean_absences <- numeric(length(goout_levels))
for(i in 1:length(goout_levels)) {
  mean_absences[i] <- mean(df$absences[df$goout == goout_levels[i]])
}
avg_df <- data.frame(goout = goout_levels, mean_absences = mean_absences)
ggplot(data = avg_df, aes(x = factor(goout), y = mean_absences)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Average Absences by Going Out with Friends",
    x = "Going Out Frequency (1=very low, 5=very high)",
    y = "Average Number of Absences"
  )

######      Avg#Absences for every weekday alc level Bar Chart ######### Average increases with consumption
Dalc_levels <- sort(unique(df$Dalc))
mean_absences <- numeric(length(Dalc_levels))
for(i in 1:length(Dalc_levels)) {
  mean_absences[i] <- mean(df$absences[df$Dalc == Dalc_levels[i]])
}
avg_df <- data.frame(Dalc = Dalc_levels, mean_absences = mean_absences)
ggplot(data = avg_df, aes(x = factor(Dalc), y = mean_absences)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Average Absences by Workday Alcohol Consumption",
    x = "Workday Alcohol Consumption (1=very low, 5=very high)",
    y = "Average Number of Absences"
  )

######      Avg#Absences for every weekend alc level Bar Chart ######### Average increases with consumption
Walc_levels <- sort(unique(df$Walc))
mean_absences <- numeric(length(Walc_levels))
for(i in 1:length(Walc_levels)) {
  mean_absences[i] <- mean(df$absences[df$Walc == Walc_levels[i]])
}
avg_df <- data.frame(Walc = Walc_levels, mean_absences = mean_absences)
ggplot(data = avg_df, aes(x = factor(Walc), y = mean_absences)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Average Absences by Weekend Alcohol Consumption",
    x = "Weekend Alcohol Consumption (1=very low, 5=very high)",
    y = "Average Number of Absences"
  )

######      Avg#Absences for every health level Bar Chart ######### Decreases slightly as health improves
health_levels <- sort(unique(df$health))
mean_absences <- numeric(length(health_levels))
for(i in 1:length(health_levels)) {
  mean_absences[i] <- mean(df$absences[df$health == health_levels[i]])
}
avg_df <- data.frame(health = health_levels, mean_absences = mean_absences)
ggplot(data = avg_df, aes(x = factor(health), y = mean_absences)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Average Absences by Current Health Status",
    x = "Health Status (1=very bad, 5=very good)",
    y = "Average Number of Absences"
  )




