# Intro to Data Science - Assignment 1
# 10/25/2025
# Daniel Toro and Ben Radoslovich 
rm(list=ls())
library(ggplot2) 

# 1. What percentage of students from each school 
#    want to pursue higher education? 
higher_education_chart = function(df) {
  ggplot(data = df, aes(x = higher, fill = higher)) +
    geom_bar() +
    facet_wrap(~ school, 
               scales = "free_y",
               labeller = labeller(school = c(GP = "Gabriel Pereira", 
                                              MS = "Mousinho da Silveira"))) +
    labs(
      title = "Number of Students by School and Higher Education Plan",
      x = "Higher Education Plan",
      y = "Number of Students",
      fill = "Higher Education"
    ) +
    scale_fill_manual(
      values = c("yes" = "#4CAF50",  
                 "no"  = "#E74C3C") 
    ) +
    theme_minimal(base_size = 12) +
    theme(
      # Title Styling
      plot.title = element_text(face = "bold", 
                                hjust = 0.5,
                                margin = margin(t = 5, b = 5)),
      
      # Legend Styling
      legend.position = "top",
      legend.direction = "horizontal",
      legend.title = element_text(face = "bold"),
      legend.box.just = "center",
      
      # Divider Styling 
      panel.spacing = unit(2, "lines"),      
      panel.border = element_rect(           
        color = "grey40", 
        fill = NA, 
        linewidth = 0.5
      )     
    )
  
}


# 2. Does the distribution of the scores change from the first quarter 
#    of the semester to the final? 
grade_distribution <- function(df) {
  ggplot() +
    # Quarter 1 histogram (upward)
    geom_histogram(
      data = df,
      aes(x = G1, y = after_stat(count), fill = "Quarter 1 Grades"),
      breaks = seq(0, 22, by = 2),   # bins: 0–2, 2–4, ..., 20–22
      color = "black",
      alpha = 0.8
    ) +
    # Final Grades histogram (mirrored downward)
    geom_histogram(
      data = df,
      aes(x = G3, y = -after_stat(count), fill = "Final Grades"),
      breaks = seq(0, 22, by = 2),   # same bins for G3
      color = "black",
      alpha = 0.8
    ) +
    # Mirror axis
    scale_y_continuous(labels = abs) +
    scale_fill_manual(
      values = c("Quarter 1 Grades"  = "#E74C3C",
                 "Final Grades" = "#4CAF50") 
    ) +
    labs(
      title = "Distribution of Midterm vs Final Grades",
      x = "Grade",
      y = "Number of Students",
      fill = "Grade Type"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      # Title Styling
      plot.title = element_text(face = "bold", 
                                hjust = 0.5),
      # Legend Styling
      legend.position = "right",
      legend.direction = "vertical",
      legend.title = element_text(face = "bold"),
      legend.box.just = "center",
      
      legend.background = element_rect(
        fill = "white",       # background color
        color = "black",      # border color
        size = 0.5,           # border thickness
        linetype = "solid"    # border style
      ),
      
      # Remove grid lines
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
}


# 3. Does quality of family relationship correlate with a student's absences?
#- Box Plot 
famrel_box <- function(df){
  ggplot(df, aes(x = factor(famrel), y = absences)) +
    geom_boxplot(
      fill = "#4CAF50",
      alpha = 0.8,
      outlier.shape = NA
    ) +
    scale_x_discrete(
      labels = c(
        "1" = "Very Bad",
        "2" = "Below Average",
        "3" = "Average",
        "4" = "Above Average",
        "5" = "Excellent"
      )
    ) +
    coord_cartesian(ylim = c(0, 20)) +  # clip visually without removing rows
    scale_y_continuous(
      breaks = seq(0, 20, by = 5)
    ) +
    labs(
      title = "Absences by Family Relationship Quality",
      x = "Family Relationship Quality",
      y = "Number of Absences"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      # Title Styling
      plot.title = element_text(face = "bold", 
                                hjust = 0.5),
      panel.grid.major = element_blank(),  # remove major grid lines
      panel.grid.minor = element_blank()   # remove minor grid lines
    )
}


# 4. Are the number of previously failed classes associated with a bad academic performance?
failures_grade_correlation = function(df) {
  ggplot(data = df, 
         aes(x = failures, y = G3)) +
    geom_jitter(color = "#2E86C1",
                size = 2.5,
                alpha = 0.5,
                width = 0.4,
                height = 0.4) +
    
    # Add linear regression line
    geom_smooth(method = "lm", 
                color = "#E74C3C",
                se=FALSE,
                size = 1.2) +
    
    labs(
      title = "Final grade of students with previously failed courses",
      y = "Final Grade",
      x = "Number of previously failed classes"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", 
                                hjust = 0.5,
                                margin = margin(t = 5, b = 5))
    )
}


# 5. Does higher study time leader to a higher grade 
study_summary <- function(df) {
  # Count of students per studytime
  counts <- table(df$studytime)
  
  # Proportion of students per category
  props <- prop.table(counts)
  
  # Mean final grade by studytime
  mean_G3 = tapply(df$G3, df$studytime, mean)
  # Standard deviation
  sd_G3 = tapply(df$G3, df$studytime, sd)
  
  # Median
  median_G3 = tapply(df$G3, df$studytime, median)
  
  # Min and Max
  min_G3 = tapply(df$G3, df$studytime, min)
  max_G3 = tapply(df$G3, df$studytime, max)
  
  # Combine into a data frame
  summary_df <- data.frame(
    Study_Time = c("<2 Hours", "2-5 Hours", "5-10 Hours", ">10 Hours"),
    Students = as.numeric(counts),
    Percent = round(100 * as.numeric(props), 1),
    Mean_Grade = round(as.numeric(mean_G3), 1),
    Std_Dev = round(as.numeric(sd_G3), 1),
    Median = round(as.numeric(median_G3), 1),
    Min = as.numeric(min_G3),
    Max = as.numeric(max_G3)
  )
  
  return(summary_df)
}


# Read csv and create data frame
readLines("data/student-mat.csv")
df <- read.csv("data/student-mat.csv",
               sep = ";",         
               dec = ".",         
               header = TRUE,    
               stringsAsFactors = TRUE)

higher_education_chart(df)
grade_distribution(df)
famrel_box(df)
failures_grade_correlation(df) 
study_summary(df)
