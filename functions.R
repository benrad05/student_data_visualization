rm(list=ls())
library(ggplot2) 

# 2. Does the distribution of the scores change from the first quarter of the semester to the final? 
grade_distribution = function(df) {
  ggplot(data=df) +
    # Distribution of final grades 
    geom_histogram(aes(x = G3), 
                   fill = 'red', 
                   bins = 20, 
                   color = "red", 
                   alpha = 0.3)+
    
    # Distribution of grades after Q1
    geom_histogram(aes(x = G1),
                   fill = 'blue',
                   bins = 20,
                   color = "blue",
                   alpha = 0.3)+
    labs(
      title = "Distribution of Grades per Quarter",
      x = "Number of Students",
      y = "Grade"
    )
}
