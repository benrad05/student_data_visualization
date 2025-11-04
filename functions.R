rm(list=ls())
library(ggplot2) 

# 1. What percentage of students from each school 
#    want to pursue higher education? 
higher_education_chart = function(df) {
  
  ggplot(data = df, aes(x = higher, fill = higher)) +
    geom_bar() +
    facet_wrap(~ school, scales = "free_y",
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
                                hjust = 0.5),
      
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
      x = "Grade",
      y = "Number of Students"
    )
}
