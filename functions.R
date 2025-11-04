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
    # Distribution of grades after Q1
    geom_histogram(aes(x = G1,
                       fill = "Quarter 1 Grades"),
                   bins = 20,
                   color= "black", 
                   alpha = 0.3)+
    # Distribution of final grades 
    geom_histogram(aes(x = G3, 
                   fill = "Final Grades"), 
                   bins = 20,
                   color="black",
                   alpha = 0.3)+
    scale_fill_brewer(
      palette = "Set2",          # Use the Set2 palette
      labels = c("Quarter 1 Grades", "Final Grades")
    ) +
    #scale_fill_manual(
      #values = c("Final Grades" = "darkgreen", "Quarter 1 Grades" = "darkblue"))+
    labs(
      title = "Distribution of Midterm Grades vs Final Grades",
      x = "Number of Students",
      y = "Grade",
      fill = "Grade Type"
    ) +
    theme_minimal()
  
}
grade_distribution_facet <- function(df) {
  library(ggplot2)
  
  ggplot() +
    # Quarter 1 histogram (upward)
    geom_histogram(
      data = df,
      aes(x = G1, y = after_stat(count), fill = "Quarter 1 Grades"),
      bins = 20,
      color = "black",
      alpha = 0.4
    ) +
    # Final Grades histogram (mirrored downward)
    geom_histogram(
      data = df,
      aes(x = G3, y = -after_stat(count), fill = "Final Grades"),
      bins = 20,
      color = "black",
      alpha = 0.4
    ) +
    # Mirror axis
    scale_y_continuous(labels = abs) +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title = "Distribution of Midterm vs Final Grades",
      x = "Grade",
      y = "Number of Students",
      fill = "Grade Type"
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
      title = "Distribution of Grades per Quarter",
      x = "Grade",
      y = "Number of Students"
    )
}

#3. 