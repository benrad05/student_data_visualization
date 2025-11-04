rm(list=ls())
library(ggplot2) 

# 2. Does the distribution of the scores change from the first quarter of the semester to the final? 
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
    )
}

#3. 