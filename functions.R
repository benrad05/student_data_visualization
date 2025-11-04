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
grade_distribution <- function(df) {
  ggplot() +
    # Quarter 1 histogram (upward)
    geom_histogram(
      data = df,
      aes(x = G1, y = after_stat(count), fill = "Quarter 1 Grades"),
      bins = 20,
      color = "black",
      alpha = 0.8
    ) +
    # Final Grades histogram (mirrored downward)
    geom_histogram(
      data = df,
      aes(x = G3, y = -after_stat(count), fill = "Final Grades"),
      bins = 20,
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
    theme_minimal(base_size=12) +
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
      
      
      #Remove grid lines
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
}

#3a. Does quality of family relationship correlate with a student's absences?
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
#3b.
study_box <- function(df){
  ggplot(df, aes(x = factor(studytime), y = G3)) +
    geom_boxplot(
      fill = "#4CAF50",
      alpha = 0.8,
      outlier.shape = NA
    ) +
    scale_x_discrete(
      labels = c(
        "1" = "<2 hours",
        "2" = "2-5 hours",
        "3" = "5-10 hours",
        "4" = ">10 hours"
      )
    ) +
    labs(
      title = "Final Grade by Study Time",
      x = "Study Time",
      y = "Final Grade"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      # Title Styling
      plot.title = element_text(face = "bold", 
                                hjust = 0.5),
      panel.grid.major.x = element_blank(),  # remove major grid lines
      panel.grid.minor = element_blank()   # remove minor grid lines
    )
  
  
  
  
}

