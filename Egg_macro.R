library(ggplot2)
library(gridExtra)
library(dplyr)
attach(Egg_macro_copy)

#Sheet-Egg_Fianl_Plot
dj<- Egg_macro_copy
str(dj)

unique(dj$Condition)

# Function to create box plots with error bars and specific Y-axis limits
create_boxplot <- function(data, y_var, title, y_limits) {
  # Calculate summary statistics for error bars
  summary_stats <- data %>%
    group_by(Condition) %>%
    summarise(
      Mean = mean(.data[[y_var]], na.rm = TRUE),
      SD = sd(.data[[y_var]], na.rm = TRUE),
      .groups = "drop"
    )
  
  # Define custom factor levels for the Condition variable to order the x-axis
  data$Condition <- factor(data$Condition, levels = c("WET", "DRY", "WD", "DW"))
  
  # Create the box plot
  ggplot(data, aes(x = Condition, y = .data[[y_var]], fill = Condition)) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    geom_errorbar(
      data = summary_stats,
      aes(x = Condition, ymin = Mean - SD, ymax = Mean + SD),
      width = 0.2,
      linewidth = 0.8,
      inherit.aes = FALSE
    ) +
    #facet_wrap(~Line, scales = "free_y") +
    scale_fill_brewer(palette = "Set3") +  # Using Set3 color palette
    labs(title = title, x = "Condition", y = y_var) +
    theme_classic() +  # Using classic theme
    theme(
      text = element_text(size = 14),
      legend.position = "none",
      strip.text = element_text(face = "bold")
    ) +
    ylim(y_limits)  # Set Y-axis limits
}

# Create the plots with specific Y-axis ranges
glycogen_plot <- create_boxplot(dj, "GLY_merged", "Glycogen Levels Across Conditions", c(0, 10))
protein_plot <- create_boxplot(dj, "Protein", "Protein Levels Across Conditions", c(0, 55))
lipid_plot <- create_boxplot(dj, "Lipid", "Lipid Levels Across Conditions", c(0, 20))

# Display the plots
print(glycogen_plot)
print(protein_plot)
print(lipid_plot)

grid.arrange(glycogen_plot, protein_plot, lipid_plot, ncol = 3)

result <- dj %>%
  group_by(Condition) %>%
  summarise(
    Avg_Glycogen = mean(GLY_merged, na.rm = TRUE),
    Avg_Protein = mean(Protein, na.rm = TRUE),
    Avg_Lipid = mean(Lipid, na.rm = TRUE)
  )

# View the result
print(result)

# One-way ANOVA for each macronutrient
aov_gly <- aov(GLY_merged ~ Condition, data = dj)
summary(aov_gly)

aov_prot <- aov(Protein ~ Condition, data = dj)
summary(aov_prot)

aov_lip <- aov(Lipid ~ Condition, data = dj)
summary(aov_lip)

TukeyHSD(aov_gly)
TukeyHSD(aov_prot)
TukeyHSD(aov_lip)








