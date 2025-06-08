

library(ggplot2)
library(broom)
library(tidyr)
library(emmeans)
library(dplyr)
library(multcomp)
library(multcompView)
library(reshape2)

attach(Thermotolerance)
dg<-Thermotolerance

str(dg)

dx_long <- dg %>% pivot_longer(cols = c(Combined, Ngoye, PK10), names_to = "Case", values_to = "Value")
summary_data_1 <- dx_long %>% group_by(Temp, Condition, Case) %>%
  summarize(Mean = mean(Value), SEM = sd(Value) / sqrt(n()), .groups = "drop")

# Average hatching at high temps (e.g., 41 & 45°C)
thermo_summary <- dx_long %>%
  filter(Temp %in% c(41, 45)) %>%
  group_by(Condition, Case) %>%
  summarise(Avg_Hatch = mean(Value), .groups = "drop")

thermo_summary

thermo_summary_modified <- thermo_summary %>%
  mutate(Condition = case_when(
    Condition == "Wet" ~ "W",
    Condition == "Dry" ~ "D",
    TRUE ~ Condition  # Keep other values as they are (though there shouldn't be any in this case)
  ))

print(thermo_summary_modified)

attach(EggCount)

ds <- EggCount%>%
  filter(day != 8) %>%
  mutate(
    eggs_per_alive = count / alive,
    main_condition = gsub("_[0-9]+$", "", condition),
    main_condition = factor(main_condition, levels = c("W", "D", "WD", "DW"))
  )

egg_summary <- ds %>%
  group_by(main_condition, Line) %>%
  summarise(Avg_EggsPerAlive = mean(eggs_per_alive), .groups = "drop")

egg_summary


# Step 1: Rename 'Condition' in thermo_summary_modified to match 'main_condition'
thermo_summary_modified <- thermo_summary_modified %>%
  rename(main_condition = Condition)

# Step 2: Perform the inner join using both matching columns
mei_data <- inner_join(egg_summary, thermo_summary_modified,
                       by = c("main_condition", "Line" = "Case"))

# Step 3: Calculate the Maternal Efficiency Index (MEI)
mei_data <- mei_data %>%
  mutate(MEI = Avg_EggsPerAlive * Avg_Hatch)

# Step 4: View the final MEI table
print(mei_data)

library(ggplot2)

ggplot(mei_data, aes(x = Line, y = MEI, fill = main_condition)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(title = "Maternal Efficiency Index (MEI)",
       x = "Line (Population)",
       y = "MEI = Eggs per Alive × Hatch Rate",
       fill = "Condition") +
  theme_minimal() +
  theme(text = element_text(size = 12),
        plot.title = element_text(face = "bold", hjust = 0.5))


# Step 5: Normalize MEI to 0–1 scale
mei_data <- mei_data %>%
  mutate(MEI_scaled = (MEI - min(MEI)) / (max(MEI) - min(MEI)))

# Optional: View the scaled MEI
print(mei_data)

# Step 6: Plot the scaled MEI
ggplot(mei_data, aes(x = Line , y = MEI_scaled, fill = main_condition)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(title = "Scaled Maternal Efficiency Index (MEI)", 
       x = "Line (Population)", 
       y = "Scaled MEI (0–1)", 
       fill = "Condition") +
  theme_minimal() +
  theme(text = element_text(size = 12),
        plot.title = element_text(face = "bold", hjust = 0.5))

mei_data$main_condition <- factor(mei_data$main_condition, levels = c("W", "D", "WD", "DW"))
custom_colors <- c("Ngoye" = "#FB6A4A","PK10" = "#08306B","Combined" = "#9B4E7E")

ggplot(mei_data, aes(x = main_condition , y = MEI_scaled, fill = Line)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(title = "Transgenerational Efficiency Index (TEI)", 
       x = "Condition", 
       y = "TEI = Eggs per Alive × Egg-thermotolerance", 
       fill = "Condition") +
  theme_minimal() +
  theme(text = element_text(size = 14),
        plot.title = element_text(face = "bold", hjust = 0.5)) +
  scale_fill_manual(values = custom_colors)

mei_data

anova_result <- aov(MEI ~ Line * main_condition, data = mei_data)
summary(anova_result)
TukeyHSD(anova_result)


ggplot(mei_data, aes(x = Line, y = main_condition, fill = MEI_scaled)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "C") +
  labs(title = "Normalized Maternal Efficiency Index (MEI)", 
       x = "Line", y = "Condition", fill = "Scaled MEI") +
  theme_minimal()

ggplot(mei_data, aes(x = Avg_EggsPerAlive, y = Avg_Hatch, color = Line, shape = main_condition)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Egg Output vs Hatch Success",
       x = "Eggs per Alive Female", y = "Avg Hatch Rate") +
  theme_minimal()


