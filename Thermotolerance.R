attach(Thermotolerance)
dg<-Thermotolerance
str(dg)
unique(dg$Condition)
library(ggplot2)
library(broom)
library(tidyr)
library(emmeans)
library(dplyr)
library(multcomp)
library(multcompView)


dx_long <- dg %>% pivot_longer(cols = c(Combined, Ngoye, PK10), names_to = "Line", values_to = "Value")
summary_data_1 <- dx_long %>% group_by(Temp, Condition, Line) %>%
  summarize(Mean = mean(Value), SEM = sd(Value) / sqrt(n()), .groups = "drop")
#View(summary_data_1)
write_xlsx(summary_data_1, "summary_data_1.xlsx")
getwd()
str(summary_data_1)
summary_data_1$Condition <- factor(summary_data_1$Condition, levels = c("Wet", "Dry", "WD", "DW"))
custom_colors <- c("Ngoye" = "#FB6A4A","PK10" = "#08306B","Combined" = "#9B4E7E")

ggplot(summary_data_1, aes(x = factor(Condition), y = Mean, fill = Line)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black", alpha = 0.8,
           aes(pattern = Line)) + # Add pattern mapping
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM),
                position = position_dodge(width = 0.9), width = 0.2, color = "black") +
  labs(
    title = "Average Egg Hatching by Temperature, Condition, and Population",
    x = "Condition",
    y = "Average % Egg Hatching",
    fill = "Line",
    pattern = "Line" # Add pattern label
  ) +
  theme_classic() +
  theme(
    legend.position = "right",
    text = element_text(size = 15)
  ) +
  scale_fill_manual(values = custom_colors) + # Apply shades
  facet_wrap(~ Temp, ncol = 4) # Facet by temperature

dx_long$Condition <- factor(dx_long$Condition, levels = c("Wet", "Dry", "WD", "DW"))
custom_colors <- c("Ngoye" = "#FB6A4A","PK10" = "#08306B","Combined" = "#9B4E7E")

ggplot(dx_long, aes(x = Condition, y = Value, fill = Line)) +
  geom_boxplot(position = position_dodge(width = 1), alpha = 0.85, outlier.shape = NA) +
  geom_jitter(aes(color = Line), position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8), size = 1, alpha = 0.9) +
  scale_fill_manual(values = custom_colors) +
  scale_color_manual(values = custom_colors) +
  labs(
    title = "Egg Hatching Distribution by Condition and Line",
    x = "Condition",
    y = "% Egg Hatching",
    fill = "Line",
    color = "Line"
  ) +
  theme_classic() +
  theme(
    legend.position = "right",
    text = element_text(size = 14)
  ) +
  facet_wrap(~ Temp, ncol = 4)

summary(model <- aov(Value ~ Condition, data = dx_long))
summary(model <- aov(Value ~ Condition*Line, data = dx_long))
results_by_temp <- dx_long %>% group_by(Temp) %>% do(tidy(aov(Value ~ Condition, data = .)))
print(results_by_temp)

#1)
#write_xlsx(results_by_temp, path = "1-results_by_temp.xlsx")


tukey_results <- dx_long %>% group_by(Temp) %>%
  do({model <- aov(Value ~ Condition, data = .)
    tidy(TukeyHSD(model))
  })

print(tukey_results)

#2)
#write_xlsx(tukey_results, path = "Tukey_PostHoc_Results2.xlsx")


ggplot(dx_long, aes(x = Condition, y = Value, fill = Condition)) +
  geom_boxplot() +
  facet_wrap(~ Temp, nrow = 1) +
  theme_classic() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Egg hatching across Conditions at each Temperature",
       y = "% Egg Hatching")


get_cld <- function(df) {
  model <- aov(Value ~ Condition, data = df)
  tukey <- TukeyHSD(model)
  cld <- multcompLetters4(model, tukey)
  
  # Get the CLD letters as a data.frame
  cld_df <- data.frame(
    Condition = names(cld$Condition$Letters),
    Letter = cld$Condition$Letters,
    stringsAsFactors = FALSE
  )
  
  cld_df$Temp <- unique(df$Temp)  # Add temp info
  return(cld_df)
}

# Apply per temperature
cld_all <- dx_long %>% group_by(Temp) %>% group_split() %>% lapply(get_cld) %>% bind_rows()
print(cld_all)


# Combine CLD letters with your summary data
summary_with_letters <- summary_data_1 %>%
  left_join(cld_all, by = c("Condition", "Temp"))

summary_with_letters$Condition <- factor(summary_with_letters$Condition, levels = c("Wet", "Dry", "WD", "DW"))

# Make the plot with letters added
ggplot(summary_with_letters, aes(x = factor(Condition), y = Mean, fill = Line)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9),
           color = "black", alpha = 0.8, aes(pattern = Line)) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM),
                position = position_dodge(width = 0.9), width = 0.2, color = "black") +
  
  # 3. Add the CLD letters above bars
  geom_text(aes(label = Letter, y = Mean + SEM + 0.03),  # Adjust offset as needed
            position = position_dodge(width = 0.9),
            vjust = 1.5, size = 4, color = "black", fontface = "bold") +
  
  labs(
    title = "Average Thermotolerance by Temperature, Condition, and Population",
    x = "Condition",
    y = "Average % Egg Hatching",
    fill = "Line",
    pattern = "Line"
  ) +
  theme_classic() +
  theme(
    legend.position = "right",
    text = element_text(size = 15)
  ) +
  scale_fill_manual(values = custom_colors) +
  facet_wrap(~ Temp, ncol = 4)


tukey_results <- dx_long %>% group_by(Temp) %>%
  do({model <- aov(Value ~ Condition, data = .)
  tidy(TukeyHSD(model))
  })

print(tukey_results)

#Line = pop
#Conditon = treatments
# Get two-way ANOVA results per Temp (Condition and Line)
two_way_results <- dx_long %>% group_by(Temp) %>%
  do(tidy(aov(Value ~ Condition * Line, data = .)))

print(two_way_results)
#write_xlsx(two_way_results, path = "TempSpecific_Tukey_PostHoc_Results.xlsx")


tukey_Line_condition <- dx_long %>%
  group_by(Temp) %>%
  do({
    model <- aov(Value ~ Condition * Line, data = .)
    tukey <- TukeyHSD(model)
    tidy(tukey)
  })

print(tukey_Line_condition)
#write_xlsx(tukey_Line_condition, path = "tukey_Line_condition.xlsx")


get_cld_full <- function(df) {
  model <- aov(Value ~ Condition * Line, data = df)
  tukey <- TukeyHSD(model)
  cld <- multcompLetters4(model, tukey)
  
  # Extract CLDs for Condition, Line, and their interaction
  cld_df_list <- lapply(names(cld), function(term) {
    data.frame(
      Term = term,
      Group = names(cld[[term]]$Letters),
      Letter = cld[[term]]$Letters,
      Temp = unique(df$Temp),
      stringsAsFactors = FALSE
    )
  })
  
  bind_rows(cld_df_list)
}

# Apply across Temps
cld_full_all <- dx_long %>% group_by(Temp) %>% group_split() %>%
  lapply(get_cld_full) %>% bind_rows()

print(cld_full_all)
dx_long$Condition<-factor(dx_long$Condition, levels = c("Wet", "Dry", "WD", "DW"))

ggplot(dx_long, aes(x = Condition, y = Value, fill = Line)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.75) +
  facet_wrap(~ Temp, ncol = 4) +
  scale_fill_manual(values = custom_colors) +
  theme_classic() +
  labs(title = "Thermotolerance by Condition and Line across Temperatures",
       y = "% Egg Hatching", x = "Condition")

# Filter only Condition:Line terms
cld_to_plot <- cld_full_all %>%
  filter(grepl(":", Term)) %>%  # keep only interaction terms
  separate(Term, into = c("Condition", "Line"), sep = ":")  # split the interaction

ggplot(dx_long, aes(x = Condition, y = Value, fill = Line)) +
  geom_boxplot(position = position_dodge(width = 0.9), alpha = 0.75) +
  geom_text(data = cld_to_plot,
            aes(x = Condition, y = 1.00, group = Line, label = Letter, fill = NULL),
            position = position_dodge(width = 0.9),
            vjust = 1.5, size = 4) +
  facet_wrap(~ Temp, ncol = 4) +
  scale_fill_manual(values = custom_colors) +
  theme_classic() +
  labs(title = "Thermotolerance by Condition and Line across Temperatures",
       y = "% Egg Hatching", x = "Condition")
dx_long

