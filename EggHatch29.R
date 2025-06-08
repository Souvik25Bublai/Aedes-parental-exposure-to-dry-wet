attach(EggHatch_29)
dc<-EggHatch_29
str(dc)
unique(dc$Condition) 

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(emmeans)
library(tidyverse)
library(writexl)
library(broom)

# Pivot data to long format for faceting
dc_long <- dc %>% pivot_longer(cols = c("Combined", "Ngoye", "PK10"), names_to = "Metric", values_to = "Value")

#View(dc_long)
str(dc_long)

summary_data <- dc_long %>% group_by(Condition, Metric) %>%
  summarize(Mean = mean(Value), SEM = sd(Value) / sqrt(n()), .groups = "drop")

str(summary_data)

custom_colors <- c("Ngoye" = "#FB6A4A","PK10" = "#08306B","Combined" = "#9B4E7E")
summary_data$Condition <- factor(summary_data$Condition, levels = c("Wet", "Dry", "WD", "DW"))

ggplot(summary_data, aes(x = factor(Condition), y = Mean, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black", alpha = 0.8) +
  geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM),
                position = position_dodge(width = 0.9), width = 0.2, color = "black") +
  labs(
    title = "Average Egg Hatching by Condition and Population",
    x = "Condition",
    y = "Average % Egg Hatching",
    fill = "Population"
  ) +
  theme_classic() +
  theme(
    legend.position = "right",
    text = element_text(size = 15)
  ) +
  scale_fill_manual(values = custom_colors)


model1 <- aov(Value ~ Condition, data = dc_long)
summary(model1)

emm1<- emmeans(model1, pairwise ~ Condition)
contrasts_table <- as.data.frame(summary(emm1$contrasts))
contrasts_table

library(multcomp)
cld_results <- cld(emm1, Letters = letters, adjust = "tukey")
print(cld_results)

write_xlsx(list("Pairwise Contrasts" = contrasts_table), "Condition-contrasts_output.xlsx")

#Metric = Populations
model2 <- aov(Value ~ Condition * Metric, data = dc_long)
summary(model2)
emm2<-emmeans(model2, pairwise ~ Metric | Condition)
contrasts_table_2 <- as.data.frame(summary(emm2$contrasts))
contrasts_table_2
write_xlsx(list("Pairwise Contrasts" = contrasts_table_2), "Line-contrasts_output.xlsx")




dc_long$Condition <- factor(dc_long$Condition, levels = c("Wet", "Dry", "WD", "DW"))

ggplot(dc_long, aes(x = Metric, y = Value, fill = Metric)) +
  geom_boxplot() +
  facet_wrap(~Condition, nrow = 1) + # Set nrow = 1 for a single row
  scale_fill_manual(values = custom_colors) +
  theme_classic() +
  labs(title = "Egg Hatching by Population across Conditions")

