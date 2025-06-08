library(dplyr) 
library(broom)
library(stringr)
library(ggplot2)
library(car)
library(emmeans)
library(forcats)
library(multcompView)
library(writexl)
library(gridExtra)
library(openxlsx)


# Attach data and filter out day 8
attach(EggCount)
dg <- EggCount%>%
  filter(day != 8) %>%
  mutate(
    eggs_per_alive = count / alive,
    main_condition = gsub("_[0-9]+$", "", condition),
    main_condition = factor(main_condition, levels = c("W", "D", "WD", "DW"))
  )

str(dg)
unique(dg$main_condition)
unique(dg$day)

custom_colors <- c("Ngoye" = "#FB6A4A", "PK10" = "#08306B", "Combined" = "#9B4E7E")

dg$main_condition <- factor(dg$main_condition, levels = c("W", "D", "WD", "DW"))
# Plot
plot_a<-ggplot(dg, aes(x = day, y = eggs_per_alive, color = Line)) +
  geom_point(alpha = 0.6) +
  geom_smooth(se = FALSE, method = "loess", span = 0.7) +
  facet_wrap(~main_condition, nrow = 1) +
  scale_x_continuous(breaks = c(12, 16, 20)) +  # show only 12,16,20
  scale_color_manual(values = custom_colors) +
  labs(
    title = "Egg Count Progression by Condition",
    x = "Day",
    y = "Eggs per Alive Female"
  ) +
  theme_classic()

plot_a

# Filter for WD condition and days 12, 16 & 20
dg_stat <- dg %>%
  filter(main_condition == "WD", day %in% c(12,16, 20)) %>%
  mutate(eggs_per_alive = count / alive)

str(dg_stat)

#  visualize group means
ggplot(dg_stat, aes(x = factor(Line), y = eggs_per_alive, fill = Line)) +
  geom_boxplot(alpha = 0.6) +
  facet_wrap(~day) +
  theme_minimal() +
  labs(title = "Eggs per Alive Female (WD Condition)", x = "Line", y = "Eggs per Alive Female")+
  scale_fill_manual(values = custom_colors)

# For each particular period
results <- list()
for (d in c(12, 16, 20)) {
  temp <- dg_stat %>% filter(day == d)
  
  # Test assumptions
  shapiro <- by(temp$eggs_per_alive, temp$Line, shapiro.test)
  levene <- leveneTest(eggs_per_alive ~ Line, data = temp)
  
  if (all(sapply(shapiro, function(x) x$p.value > 0.05)) & levene$`Pr(>F)`[1] > 0.05) {
    # Normal data, run ANOVA
    aov_model <- aov(eggs_per_alive ~ Line, data = temp)
    tukey <- TukeyHSD(aov_model)
    results[[paste0("Day_", d)]] <- list(
      method = "ANOVA",
      summary = summary(aov_model),
      tukey = tukey
    )
  } else {
    # Not normal, use Kruskal-Wallis
    kruskal <- kruskal.test(eggs_per_alive ~ Line, data = temp)
    pairwise <- pairwise.wilcox.test(temp$eggs_per_alive, temp$Line, p.adjust.method = "BH")
    results[[paste0("Day_", d)]] <- list(
      method = "Kruskal-Wallis",
      kruskal = kruskal,
      pairwise = pairwise
    )
  }
}

# View results
results$Day_12
results$Day_16
results$Day_20



# Ensure eggs_per_alive is computed
dg_stat <- dg %>%
  filter(day %in% c(12, 16, 20)) %>%
  mutate(eggs_per_alive = count / alive)

# Initialize empty list for results
results_all <- list()

# Loop over each main_condition and day
for (cond in levels(dg_stat$main_condition)) {
  for (d in c(12, 16, 20)) {
    
    temp <- dg_stat %>%
      filter(main_condition == cond, day == d)
    
    if (nrow(temp) < 2 || length(unique(temp$Line)) < 2) {
      next  # Skip if not enough data for testing
    }
    
    # Run normality and variance tests
    shapiro <- by(temp$eggs_per_alive, temp$Line, shapiro.test)
    levene <- leveneTest(eggs_per_alive ~ Line, data = temp)
    
    key <- paste0("Condition_", cond, "_Day_", d)
    
    if (all(sapply(shapiro, function(x) x$p.value > 0.05)) & levene$`Pr(>F)`[1] > 0.05) {
      # Use ANOVA
      aov_model <- aov(eggs_per_alive ~ Line, data = temp)
      tukey <- TukeyHSD(aov_model)
      results_all[[key]] <- list(
        method = "ANOVA",
        summary = summary(aov_model),
        tukey = tukey
      )
    } else {
      # Use Kruskal-Wallis
      kruskal <- kruskal.test(eggs_per_alive ~ Line, data = temp)
      pairwise <- pairwise.wilcox.test(temp$eggs_per_alive, temp$Line, p.adjust.method = "BH")
      results_all[[key]] <- list(
        method = "Kruskal-Wallis",
        kruskal = kruskal,
        pairwise = pairwise
      )
    }
  }
}


print(results_all)

wb <- createWorkbook()

# Loop through each result and add as a new worksheet
for (name in names(results_all)) {
  res <- results_all[[name]]
  
  # Create a simple summary for each
  if (res$method == "ANOVA") {
    summary_df <- as.data.frame(res$summary[[1]])  # Extract summary table
    tukey_df <- as.data.frame(res$tukey[[1]])      # Tukey HSD
    addWorksheet(wb, sheetName = paste0(name, "_ANOVA"))
    writeData(wb, sheet = paste0(name, "_ANOVA"), x = summary_df, startRow = 1, startCol = 1)
    writeData(wb, sheet = paste0(name, "_ANOVA"), x = tukey_df, startRow = nrow(summary_df) + 3, startCol = 1)
    
  } else if (res$method == "Kruskal-Wallis") {
    kruskal_df <- data.frame(
      statistic = res$kruskal$statistic,
      p_value = res$kruskal$p.value,
      method = res$kruskal$method
    )
    pairwise_df <- as.data.frame(res$pairwise$p.value)
    addWorksheet(wb, sheetName = paste0(name, "_KW"))
    writeData(wb, sheet = paste0(name, "_KW"), x = kruskal_df, startRow = 1, startCol = 1)
    writeData(wb, sheet = paste0(name, "_KW"), x = pairwise_df, startRow = 5, startCol = 1)
  }
}

#saveWorkbook(wb, file = "eggs_analysis_results.xlsx", overwrite = TRUE)
getwd()

# Reorder condition factor so plots are in desired order
dg$main_condition <- factor(dg$main_condition, levels = c("W", "D", "WD", "DW"))

# Plot
ggplot(dg, aes(x = main_condition, y = eggs_per_alive, fill = Line)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1, alpha = 0.7, width = 0.65, position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = custom_colors) +
  facet_wrap(~Line, nrow = 1) +
  labs(title = "Eggs per Alive Female by Condition and Line",
    x = "Condition",
    y = "Eggs per Alive Female") +
  theme_classic(base_size = 14) +
  theme(legend.position = "none",
    strip.text = element_text(face = "bold"),
    panel.grid.major.x = element_blank())


results_by_line <- dg %>% group_by(Line) %>%
  do(tidy(aov(count ~ main_condition + day, data = .)))

print(results_by_line)

tukey_results <- dg %>%
  group_by(Line) %>%
  do({model <- aov(count ~ main_condition + day, data = .)
    tidy(TukeyHSD(model, which = "main_condition"))})

print(tukey_results)
#write_xlsx(tukey_results, path = "Tukey_PostHoc_Results.xlsx")
getwd()
anova_05<-aov(eggs_per_alive ~ day, data = dg)
summary(anova_05)
unique(dg$day)

cld_data <- dg %>%
  group_by(Line) %>%
  do({
    model <- aov(count ~ main_condition + day, data = .)
    tukey <- TukeyHSD(model, "main_condition")
    letters <- multcompView::multcompLetters(tukey$main_condition[, "p adj"])$Letters
    data.frame(main_condition = names(letters), cld = letters)
  }) %>%
  ungroup()

cld_data <- dg %>%
  group_by(Line) %>%
  do({
    current_line <- unique(.$Line)
    model <- aov(count ~ main_condition + day, data = .)
    tukey <- TukeyHSD(model, "main_condition")
    letters <- multcompView::multcompLetters(tukey$main_condition[, "p adj"])$Letters
    data.frame(Line = current_line, main_condition = names(letters), cld = letters)
  })


cld_data

ggplot(dg, aes(x = main_condition, y = count, fill = Line)) +
  geom_boxplot(alpha = 0.8) +
  #facet_wrap(~Line, scales = "free_y") +
  theme_classic() +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Four days cumulative egg numbers by condition and population (Death normalized)", y = "Total Eggs", x = "Condition") +
  geom_text(data = cld_data, 
            aes(x = main_condition, y = max(dg$count) * 1.05, label = cld), 
            inherit.aes = FALSE, size = 3)


plot_b<-ggplot(dg, aes(x = main_condition, y = count/30, fill = Line)) +
  geom_boxplot(alpha = 0.8) +
  #facet_wrap(~Line, scales = "free_y") +
  theme_classic() +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Daily egg numbers by condition and population", y = "Total Eggs", x = "Condition") +
  geom_text(data = cld_data, 
            aes(x = main_condition, y = max(dg$count) * 1.25, label = cld), 
            inherit.aes = FALSE, size = 3) + ylim(0,25)
plot_b

str(dg)

anova_model <- aov(eggs_per_alive ~ main_condition, data = dg)
anova_model_2 <-  aov(eggs_per_alive ~ main_condition + Line, data = dg)
anova_model_3 <- aov(eggs_per_alive ~ main_condition + Line + main_condition * Line, data = dg)
summary(anova_model)
TukeyHSD(anova_model)
summary(anova_model_2)
summary(anova_model_3)

unique(dg$Line)

dg_split <- dg %>% group_split(Line)

anova_summary_list <- lapply(dg_split, function(df) {
  line_name <- unique(df$Line)
  model <- aov(eggs_per_alive ~ main_condition, data = df)
  tidy_result <- broom::tidy(model)
  tidy_result$Line <- line_name
  return(tidy_result)
})

anova_summary_df <- bind_rows(anova_summary_list)
anova_summary_df
#write_xlsx(anova_summary_df, "ANOVA_by_Line.xlsx")

# Tidy the overall ANOVA result
anova_model_all <- broom::tidy(anova_model)
anova_model_all$Line <- "All"  # Label it for clarity

# Combine with your existing summary dataframe
anova_summary_df_full <- bind_rows(anova_summary_df, anova_model_all)

# View the full summary
print(anova_summary_df_full)
#write_xlsx(anova_summary_df_full, "ANOVA_by_Line.xlsx")


grid.arrange(plot_a, plot_b, ncol = 2)
