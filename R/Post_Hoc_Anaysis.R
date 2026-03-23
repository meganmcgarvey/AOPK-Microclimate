### Mon 23 March 2026
### Megan McGarvey
# ==========================================================================
# POST HOC ANALYSIS
# ===================================================================
# MICROCLIMATE ANALYSIS: LINEAR MIXED-EFFECTS MODELS (lme4)
# =========================================================

# 1. Setup and Data Loading
setwd("~/AOPK-Microclimate")

# Load the core package and helpers
install.packages("pbkrtest")
library(pbkrtest)

library(lme4)       # For building the mixed models
library(lmerTest)   # Provides p-values for lme4 models
library(emmeans)    # For Tukey HSD pairwise comparisons
library(dplyr)      # For data manipulation

# Load your summary statistics file
my_data <- read.csv("Outputs/Final_Microclimate_Stats.csv")
names(my_data)

# =========================================================
# UPDATED SCRIPT MATCHING YOUR COLUMN NAMES
# =========================================================

# 1. Convert categorical variables to factors
my_data$treatment <- as.factor(my_data$treatment)
my_data$sensor <- as.factor(my_data$sensor)
my_data$site_name <- as.factor(my_data$site_name)
my_data$datalogger_id <- as.factor(my_data$datalogger_ID) # Note: matching the 'ID' case from your output
my_data$year <- as.factor(my_data$year)

# 2. Set "control" as the baseline (Ensure "control" is spelled exactly like this in your CSV)
my_data$treatment <- relevel(my_data$treatment, ref = "control")

# ---------------------------------------------------------
# ANALYSIS: MEAN DAILY RANGE
# ---------------------------------------------------------
# Using your exact column name: mean_daily_range
model_range <- lmer(mean_daily_range ~ treatment * sensor + (1 | site_name/datalogger_id) + (1 | year), 
                    data = my_data)

# Summary of the model
summary(model_range)

# Tukey HSD Post-hoc
tukey_range <- emmeans(model_range, pairwise ~ treatment | sensor, adjust = "tukey")
print(tukey_range$contrasts)

# ---------------------------------------------------------
# ANALYSIS: MEAN DAILY SD
# ---------------------------------------------------------
# Using your exact column name: mean_daily_sd
model_sd <- lmer(mean_daily_sd ~ treatment * sensor + (1 | site_name/datalogger_id) + (1 | year), 
                 data = my_data)

summary(model_sd)

# --- POST-HOC: Tukey HSD ---
# Compare treatments within each specific sensor depth (T1, T2, T3)
tukey_sd <- emmeans(model_sd, pairwise ~ treatment | sensor, adjust = "tukey")
print(tukey_sd$contrasts)

# =========================================================
# FINAL OUTPUT: Significance Classification
# =========================================================

# Example: Create a dataframe of results for your report
results_table <- as.data.frame(tukey_range$contrasts) %>%
  mutate(Significance = ifelse(p.value < 0.05, "Significant", "Not Significant"))

write.csv(results_table, "Outputs/Tukey_Results_Daily_Range.csv")

# ==========================================================================
# FINAL REPORT MEAN DAILY SD RAINCLOUD PLOT PNG GENERATION 
# ==========================================================================

# 1. CREATE THE PLOT WITH UPDATED LABELS
p_sd_final <- ggplot(data = plot_data, aes(x = sensor_label, y = mean_daily_sd, fill = treatment)) +
  stat_halfeye(
    adjust = 0.6, justification = 0, .width = 0, point_colour = NA, 
    alpha = 0.7, scale = 2.2, 
    position = position_dodge(width = 0.8, reverse = TRUE),
    na.rm = TRUE 
  ) +
  geom_boxplot(
    width = 0.15, outlier.shape = NA, alpha = 0.4,
    position = position_dodge(width = 0.8, reverse = TRUE),
    na.rm = TRUE
  ) +
  geom_point(
    aes(color = treatment), # This allows pasture dots to show up
    alpha = 0.6, size = 1.5,
    position = position_dodge(width = 0.8, reverse = TRUE),
    na.rm = TRUE
  ) +
  scale_fill_manual(values = my_colors) +
  scale_color_manual(values = my_colors) +
  labs(
    title = "Mean Daily Standard Deviation\n(All Years Combined: 2023 - 2025)",
    subtitle = "Vertical Profile: T3 (Air), T2 (Surface), T1 (Soil)",
    x = "Sensor", 
    y = "Standard Deviation",
    fill = "Treatment Type"
  ) +
  theme_niwot() +
  coord_flip() +
  expand_limits(y = 0) +
  # THIS PART MERGES THE LEGEND INTO ONE
  guides(
    fill = guide_legend(title = "Treatment Type"),
    color = "none" # This hides the second "dots" legend
  )

# 2. SAVE AS PNG
ggsave(
  filename = "Mean_Daily_SD_Raincloud_Final_Report.png", 
  plot = p_sd_final,
  path = "Outputs/Summary Statistics",
  width = 10, 
  height = 7, 
  dpi = 300
)

cat("Success: Legend consolidated and titles updated.")

