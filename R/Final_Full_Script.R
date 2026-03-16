# ==========================================================================
# 1. ENVIRONMENT & DIRECTORY SETUP
# ==========================================================================
setwd("~/AOPK-Microclimate")

library(tidyverse)
library(lubridate)

# Scan for all CSV files in Data and its subfolders starting with "data_"
root_data_path <- "Data"
file_list <- list.files(path = root_data_path, 
                        pattern = "^data_.*\\.csv$", 
                        full.names = TRUE, 
                        recursive = TRUE)

# ==========================================================================
# 2. THE COMPILATION FUNCTION
# ==========================================================================
read_tms_file <- function(f) {
  
  # 1. Extract 8-digit ID from the filename
  file_name <- basename(f)
  id_match <- regmatches(file_name, regexpr("[0-9]{8}", file_name))
  if(length(id_match) == 0) id_match <- "UNKNOWN"
  
  # 2. Read lines as raw text to handle the "single column" structure
  raw_lines <- readLines(f, warn = FALSE)
  if (length(raw_lines) == 0) return(NULL)
  
  # 3. Split by semicolon
  split_list <- strsplit(raw_lines, ";")
  
  # 4. Map the 9 columns explicitly (disregarding the bit after the final ;)
  # Using a matrix for stability
  clean_matrix <- do.call(rbind, lapply(split_list, function(x) x[1:9]))
  df <- as.data.frame(clean_matrix, stringsAsFactors = FALSE)
  colnames(df) <- c("index", "datetime_utc", "timezone", "T1", "T2", "T3", 
                    "moisture_raw", "shake", "errFlag")
  
  # 5. Data Formatting
  df %>%
    mutate(
      datalogger_ID = as.character(id_match),
      
      # THOROUGH DATE PARSING: 
      # We keep the dots and tell R exactly where they are using format =
      datetime = as.POSIXct(datetime_utc, format = "%Y.%m.%d %H:%M", tz = "UTC"),
      
      # Convert all data to numeric
      across(c(T1, T2, T3, moisture_raw, shake, errFlag), as.numeric)
    ) %>%
    select(datalogger_ID, datetime, T1, T2, T3, moisture_raw, shake, errFlag)
}

# ==========================================================================
# 3. EXECUTE COMPILATION
# ==========================================================================
compiled_data <- map_df(file_list, read_tms_file)

# Save master raw compilation
write.csv(compiled_data, "Outputs/Compiled_Raw_Microclimate.csv", row.names = FALSE)

# --- SUCCESS CHECK: COMPILED DATA ---
cat("\n==============================================")
cat("\nCHECK: COMPILED_RAW_MICROCLIMATE.CSV")
cat("\n==============================================")
cat("\nTotal Files Read:      ", length(file_list))
cat("\nTotal Rows:            ", nrow(compiled_data))
cat("\nTotal Columns:         ", ncol(compiled_data))
cat("\nFailed Date Parses:    ", sum(is.na(compiled_data$datetime))) 
cat("\nUnique Logger IDs:     ", length(unique(compiled_data$datalogger_ID)))
cat("\n==============================================\n")

# ==========================================================================
# 4. TEMPORAL TRIMMING & CLEANING
# ==========================================================================
drbakov_ids <- c("94234715", "94206238")

trimmed_data <- compiled_data %>%
  mutate(
    year = year(datetime),
    month = month(datetime)
  ) %>%
  # Filter: April to September
  filter(month >= 4 & month <= 9) %>%
  
  # Filter: Drbákov (2024 only) vs Others (2023-2025)
  filter(
    (datalogger_ID %in% drbakov_ids & year == 2024) | 
      (!(datalogger_ID %in% drbakov_ids) & year %in% c(2023, 2024, 2025))
  ) %>%
  select(datalogger_ID, datetime, year, month, T1, T2, T3, moisture_raw, shake, errFlag)

# Save the final trimmed data
write.csv(trimmed_data, "Outputs/Trimmed_Microclimate_Data.csv", row.names = FALSE)

# ==========================================================================
# 5. FINAL ROBUSTNESS CHECK (Blank/Empty Cells)
# ==========================================================================
# This function counts both NA values and empty character strings ""
check_blanks <- function(data) {
  sapply(data, function(x) sum(is.na(x) | as.character(x) == "", na.rm = TRUE))
}

blank_report <- check_blanks(trimmed_data)

cat("\n==============================================")
cat("\nFINAL CHECK: TRIMMED_MICROCLIMATE_DATA.CSV")
cat("\n==============================================")
cat("\nTotal Rows Remaining:  ", nrow(trimmed_data))
cat("\nUnique IDs Remaining:  ", length(unique(trimmed_data$datalogger_ID)))
cat("\n----------------------------------------------")
cat("\nEMPTY/BLANK CELL COUNT PER COLUMN:")
print(blank_report)
cat("\n----------------------------------------------")

# Summary logic
if(sum(blank_report) == 0) {
  cat("\nRESULT: Data is 100% complete. No empty cells found.")
} else {
  cat("\nRESULT: Found", sum(blank_report), "total empty cells. See breakdown above.")
}
cat("\n==============================================\n")

# ==========================================================================
# 6. METADATA JOIN
# ==========================================================================
library(readxl)

# Define the correct path since the file is inside the Data folder
metadata_path <- "Data/metadata_microclimate_dataloggers.xlsx"

if (!file.exists(metadata_path)) {
  stop(paste("ERROR: File not found at", metadata_path))
}

# Read metadata and force ID to character for a perfect match
metadata <- read_excel(metadata_path) %>%
  mutate(datalogger_ID = as.character(datalogger_ID)) %>%
  # Select ONLY the mapping columns to keep the final file clean
  select(datalogger_ID, site_name, treatment)

# Join metadata to the trimmed data
final_df <- trimmed_data %>%
  mutate(datalogger_ID = as.character(datalogger_ID)) %>%
  left_join(metadata, by = "datalogger_ID")

# --- CHECK FOR BLANKS AFTER JOIN ---
# This ensures every logger ID actually had a match in the Excel file
blank_site <- sum(is.na(final_df$site_name) | final_df$site_name == "")
blank_treat <- sum(is.na(final_df$treatment) | final_df$treatment == "")

cat("\n==============================================")
cat("\nMETADATA JOIN REPORT")
cat("\n==============================================")
cat("\nTotal rows in data:    ", nrow(final_df))
cat("\nBlank Site Names:      ", blank_site)
cat("\nBlank Treatments:      ", blank_treat)

if(blank_site > 0) {
  cat("\n\n!!! WARNING: Some logger IDs in your data were NOT found in the Excel metadata.")
  cat("\nCheck these IDs: ", paste(unique(final_df$datalogger_ID[is.na(final_df$site_name)]), collapse=", "))
} else {
  cat("\n\nSUCCESS: All data rows matched successfully with Metadata.")
}
cat("\n==============================================\n")

# Save a copy of the joined long-form raw data
write.csv(final_df, "Outputs/Final_Joined_Raw_Data.csv", row.names = FALSE)

# ==========================================================================
# 7. CALCULATE 15 SUMMARY STATISTICS
# ==========================================================================
# We pivot to "Long" format so T1, T2, and T3 are processed identically.
stats_results <- final_df %>%
  # Pivot T1, T2, T3 into a 'sensor' and 'temp' column
  pivot_longer(cols = c(T1, T2, T3), names_to = "sensor", values_to = "temp") %>%
  mutate(date_only = as.Date(datetime)) %>%
  
  # Grouping by site, treatment, ID, year, and which sensor it is
  group_by(site_name, treatment, datalogger_ID, year, sensor) %>%
  
  summarise(
    # 1. mean
    mean_temp = mean(temp, na.rm = TRUE),
    
    # 2. mean of the daily minima
    mean_daily_min = mean(tapply(temp, date_only, min, na.rm = TRUE)),
    
    # 3. mean of the daily maxima
    mean_daily_max = mean(tapply(temp, date_only, max, na.rm = TRUE)),
    
    # 4. absolute minimum
    abs_min = min(temp, na.rm = TRUE),
    
    # 5. absolute maximum
    abs_max = max(temp, na.rm = TRUE),
    
    # 6. mean of daily standard deviations
    mean_daily_sd = mean(tapply(temp, date_only, sd, na.rm = TRUE)),
    
    # 7. mean of the daily range
    mean_daily_range = mean(tapply(temp, date_only, function(x) diff(range(x, na.rm = TRUE)))),
    
    # GDD Stats (Growing Degree Days: sum of [daily mean - threshold] where mean > threshold)
    gdd_0  = sum(pmax(0, tapply(temp, date_only, mean, na.rm = TRUE) - 0)),
    gdd_2  = sum(pmax(0, tapply(temp, date_only, mean, na.rm = TRUE) - 2)),
    gdd_5  = sum(pmax(0, tapply(temp, date_only, mean, na.rm = TRUE) - 5)),
    gdd_10 = sum(pmax(0, tapply(temp, date_only, mean, na.rm = TRUE) - 10)),
    
    # THD Stats (Threshold Days: count of days where daily mean > threshold)
    thd_0  = sum(tapply(temp, date_only, mean, na.rm = TRUE) > 0),
    thd_2  = sum(tapply(temp, date_only, mean, na.rm = TRUE) > 2),
    thd_5  = sum(tapply(temp, date_only, mean, na.rm = TRUE) > 5),
    thd_10 = sum(tapply(temp, date_only, mean, na.rm = TRUE) > 10),
    
    .groups = "drop"
  )

# ==========================================================================
# 8. FINAL OUTPUT & VERIFICATION
# ==========================================================================
write.csv(stats_results, "Outputs/Final_Microclimate_Stats.csv", row.names = FALSE)

cat("\n--- FINAL VERIFICATION ---")
cat("\nTotal rows in stats: ", nrow(stats_results))

# Drbákov Check (should only have 2024)
cat("\n\nChecking Drbákov Site Results (T3):\n")
print(stats_results %>% filter(site_name == "Drbákov", sensor == "T3"))

# Bojanovická Check (should have 2023, 2024, 2025)
cat("\nChecking Bojanovická alej Results (T2):\n")
print(stats_results %>% filter(grepl("Bojanovická", site_name), sensor == "T2"))

# ==========================================================================
# 9. FINAL CSV AUDIT
# ==========================================================================

# 1. Check if sites have expected associated years, Drbákov should have 1 and others 3
stats_results %>% 
  group_by(site_name, datalogger_ID) %>% 
  summarise(years_present = n_distinct(year), .groups = "drop")

# 2. Review Column Names
cat("\n--- COLUMN NAME CHECK ---\n")
print(colnames(stats_results))

# 3. Comprehensive Blank/Empty Check 
# This looks for NAs, empty strings "", and "Inf" (which can happen in min/max of empty sets)
final_check <- function(data) {
  sapply(data, function(x) {
    sum(is.na(x) | as.character(x) == "" | as.character(x) == "Inf" | as.character(x) == "-Inf")
  })
}

final_blank_report <- final_check(stats_results)

cat("\n==============================================")
cat("\nFINAL AUDIT: Final_Microclimate_Stats.csv")
cat("\n==============================================")
cat("\nTotal Rows:         ", nrow(stats_results))
cat("\nTotal Columns:      ", ncol(stats_results))
cat("\n----------------------------------------------")
cat("\nEMPTY/BLANK/INF COUNT PER COLUMN:")
print(final_blank_report)
cat("\n----------------------------------------------")

if(sum(final_blank_report) == 0) {
  cat("\nRESULT: 100% Clean. No empty or infinite cells found.")
} else {
  cat("\nWARNING: Found", sum(final_blank_report), "potential issues. See breakdown above.")
}
cat("\n==============================================\n")

# ==========================================================================
# 9. FINAL REORDER, AUDIT & OVERWRITE
# ==========================================================================

# 1. Move datalogger_ID to the first position
stats_results <- stats_results %>%
  relocate(datalogger_ID, .before = site_name)

# 2. Comprehensive Blank/Empty/Inf Check 
# This scans for NAs, empty strings, and infinite values
final_audit <- function(data) {
  sapply(data, function(x) {
    sum(is.na(x) | as.character(x) == "" | as.character(x) == "Inf" | as.character(x) == "-Inf")
  })
}

audit_report <- final_audit(stats_results)

# 3. Print Audit Results to Console
cat("\n==============================================")
cat("\nFINAL AUDIT: datalogger_ID Reordered")
cat("\n==============================================")
cat("\nNew Column Order:\n")
print(colnames(stats_results))
cat("\n----------------------------------------------")
cat("\nEMPTY/BLANK/INF COUNT PER COLUMN:")
print(audit_report)
cat("\n----------------------------------------------")

if(sum(audit_report) == 0) {
  cat("\nRESULT: 100% Clean. No empty or infinite cells found.")
} else {
  cat("\nWARNING: Issues found. Check the report above.")
}
cat("\n==============================================\n")

# 4. Save and Overwrite the original Final file
write.csv(stats_results, "Outputs/Final_Microclimate_Stats.csv", row.names = FALSE)


# View the whole table in a new tab
View(stats_results)

# Or get a quick summary of column types and missing values
library(dplyr)
glimpse(stats_results)
anyNA(stats_results) # Should return FALSE


# =============================================================================
# 10. MEAN DAILY RANGE REFINED STACKED RAINCLOUD (Puffy & Matched Legend)
# =============================================================================
library(ggplot2)
library(ggdist)
library(dplyr)

# --- 1. DATA PREP ---
plot_data <- stats_results %>%
  mutate(year = factor(year, levels = c("2023", "2024", "2025"))) %>%
  mutate(sensor_label = case_when(
    sensor == "T1" ~ "T1 (Soil)",
    sensor == "T2" ~ "T2 (Surface)",
    sensor == "T3" ~ "T3 (Air)"
  )) %>%
  # Ensures Air (T3) is at the top of the flipped plot 
  mutate(sensor_label = factor(sensor_label, levels = c("T1 (Soil)", "T2 (Surface)", "T3 (Air)"))) %>%
  # Matches Legend Order precisely [cite: 326]
  mutate(treatment = factor(treatment, levels = c(
    "control", "coppiced", "litter_removed", "coppiced_and_litter_removed", "pasture"
  )))

# --- 2. COLORS: Vibrant Pasture Green ---
my_colors <- c(
  "control" = "#5A4A6F", 
  "coppiced" = "#E47250", 
  "litter_removed" = "#EBB261", 
  "coppiced_and_litter_removed" = "#9D5A6C", 
  "pasture" = "#2ECC71"
)

# --- 3. PLOTTING ---
raincloud_final <- 
  ggplot(data = plot_data, aes(x = sensor_label, y = mean_daily_range, fill = treatment)) +
  
  # A. THE CLOUD (Density): High Scale for Maximum Puffiness
  # scale = 2.2 allows clouds to overlap the lane above for a "puffy" feel
  stat_halfeye(
    adjust = 0.6, 
    justification = 0, 
    .width = 0, 
    point_colour = NA, 
    alpha = 0.7, 
    scale = 2.2, 
    position = position_dodge(width = 0.8, reverse = TRUE)
  ) +
  
  # B. THE PUDDLE (Boxplot)
  geom_boxplot(
    width = 0.15, 
    outlier.shape = NA, 
    alpha = 0.4,
    position = position_dodge(width = 0.8, reverse = TRUE)
  ) +
  
  # C. THE RAIN (Points)
  geom_point(
    aes(color = treatment),
    alpha = 0.6, 
    size = 1.5,
    position = position_dodge(width = 0.8, reverse = TRUE)
  ) +
  
  facet_grid(year ~ .) +
  scale_fill_manual(values = my_colors) +
  scale_color_manual(values = my_colors) +
  scale_y_continuous(breaks = seq(0, 25, by = 5), limits = c(0, 26)) + 
  
  labs(title = "Mean Daily Temperature Range (2023 - 2025)",
       subtitle = "Vertical Profile: T3 (Air) to T1 (Soil)",
       x = "Sensor", 
       y = expression("Temperature Range ("*degree*"C)"),
       fill = "Treatment Type", 
       color = "Treatment Type") +
  
  coord_flip() + 
  theme_niwot() +
  
  # F. LEGEND CUSTOMIZATION: Matching Alpha & Pasture Dot [cite: 332]
  guides(
    fill = guide_legend(override.aes = list(
      shape = c(NA, NA, NA, NA, 16), # Dot for pasture only
      linetype = c(1, 1, 1, 1, 0),   # Remove outline from pasture dot
      alpha = 0.7,                   # MATCHES PLOT ALPHA
      color = my_colors              # Matches palette [cite: 326]
    )),
    color = "none" 
  )

# --- 4. SAVE AS PDF (Increased Vertical Space) ---
# Extra height ensures the puffy clouds don't look cramped
ggsave(raincloud_final, filename = "Outputs/Mean_Daily_Range_Puffy_Final.pdf", 
       height = 28, width = 14, device = "pdf")


###############################################################################
## Happy with visual, now can generate raincloud plots for all 15 summary stats
###############################################################################

# ==========================================================================
# 11. COMPLETE DATA VISUALIZATION & OUTLIER REMOVAL (ALL STATS)
#     15 SUMMARY STATS GRAPHS - RAINCLOUD PLOTS - COMBINE YEARS
# ==========================================================================

stats_results <- read.csv("Outputs/Final_Microclimate_Stats.csv")
colnames(stats_results)

library(ggplot2)
library(ggdist)
library(dplyr)

# --- 1. THEME DEFINITION ---
theme_niwot <- function(){
  theme_bw() +
    theme(
      text = element_text(family = "sans"),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      panel.border = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      legend.position = "right",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10)
    )
}

# --- 2. FOLDER SETUP ---
output_path <- "Outputs/Summary Statistics"
if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)

# --- 3. DATA PREP & OUTLIER REMOVAL ---
# Helper function to find and NA out a single extreme miscalibration point
remove_extreme <- function(data, col, yr, sens, treat, type = "max") {
  idx <- which(data$year == as.character(yr) & data$sensor == sens & data$treatment == treat)
  if(length(idx) > 0) {
    if(type == "max") {
      target <- idx[which.max(data[[col]][idx])]
    } else {
      target <- idx[which.min(data[[col]][idx])]
    }
    data[[col]][target] <- NA
  }
  return(data)
}

# Apply base formatting
plot_data <- stats_results %>%
  mutate(year = factor(year, levels = c("2023", "2024", "2025"))) %>%
  mutate(sensor_label = factor(case_when(
    sensor == "T1" ~ "T1 (Soil)",
    sensor == "T2" ~ "T2 (Surface)",
    sensor == "T3" ~ "T3 (Air)"
  ), levels = c("T1 (Soil)", "T2 (Surface)", "T3 (Air)"))) %>%
  mutate(treatment = factor(treatment, levels = c(
    "control", "coppiced", "litter_removed", "coppiced_and_litter_removed", "pasture"
  )))

# Count NAs before cleaning
na_before <- sum(is.na(plot_data))

# Surgically remove miscalibrated outliers
plot_data <- plot_data %>%
  # mean_daily_max
  remove_extreme("mean_daily_max", "2024", "T1", "control", "max") %>%
  remove_extreme("mean_daily_max", "2025", "T1", "control", "max") %>%
  # abs_min 
  remove_extreme("abs_min", "2025", "T1", "litter_removed", "min") %>%
  remove_extreme("abs_min", "2025", "T2", "litter_removed", "min") %>%
  remove_extreme("abs_min", "2025", "T3", "litter_removed", "min") %>%
  # abs_max
  remove_extreme("abs_max", "2024", "T1", "control", "max") %>%
  remove_extreme("abs_max", "2025", "T1", "control", "max") %>%
  remove_extreme("abs_max", "2025", "T1", "coppiced_and_litter_removed", "max") %>%
  # mean_daily_sd
  remove_extreme("mean_daily_sd", "2024", "T1", "control", "max") %>%
  remove_extreme("mean_daily_sd", "2025", "T1", "control", "max") %>%
  remove_extreme("mean_daily_sd", "2025", "T1", "coppiced_and_litter_removed", "max") %>%
  # mean_daily_range
  remove_extreme("mean_daily_range", "2024", "T1", "control", "max") %>%
  remove_extreme("mean_daily_range", "2025", "T1", "control", "max") %>%
  remove_extreme("mean_daily_range", "2025", "T1", "coppiced_and_litter_removed", "max")

na_after <- sum(is.na(plot_data))

cat("\n==============================================")
cat("\nMISCALIBRATION CLEANING REPORT")
cat("\n==============================================")
cat("\nExpected extreme outliers removed: 14")
cat("\nActual extreme outliers removed:   ", na_after - na_before)
cat("\n==============================================\n")

# --- 4. COLOR PALETTE ---
my_colors <- c(
  "control" = "#5A4A6F", "coppiced" = "#E47250", "litter_removed" = "#EBB261", 
  "coppiced_and_litter_removed" = "#9D5A6C", "pasture" = "#2ECC71"
)

# --- 5. STATISTIC MAPPING ---
stats_to_plot <- list(
  "mean_temp"        = c("Mean Daily Temperature", "Temperature (°C)"),
  "mean_daily_min"   = c("Mean Daily Minimum Temperature", "Temperature (°C)"),
  "mean_daily_max"   = c("Mean Daily Maximum Temperature", "Temperature (°C)"),
  "abs_min"          = c("Absolute Minimum Temperature", "Temperature (°C)"),
  "abs_max"          = c("Absolute Maximum Temperature", "Temperature (°C)"),
  "mean_daily_sd"    = c("Mean Daily Standard Deviation", "Standard Deviation"),
  "mean_daily_range" = c("Mean Daily Temperature Range", "Temperature Range (°C)"),
  "gdd_0"            = c("Growing Degree Days (Base 0)", "Degree Days"),
  "gdd_2"            = c("Growing Degree Days (Base 2)", "Degree Days"),
  "gdd_5"            = c("Growing Degree Days (Base 5)", "Degree Days"),
  "gdd_10"           = c("Growing Degree Days (Base 10)", "Degree Days"),
  "thd_0"            = c("Threshold Days (Base 0)", "Count of Days"),
  "thd_2"            = c("Threshold Days (Base 2)", "Count of Days"),
  "thd_5"            = c("Threshold Days (Base 5)", "Count of Days"),
  "thd_10"           = c("Threshold Days (Base 10)", "Count of Days")
)

# --- 6. PLOTTING LOOP ---
for (stat_name in names(stats_to_plot)) {
  if (!(stat_name %in% colnames(plot_data))) next 
  
  current_title <- stats_to_plot[[stat_name]][1]
  current_y_lab <- stats_to_plot[[stat_name]][2]
  
  cat(paste("Processing:", current_title, "... "))
  
  # Logic: Account for Pasture N=1 group
  current_plot_data <- plot_data
  if (stat_name == "mean_daily_sd") {
    current_plot_data <- current_plot_data %>%
      mutate(!!sym(stat_name) := ifelse(treatment == "pasture", NA, .data[[stat_name]]))
  }
  
  p <- ggplot(data = current_plot_data, aes(x = sensor_label, y = .data[[stat_name]], fill = treatment)) +
    
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
      aes(color = treatment),
      alpha = 0.6, size = 1.5,
      position = position_dodge(width = 0.8, reverse = TRUE),
      na.rm = TRUE
    ) +
    
    scale_fill_manual(values = my_colors) +
    scale_color_manual(values = my_colors) +
    
    labs(
      title = paste(current_title, "(All Years Combined: 2023 - 2025)"),
      subtitle = "Vertical Profile: T3 (Air) to T1 (Soil)",
      x = "Sensor", y = current_y_lab,
      fill = "Treatment Type", color = "Treatment Type"
    ) +
    
    theme_niwot() +
    
    guides(
      fill = guide_legend(override.aes = list(
        shape = c(NA, NA, NA, NA, 16),
        linetype = c(1, 1, 1, 1, 0),
        alpha = 0.7,
        color = my_colors
      )),
      color = "none" 
    )
  
  # Check for THD plots to blow up the scale to 100 - 200 days
  if(grepl("thd", stat_name, ignore.case = TRUE)){
    p <- p + coord_flip(ylim = c(100, 200))
  } else {
    p <- p + coord_flip()
  }
  
  # Check for 0-baseline (Range, SD, GDD)
  if(grepl("range|gdd|sd", stat_name, ignore.case = TRUE)){
    p <- p + expand_limits(y = 0)
  }
  
  # Reduced height since years are no longer faceted (14 instead of 28)
  ggsave(p, filename = file.path(output_path, paste0(stat_name, "_Raincloud_Final.pdf")), 
         height = 14, width = 14, device = "pdf", limitsize = FALSE)
  
  cat("Success.\n")
}
