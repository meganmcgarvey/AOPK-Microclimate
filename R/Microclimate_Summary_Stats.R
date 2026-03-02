setwd("~/AOPK-Microclimate")
## 01_Master_Compile.R
# Sets environment, compiles all raw TOMST data, and filters by deployment dates.

# 1. SET ENVIRONMENT
setwd("~/AOPK-Microclimate")
library(tidyverse)
library(lubridate)

# 2. THE DEPLOYMENT LOOKUP (Site Names + Start/End Dates)
# Data outside these windows is considered "cupboard/artifact" data and removed.
deployment_info <- tribble(
  ~datalogger_ID, ~site_name, ~start, ~end,
  "94234715", "Drbákov", "2023-05-31", "2025-07-15",
  "94206238", "Drbákov", "2023-05-29", "2025-07-15",
  "94217437", "Bojanovická alej (Hodonínská Doubrava)", "2022-05-10", "2025-10-02",
  "94217439", "Bojanovická alej (Hodonínská Doubrava)", "2022-05-10", "2025-10-02",
  "94217440", "Bojanovická alej (Hodonínská Doubrava)", "2022-05-10", "2025-10-02",
  "94217443", "Bojanovická alej (Hodonínská Doubrava)", "2022-05-10", "2025-10-02",
  "94217444", "Bojanovická alej (Hodonínská Doubrava)", "2022-05-10", "2025-10-02",
  "94217456", "Bojanovická alej (Hodonínská Doubrava)", "2022-05-10", "2025-10-02",
  "94217457", "Bojanovická alej (Hodonínská Doubrava)", "2022-05-10", "2025-09-27",
  "94217460", "Bojanovická alej (Hodonínská Doubrava)", "2022-05-10", "2025-09-26",
  "94217422", "Hovoranská cesta (Hodonínská Doubrava)", "2022-05-10", "2025-09-22",
  "94217424", "Hovoranská cesta (Hodonínská Doubrava)", "2022-05-10", "2025-09-22",
  "94217426", "Hovoranská cesta (Hodonínská Doubrava)", "2022-05-10", "2025-09-22",
  "94217427", "Hovoranská cesta (Hodonínská Doubrava)", "2022-05-10", "2025-09-22",
  "94217428", "Hovoranská cesta (Hodonínská Doubrava)", "2022-05-10", "2025-09-22",
  "94217429", "Hovoranská cesta (Hodonínská Doubrava)", "2022-05-10", "2025-09-22",
  "94217430", "Hovoranská cesta (Hodonínská Doubrava)", "2022-05-10", "2025-09-22",
  "94217436", "Hovoranská cesta (Hodonínská Doubrava)", "2022-05-10", "2025-09-22",
  "94217411", "Karlštejn - Koda (Vysoká stráň)", "2023-03-15", "2025-09-15",
  "94217412", "Karlštejn - Koda (Vysoká stráň)", "2023-03-15", "2025-09-15",
  "94217423", "Karlštejn - Koda (Vysoká stráň)", "2023-03-15", "2025-09-15",
  "94217431", "Karlštejn - Koda (Vysoká stráň)", "2023-03-15", "2025-09-15",
  "94217432", "Karlštejn - Koda (Vysoká stráň)", "2023-03-15", "2025-09-18",
  "94217433", "Karlštejn - Koda (Vysoká stráň)", "2023-03-15", "2025-09-18",
  "94217434", "Karlštejn - Koda (Vysoká stráň)", "2023-03-15", "2025-09-18",
  "94217441", "Karlštejn - Koda (Vysoká stráň)", "2023-03-15", "2025-09-18",
  "94217442", "Karlštejn - Koda (Vysoká stráň)", "2023-03-15", "2025-09-18",
  "94217453", "Karlštejn - Koda (Vysoká stráň)", "2023-03-15", "2025-09-15",
  "94217454", "Karlštejn - Koda (Vysoká stráň)", "2023-03-15", "2025-09-15",
  "94217455", "Karlštejn - Koda (Vysoká stráň)", "2023-03-15", "2025-09-15",
  "94217458", "Karlštejn - Koda (Vysoká stráň)", "2023-03-15", "2025-09-15",
  "94217459", "Karlštejn - Koda (Vysoká stráň)", "2023-03-15", "2025-09-15"
) %>% mutate(start = as_date(start), end = as_date(end))

# 3. IDENTIFY ALL FILES
# Recursive search through all subfolders in the 'Data' directory
data_path <- "Data/"
files <- list.files(path = data_path, pattern = "data_.*\\.csv", full.names = TRUE, recursive = TRUE)

# 4. COMPILE AND PARSE RAW DATA
raw_data <- files %>%
  map_df(~{
    # Extract the full 8-digit ID from the filename
    id_ext <- str_extract(basename(.x), "\\d{8}")
    
    # Read semicolon delimited file
    read_delim(.x, delim = ";", col_names = FALSE, show_col_types = FALSE, locale = locale(encoding = "UTF-8")) %>%
      mutate(datalogger_ID = id_ext) %>%
      rename(
        index = X1, datetime_utc = X2, timezone = X3,
        T1 = X4, T2 = X5, T3 = X6, moisture_raw = X7, shake = X8, errFlag = X9
      )
  })

# 5. CLEAN, FILTER, AND JOIN
# Using inner_join means if a logger isn't on your list, it's discarded (safety measure)
robust_dataset <- raw_data %>%
  mutate(date_parsed = as_date(ymd_hm(datetime_utc, truncated = 1))) %>%
  inner_join(deployment_info, by = "datalogger_ID") %>%
  # Filter only data within the valid deployment window provided
  filter(date_parsed >= start & date_parsed <= end) %>%
  # Organize final columns as requested
  select(
    datalogger_ID, site_name, index, datetime_utc, 
    timezone, T1, T2, T3, moisture_raw, shake, errFlag
  )

# 6. SAVE OUTPUT
if(!dir.exists("Outputs")) dir.create("Outputs")
write_excel_csv(robust_dataset, "Outputs/Compiled_Microclimate_Data.csv")

# 7. --- VERIFICATION REPORT ---
# This prints to the console so you don't have to open the massive CSV.
verification_report <- robust_dataset %>%
  group_by(site_name, datalogger_ID) %>%
  summarise(
    Row_Count = n(),
    Min_Date = min(as_date(ymd_hm(datetime_utc, truncated = 1))),
    Max_Date = max(as_date(ymd_hm(datetime_utc, truncated = 1))),
    Avg_Air_Temp = round(mean(T3, na.rm = TRUE), 2),
    .groups = "drop"
  )

cat("\n--- DATA COMPILATION SUCCESSFUL ---\n")
print(verification_report)

##################################
## Mon 2 March 2026
## MICROCLIMATE SUMMARY STATISTICS

### 02_Calculate_Indicators.R
setwd("~/AOPK-Microclimate")
library(tidyverse)
library(lubridate)

# 1. Load the robust master file
df <- read_csv("Outputs/Compiled_Microclimate_Data.csv", show_col_types = FALSE)

# EXTRA
# Validation Check: Distribution of Daily Means
df_check <- read_csv("Outputs/Compiled_Microclimate_Data.csv") %>%
  mutate(date = as_date(ymd_hm(datetime_utc, truncated = 1))) %>%
  group_by(datalogger_ID, date) %>%
  summarise(daily_mean = mean(T3, na.rm = TRUE), .groups = "drop")

# See what temperatures actually occur in your forest
quantile(df_check$daily_mean, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))

# 2. Daily Aggregation (The essential middle step)
daily_df <- df %>%
  mutate(date = as_date(ymd_hm(datetime_utc, truncated = 1))) %>%
  group_by(site_name, datalogger_ID, date) %>%
  summarise(
    d_mean = mean(T3, na.rm = TRUE),
    d_min  = min(T3, na.rm = TRUE),
    d_max  = max(T3, na.rm = TRUE),
    d_sd   = sd(T3, na.rm = TRUE),
    d_range = d_max - d_min,
    .groups = "drop"
  )

# 3. Calculate the 15 Summary Statistics
bioclim_summary <- daily_df %>%
  group_by(site_name, datalogger_ID) %>%
  summarise(
    # 1-3) Temperature Averages
    mean_temp = mean(d_mean),
    mean_daily_min = mean(d_min),
    mean_daily_max = mean(d_max),
    
    # 4-5) Extremes
    abs_min = min(d_min),
    abs_max = max(d_max),
    
    # 6-7) Variability
    mean_daily_sd = mean(d_sd),
    mean_daily_range = mean(d_range),
    
    # 8-11) GDD (Growing Degree Days) - Sum of (Mean - Threshold)
    GDD_0 = sum(pmax(d_mean - 0, 0)),
    GDD_2 = sum(pmax(d_mean - 2, 0)),
    GDD_5 = sum(pmax(d_mean - 5, 0)),
    GDD_10 = sum(pmax(d_mean - 10, 0)),
    
    # 12-15) THD (Threshold Days) - Count of days above Threshold
    THD_0 = sum(d_mean > 0),
    THD_2 = sum(d_mean > 2),
    THD_5 = sum(d_mean > 5),
    THD_10 = sum(d_mean > 10),
    
    .groups = "drop"
  )

# 4. Save to Outputs
write_excel_csv(bioclim_summary, "Outputs/Microclimate_Indicators_Summary.csv")

cat("Success! 15 indicators calculated for all 32 loggers.\n")

### 03_Normalize_and_Plot.R
setwd("~/AOPK-Microclimate")
library(tidyverse)

# 1. Load the summary data
summary_data <- read_csv("Outputs/Microclimate_Indicators_Summary.csv")

# 2. Normalize GDD and THD
# We calculate the number of days by looking at the THD_0 (or a similar count)
# To be safe, we'll calculate the actual span of days per logger.
normalized_data <- summary_data %>%
  # We use THD_0 as a proxy for total days sampled, or you can use a custom day count
  mutate(
    total_days = THD_0 + (abs_min <= 0), # Simple way to get total days if THD_0 is days > 0
    GDD_5_norm = GDD_5 / total_days,
    GDD_10_norm = GDD_10 / total_days,
    THD_10_perc = (THD_10 / total_days) * 100 # Percentage of days above 10°C
  )

# 3. Create a Boxplot of Daily Amplitude (Indicator #7)
# This is usually the most significant difference between forest sites
ggplot(normalized_data, aes(x = site_name, y = mean_daily_range, fill = site_name)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.1, alpha = 0.5) + # Shows the individual loggers as dots
  theme_minimal() +
  labs(
    title = "Comparison of Daily Temperature Amplitude by Site",
    subtitle = "Indicator #7: Mean of the Daily Range (Tmax - Tmin)",
    x = "Site Name",
    y = "Temperature Range (°C)",
    fill = "Site"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 4. Save the Normalized Data
write_excel_csv(normalized_data, "Outputs/Normalized_Microclimate_Indicators.csv")

# 5. Save the Plot
ggsave("Outputs/Site_Comparison_Amplitude.png", width = 10, height = 6)
