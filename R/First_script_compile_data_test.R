# 23 Feb 2026

setwd("~/AOPK-Microclimate")

# 1. Load libraries
library(tidyverse)
library(vroom)

# 2. Define Paths
root_path <- "/Users/megan/AOPK-Microclimate/Data/Výstupy_z_dataloggerů"
output_path <- "/Users/megan/AOPK-Microclimate/Outputs"

# 3. Find the "data_" CSV files
# Note: I'm using the root_path directly; R will find the subfolders automatically
file_list <- list.files(path = root_path, 
                        pattern = "data_.*\\.csv$", 
                        recursive = TRUE, 
                        full.names = TRUE)

# 4. Define TOMST Column Names
tomst_colnames <- c("index", "datetime_utc", "timezone", "T1", "T2", "T3", "moisture_raw", "shake", "errFlag")

# 5. Read and Combine
combined_data <- vroom(file_list, 
                       id = "source_path", 
                       delim = ";", 
                       col_names = tomst_colnames,
                       col_types = cols(.default = "c")) 

# 6. Extract Info and Reorder Columns
final_data <- combined_data %>%
  mutate(
    # Pull the 2-digit logger number from the folder path
    datalogger_ID = str_extract(source_path, "/\\d{2}/") %>% str_replace_all("/", ""),
    
    # Clean Site Names
    site_name = case_when(
      str_detect(source_path, "Drbákov") ~ "Drbakov",
      str_detect(source_path, "Karlštejn") ~ "Karlstejn",
      str_detect(source_path, "Bojanovická") ~ "Bojanovicka_Alej",
      str_detect(source_path, "Hovoranská") ~ "Hovoranska_Cesta",
      TRUE ~ "Other"
    )
  ) %>%
  # --- THE COLUMN ORDER HAPPENS HERE ---
  # We put datalogger_ID first, then site_name, then everything else
  select(datalogger_ID, site_name, all_of(tomst_colnames))

# 7. Save the file
write_csv(final_data, file.path(output_path, "Compiled_Microclimate_Data.csv"))

message("Success! File saved with datalogger_ID in the first column.")

## DIAGNOSTIC PLOTS

# 1. Load libraries
library(tidyverse)
library(lubridate)

# 2. Path
output_path <- "/Users/megan/AOPK-Microclimate/Outputs"

# 3. Load the compiled data
df <- read_csv(file.path(output_path, "Compiled_Microclimate_Data.csv"), 
               col_types = cols(.default = "c"))

# 4. Open the PDF device
pdf(file.path(output_path, "Diagnostic_Start_End_Dates.pdf"), width = 11, height = 8)

# 5. Fix Site Names and Get unique list
# We use regex (regex(..., ignore_case = TRUE)) to be more "forgiving"
logger_list <- df %>% 
  mutate(site_name = case_when(
    str_detect(site_name, regex("Drbakov|Drbákov", ignore_case = TRUE)) ~ "Drbakov",
    str_detect(site_name, regex("Karlstejn|Karlštejn", ignore_case = TRUE)) ~ "Karlstejn",
    str_detect(site_name, regex("Bojanovická|Bojanovicka", ignore_case = TRUE)) ~ "Bojanovicka_Alej",
    str_detect(site_name, regex("Hovoranská|Hovoranska", ignore_case = TRUE)) ~ "Hovoranska_Cesta",
    TRUE ~ site_name # Keep original if no match
  )) %>%
  distinct(site_name, datalogger_ID) %>% 
  arrange(site_name, datalogger_ID)

# 6. Loop through each logger
for(i in 1:nrow(logger_list)){
  curr_id   <- logger_list$datalogger_ID[i]
  curr_site <- logger_list$site_name[i]
  
  plot_data <- df %>% 
    filter(datalogger_ID == curr_id) %>%
    mutate(
      plot_time = ymd_hm(datetime_utc),
      T1 = as.numeric(T1),
      T3 = as.numeric(T3)
    )
  
  if(all(is.na(plot_data$plot_time))) next
  
  p <- ggplot(plot_data, aes(x = plot_time)) +
    geom_line(aes(y = T1, color = "Soil (T1)"), linewidth = 0.5) +
    geom_line(aes(y = T3, color = "Air (T3)"), alpha = 0.3, linewidth = 0.5) +
    scale_color_manual(values = c("Soil (T1)" = "darkgreen", "Air (T3)" = "red")) +
    scale_x_datetime(date_breaks = "3 months", date_labels = "%b %Y") +
    labs(title = paste("Site:", curr_site, "| Logger ID:", curr_id),
         subtitle = "Check for the 'smooth' Green line (T1) to confirm burial date",
         x = "Time", y = "Temperature (°C)") +
    theme_minimal() +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
}

dev.off()

message("Diagnostic_Start_End_Dates.pdf created. Site names should now be corrected!")

# INTERACTIVE PLOTS USING plotly::

  # 1. Load libraries
  if (!require("plotly")) install.packages("plotly")
library(tidyverse)
library(lubridate)
library(plotly)

# 2. Path & Data Loading
output_path <- "/Users/megan/AOPK-Microclimate/Outputs"
df <- read_csv(file.path(output_path, "Compiled_Microclimate_Data.csv"), 
               col_types = cols(.default = "c"))

# 3. Filter for Drbákov loggers and convert types
drbakov_data <- df %>%
  filter(datalogger_ID %in% c("15", "38")) %>%
  mutate(
    plot_time = ymd_hm(datetime_utc),
    T1 = as.numeric(T1),
    T2 = as.numeric(T2),
    T3 = as.numeric(T3)
  )

# 4. Create the interactive plot
# We use facet_wrap to see both loggers at once, or you can look at one at a time
p <- ggplot(drbakov_data, aes(x = plot_time)) +
  geom_line(aes(y = T1, color = "Soil (T1)"), linewidth = 0.5) +
  geom_line(aes(y = T2, color = "Surface (T2)"), alpha = 0.5) +
  geom_line(aes(y = T3, color = "Air (T3)"), alpha = 0.3) +
  scale_color_manual(values = c("Soil (T1)" = "darkgreen", 
                                "Surface (T2)" = "orange", 
                                "Air (T3)" = "red")) +
  facet_wrap(~datalogger_ID, ncol = 1) +
  labs(title = "Drbákov Loggers: Zoom in to find Burial Date",
       x = "Time", y = "Temp °C") +
  theme_minimal()

# 5. Convert to Plotly and Launch
# This will open in your 'Viewer' pane in RStudio or your web browser
ggplotly(p)
