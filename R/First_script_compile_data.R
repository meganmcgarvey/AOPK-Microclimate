# 23 Feb 2026

setwd("~/AOPK-Microclimate")

library(tidyverse)
library(lubridate)

# 1. THE LOOKUP TABLE (2-digit short IDs)
site_lookup <- tribble(
  ~site_name, ~short_id,
  "Drbákov", "15",
  "Drbákov", "38",
  "Bojanovická alej - Hodonínská Doubrava", "37",
  "Bojanovická alej - Hodonínská Doubrava", "39",
  "Bojanovická alej - Hodonínská Doubrava", "40",
  "Bojanovická alej - Hodonínská Doubrava", "43",
  "Bojanovická alej - Hodonínská Doubrava", "44",
  "Bojanovická alej - Hodonínská Doubrava", "56",
  "Bojanovická alej - Hodonínská Doubrava", "57",
  "Bojanovická alej - Hodonínská Doubrava", "60",
  "Hovoranská cesta - Hodonínská Doubrava", "22",
  "Hovoranská cesta - Hodonínská Doubrava", "24",
  "Hovoranská cesta - Hodonínská Doubrava", "26",
  "Hovoranská cesta - Hodonínská Doubrava", "27",
  "Hovoranská cesta - Hodonínská Doubrava", "28",
  "Hovoranská cesta - Hodonínská Doubrava", "29",
  "Hovoranská cesta - Hodonínská Doubrava", "30",
  "Hovoranská cesta - Hodonínská Doubrava", "36",
  "Koda (Vysoká stráň) - Karlštejn", "11",
  "Koda (Vysoká stráň) - Karlštejn", "12",
  "Koda (Vysoká stráň) - Karlštejn", "23",
  "Koda (Vysoká stráň) - Karlštejn", "31",
  "Koda (Vysoká stráň) - Karlštejn", "32",
  "Koda (Vysoká stráň) - Karlštejn", "33",
  "Koda (Vysoká stráň) - Karlštejn", "34",
  "Koda (Vysoká stráň) - Karlštejn", "35",
  "Koda (Vysoká stráň) - Karlštejn", "41",
  "Koda (Vysoká stráň) - Karlštejn", "42",
  "Koda (Vysoká stráň) - Karlštejn", "52",
  "Koda (Vysoká stráň) - Karlštejn", "53",
  "Koda (Vysoká stráň) - Karlštejn", "54",
  "Koda (Vysoká stráň) - Karlštejn", "55",
  "Koda (Vysoká stráň) - Karlštejn", "58",
  "Koda (Vysoká stráň) - Karlštejn", "59"
)

# 2. IDENTIFY FILES (RECURSIVE)
data_folder <- "Data/" 
file_list <- list.files(path = data_folder, 
                        pattern = "data_.*\\.csv", 
                        full.names = TRUE, 
                        recursive = TRUE)

# 3. COMPILE AND EXTRACT IDs
compiled_data <- file_list %>%
  map_df(~{
    # Extract the full ID from filename
    full_id <- str_extract(basename(.x), "(?<=data_)\\d+")
    # Grab last 2 digits for site matching
    short_id_extracted <- str_sub(full_id, -2, -1)
    
    read_delim(.x, delim = ";", col_names = FALSE, show_col_types = FALSE, 
               locale = locale(encoding = "UTF-8")) %>%
      mutate(
        datalogger_ID = full_id,
        short_id = short_id_extracted
      ) %>%
      rename(
        index = X1, datetime_utc = X2, timezone = X3,
        T1 = X4, T2 = X5, T3 = X6,
        moisture_raw = X7, shake = X8, errFlag = X9
      )
  })

# 4. JOIN WITH THE LOOKUP TABLE
final_output <- compiled_data %>%
  left_join(site_lookup, by = "short_id") %>%
  select(
    datalogger_ID, site_name, index, datetime_utc, 
    timezone, T1, T2, T3, moisture_raw, shake, errFlag
  )

# 5. SAVE TO OUTPUTS FOLDER
# Create folder if it doesn't exist
if (!dir.exists("Outputs")) {
  dir.create("Outputs")
}

write_excel_csv(final_output, "Outputs/Compiled_Microclimate_Data.csv")

cat("Success! File saved to AOPK-Microclimate/Outputs/Compiled_Microclimate_Data.csv\n")
