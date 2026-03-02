##################################
## Mon 2 March 2026
## VISUALISE DOY OVERLAP
##################################
### BEFORE - UNEDITED SCRIPT
# ---------- Static ggplot Gantt chart ----------
p_gantt <- ggplot(deploy_for_plot) +
  geom_segment(
    aes(x = deployment_date, xend = retrieval_date, y = pid, yend = pid, color = device_code),
    size = 6, lineend = "round"
  ) +
  # start/end markers
  geom_point(aes(x = deployment_date, y = pid), size = 1.8, alpha = 0.9) +
  geom_point(aes(x = retrieval_date, y = pid), size = 1.8, alpha = 0.9, shape = 21) +
  # facet per partner (each partner gets its own small y axis)
  facet_wrap(~partner, scales = "free_y", ncol = 2) +
  labs(
    title = "Device deployment Gantt chart",
    subtitle = paste0("Bars show operational period; ongoing deployments shown up to ", as.character(as_of)),
    x = "Date",
    y = NULL,
    color = "Device type"
  ) +
  scale_x_date(
    expand = c(0, 0),
    date_breaks = "1 month",
    date_labels = "%b %Y"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(size = 8), # device labels
    strip.text = element_text(face = "bold"), # partner facet titles
    panel.grid.major.y = element_blank(),
    legend.position = "bottom"
  )

# Print static plot
print(p_gantt)

# ---------- Filter only AMI devices ----------
deploy_for_plot_ami <- deploy_for_plot %>%
  filter(device_code == "AMI") # if your AMI type has a different spelling, change here

date_min <- min(deploy_for_plot_ami$deployment_date, na.rm = TRUE)
date_max <- max(deploy_for_plot_ami$retrieval_date, na.rm = TRUE)

# add ±7 days
date_min <- date_min - lubridate::days(14)
date_max <- date_max + lubridate::days(14)

# ---------- One long panel, no facets ----------
p_gantt_ami <- ggplot(deploy_for_plot_ami) +
  geom_segment(
    aes(x = deployment_date, xend = retrieval_date, y = pid, yend = pid, color = type_code),
    size = 6, lineend = "round"
  ) +
  scale_color_manual(values = okabe_ito) +
  labs(
    title = "AMI deployment timeline\n",
    x = "\nDate",
    y = NULL,
    color = "Device type: "
  ) +
  scale_x_date(
    limits = c(date_min, date_max),
    expand = c(0, 0),
    date_breaks = "1 month",
    date_labels = "%b %Y"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.y = element_text(size = 8),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom"
  )

print(p_gantt_ami)

# count number of rows (devices)
n_devices <- nrow(deploy_for_plot_ami)

# choose how much vertical space each device gets
# 0.35–0.5 usually works well depending on label length
height_inches <- n_devices * 0.2

ggsave(
  filename = "Outputs/Plots/ami_deployments.png",
  plot = p_gantt_ami,
  width = 14, # adjust if needed
  height = height_inches, # scales automatically
  dpi = 300
)


# ---------- Filter only acoustic devices ----------
deploy_for_plot_acoustic <- deploy_for_plot %>%
  filter(grepl("Mini", device_code)) # if your AMI type has a different spelling, change here

date_min <- min(deploy_for_plot_acoustic$deployment_date, na.rm = TRUE)
date_max <- max(deploy_for_plot_acoustic$retrieval_date, na.rm = TRUE)

# add ±7 days
date_min <- date_min - lubridate::days(14)
date_max <- date_max + lubridate::days(14)

# ---------- One long panel, no facets ----------
p_gantt_acoustic <- ggplot(deploy_for_plot_acoustic) +
  geom_segment(
    aes(x = deployment_date, xend = retrieval_date, y = pid, yend = pid, color = type_code),
    size = 6, lineend = "round"
  ) +
  scale_color_manual(values = okabe_ito) +
  labs(
    title = "Acoustic deployment timeline\n",
    x = "\nDate",
    y = NULL,
    color = "Device type: "
  ) +
  scale_x_date(
    limits = c(date_min, date_max),
    expand = c(0, 0),
    date_breaks = "1 month",
    date_labels = "%b %Y"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.y = element_text(size = 8),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom"
  )

print(p_gantt_acoustic)

# count number of rows (devices)
n_devices <- nrow(deploy_for_plot_acoustic)

# choose how much vertical space each device gets
# 0.35–0.5 usually works well depending on label length
height_inches <- n_devices * 0.1

ggsave(
  filename = "Outputs/Plots/acoustic_deployments.png",
  plot = p_gantt_acoustic,
  width = 14, # adjust if needed
  height = height_inches, # scales automatically
  dpi = 300
)
##################################
### AFTER - EDITED SCRIPT 1
### DO NOT USE - SKIP TO 3

setwd("~/AOPK-Microclimate")

# ---------- Static ggplot Gantt chart ----------
library(tidyverse)
library(lubridate)

# Load your specific file
df <- read_csv("Outputs/Compiled_Microclimate_Data.csv")

# NECESSARY ADJUSTMENTS: 
# 1. Parse the 2023.05.31 00:00 format
# 2. Prepare the 'deploy_for_plot' object using your exact column names
deploy_for_plot <- df %>%
  mutate(datetime_utc = ymd_hm(datetime_utc)) %>%
  filter(!is.na(datetime_utc)) %>%
  mutate(year = year(datetime_utc)) %>%
  group_by(datalogger_ID, site_name, year) %>%
  summarise(
    deployment_date = as.Date(min(datetime_utc)),
    retrieval_date = as.Date(max(datetime_utc)),
    .groups = "drop"
  ) %>%
  # Mapping your columns to your boss's variable names
  mutate(pid = paste(datalogger_ID, year, sep = "_"), # Y-axis
         device_code = site_name)                   # Legend/Color

# Highlight coordinates for the growing period
season_start <- as.Date("2023-04-01")
season_end   <- as.Date("2023-09-30")

p_gantt <- ggplot(deploy_for_plot) +
  # Visual highlight for the April-Sept window
  annotate("rect", xmin = season_start, xmax = season_end, 
           ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "green") +
  geom_segment(
    aes(x = deployment_date, xend = retrieval_date, y = pid, yend = pid, color = device_code),
    linewidth = 6, lineend = "round"
  ) +
  # start/end markers
  geom_point(aes(x = deployment_date, y = pid), size = 1.8, alpha = 0.9) +
  geom_point(aes(x = retrieval_date, y = pid), size = 1.8, alpha = 0.9, shape = 21) +
  # labels and styling from boss's original script
  labs(
    title = "Device deployment Gantt chart",
    subtitle = "Green box shows target growing period: April 1 - Sept 30",
    x = "Date",
    y = "Datalogger ID & Year",
    color = "Site Name"
  ) +
  scale_x_date(
    expand = c(0, 0),
    date_breaks = "1 month",
    date_labels = "%b %Y"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(size = 8), 
    panel.grid.major.y = element_blank(),
    legend.position = "bottom"
  )

# Print static plot
print(p_gantt)

# count number of rows (devices) for automatic height calculation
n_devices <- nrow(deploy_for_plot)

# choose how much vertical space each device gets
height_inches <- n_devices * 0.25

# Save the plot to your Outputs folder
ggsave(
  filename = "Outputs/Plots/microclimate_deployments.png",
  plot = p_gantt,
  width = 14, 
  height = max(5, height_inches), # Ensures it's at least 5 inches tall
  dpi = 300
)

##################################
### AFTER - EDITED SCRIPT 2
### DO NOT USE - SKIP TO 3

# ---------- Updated Gantt Chart Script ----------
library(tidyverse)
library(lubridate)

# Load data
df <- read_csv("Outputs/Compiled_Microclimate_Data.csv") %>%
  mutate(datetime_utc = ymd_hm(datetime_utc)) %>%
  filter(!is.na(datetime_utc)) %>%
  mutate(year = year(datetime_utc))

# Prepare plot data
deploy_for_plot <- df %>%
  group_by(datalogger_ID, site_name, year) %>%
  summarise(
    deployment_date = as.Date(min(datetime_utc)),
    retrieval_date = as.Date(max(datetime_utc)),
    .groups = "drop"
  ) %>%
  mutate(pid = as.character(datalogger_ID)) # Just use ID; facet will handle site

# Define Growing Periods for multiple years
# Adjust years as needed based on your data range
growing_periods <- data.frame(
  start = as.Date(c("2022-04-01", "2023-04-01", "2024-04-01", "2025-04-01")),
  end   = as.Date(c("2022-09-30", "2023-09-30", "2024-09-30", "2025-09-30"))
)

p_gantt <- ggplot(deploy_for_plot) +
  # Add multiple green bars for each year's growing season
  geom_rect(data = growing_periods, 
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            fill = "green", alpha = 0.1, inherit.aes = FALSE) +
  # Bars
  geom_segment(
    aes(x = deployment_date, xend = retrieval_date, y = pid, yend = pid, color = site_name),
    linewidth = 5, lineend = "round"
  ) +
  geom_point(aes(x = deployment_date, y = pid), size = 1.5) +
  geom_point(aes(x = retrieval_date, y = pid), size = 1.5, shape = 21, fill = "white") +
  # SEPARATE BY SITE (Vertical orientation)
  facet_grid(site_name ~ ., scales = "free_y", space = "free_y") +
  labs(
    title = "Microclimate Deployment Timeline",
    subtitle = "Green shaded areas: April 1 - Sept 30 (Multiple Years)",
    x = "Date",
    y = "Datalogger ID",
    color = "Site Name"
  ) +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b %Y",
    expand = c(0.02, 0)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate dates to prevent overlap
    axis.text.y = element_text(size = 7),
    strip.text.y = element_text(angle = 0, face = "bold", size = 10), # Site labels on the right
    panel.grid.major.y = element_blank(),
    panel.spacing = unit(1, "lines"), # Add space between sites
    legend.position = "none" # Site names are already on the side facets
  )

# SAVE AS A TALL SCROLLABLE FILE
n_loggers <- length(unique(deploy_for_plot$datalogger_ID))
# Increase 0.4 to make it even taller/more spread out
save_height <- max(8, n_loggers * 0.4) 

ggsave(
  filename = "Outputs/Plots/microclimate_deployments_V2.png",
  plot = p_gantt,
  width = 12,
  height = save_height,
  dpi = 300
)

print(p_gantt)

##################################
##################################
##################################
### AFTER - EDITED SCRIPT 3
### FINAL 

# ---------- Vertical Scrolling Gantt V3 ----------
library(tidyverse)
library(lubridate)

# Load data - ensure all sites are present
df <- read_csv("Outputs/Compiled_Microclimate_Data.csv") %>%
  mutate(datetime_utc = ymd_hm(datetime_utc)) %>%
  filter(!is.na(datetime_utc))

# Check: Run 'unique(df$site_name)' in console to see if all 4 sites are here
# If not, the issue is in your Compiled CSV or the formatting of those specific sites

deploy_for_plot <- df %>%
  group_by(datalogger_ID, site_name) %>% # Removed 'year' to keep one long bar per ID
  summarise(
    deployment_date = as.Date(min(datetime_utc)),
    retrieval_date = as.Date(max(datetime_utc)),
    .groups = "drop"
  )

# Define all 4 growing periods (2022-2025)
growing_periods <- data.frame(
  start = as.Date(c("2022-04-01", "2023-04-01", "2024-04-01", "2025-04-01")),
  end   = as.Date(c("2022-09-30", "2023-09-30", "2024-09-30", "2025-09-30"))
)

p_gantt_v3 <- ggplot(deploy_for_plot) +
  # Highlight boxes
  geom_rect(data = growing_periods, 
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            fill = "green", alpha = 0.1, inherit.aes = FALSE) +
  # Bars
  geom_segment(aes(x = deployment_date, xend = retrieval_date, 
                   y = as.character(datalogger_ID), yend = as.character(datalogger_ID), 
                   color = site_name),
               linewidth = 6, lineend = "round") +
  # Month markers
  geom_point(aes(x = deployment_date, y = as.character(datalogger_ID)), size = 1.2) +
  geom_point(aes(x = retrieval_date, y = as.character(datalogger_ID)), size = 1.2, shape = 21, fill = "white") +
  # Facet by Site (This should show all 4 if they exist in your data)
  facet_grid(site_name ~ ., scales = "free_y", space = "free_y") +
  labs(title = "Microclimate Deployment Timeline (All Sites)",
       x = "Date", y = "Datalogger ID") +
  # MONTHLY LABELS: breaks = 3 months, minor_breaks = 1 month for the grid lines
  scale_x_date(
    date_breaks = "3 months", 
    minor_breaks = "1 month", 
    date_labels = "%b %y",
    expand = c(0, 0)
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8), # Smaller text
    axis.text.y = element_text(size = 7),
    panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.2), # Monthly lines
    panel.grid.major.y = element_blank(),
    strip.text.y = element_text(angle = 0, face = "bold"),
    legend.position = "none"
  )

# Save very tall for scrolling
n_ids <- length(unique(deploy_for_plot$datalogger_ID))
ggsave("Outputs/Plots/microclimate_deployments_V3.png", 
       plot = p_gantt_v3, width = 12, height = max(7, n_ids * 0.3), dpi = 300)

