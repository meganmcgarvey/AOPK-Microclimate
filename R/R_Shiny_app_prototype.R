# 23 Feb 2026

setwd("~/AOPK-Microclimate")

install.packages("leaflet")
install.packages("lubridate")
install.packages("DT")
install.packages("bslib")

library(shiny)
library(leaflet)
library(tidyverse)
library(lubridate)
library(DT)
library(bslib)

# --- 1. DATA LOADING ---
# Load metadata and raw data at startup
metadata <- read_csv("Data/GPS_dates.csv") %>%
  mutate(start_date = ymd(start_date), end_date = ymd(end_date))

raw_data <- read_csv("Outputs/Compiled_Microclimate_Data.csv", col_types = cols(.default = "c"))

# --- 2. UI ---
ui <- navbarPage(
  title = "AOPK Microclimate Portal",
  theme = bs_theme(bootswatch = "flatly"),
  
  tabPanel("Map & Data View",
           sidebarLayout(
             sidebarPanel(
               h4("Filter Data"),
               selectInput("site_select", "Select Locality:", choices = c("All", unique(metadata$site_name))),
               uiOutput("id_selector"),
               hr(),
               h4("Export Data"),
               downloadButton("download_data", "Export Trimmed CSV", class = "btn-success")
             ),
             mainPanel(
               leafletOutput("map", height = "400px"),
               br(),
               h4("Data Preview (First 100 rows)"),
               DTOutput("data_preview")
             )
           )
  ),
  
  tabPanel("Upload New Data",
           sidebarLayout(
             sidebarPanel(
               h4("New Data Submission"),
               textInput("user_name_upload", "Your Name:", placeholder = "e.g. Megan"),
               fileInput("file_upload", "Choose TOMST data_*.csv file", accept = ".csv"),
               helpText("The system will extract the Datalogger ID from the filename."),
               actionButton("process_btn", "Process Upload", class = "btn-primary")
             ),
             mainPanel(
               h4("Upload Status"),
               verbatimTextOutput("upload_log"),
               uiOutput("new_logger_form")
             )
           )
  )
)

# --- 3. SERVER ---
server <- function(input, output, session) {
  
  # --- TAB 1 LOGIC: MAP & PREVIEW ---
  output$id_selector <- renderUI({
    ids <- if(input$site_select == "All") metadata$datalogger_ID else 
      metadata %>% filter(site_name == input$site_select) %>% pull(datalogger_ID)
    selectInput("id_select", "Select Data Logger ID:", choices = c("All", ids))
  })
  
  filtered_df <- reactive({
    req(input$site_select)
    
    site_meta <- metadata
    if(input$site_select != "All") site_meta <- site_meta %>% filter(site_name == input$site_select)
    if(!is.null(input$id_select) && input$id_select != "All") site_meta <- site_meta %>% filter(datalogger_ID == input$id_select)
    
    # Ensure ID match is treated as character to avoid join errors
    res <- raw_data %>%
      filter(datalogger_ID %in% site_meta$datalogger_ID) %>%
      left_join(site_meta, by = "datalogger_ID") %>%
      mutate(datetime_parsed = ymd_hm(datetime_utc)) %>%
      filter(datetime_parsed >= (as.Date(start_date) + 1) & 
               datetime_parsed <= as.Date(end_date)) %>%
      select(-datetime_parsed, -latitude, -longitude) # Cleaning for table display
    
    return(res)
  })
  
  output$map <- renderLeaflet({
    leaflet(metadata) %>%
      addTiles() %>%
      addMarkers(lng = ~longitude, lat = ~latitude, 
                 popup = ~paste0("<b>Site:</b> ", site_name, "<br><b>ID:</b> ", datalogger_ID))
  })
  
  output$data_preview <- renderDT({
    datatable(head(filtered_df(), 100), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$download_data <- downloadHandler(
    filename = function() { paste0("Trimmed_Data_", input$site_select, ".csv") },
    content = function(file) { write_csv(filtered_df(), file) }
  )
  
  # --- TAB 2 LOGIC: UPLOAD ---
  observeEvent(input$process_btn, {
    req(input$file_upload)
    
    # Extract Serial Number from filename (e.g. data_94123456_0.csv)
    fname <- input$file_upload$name
    extracted_id <- str_extract(fname, "(?<=data_)\\d+")
    
    output$upload_log <- renderText({
      if(is.na(extracted_id)) return("Error: Filename must follow 'data_XXXXXXXX_0.csv' format.")
      paste0("User: ", input$user_name_upload, 
             "\nDetected Logger ID: ", extracted_id, 
             "\nStatus: File read successfully. Processing metadata check...")
    })
    
    # Check if this ID already exists in our metadata
    if(!(extracted_id %in% metadata$datalogger_ID)) {
      output$new_logger_form <- renderUI({
        wellPanel(
          h5("New Datalogger Detected!", style = "color: red;"),
          p("This ID is not in the system. Please provide details:"),
          textInput("new_site_name", "Locality Name:"),
          numericInput("new_lat", "Latitude:", value = 0),
          numericInput("new_lon", "Longitude:", value = 0),
          actionButton("save_new_meta", "Register Datalogger", class = "btn-warning")
        )
      })
    }
  })
}

shinyApp(ui, server)
