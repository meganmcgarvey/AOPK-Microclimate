###########################
##  Fri 24 April 2026
##  R Shiny App v16
## AOPK Datalogger Portal
###########################

library(shiny)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata) 
library(leaflet)
library(shinyjs)
library(rdrop2)
library(httr)
library(fontawesome)

# --- 1. CONFIGURATION ---
options(shiny.maxRequestSize = 30 * 1024^2) 

if(file.exists("droptoken.rds")) {
  message("--- ATTEMPTING DROPBOX AUTH ---")
  drop_auth(rdstoken = "droptoken.rds")
  message("--- DROPBOX AUTH SUCCESSFUL ---")
}

outputDir <- "AOPK_Datalogger_Portal"

# --- 2. SPATIAL DATA ---
cz_border <- ne_countries(country = "Czechia", scale = "medium", returnclass = "sf")

# --- 3. DICTIONARY ---
dict <- list(
  app_title = c(cz = "AOPK ČR | Portál pro nahrávání dat z dataloggerů", en = "AOPK ČR | Datalogger Data Upload Portal"),
  instr = c(
    cz = "Nahrajte prosím soubor CSV exportovaný z dataloggeru. <b>Termín nasazení označuje dny, kdy byl přístroj aktivní v terénu.</b> Pole s <span style='color:red;'>*</span> jsou povinná. Mapu posouvejte tažením, kliknutím na bod vygenerujete GPS.",
    en = "Please upload the CSV file from the datalogger. <b>Deployment Period refers to the exact days the device was active in the field.</b> Fields marked <span style='color:red;'>*</span> are required. Drag the map to move, click a point to generate GPS."
  ),
  user_name = c(cz = "Uživatelské jméno <span style='color:red;'>*</span>", en = "User Name <span style='color:red;'>*</span>"),
  file = c(cz = "Nahrajte soubor CSV (.csv) <span style='color:red;'>*</span>", en = "Upload CSV File (.csv) <span style='color:red;'>*</span>"),
  dates = c(cz = "Termín nasazení <span style='color:red;'>*</span>", en = "Deployment Period <span style='color:red;'>*</span>"),
  coords = c(cz = "GPS Souřadnice <span style='color:red;'>*</span>", en = "GPS Coordinates <span style='color:red;'>*</span>"),
  notes = c(cz = "Poznámky (volitelné)", en = "Notes (optional)"),
  upload_btn = c(cz = "NAHRÁT DATA", en = "UPLOAD DATA"),
  footer_agency = c(cz = "Agentura ochrany přírody a krajiny České republiky", en = "Nature Conservation Agency of the Czech Republic"),
  map_err = c(cz = "Bod musí ležet v České republice!", en = "Point must be within the Republic!"),
  success_label = c(cz = "Data byla úspěšně nahrána.", en = "Data successfully uploaded."),
  error_label = c(cz = "Chyba při nahrávání.", en = "Upload error.")
)

# --- 4. UI ---
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      body, html { margin: 0; padding: 0; height: 100vh; overflow: hidden; font-family: 'Helvetica Neue', Arial, sans-serif; }
      .wrapper { display: grid; grid-template-rows: 75px 1fr 100px; grid-template-columns: 460px 1fr; height: 100vh; width: 100vw; }
      #header { grid-column: 1 / span 2; background-color: #2b5d3f; color: white; display: flex; justify-content: space-between; align-items: center; padding: 0 25px; }
      #sidebar { grid-column: 1; grid-row: 2; background: white; border-right: 2px solid #2b5d3f; padding: 25px; overflow-y: auto; }
      #map-container { grid-column: 2; grid-row: 2; position: relative; }
      #footer { grid-column: 1 / span 2; background-color: #5d524d; color: white; display: flex; flex-direction: column; justify-content: center; align-items: center; text-align: center; }
      .app-title-text { font-size: 1.8rem; }
      .instr-text { font-size: 1.1rem; color: #333; line-height: 1.4; margin-bottom: 20px; }
      .footer-main-line { font-size: 1.3rem; margin-bottom: 4px; }
      .footer-main-line a { color: white; text-decoration: underline; }
      .footer-copyright { font-size: 0.9rem; opacity: 0.8; }
      .form-control { font-size: 1.2rem !important; border-radius: 4px; padding: 8px 12px; }
      .form-control::placeholder { color: #aaaaaa !important; font-size: 1.1rem !important; text-align: center; }
      #upload_btn { background-color: #d97036; color: white; border: none; font-weight: bold; width: 100%; padding: 15px; font-size: 1.3rem; border-radius: 4px; cursor: pointer; }
      #upload_btn:hover { background-color: #bf5e2a; }
      .flag-img { height: 40px; width: 40px; border-radius: 50%; border: 2px solid white; object-fit: cover; }
      #lat, #lon { text-align: center; }
    ")),
    tags$script(HTML("
      function setPlaceholders() {
        $('#deploy_dates input:first').attr('placeholder', 'START DATE');
        $('#deploy_dates input:last').attr('placeholder', 'END DATE');
        $('#lat').attr('placeholder', 'LATITUDE');
        $('#lon').attr('placeholder', 'LONGITUDE');
      }
      $(document).on('shiny:connected', function() { setInterval(setPlaceholders, 500); });
    "))
  ),
  
  div(class = "wrapper",
      div(id = "header", uiOutput("ui_title"), actionLink("lang_toggle", uiOutput("flag_ui"))),
      div(id = "sidebar",
          uiOutput("ui_instr"),
          uiOutput("ui_name"),
          uiOutput("ui_file"),
          tags$label(uiOutput("lbl_dates"), style="font-weight:bold; margin-top:10px;"),
          dateRangeInput("deploy_dates", label = NULL, separator = " — ", start = NA, end = NA),
          tags$label(uiOutput("lbl_coords"), style="font-weight:bold; margin-top: 15px;"),
          fluidRow(column(6, numericInput("lat", NULL, value = NA)), column(6, numericInput("lon", NULL, value = NA))),
          uiOutput("ui_notes"), br(), 
          actionButton("upload_btn", "UPLOAD DATA"),
          hr()
      ), # Sidebar close
      div(id = "map-container", leafletOutput("map", height = "100%")),
      div(id = "footer", uiOutput("ui_footer_content"))
  )
)

# --- 5. SERVER ---
server <- function(input, output, session) {
  lang <- reactiveVal("cz")
  observeEvent(input$lang_toggle, { if (lang() == "cz") lang("en") else lang("cz") })
  
  output$ui_title <- renderUI({
    parts <- strsplit(dict$app_title[[lang()]], " \\| ")[[1]]
    div(class = "app-title-text", tags$b(parts[1]), paste0(" | ", parts[2]))
  })
  
  output$flag_ui <- renderUI({
    url <- if(lang() == "cz") "https://upload.wikimedia.org/wikipedia/en/a/ae/Flag_of_the_United_Kingdom.svg" else "https://upload.wikimedia.org/wikipedia/commons/c/cb/Flag_of_the_Czech_Republic.svg"
    tags$img(src = url, class = "flag-img")
  })
  
  output$ui_instr <- renderUI({ div(class="instr-text", HTML(dict$instr[lang()])) })
  output$ui_name <- renderUI({ textInput("user_name", HTML(dict$user_name[lang()])) })
  output$ui_file <- renderUI({ fileInput("csv_file", HTML(dict$file[lang()]), accept = ".csv") })
  output$lbl_dates <- renderUI({ HTML(dict$dates[lang()]) })
  output$lbl_coords <- renderUI({ HTML(dict$coords[lang()]) })
  output$ui_notes <- renderUI({ textAreaInput("notes", HTML(dict$notes[lang()]), height = "90px") })
  
  output$ui_footer_content <- renderUI({
    tagList(div(class = "footer-main-line", HTML(paste0(dict$footer_agency[lang()], " | <a href='https://www.nature.cz' target='_blank'>aopk.gov.cz</a>"))),
            div(class = "footer-copyright", "© 2026 AOPK ČR"))
  })
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 7, maxBounds = list(list(47.5, 11.0), list(52.0, 20.0)))) %>%
      addProviderTiles(providers$CartoDB.Voyager) %>%
      addPolylines(data = cz_border, color = "#2b5d3f", weight = 3, opacity = 1) %>%
      setView(lng = 15.47, lat = 49.81, zoom = 7)
  })
  
  observeEvent(input$map_click, {
    click <- input$map_click
    pt <- st_point(c(click$lng, click$lat)) %>% st_sfc(crs = 4326) %>% st_sf()
    if (nrow(st_filter(pt, cz_border)) > 0) {
      updateNumericInput(session, "lat", value = round(click$lat, 5))
      updateNumericInput(session, "lon", value = round(click$lng, 5))
      leafletProxy("map") %>% clearGroup("selection") %>% addMarkers(lng = click$lng, lat = click$lat, group = "selection")
    } else { showNotification(dict$map_err[lang()], type = "warning") }
  })
  
  observeEvent(input$upload_btn, {
    if(is.null(input$csv_file) || is.na(input$lat) || input$user_name == "") {
      showNotification("Missing Information / Chybějící údaje", type = "error")
      return()
    }
    
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    clean_user <- gsub("[^[:alnum:]]", "_", input$user_name)
    raw_filename <- paste0("DATA_", clean_user, "_", timestamp, ".csv")
    meta_filename <- paste0("META_", clean_user, "_", timestamp, ".csv")
    
    local_raw_path <- file.path(tempdir(), raw_filename)
    file.copy(input$csv_file$datapath, local_raw_path)
    
    metadata <- data.frame(
      User = input$user_name,
      Start_Date = as.character(input$deploy_dates[1]),
      End_Date = as.character(input$deploy_dates[2]),
      Lat = input$lat,
      Lon = input$lon,
      Notes = input$notes,
      Original_File = input$csv_file$name
    )
    
    local_meta_path <- file.path(tempdir(), meta_filename)
    write.csv(metadata, local_meta_path, row.names = FALSE)
    
    tryCatch({
      drop_upload(local_raw_path, path = outputDir)
      drop_upload(local_meta_path, path = outputDir)
      
      showNotification(dict$success_label[lang()], type = "message")
      
      green_icon <- makeAwesomeIcon(
        icon = "check", 
        markerColor = "green", 
        library = "fa"
      )
      
      leafletProxy("map") %>% 
        clearGroup("selection") %>% 
        addAwesomeMarkers(
          lng = input$lon, 
          lat = input$lat, 
          icon = green_icon,
          label = dict$success_label[lang()], 
          group = "upload_markers"
        )
      
    }, error = function(e) {
      message("--- DROPBOX UPLOAD ERROR ---")
      print(e)
      showNotification(dict$error_label[lang()], type = "error")
    })
  })
}

shinyApp(ui, server)