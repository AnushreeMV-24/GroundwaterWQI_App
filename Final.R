# Load Libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(leaflet)
library(dplyr)
library(magrittr) 
library(ggplot2)
library(sf)
library(DT)
library(smwrGraphs)
library(smwrBase)
library(readr)
library(tidyr)
library(rmarkdown)
library(shinycssloaders)

# Custom color palette
iisc_palette <- c(
  primary = "#005A9C",
  secondary = "#00A86B",
  accent = "#4FC3F7",
  background = "#F0F4F8",
  text = "#2C3E50"
)

# Function to convert mg/L to meq/L
to_meq <- function(mgL, mol_weight, valence) {
  (mgL / mol_weight) * valence
}

# Sample water quality data for Bengaluru (mg/L)
bengaluru_data <- data.frame(
  Site = paste("Sample", 1:5),
  Ca = c(40, 35, 50, 60, 45),       
  Mg = c(12, 15, 10, 20, 18),
  Na = c(25, 30, 22, 28, 27),
  K  = c(2, 2.5, 2, 3, 2.8),
  Cl = c(30, 35, 28, 40, 33),
  SO4 = c(20, 25, 18, 22, 21),
  HCO3 = c(90, 100, 85, 110, 95)
)

# Convert to meq/L
bengaluru_meq <- transform(bengaluru_data,
                           Ca   = to_meq(Ca,   40.08, 2),
                           Mg   = to_meq(Mg,   24.31, 2),
                           NaK  = to_meq(Na + K, 22.99, 1),
                           Cl   = to_meq(Cl,   35.45, 1),
                           SO4  = to_meq(SO4,  96.06, 2),
                           HCO3 = to_meq(HCO3, 61.02, 1)
)

# WQI Calculation Function
calculate_wqi <- function(gpkg_path) {
  layer_name <- st_layers(gpkg_path)$name[1]
  water_sf <- st_read(gpkg_path, layer = layer_name, quiet = TRUE)
  
  if (sf::st_crs(water_sf)$epsg != 4326) {
    water_sf <- st_transform(water_sf, 4326)
  }
  
  if (!all(st_is_valid(water_sf))) {
    water_sf <- st_make_valid(water_sf)
  }
  
  if ("NA" %in% names(water_sf)) {
    names(water_sf)[names(water_sf) == "NA"] <- "Sodium"
  }
  
  standards <- list(
    TDS = list(St = 1000, Wi = 0.121),
    EC = list(St = 2500, Wi = 0.121),
    NITRATE = list(St = 50, Wi = 0.152),
    SULPHATE = list(St = 250, Wi = 0.121),
    CHLORIDE = list(St = 250, Wi = 0.093),
    BICARBONATE = list(St = 500, Wi = 0.152),
    FLUORIDE = list(St = 1.2, Wi = 0.030),
    CA = list(St = 100, Wi = 0.060),
    MG = list(St = 50, Wi = 0.060),
    Sodium = list(St = 200, Wi = 0.060),
    K = list(St = 20, Wi = 0.030)
  )
  
  water_sf$WQI <- NA_real_
  param_names <- names(standards)
  
  for (param in param_names) {
    if (!param %in% names(water_sf)) {
      water_sf[[param]] <- 0
    } else {
      water_sf[[param]][is.na(water_sf[[param]])] <- 0
    }
  }
  
  for (param in param_names) {
    qi_col <- paste0("qi_", param)
    sli_col <- paste0("SLi_", param)
    water_sf[[qi_col]] <- water_sf[[param]] / standards[[param]]$St
    water_sf[[sli_col]] <- water_sf[[qi_col]] * standards[[param]]$Wi
  }
  
  sli_cols <- paste0("SLi_", param_names)
  sli_values <- st_drop_geometry(water_sf)[, sli_cols]
  sli_values[] <- lapply(sli_values, as.numeric)
  water_sf$WQI <- rowSums(sli_values, na.rm = TRUE)
  
  water_sf$Quality <- cut(
    water_sf$WQI,
    breaks = c(-Inf, 0.5, 1, 2, 3, Inf),
    labels = c("Excellent", "Good", "Poor", "Very Poor", "Unsuitable"),
    right = FALSE
  )
  
  write.csv(st_drop_geometry(water_sf), "water_quality_output.csv", row.names = FALSE)
  return(water_sf)
}

# UI
ui <- dashboardPage(
  title = "Ground Water Assessment Dashboard",
  skin = "blue",
  
  dashboardHeader(
    title = tags$div(
      style = "display: flex; align-items: center; height: 60px;",
      tags$img(src = "sirpilogo white.avif", height = "30px", style = "margin-right: 10px;"),
      tags$img(src = "gdx_logo.png", height = "30px", style = "margin-right: 10px;"),
      tags$span("Ground Water Assessment Dashboard",
                style = "color: white; font-weight: bold; font-size: 18px; margin-left: 30px;")
    ),
    titleWidth = 650
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("File Upload", tabName = "upload", icon = icon("upload")),
      menuItem("Data Exploration", tabName = "data_explore", icon = icon("magnifying-glass")),
      menuItem("Ground Water Chemistry", tabName = "chemistry", icon = icon("flask"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML(sprintf('
        .skin-blue .main-header .logo { background-color: %s; color: white; }
        .skin-blue .main-header .navbar { background-color: %s; }
        body { background-color: %s; color: %s; }
        .box { border-top-color: %s; }
      ', iisc_palette["primary"], iisc_palette["secondary"],
                              iisc_palette["background"], iisc_palette["text"],
                              iisc_palette["accent"])))),
    
    tabItems(
      tabItem(tabName = "upload",
              fluidRow(
                box(title = "Upload GPKG File", status = "primary", solidHeader = TRUE,
                    fileInput("gpkg_upload", "Choose GPKG File", accept = c(".gpkg")),
                    actionButton("load_data", "Load Data", icon = icon("database"))
                ),
                box(title = "File Information", status = "success", solidHeader = TRUE,
                    verbatimTextOutput("file_info"))
              )
      ),
      
      tabItem(tabName = "data_explore",
              fluidRow(
                box(title = "Dataset Overview", status = "primary", solidHeader = TRUE, uiOutput("dataset_summary")),
                box(title = "Column Details", status = "success", solidHeader = TRUE, DTOutput("column_details"))
              ),
              fluidRow(
                box(title = "Data Preview", status = "warning", solidHeader = TRUE, DTOutput("data_preview")),
                box(title = "Data Statistics", status = "info", solidHeader = TRUE, uiOutput("data_statistics"))
              )
      ),
      
      tabItem(tabName = "chemistry",
              fluidRow(
                box(title = "Filters", status = "info", solidHeader = TRUE, width = 12,
                    fluidRow(
                      column(4, uiOutput("state_ui")),
                      column(4, uiOutput("district_ui")),
                      column(4, uiOutput("block_ui"))
                    ),
                    br(),
                    uiOutput("download_chemistry_ui")
                )
              ),
              fluidRow(
                box(title = "Piper Plot", status = "primary", solidHeader = TRUE, width = 12,
                    withSpinner(plotOutput("piperPlot", height = "700px"), type = 6)
                )
              ),
              fluidRow(
                box(title = "Chemical Composition", status = "success", solidHeader = TRUE, width = 12,
                    p("Chemical composition visualizations to be added...")
                )
              ),
              fluidRow(
                box(title = "Water Types", status = "warning", solidHeader = TRUE, width = 12,
                    p("Water type classification coming soon...")
                )
              ),
              fluidRow(
                box(title = "Suggested Measures", status = "info", solidHeader = TRUE, width = 12,
                    p("Remedial measures based on chemistry coming soon...")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  data_storage <- reactiveValues(csv_data = NULL, sf_data = NULL, water_sf = NULL, data_loaded = FALSE)
  
  observeEvent(input$load_data, {
    req(input$gpkg_upload)
    tryCatch({
      water_sf <- calculate_wqi(input$gpkg_upload$datapath)
      data_storage$csv_data <- st_drop_geometry(water_sf)
      data_storage$sf_data <- water_sf
      data_storage$water_sf <- water_sf
      data_storage$data_loaded <- TRUE
      showNotification("GPKG loaded and WQI calculated successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error processing GPKG file:", e$message), type = "error")
    })
  })
  
  output$file_info <- renderPrint({
    req(data_storage$csv_data)
    cat("File Uploaded Successfully!\n",
        "Number of Rows:", nrow(data_storage$csv_data), "\n",
        "Number of Columns:", ncol(data_storage$csv_data))
  })
  
  output$dataset_summary <- renderUI({
    req(data_storage$csv_data)
    HTML(paste(
      "<h4>Dataset Characteristics</h4>",
      "<p><strong>Total Rows:</strong>", nrow(data_storage$csv_data), "</p>",
      "<p><strong>Total Columns:</strong>", ncol(data_storage$csv_data), "</p>"
    ))
  })
  
  output$column_details <- renderDT({
    req(data_storage$csv_data)
    column_info <- data.frame(
      Column = names(data_storage$csv_data),
      Type = sapply(data_storage$csv_data, function(x) class(x)[1]),
      Unique_Values = sapply(data_storage$csv_data, function(x) length(unique(x))),
      Missing_Values = sapply(data_storage$csv_data, function(x) sum(is.na(x)))
    )
    datatable(column_info, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$data_preview <- renderDT({
    req(data_storage$csv_data)
    datatable(data_storage$csv_data, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$data_statistics <- renderUI({
    req(data_storage$csv_data)
    numeric_cols <- names(data_storage$csv_data)[sapply(data_storage$csv_data, is.numeric)]
    if (length(numeric_cols) > 0) {
      stats <- data_storage$csv_data %>%
        select(all_of(numeric_cols)) %>%
        summarise(across(everything(), list(Mean = mean, Median = median, Min = min, Max = max, SD = sd), na.rm = TRUE))
      HTML(paste("<h4>Numeric Column Statistics</h4><pre>", capture.output(print(stats)), "</pre>"))
    } else HTML("<p>No numeric columns found.</p>")
  })
  
  # Piper plot render
  output$piperPlot <- renderPlot({
    piperPlot(
      bengaluru_meq$Ca,
      bengaluru_meq$Mg,
      bengaluru_meq$NaK,
      bengaluru_meq$Cl,
      bengaluru_meq$SO4,
      bengaluru_meq$HCO3,
      Plot = list(name = bengaluru_data$Site),
      caption = "Piper Diagram – Bengaluru City Water Samples"
    )
  })
  # Dynamic UI for State
  output$state_ui <- renderUI({
    req(data_storage$data_loaded)
    states <- unique(data_storage$csv_data$State)
    selectInput("selected_state", "Select State", choices = c("All", sort(states)))
  })
  
  # Dynamic UI for District
  output$district_ui <- renderUI({
    req(data_storage$data_loaded)
    data <- data_storage$csv_data
    if (!is.null(input$selected_state) && input$selected_state != "All") {
      data <- data[data$State == input$selected_state, ]
    }
    districts <- unique(data$District)
    selectInput("selected_district", "Select District", choices = c("All", sort(districts)))
  })
  
  # Dynamic UI for Block
  output$block_ui <- renderUI({
    req(data_storage$data_loaded)
    data <- data_storage$csv_data
    if (!is.null(input$selected_state) && input$selected_state != "All") {
      data <- data[data$State == input$selected_state, ]
    }
    if (!is.null(input$selected_district) && input$selected_district != "All") {
      data <- data[data$District == input$selected_district, ]
    }
    blocks <- unique(data$Block)
    selectInput("selected_block", "Select Block", choices = c("All", sort(blocks)))
  })
  
}

# Run App
shinyApp(ui, server)
