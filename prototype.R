# Final - 21st april Load Libraries
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



# WQI Calculation Function
calculate_wqi <- function(gpkg_path) {
  layer_name <- st_layers(gpkg_path)$name[1]
  water_sf <- st_read(gpkg_path, layer = layer_name, quiet = TRUE)
  
  # Ensure WGS84 projection
  if (sf::st_crs(water_sf)$epsg != 4326) {
    water_sf <- st_transform(water_sf, 4326)
  }
  
  # Validate geometries
  if (!all(st_is_valid(water_sf))) {
    water_sf <- st_make_valid(water_sf)
  }
  
  # Rename "NA" column to "Sodium" if present
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
  
  # Replace missing parameters with 0
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
  
  # Write CSV output with WQI and Quality columns
  csv_path <- file.path(getwd(), "water_quality_output.csv")
  write.csv(st_drop_geometry(water_sf), csv_path, row.names = FALSE)
  
  return(water_sf)
}

# UI
ui <- dashboardPage(
  
  
  
  title = "Ground Water Assessment Dashboard",
  skin = "blue",
  
  dashboardHeader(
    title = tags$div(
      
      
      style = "display: flex; align-items: center; height: 60px; background-color: transparent;",
      tags$img(src = "sirpilogo white.avif", height = "30px", style = "margin-right: 10px;"),
      tags$img(src = "gdx_logo.png", height = "30px", style = "margin-right: 10px;"), # Add your second logo here
      
      
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
      tags$title("Ground Water Dashboard"),
      tags$style(HTML(sprintf('
      .skin-blue .main-header .logo { background-color: %s; color: white; }
      .skin-blue .main-header .navbar { background-color: %s; }
      body { background-color: %s; color: %s; }
      .box { border-top-color: %s; }
    ', iisc_palette["primary"], iisc_palette["secondary"],
                              iisc_palette["background"], iisc_palette["text"],
                              iisc_palette["accent"]))),
      tags$style(HTML("
      .main-header {
        height: 60px !important; /* Adjust this value as needed */
      }
      .main-header .logo {
        height: 60px !important; /* Match the main-header height */
        line-height: 60px !important; /* Vertically center the logo text if any (though you have images) */
      }
      .main-header .navbar {
        min-height: 60px !important; /* Ensure navbar doesn't collapse */
      }
      .main-header .title {
        height: 60px !important; /* Match the main-header height */
        line-height: 60px !important; /* Vertically center the title text */
      }
      .main-header .title > div { /* Target the div containing your logos and text */
        display: flex;
        align-items: center; /* Vertically align items within the div */
        height: 100%; /* Ensure the div takes full height of the title */
      }
    "))
    ),
    
    # ... rest of your dashboardBody ...
    
    
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
                    #withSpinner(plotOutput("piper_plot", height = "700px"), type = 6)
                    
                  
                )
              ),
              fluidRow(
                box(title = "Chemical Composition", status = "success", solidHeader = TRUE, width = 12,
                    #withSpinner(plotOutput("chemCompPlot", height = "700px"), type = 6)
                )
              ),
              fluidRow(
                box(title = "Water Types", status = "warning", solidHeader = TRUE, width = 12,
                    #withSpinner(uiOutput("water_types_section"), type = 6)
                )
              ),
              fluidRow(
                box(title = "Suggested Measures", status = "info", solidHeader = TRUE, width = 12,
                    #withSpinner(uiOutput("measures_section"), type = 6)
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
      
      # Update State choices immediately after data load
      updateSelectInput(session, "state_chem", choices = c("Select State", unique(data_storage$csv_data$STATE_UT)), selected = "Select State")
      updateSelectInput(session, "state_filter", choices = c("None", unique(data_storage$water_sf$STATE_UT)), selected = "None")
      
      # Reset subsequent filters
      updateSelectInput(session, "district_chem", choices = "Select District", selected = "Select District")
      updateSelectInput(session, "block_chem", choices = "Select Block", selected = "Select Block")
      updateSelectInput(session, "district_filter", choices = c("None"), selected = "None")
      updateSelectInput(session, "block_filter", choices = c("None"), selected = "None")
      
      # 
      # # Update pickerInput choices
      # updatePickerInput(session, "wqiClass",
      #                   choices = unique(water_sf$Quality),
      #                   selected = unique(water_sf$Quality))
      
      showNotification("GPKG loaded and WQI calculated successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error processing GPKG file:", e$message), type = "error")
      data_storage$data_loaded <- FALSE
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
   
  library(shiny)
  library(smwrGraphs)
  library(smwrData)
  
  # Load and prepare data
  data(MiscGW)
  PD <- transform(MiscGW, 
                  Ca.meq = conc2meq(Calcium, "calcium"),
                  Mg.meq = conc2meq(Magnesium, "magnesium"),
                  Na.meq = conc2meq(Sodium, "sodium"),
                  Cl.meq = conc2meq(Chloride, "chloride"),
                  SO4.meq = conc2meq(Sulfate, "sulfate"),
                  HCO3.meq = conc2meq(Bicarbonate, "bicarb"))
  PD$SS <- row.names(PD)
  
  # Define UI
  ui <- fluidPage(
    titlePanel("Piper Diagram Example"),
    plotOutput("piperPlot")
  )
  
  # Define server logic
  server <- function(input, output) {
    output$piperPlot <- renderPlot({
      piperPlot(PD$Ca.meq, PD$Mg.meq, PD$Na.meq, 
                PD$Cl.meq, PD$HCO3.meq, PD$SO4.meq, 
                Plot = list(name = PD$SS, color = setColor(PD$SS)), 
                zCat.title = "Sodium", 
                xAn.title = "Chloride", 
                yAn.title = "Bicarbonate")
    })
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
  
    }
  
# Run App
shinyApp(ui, server)
# The code which we are giving them in final(Water Chemistry, 3 Tabs)