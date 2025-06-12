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
      #tags$img(src = "sirpilogo white.avif", height = "30px", style = "margin-right: 10px;"),
      #tags$img(src = "gdx_logo.png", height = "30px", style = "margin-right: 10px;"), # Add your second logo here
      
      
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
                    withSpinner(plotOutput("piper_plot", height = "700px"), type = 6)
                )
              ),

              fluidRow(
                box(title = "Water Types", status = "warning", solidHeader = TRUE, width = 12,
                    withSpinner(uiOutput("water_types_section"), type = 6)
                )
              ),
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
  
  
  # Reactive UI for Filters
  output$state_ui <- renderUI({
    req(data_storage$csv_data)
    selectInput("state_chem", "State/UT:", choices = unique(data_storage$csv_data$STATE_UT))
  })
  
  output$district_ui <- renderUI({
    req(data_storage$csv_data, input$state_chem)
    districts <- unique(data_storage$csv_data[data_storage$csv_data$STATE_UT == input$state_chem, ]$DISTRICT)
    selectInput("district_chem", "District:", choices = districts)
  })
  
  output$block_ui <- renderUI({
    req(data_storage$csv_data, input$district_chem)
    blocks <- unique(data_storage$csv_data[data_storage$csv_data$DISTRICT == input$district_chem, ]$BLOCK)
    selectInput("block_chem", "Block:", choices = blocks)
  })
  
  # Reactive UI for Filters (Quality Tab)
  output$state_filter_ui <- renderUI({
    req(data_storage$water_sf)
    selectInput("state_filter", "State:", choices = c("None", unique(data_storage$water_sf$STATE_UT)), selected = input$state_filter)
  })
  
  output$district_filter_ui <- renderUI({
    req(data_storage$water_sf, input$state_filter)
    districts <- if (input$state_filter == "None") c("None", unique(data_storage$water_sf$DISTRICT)) else c("None", unique(data_storage$water_sf[data_storage$water_sf$STATE_UT == input$state_filter, ]$DISTRICT))
    selectInput("district_filter", "District:", choices = districts, selected = input$district_filter)
  })
  
  output$block_filter_ui <- renderUI({
    req(data_storage$water_sf, input$district_filter)
    blocks <- if (input$district_filter == "None") c("None", unique(data_storage$water_sf$BLOCK)) else c("None", unique(data_storage$water_sf[data_storage$water_sf$DISTRICT == input$district_filter, ]$BLOCK))
    selectInput("block_filter", "Block:", choices = blocks, selected = input$block_filter)
  })
  
  
  # UI for state filter with default set to Karnataka
  output$state_filter_ui <- renderUI({
    req(data_storage$water_sf)
    states <- sort(unique(data_storage$water_sf$STATE_UT))
    
    selectInput("state_filter", "Select State:",
                choices = c("None", states),
                selected = "None")  # Default selection
  })
  
  # UI for district filter (depends on state)
  output$district_filter_ui <- renderUI({
    req(data_storage$water_sf, input$state_filter)
    districts <- data_storage$water_sf %>%
      filter(STATE_UT == input$state_filter) %>%
      pull(DISTRICT) %>%
      unique() %>%
      sort()
    
    selectInput("district_filter", "Select District:",
                choices = c("None", districts),
                selected = "None")
  })
  
  # UI for block filter (depends on district)
  output$block_filter_ui <- renderUI({
    req(data_storage$water_sf, input$state_filter, input$district_filter)
    blocks <- data_storage$water_sf %>%
      filter(STATE_UT == input$state_filter,
             DISTRICT == input$district_filter) %>%
      pull(BLOCK) %>%
      unique() %>%
      sort()
    
    selectInput("block_filter", "Select Block:",
                choices = c("None", blocks),
                selected = "None")
  })
  
  
  output$debug_filtered_data <- renderPrint({
    req(filtered_data_val())
    summary(filtered_data_val())
  })
  
  
  # Chemistry Tab Server Logic
  PD_for_water_type_reactive <- reactive({
    
    req(data_storage$csv_data)           # Make sure data is loaded
    req(input$state_chem)                 # Make sure state is selected
    req(input$district_chem)              # Make sure district is selected
    req(input$block_chem)                 # Make sure block is selected
    
    selected_data <- data_storage$csv_data %>%
      filter(
        STATE_UT == input$state_chem,
        DISTRICT == input$district_chem,
        BLOCK == input$block_chem
      ) %>%
      mutate(
        CA = as.numeric(CA),
        MG = as.numeric(MG),
        `NA.` = as.numeric(`NA.`),
        CHLORIDE = as.numeric(CHLORIDE),
        SULPHATE = as.numeric(SULPHATE),
        BICARBONATE = as.numeric(BICARBONATE)
      )
    
    missing_ions <- character(0)
    if(any(is.na(selected_data$CA))) missing_ions <- c(missing_ions, "Calcium (Ca)")
    if(any(is.na(selected_data$MG))) missing_ions <- c(missing_ions, "Magnesium (Mg)")
    if(any(is.na(selected_data$`NA.`))) missing_ions <- c(missing_ions, "Sodium (Na)")
    if(any(is.na(selected_data$CHLORIDE))) missing_ions <- c(missing_ions, "Chloride (Cl)")
    if(any(is.na(selected_data$SULPHATE))) missing_ions <- c(missing_ions, "Sulfate (SO4)")
    if(any(is.na(selected_data$BICARBONATE))) missing_ions <- c(missing_ions, "Bicarbonate (HCO3)")
    
    if (length(missing_ions) > 0) {
      attr(selected_data, "missing_ions") <- paste("Warning: Missing data for some ions:", paste(missing_ions, collapse = ", "))
      return(selected_data)
    } else {
      return(NULL)
    }
  })
  
  PD_final_reactive <- reactive({
    state_val <- input$state_chem
    district_val <- input$district_chem
    block_val <- input$block_chem
    
    cat(file = stderr(), paste("Running PD_final_reactive - State:", state_val, ", District:", district_val, ", Block:", block_val, "\n"))
    
    req(data_storage$csv_data)
    req(state_val)
    req(district_val)
    req(block_val)
    
    selected_data <- data_storage$csv_data %>%
      filter(
        STATE_UT == state_val,
        DISTRICT == district_val,
        BLOCK == block_val
      ) %>%
      mutate(
        CA = as.numeric(CA),
        MG = as.numeric(MG),
        `NA.` = as.numeric(`NA.`),
        CHLORIDE = as.numeric(CHLORIDE),
        SULPHATE = as.numeric(SULPHATE),
        BICARBONATE = as.numeric(BICARBONATE)
      ) %>%
      drop_na(CA, MG, `NA.`, CHLORIDE, SULPHATE, BICARBONATE)
    
    cat(file = stderr(), paste("PD_final_reactive - Filtered Rows:", nrow(selected_data), "\n"))
    
    if (nrow(selected_data) == 0) {
      return(NULL)
    }
    
    PD <- transform(selected_data,
                    Ca.meq = conc2meq(CA, "calcium"),
                    Mg.meq = conc2meq(MG, "magnesium"),
                    Na.meq = conc2meq(`NA.`, "sodium"),
                    Cl.meq = conc2meq(CHLORIDE, "chloride"),
                    SO4.meq = conc2meq(SULPHATE, "sulfate"),
                    HCO3.meq = conc2meq(BICARBONATE, "bicarb"))
    PD$SS <- PD$SITE_NAME
    
    PD <- PD %>%
      mutate(
        total_cations = Ca.meq + Mg.meq + Na.meq,
        total_anions = Cl.meq + SO4.meq + HCO3.meq,
        Ca_pct = round(100 * Ca.meq / total_cations, 1),
        Mg_pct = round(100 * Mg.meq / total_cations, 1),
        Na_pct = round(100 * Na.meq / total_cations, 1),
        Cl_pct = round(100 * Cl.meq / total_anions, 1),
        SO4_pct = round(100 * SO4.meq / total_anions, 1),
        HCO3_pct = round(100 * HCO3.meq / total_anions, 1),
        cation_type = case_when(
          total_cations > 0 & Ca_pct + Mg_pct > 50 ~ "Hard",
          total_cations > 0 & Na_pct > 50 ~ "Alkali",
          total_cations > 0 ~ "Mixed",
          TRUE ~ NA_character_
        ),
        anion_type = case_when(
          total_anions > 0 & HCO3_pct > 50 ~ "Carbonate",
          total_anions > 0 & Cl_pct + SO4_pct > 50 ~ "Non-carbonate",
          total_anions > 0 ~ "Mixed",
          TRUE ~ NA_character_
        ),
        water_type = case_when(
          cation_type == "Hard" & anion_type == "Non-carbonate" ~ "Permanent Hardness",
          cation_type == "Hard" & anion_type == "Carbonate" ~ "Temporary Hardness",
          cation_type == "Alkali" & anion_type == "Carbonate" ~ "Alkali Carbonates",
          cation_type == "Alkali" & anion_type == "Non-carbonate" ~ "Saline",
          !is.na(cation_type) & !is.na(anion_type) ~ "Mixed Type",
          TRUE ~ "Unable to compute"
        )
      )
    
    return(PD)
  })
  
  output$piper_plot <- renderPlot({
    PD <- PD_final_reactive()
    req(PD)
    
    water_colors <- c(
      "Permanent Hardness" = "red",
      "Temporary Hardness" = "blue",
      "Alkali Carbonates" = "green",
      "Saline" = "purple",
      "Mixed Type" = "orange",
      "Unable to compute" = "gray"
    )
    
    PD$color <- water_colors[PD$water_type]
    
    with(PD, piperPlot(
      Ca.meq, Mg.meq, Na.meq,
      Cl.meq, HCO3.meq, SO4.meq,
      Plot = list(name = water_type, color = color),
      xAn.title = "Chloride"
    ))
    
    legend("topright", legend = names(water_colors), col = water_colors, pch = 16, title = "Water Chemistry Type")
  })
  
  output$waterChemistry <- renderText({
    missing_data <- PD_for_water_type_reactive()
    warning_msg <- attr(missing_data, "missing_ions")
    PD <- PD_final_reactive()
    
    water_type_text <- if (!is.null(PD) && nrow(PD) > 0) {
      paste("Water Chemistry Type(s):", paste(unique(PD$water_type), collapse = ", "))
    } else {
      "No valid data available for plotting."
    }
    
    if (!is.null(warning_msg)) {
      paste(water_type_text, "\n", warning_msg)
    } else {
      water_type_text
    }
  })
  
  
  # Water Types Tab
  output$water_types_section <- renderUI({
    if (is.null(input$state_chem) || input$state_chem == "" ||
        is.null(input$district_chem) || input$district_chem == "" ||
        is.null(input$block_chem) || input$block_chem == "") {
      return(NULL) # Return NULL to display nothing
    }
    
    PD <- PD_final_reactive()
    
    if (is.null(PD) || nrow(PD) == 0) {
      HTML("<div class='warning-message'>Unable to determine water types or generate the Piper plot for the selected location due to missing data. Please ensure all required ion values are provided.</div>")
    } else {
      water_type_text <- paste("<b>Water Types:</b><br>", paste(unique(PD$water_type), collapse = "<br>"))
      HTML(water_type_text)
    }
  })
  

  
  # Water Chemistry download Report
  output$download_chemistry <- downloadHandler(
    filename = function() {
      paste("ground_water_chemistry_", input$state_chem, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "groundwater_chemistry_report.Rmd")
      file.copy("groundwater_chemistry_report.Rmd", tempReport, overwrite = TRUE)
      
      PD <- PD_final_reactive()
      req(PD)
      
      water_colors <- c(
        "Permanent Hardness" = "red",
        "Temporary Hardness" = "blue",
        "Alkali Carbonates" = "green",
        "Saline" = "purple",
        "Mixed Type" = "orange",
        "Unable to compute" = "gray"
      )
      PD$color <- water_colors[PD$water_type]
      
      plot_file <- file.path(tempdir(), "piper_plot_for_report.png")
      png(filename = plot_file, width = 1200, height = 1000, res = 150, bg = "white", pointsize = 12)
      par(mar = c(5, 4, 4, 8))
      with(PD, piperPlot(
        Ca.meq, Mg.meq, Na.meq,
        Cl.meq, HCO3.meq, SO4.meq,
        Plot = list(name = water_type, color = color),
        xAn.title = "Chloride"
      ))
      legend(
        x = 0.6,
        y = 1,
        legend = names(water_colors),
        col = water_colors,
        pch = 16,
        title = "Water Chemistry Type",
        xpd = TRUE
      )
      dev.off()
      
      params_list <- list(
        data = PD,
        state = input$state_chem, # Use the correct input ID
        district = input$district_chem, # Use the correct input ID
        block = input$block_chem, # Use the correct input ID
        image_path = plot_file
      )
      
      rmarkdown::render(
        tempReport,
        output_file = file,
        params = params_list,
        envir = new.env(parent = globalenv())
      )
    }
  )
  
}

# Run App
shinyApp(ui, server)
# The code which we are giving them in final(Water Chemistry, 3 Tabs)