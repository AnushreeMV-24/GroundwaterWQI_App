library(shiny)
library(shinydashboard)
library(smwrGraphs)

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

# UI
ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(title = "Bengaluru Water Quality"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Piper Diagram", tabName = "piper", icon = icon("flask")),
      br(),
      actionButton("plotBtn", "Generate Piper Plot", icon = icon("chart-area")),
      br(),
      helpText("Static sample dataset (mg/L) is used.")
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "piper",
              fluidRow(
                box(title = "Piper Diagram",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    plotOutput("piperPlot", height = "700px")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Reactive conversion to meq/L
  bengaluru_meq <- reactive({
    transform(bengaluru_data,
              Ca   = to_meq(Ca,   40.08, 2),
              Mg   = to_meq(Mg,   24.31, 2),
              NaK  = to_meq(Na + K, 22.99, 1),  # Na + K
              Cl   = to_meq(Cl,   35.45, 1),
              SO4  = to_meq(SO4,  96.06, 2),
              HCO3 = to_meq(HCO3, 61.02, 1)
    )
  })
  
  output$piperPlot <- renderPlot({
    input$plotBtn
    isolate({
      data_meq <- bengaluru_meq()
      
      piperPlot(
        data_meq$Ca,
        data_meq$Mg,
        data_meq$NaK,
        data_meq$Cl,
        data_meq$SO4,
        data_meq$HCO3,
        Plot = list(name = bengaluru_data$Site),
        caption = "Piper Diagram – Bengaluru City Water Samples"
      )
    })
  })
}

# Run the app
shinyApp(ui = ui, server = server)
