library(shiny)
library(plotly)
library(shinyjs)

fluidPage(
  useShinyjs(),
  verbatimTextOutput("py_status"), 
  # Keyboard arrow navigation
  tags$script(HTML("
    $(document).on('keydown', function(e) {
      if (e.key === 'ArrowLeft' || e.key === 'ArrowRight') {
        e.preventDefault();
        e.stopPropagation();
        if (e.key === 'ArrowLeft') {
          Shiny.setInputValue('scroll_key', 'left', {priority: 'event'});
        } else if (e.key === 'ArrowRight') {
          Shiny.setInputValue('scroll_key', 'right', {priority: 'event'});
        }
      }
    });
  ")),
  
  titlePanel("Empatica Watch Signal Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("ring_zip", "Upload Ring data (ZIP for event markers)", accept = ".zip"),
      fileInput("watch_zip", "Upload Watch data (ZIP with AVRO files)", accept = ".zip"),
      selectInput("signal", "Signal to display", choices = c("EDA" = "eda", "PPG/BVP" = "bvp"), selected = "eda"),
      selectInput("period", "Testing period", choices = c("Baseline", "First Ride", "Second Ride"), selected = "Baseline"),
      numericInput("window_size", "Display Window size (seconds)", value = 2, min = 0.5, max = 30, step = 0.5),
      sliderInput("window_start", "Scroll: Start time (s)", min = 0, max = 100, value = 0, step = 0.1),
      helpText("⬅ ➡ Use keyboard arrows or quick nav buttons to scroll"),
      actionButton("plot_button", "Update Plot", class = "btn-primary"),
      br(), br(),
      verbatimTextOutput("status")
    ),
    mainPanel(
      h4("Signal Plot"),
      plotlyOutput("signalPlot", height = "400px"),
      br(),
      plotlyOutput("scrollbarPlot", height = "100px"),
      br(),
      fluidRow(
        column(3, align = "center", actionButton("scroll_left_fast", "⏪ -5×")),
        column(3, align = "center", actionButton("scroll_left", "⬅ -1×")),
        column(3, align = "center", actionButton("scroll_right", "➡ +1×")),
        column(3, align = "center", actionButton("scroll_right_fast", "⏩ +5×"))
      ),
      br(),
      h4("Info"),
      verbatimTextOutput("info"),
      br(),
      h4("Event Marker Table"),
      tableOutput("markerTable")
    )
  )
)
