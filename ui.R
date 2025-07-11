library(shiny)

fluidPage(
  titlePanel("Empatica Watch Signal Visualization"),
  sidebarLayout(
    sidebarPanel(
      fileInput("ring_zip", "Upload Ring data (ZIP for event markers)", accept = ".zip"),
      fileInput("watch_zip", "Upload Watch data (ZIP with AVRO files)", accept = ".zip"),
      selectInput("signal", "Signal to display", choices = c("EDA" = "eda", "PPG/BVP" = "bvp"), selected = "eda"),
      selectInput("period", "Experimental Period", choices = c("Baseline", "Ride 1", "Ride 2"), selected = "Baseline"),
      actionButton("plot_button", "Plot Segment", class = "btn-primary"),
      br(),
      verbatimTextOutput("status")
    ),
    mainPanel(
      h4("Period Marker Table"),
      tableOutput("markerTable"),
      h4("Signal Plot"),
      plotOutput("segmentPlot", height = "350px")
    )
  )
)
