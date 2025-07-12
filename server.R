library(shiny)
library(readr)
library(dplyr)
library(plotly)
library(jsonlite)
library(lubridate)
library(reticulate)
library(shinyjs)

options(shiny.maxRequestSize = 200 * 1024^2)

if (Sys.info()[["user"]] == "shiny") {
  reticulate::py_install(packages = c("pandas", "numpy", "avro-python3"), envname = "r-reticulate", method = "auto")
}

# if (Sys.info()[["user"]] == "shiny") {
#   # Use system Python (Python 3.8 on shinyapps.io)
#   reticulate::use_python(Sys.which("python3"), required = TRUE)
#   
#   reticulate::py_install(c("pandas", "numpy", "avro-python3"), pip = TRUE)
#   
#   
#   # Install Python packages if they don't already exist
#   if (!reticulate::py_module_available("pandas")) {
#     reticulate::py_install(c("pandas", "numpy", "avro-python3"), pip = TRUE)
#   }
# }

# if (Sys.info()[["user"]] == "shiny") {
#   reticulate::use_virtualenv("r-reticulate", required = TRUE)
# }



# # --- Python setup (local vs shinyapps.io) ---
# user <- Sys.info()[["user"]]
# virtualenv_dir <- Sys.getenv("VIRTUALENV_NAME", unset = "watchapp_env")
# # python_path <- Sys.getenv("PYTHON_PATH", unset = "python3")
# python_path <- "/opt/python/3.9.13/bin/python3"
# 
# 
# # Only run this block if on shinyapps.io
# if (user == "shiny") {
#   tryCatch({
#     if (!reticulate::virtualenv_exists(virtualenv_dir)) {
#       # reticulate::install_python(version = "3.10.13")
#       # reticulate::install_python(version = "3.9.13")
#       reticulate::virtualenv_create(envname = virtualenv_dir, python = python_path)
#       reticulate::virtualenv_install(virtualenv_dir, packages = c("pandas", "avro-python3", "numpy"), ignore_installed = TRUE)
#     }
#     reticulate::use_virtualenv(virtualenv_dir, required = TRUE)
#   }, error = function(e) {
#     stop("Python virtualenv setup failed on server: ", e$message)
#   })
# }


# If running locally, .Rprofile will already call use_python(), so do nothing
# Load Python functions
reticulate::source_python("python_functions.py")


server <- function(input, output, session) {
  output$py_status <- renderPrint({ py_config() })
  
  
  
  # Internal storage
  values <- reactiveValues(
    markers = NULL, watch_data = NULL, status = ""
  )
  
  # --- Data Processing ---
  observeEvent(input$plot_button, {
    req(input$ring_zip, input$watch_zip)
    values$status <- "Processing files..."
    
    # Unzip
    tmp_ring <- tempfile(); tmp_watch <- tempfile()
    dir.create(tmp_ring); dir.create(tmp_watch)
    unzip(input$ring_zip$datapath, exdir = tmp_ring)
    unzip(input$watch_zip$datapath, exdir = tmp_watch)
    
    # Markers
    event_path <- list.files(tmp_ring, pattern = "eventMarkers.csv", recursive = TRUE, full.names = TRUE)[1]
    session_path <- list.files(tmp_ring, pattern = "session.json", recursive = TRUE, full.names = TRUE)[1]
    if (is.na(event_path) || is.na(session_path)) {
      values$status <- "Missing eventMarkers.csv or session.json in Ring ZIP!"; return()
    }
    markers <- read_csv(event_path, col_names = FALSE, show_col_types = FALSE)
    colnames(markers) <- c("timestamp", "event")
    session_info <- fromJSON(session_path)
    session_start <- ymd_hms(session_info$start, tz = "UTC")
    session_start_us <- as.numeric(session_start) * 1e6
    marker_labels <- c("Baseline_Start", "Ride1_Start", "Ride1_End", "Ride2_Start", "Ride2_End")
    markers <- markers %>%
      mutate(abs_timestamp = session_start_us + timestamp, label = marker_labels[1:n()])
    values$markers <- markers
    
    # AVRO Extraction
    avro_dir <- list.dirs(tmp_watch, recursive = TRUE, full.names = TRUE)
    avro_dir <- avro_dir[sapply(avro_dir, function(d) length(list.files(d, pattern = "\\.avro$")) > 0)][1]
    if (is.na(avro_dir)) {
      values$status <- "No AVRO files found in Watch ZIP!"; return()
    }
    py$avro_dir <- avro_dir
    # py$watch_data <- py$empatica_to_csv_memory(py$avro_dir) this is old version
    watch_data_raw <- empatica_to_csv_memory(avro_dir)
    
    values$watch_data <- list(
      eda = as_tibble(watch_data_raw$eda),
      bvp = as_tibble(watch_data_raw$bvp)
    )
    values$status <- "Data loaded!"
    updateSliderInput(session, "window_start", value = 0)
  })
  
  # --- Period Table and Info ---
  output$markerTable <- renderTable({
    req(values$markers)
    values$markers %>%
      mutate(EST_time = format(as.POSIXct(abs_timestamp/1e6, origin="1970-01-01", tz="America/New_York"), "%Y-%m-%d %H:%M:%S")) %>%
      select(label, EST_time)
  })
  
  # --- Get period bounds by label ---
  period_bounds <- reactive({
    mk <- values$markers
    if (is.null(mk) || nrow(mk) < 5) return(c(NA, NA))
    bounds <- switch(input$period,
                     "Baseline" = c(mk$abs_timestamp[1], mk$abs_timestamp[2]),
                     "First Ride" = c(mk$abs_timestamp[2], mk$abs_timestamp[3]),
                     "Second Ride" = c(mk$abs_timestamp[4], mk$abs_timestamp[5]),
                     c(NA, NA)
    )
    bounds
  })
  
  # --- Get raw period data for windowing ---
  period_data <- reactive({
    req(values$watch_data)
    dat <- values$watch_data[[input$signal]]
    bounds <- period_bounds()
    if (is.null(dat) || nrow(dat) == 0 || any(is.na(bounds))) return(tibble())
    col <- if (input$signal == "eda") "eda" else "bvp"
    seg <- dat %>% filter(unix_timestamp >= bounds[1], unix_timestamp <= bounds[2]) %>%
      mutate(rel_time = (unix_timestamp - bounds[1]) / 1e6)
    seg
  })
  
  # --- Auto-update slider max on new data ---
  observe({
    df <- period_data()
    if (nrow(df) > 0) {
      max_time <- max(df$rel_time, na.rm = TRUE)
      slider_max <- round(max(0, max_time - input$window_size), 2)
      current_value <- round(min(input$window_start, slider_max), 2)
      updateSliderInput(session, "window_start", max = slider_max, value = current_value, step = 0.01)
    }
  })
  
  # --- Navigation Buttons & Keyboard ---
  observeEvent(input$scroll_left, {
    new_start <- round(max(0, input$window_start - input$window_size), 2)
    updateSliderInput(session, "window_start", value = new_start)
  })
  observeEvent(input$scroll_right, {
    df <- period_data()
    if (nrow(df) > 0) {
      max_time <- max(df$rel_time, na.rm = TRUE)
      slider_max <- max(0, max_time - input$window_size)
      new_start <- round(min(input$window_start + input$window_size, slider_max), 2)
      updateSliderInput(session, "window_start", value = new_start)
    }
  })
  observeEvent(input$scroll_left_fast, {
    new_start <- round(max(0, input$window_start - (5 * input$window_size)), 2)
    updateSliderInput(session, "window_start", value = new_start)
  })
  observeEvent(input$scroll_right_fast, {
    df <- period_data()
    if (nrow(df) > 0) {
      max_time <- max(df$rel_time, na.rm = TRUE)
      slider_max <- max(0, max_time - input$window_size)
      new_start <- round(min(input$window_start + (5 * input$window_size), slider_max), 2)
      updateSliderInput(session, "window_start", value = new_start)
    }
  })
  observeEvent(input$scroll_key, {
    isolate({
      df <- period_data()
      if (nrow(df) == 0) return()
      max_time <- max(df$rel_time, na.rm = TRUE)
      slider_max <- max(0, max_time - input$window_size)
      current <- input$window_start
      step <- input$window_size
      if (input$scroll_key == "right") {
        new_start <- round(min(current + step, slider_max), 2)
        updateSliderInput(session, "window_start", value = new_start)
      } else if (input$scroll_key == "left") {
        new_start <- round(max(current - step, 0), 2)
        updateSliderInput(session, "window_start", value = new_start)
      }
    })
  })
  
  # --- Main Windowed Plot (auto Y only) ---
  output$signalPlot <- renderPlotly({
    df <- period_data()
    start <- input$window_start
    end <- start + input$window_size
    if (nrow(df) == 0) {
      return(plot_ly() %>%
               layout(title = "No data available for selected period"))
    }
    window_data <- df %>% filter(rel_time >= start, rel_time <= end)
    if (nrow(window_data) == 0) {
      return(plot_ly() %>%
               layout(title = "No data for selected window"))
    }
    y_range <- range(window_data[[ifelse(input$signal == "eda", "eda", "bvp")]], na.rm = TRUE)
    y_padding <- diff(y_range) * 0.1
    y_limits <- c(y_range[1] - y_padding, y_range[2] + y_padding)
    plot_ly(
      data = window_data,
      x = ~rel_time, y = ~get(ifelse(input$signal == "eda", "eda", "bvp")),
      type = 'scatter', mode = 'lines',
      line = list(color = '#0066CC', width = 1)
    ) %>%
      layout(
        title = paste(input$signal, "-", input$period),
        xaxis = list(title = "Time (seconds from start of period)",
                     range = c(start, end), fixedrange = TRUE),
        yaxis = list(title = "Signal value", range = y_limits, fixedrange = FALSE),
        showlegend = FALSE
      )
  })
  
  # --- Scrollbar/Overview Plot ---
  output$scrollbarPlot <- renderPlotly({
    df <- period_data()
    req(nrow(df) > 0)
    # down-sample very large data
    if (nrow(df) > 5000) {
      step <- ceiling(nrow(df) / 5000)
      df <- df[seq(1, nrow(df), by = step), ]
    }
    current_start <- input$window_start
    current_end <- current_start + input$window_size
    col <- ifelse(input$signal == "eda", "eda", "bvp")
    plot_ly(df, x = ~rel_time, y = ~get(col),
            type = 'scatter', mode = 'lines',
            line = list(color = 'lightgray')) %>%
      add_segments(x = current_start, xend = current_end,
                   y = min(df[[col]], na.rm = TRUE),
                   yend = min(df[[col]], na.rm = TRUE),
                   line = list(color = 'red', width = 3),
                   name = "Current View") %>%
      layout(
        xaxis = list(title = "Full Signal Time (s)", fixedrange = TRUE),
        yaxis = list(visible = FALSE),
        showlegend = FALSE,
        margin = list(t = 5)
      )
  })
  
  # --- Info Panel ---
  output$info <- renderPrint({
    bounds <- period_bounds()
    df <- period_data()
    mk <- values$markers
    if (is.null(mk)) return("No marker table loaded yet.")
    cat("Current period:", input$period, "\n")
    # cat("Period bounds (µs):", bounds[1], "to", bounds[2], "\n")   # <---- THIS LINE
    cat("Period bounds (s):", round(bounds[1] / 1e6, 2), "to", round(bounds[2] / 1e6, 2), "\n")
    expected_duration <- round((bounds[2] - bounds[1]) / 1e6, 2)
    cat("Expected duration from markers:", expected_duration, "seconds\n")
    cat("Window:", input$window_size, "seconds starting at", round(input$window_start, 2), "s\n")
    if (nrow(df) > 0) {
      actual_duration <- round(max(df$rel_time), 2)
      cat("Actual data duration:", actual_duration, "seconds\n")
      cat("Data points in period:", nrow(df), "\n")
      window_data <- df %>% filter(rel_time >= input$window_start, rel_time <= input$window_start + input$window_size)
      # if (nrow(window_data) > 0) {
      #   signal_col <- ifelse(input$signal == "eda", "eda", "bvp")
      #   cat("Current window - Min:", round(min(window_data[[signal_col]], na.rm = TRUE), 2),
      #       "Max:", round(max(window_data[[signal_col]], na.rm = TRUE), 2), "\n")   # <--- THIS LINE
      # }
    }
  })
}
