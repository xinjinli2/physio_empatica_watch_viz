library(shiny)
library(readr)
library(dplyr)
library(jsonlite)
library(lubridate)
library(reticulate)

options(shiny.maxRequestSize = 500 * 1024^2)

# Python AVRO code (insert your proven function)
py_run_string("
import os
import pandas as pd
from datetime import datetime, timezone, timedelta
from avro.datafile import DataFileReader
from avro.io import DatumReader

def empatica_to_csv_memory(avro_dir):
    eda_all = []
    bvp_all = []
    for filename in os.listdir(avro_dir):
        if filename.endswith('.avro'):
            filepath = os.path.join(avro_dir, filename)
            reader = DataFileReader(open(filepath, 'rb'), DatumReader())
            data = next(reader)
            reader.close()
            # ---- EDA ----
            eda = data['rawData']['eda']
            freq_eda = eda['samplingFrequency']
            if freq_eda > 0 and eda['values']:
                start_eda = eda['timestampStart']
                eda_timestamps = [round(start_eda + i * (1e6 / freq_eda)) for i in range(len(eda['values']))]
                eda_df = pd.DataFrame({'unix_timestamp': eda_timestamps, 'eda': eda['values']})
                eda_all.append(eda_df)
            # ---- BVP ----
            bvp = data['rawData']['bvp']
            freq_bvp = bvp['samplingFrequency']
            if freq_bvp > 0 and bvp['values']:
                start_bvp = bvp['timestampStart']
                bvp_timestamps = [round(start_bvp + i * (1e6 / freq_bvp)) for i in range(len(bvp['values']))]
                bvp_df = pd.DataFrame({'unix_timestamp': bvp_timestamps, 'bvp': bvp['values']})
                bvp_all.append(bvp_df)
    eda_concat = pd.concat(eda_all).sort_values('unix_timestamp').reset_index(drop=True) if eda_all else pd.DataFrame()
    bvp_concat = pd.concat(bvp_all).sort_values('unix_timestamp').reset_index(drop=True) if bvp_all else pd.DataFrame()
    return {'eda': eda_concat, 'bvp': bvp_concat}
")

server <- function(input, output, session) {
  values <- reactiveValues(
    markers = NULL, watch_data = NULL, status = ""
  )
  
  observeEvent(input$plot_button, {
    req(input$ring_zip, input$watch_zip)
    values$status <- "Processing files..."
    
    # --- Unzip files ---
    tmp_ring <- tempfile()
    tmp_watch <- tempfile()
    dir.create(tmp_ring)
    dir.create(tmp_watch)
    unzip(input$ring_zip$datapath, exdir = tmp_ring)
    unzip(input$watch_zip$datapath, exdir = tmp_watch)
    
    # --- Read event markers & session.json ---
    event_path <- list.files(tmp_ring, pattern = "eventMarkers.csv", recursive = TRUE, full.names = TRUE)[1]
    session_path <- list.files(tmp_ring, pattern = "session.json", recursive = TRUE, full.names = TRUE)[1]
    if (is.na(event_path) || is.na(session_path)) {
      values$status <- "Missing eventMarkers.csv or session.json in Ring ZIP!"
      return()
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
    
    # --- Get period bounds ---
    periods <- list(
      Baseline = c(markers$abs_timestamp[1], markers$abs_timestamp[2]),
      "Ride 1" = c(markers$abs_timestamp[2], markers$abs_timestamp[3]),
      "Ride 2" = c(markers$abs_timestamp[4], markers$abs_timestamp[5])
    )
    
    # --- Find AVRO dir ---
    avro_dir <- list.dirs(tmp_watch, recursive = TRUE, full.names = TRUE)
    avro_dir <- avro_dir[sapply(avro_dir, function(d) length(list.files(d, pattern = "\\.avro$")) > 0)][1]
    if (is.na(avro_dir)) {
      values$status <- "No AVRO files found in Watch ZIP!"
      return()
    }
    
    # --- Python AVRO extraction ---
    py$avro_dir <- avro_dir
    py$watch_data <- py$empatica_to_csv_memory(py$avro_dir)
    eda <- py$watch_data$eda
    bvp <- py$watch_data$bvp
    values$watch_data <- list(
      eda = as_tibble(eda),
      bvp = as_tibble(bvp)
    )
    values$status <- "Data loaded!"
  })
  
  output$status <- renderText({ values$status })
  
  output$markerTable <- renderTable({
    req(values$markers)
    values$markers %>%
      mutate(EST_time = format(as.POSIXct(abs_timestamp/1e6, origin="1970-01-01", tz="America/New_York"), "%Y-%m-%d %H:%M:%S")) %>%
      select(label, EST_time)
  })
  
  output$segmentPlot <- renderPlot({
    req(values$watch_data, values$markers)
    data <- values$watch_data[[input$signal]]
    periods <- list(
      Baseline = c(values$markers$abs_timestamp[1], values$markers$abs_timestamp[2]),
      "Ride 1" = c(values$markers$abs_timestamp[2], values$markers$abs_timestamp[3]),
      "Ride 2" = c(values$markers$abs_timestamp[4], values$markers$abs_timestamp[5])
    )
    pb <- periods[[input$period]]
    if (is.null(data) || nrow(data) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "No data for this signal")
      return()
    }
    seg <- data %>% filter(unix_timestamp >= pb[1], unix_timestamp <= pb[2]) %>%
      mutate(time_sec = (unix_timestamp - pb[1]) / 1e6)
    if (nrow(seg) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "No signal in selected period")
      return()
    }
    plot(seg$time_sec, seg[[input$signal]], type = "l",
         xlab = "Time (seconds from period start)",
         ylab = input$signal,
         main = paste(input$signal, "-", input$period, "\n", nrow(seg), "points"))
  })
}
