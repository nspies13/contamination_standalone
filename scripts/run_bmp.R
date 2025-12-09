#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(tidyverse)
  library(tidymodels)
  library(jsonlite)
  library(readr)
  library(lubridate)
  source("scripts/bmp_helpers.R")
})

cli_stop <- function(msg, status = 1) {
  message(msg)
  quit(status = status)
}

parse_args <- function(args) {
  parsed <- list(
    mode = "single",
    input = NULL,
    input_file = NULL,
    output_file = NULL,
    input_format = "wide", # wide JSON row for single; batch defaults to long
    lookback_hours = 48
  )

  i <- 1
  while (i <= length(args)) {
    flag <- args[[i]]

    value <- if (i + 1 <= length(args)) args[[i + 1]] else NULL

    if (flag %in% c("-m", "--mode")) {
      parsed$mode <- value
      i <- i + 2
    } else if (flag %in% c("-i", "--input")) {
      parsed$input <- value
      i <- i + 2
    } else if (flag %in% c("-f", "--input-file")) {
      parsed$input_file <- value
      i <- i + 2
    } else if (flag %in% c("-o", "--output-file")) {
      parsed$output_file <- value
      i <- i + 2
    } else if (flag %in% c("-l", "--lookback-hours")) {
      parsed$lookback_hours <- as.numeric(value)
      i <- i + 2
    } else if (flag %in% c("-F", "--input-format")) {
      parsed$input_format <- value
      i <- i + 2
    } else if (flag %in% c("-h", "--help")) {
      cat(
        "\nBMP prediction CLI\n",
        "  --mode / -m            Mode: single (JSON I/O) or batch (CSV I/O). Default: single\n",
        "  --input / -i           Inline JSON payload for single mode (optional)\n",
        "  --input-file / -f      Path to JSON (single) or CSV (batch) input file\n",
        "  --output-file / -o     Path to CSV output (batch mode only)\n",
        "  --input-format / -F    'wide' (default for single) or 'long' (will preprocess)\n",
        "  --lookback-hours / -l  Hours window for prior/post lab search (long input only). Default: 48\n",
        sep = ""
      )
      quit(status = 0)
    } else {
      cli_stop(paste("Unknown flag:", flag))
    }
  }

  parsed
}

read_json_input <- function(inline, file_path) {
  payload <- NULL

  if (!is.null(file_path)) {
    if (!file.exists(file_path)) {
      cli_stop(paste("Input file does not exist:", file_path))
    }
    payload <- read_file(file_path)
  } else if (!is.null(inline)) {
    payload <- inline
  } else {
    payload <- paste(readLines(file("stdin")), collapse = "\n")
  }

  if (is.null(payload) || payload == "") {
    cli_stop("No JSON input provided for single mode.")
  }

  as_tibble(jsonlite::fromJSON(payload, simplifyDataFrame = TRUE))
}

write_json_output <- function(df) {
  cat(jsonlite::toJSON(df, dataframe = "rows", auto_unbox = TRUE, na = "null", POSIXt = "ISO8601"))
}

run_single <- function(args, models_combined, mix_ratio_models) {
  input <- read_json_input(args$input, args$input_file)

  prepped <-
    if (identical(args$input_format, "long")) {
      preprocessBmpData(input, lookback_hours = args$lookback_hours)
    } else {
      input
    }

  preds <- makeBmpPredictions(
    input_raw = prepped,
    models_combined = models_combined,
    mix_ratio_models = mix_ratio_models
  )
  write_json_output(preds)
}

run_batch <- function(args, models_combined, mix_ratio_models) {
  if (is.null(args$input_file) || is.null(args$output_file)) {
    cli_stop("Batch mode requires --input-file (CSV) and --output-file (CSV).")
  }

  input <- read_csv(args$input_file, show_col_types = FALSE, progress = FALSE)

  prepped <-
    if (identical(args$input_format, "long")) {
      preprocessBmpData(input, lookback_hours = args$lookback_hours)
    } else {
      input
    }

  preds <- makeBmpPredictions(
    input_raw = prepped,
    models_combined = models_combined,
    mix_ratio_models = mix_ratio_models
  )

  write_csv(preds, args$output_file)
  message("Wrote BMP predictions to ", args$output_file)
}

main <- function() {
  args <- parse_args(commandArgs(trailingOnly = TRUE))

  model_path <- Sys.getenv("BMP_MODEL_PATH", "models/bmp_models_combined.RDS")
  mix_ratio_path <- Sys.getenv("BMP_MIX_MODEL_PATH", "models/bmp_mix_ratio_models_combined.RDS")

  models_combined <- read_rds(model_path)
  mix_ratio_models <- read_rds(mix_ratio_path)

  if (identical(args$mode, "single")) {
    run_single(args, models_combined, mix_ratio_models)
  } else if (identical(args$mode, "batch")) {
    run_batch(args, models_combined, mix_ratio_models)
  } else {
    cli_stop("Mode must be 'single' or 'batch'.")
  }
}

main()
