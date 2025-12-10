#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(tidyverse)
  library(tidymodels)
  library(jsonlite)
  library(readr)
  library(lubridate)
  library(plumber)
  source("scripts/cbc_helpers.R")
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
    input_format = "wide", # single defaults to wide JSON; batch defaults to long CSV
    collection_interval = 48,
    stream = FALSE,
    port = 8000,
    host = "0.0.0.0"
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
    } else if (flag %in% c("-c", "--collection-interval")) {
      parsed$collection_interval <- as.numeric(value)
      i <- i + 2
    } else if (flag %in% c("-F", "--input-format")) {
      parsed$input_format <- value
      i <- i + 2
    } else if (identical(flag, "--stream")) {
      parsed$stream <- TRUE
      i <- i + 1
    } else if (flag %in% c("-p", "--port")) {
      parsed$port <- as.integer(value)
      i <- i + 2
    } else if (identical(flag, "--host")) {
      parsed$host <- value
      i <- i + 2
    } else if (flag %in% c("-h", "--help")) {
      cat(
        "\nCBC prediction CLI\n",
        "  --mode / -m                  Mode: single | batch | api. Default: single\n",
        "  --input / -i                 Inline JSON payload for single mode (optional)\n",
        "  --input-file / -f            Path to JSON (single) or CSV (batch) input file\n",
        "  --output-file / -o           Path to CSV output (batch mode only)\n",
        "  --input-format / -F          'wide' (default for single) or 'long' (will preprocess)\n",
        "  --stream                     Stream newline-delimited JSON on stdin for single mode (keeps process alive)\n",
        "  --port / -p                  Port for API mode. Default: 8000\n",
        "  --host                       Bind host for API mode. Default: 0.0.0.0\n",
        "  --collection-interval / -c   Hours window for prior/post lab search. Default: 48\n",
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

prep_and_predict <- function(input, args, models_combined, mix_ratio_workflows) {
  prepped <-
    if (identical(args$input_format, "long")) {
      preprocessCBCData(input, collection_interval = args$collection_interval)
    } else {
      input
    }

  makeCbcPredictions(
    input_raw = prepped,
    models_combined = models_combined,
    mix_ratio_workflows = mix_ratio_workflows
  )
}

run_single <- function(args, models_combined, mix_ratio_workflows) {
  input <- read_json_input(args$input, args$input_file)

  preds <- prep_and_predict(input, args, models_combined, mix_ratio_workflows)
  write_json_output(preds)
}

run_single_stream <- function(args, models_combined, mix_ratio_workflows) {
  con <- file("stdin")
  while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
    json_line <- trimws(line)
    if (identical(json_line, "")) next

    res <- tryCatch(
      {
        input <- as_tibble(jsonlite::fromJSON(json_line, simplifyDataFrame = TRUE))
        preds <- prep_and_predict(input, args, models_combined, mix_ratio_workflows)
        write_json_output(preds)
        cat("\n")
        flush.console()
        NULL
      },
      error = function(e) e
    )

    if (inherits(res, "error")) {
      message("Skipping payload: ", res$message)
    }
  }
}

run_batch <- function(args, models_combined, mix_ratio_workflows) {
  if (is.null(args$input_file) || is.null(args$output_file)) {
    cli_stop("Batch mode requires --input-file (CSV) and --output-file (CSV).")
  }

  input <- read_csv(args$input_file, show_col_types = FALSE, progress = FALSE)

  prepped <-
    if (identical(args$input_format, "long")) {
      preprocessCBCData(input, collection_interval = args$collection_interval)
    } else {
      input
    }

  preds <- makeCbcPredictions(
    input_raw = prepped,
    models_combined = models_combined,
    mix_ratio_workflows = mix_ratio_workflows
  )

  write_csv(preds, args$output_file)
  message("Wrote CBC predictions to ", args$output_file)
}

run_api <- function(args, models_combined, mix_ratio_workflows) {
  base_args <- args

  router <- pr()

  router <- pr_get(router, "/health", function() list(status = "ok"))

  router <- pr_post(router, "/predict", function(req, res) {
    body <- req$postBody
    if (is.null(body) || identical(body, "")) {
      res$status <- 400
      return(list(error = "Empty request body"))
    }

    parsed <- tryCatch(
      as_tibble(jsonlite::fromJSON(body, simplifyDataFrame = TRUE)),
      error = function(e) e
    )
    if (inherits(parsed, "error")) {
      res$status <- 400
      return(list(error = parsed$message))
    }

    local_args <- base_args
    if (!is.null(req$args$query$input_format)) {
      local_args$input_format <- req$args$query$input_format
    }
    if (!is.null(req$args$query$collection_interval)) {
      local_args$collection_interval <- as.numeric(req$args$query$collection_interval)
    }

    preds <- tryCatch(
      prep_and_predict(parsed, local_args, models_combined, mix_ratio_workflows),
      error = function(e) e
    )
    if (inherits(preds, "error")) {
      res$status <- 400
      return(list(error = preds$message))
    }

    preds
  })

  pr_run(router, host = args$host, port = args$port)
}

main <- function() {
  args <- parse_args(commandArgs(trailingOnly = TRUE))

  models_combined <- read_rds("models/cbc_models_combined.RDS")
  mix_ratio_workflows <- read_rds("models/cbc_mix_ratio_model.RDS")

  if (identical(args$mode, "single")) {
    if (isTRUE(args$stream)) {
      run_single_stream(args, models_combined, mix_ratio_workflows)
    } else {
      run_single(args, models_combined, mix_ratio_workflows)
    }
  } else if (identical(args$mode, "batch")) {
    run_batch(args, models_combined, mix_ratio_workflows)
  } else if (identical(args$mode, "api")) {
    run_api(args, models_combined, mix_ratio_workflows)
  } else {
    cli_stop("Mode must be 'single', 'batch', or 'api'.")
  }
}

main()
