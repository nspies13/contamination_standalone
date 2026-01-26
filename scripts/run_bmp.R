#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(tidyverse)
  library(tidymodels)
  library(jsonlite)
  library(plumber)
  library(probably)
  source("scripts/bmp_helpers.R")
})

# Ensure cur_column is available when butchered recipes are evaluated.
assign("cur_column", dplyr::cur_column, envir = .GlobalEnv)

load_models <- function() {
  list(
    models_combined = read_rds(Sys.getenv("BMP_MODEL_PATH", "models/bmp_models_combined.RDS")),
    mix_ratio_models = read_rds(Sys.getenv("BMP_MIX_MODEL_PATH", "models/bmp_mix_ratio_models_combined.RDS"))
  )
}

run_batch <- function(input_file, output_file, input_format, models) {
  if (is.null(input_file) || is.null(output_file)) {
    stop("Batch mode requires --input-file and --output-file")
  }

  input <- read_csv(input_file, show_col_types = FALSE, progress = FALSE)
  input_wide <- if (identical(input_format, "long")) {
    required <- c("PATIENT_ID", "DRAWN_DT_TM", "TASK_ASSAY", "RESULT_VALUE")
    missing <- setdiff(required, toupper(names(input)))
    if (length(missing) > 0) {
      stop("Long-form input requires columns: ", paste(required, collapse = ", "), call. = FALSE)
    }
    preprocessBmpData(input)
  } else {
    input
  }
  preds <- makeBmpPredictions(
    input_raw = input_wide,
    models_combined = models$models_combined,
    mix_ratio_models = models$mix_ratio_models
  )
  extra_cols <- setdiff(names(preds), names(input_wide))
  output <- bind_cols(input_wide, preds[, extra_cols, drop = FALSE])
  write_csv(output, output_file)
  message("Wrote BMP predictions to ", output_file)
}

start_api <- function(host, port, models) {
  router <- pr()

  router <- pr_get(router, "/health", function() list(status = "ok"))

  router <- pr_post(router, "/predict_stream", function(req, res) {
    body <- req$postBody
    if (is.null(body) || identical(body, "")) {
      res$status <- 400
      return(list(error = "Empty request body"))
    }

    lines <- strsplit(body, "\n", fixed = TRUE)[[1]]
    lines <- lines[nzchar(trimws(lines))]
    if (length(lines) == 0) {
      res$status <- 400
      return(list(error = "No JSON lines provided"))
    }

    results <- lapply(lines, function(line) {
      payload <- tryCatch(
        as_tibble(jsonlite::fromJSON(line, simplifyDataFrame = TRUE)),
        error = function(e) e
      )
      if (inherits(payload, "error")) {
        return(list(error = payload$message))
      }

      preds <- tryCatch(
        makeBmpPredictions(
          input_raw = payload,
          models_combined = models$models_combined,
          mix_ratio_models = models$mix_ratio_models
        ),
        error = function(e) e
      )
      if (inherits(preds, "error")) {
        return(list(error = preds$message))
      }
      preds
    })

    results
  })

  router <- pr_post(router, "/predict", function(req, res) {
    body <- req$postBody
    if (is.null(body) || identical(body, "")) {
      res$status <- 400
      return(list(error = "Empty request body"))
    }

    payload <- tryCatch(
      as_tibble(jsonlite::fromJSON(body, simplifyDataFrame = TRUE)),
      error = function(e) e
    )
    if (inherits(payload, "error")) {
      res$status <- 400
      return(list(error = payload$message))
    }

    preds <- tryCatch(
      makeBmpPredictions(
        input_raw = payload,
        models_combined = models$models_combined,
        mix_ratio_models = models$mix_ratio_models
      ),
      error = function(e) e
    )
    if (inherits(preds, "error")) {
      res$status <- 400
      return(list(error = preds$message))
    }

    preds
  })

  pr_run(router, host = host, port = port)
}

main <- function() {
  args <- list(
    mode = Sys.getenv("MODE", "api"),
    host = Sys.getenv("HOST", "0.0.0.0"),
    port = as.integer(Sys.getenv("PORT", "8000")),
    input_file = NULL,
    output_file = NULL,
    input_format = "wide"
  )

  cli_args <- commandArgs(trailingOnly = TRUE)
  i <- 1
  while (i <= length(cli_args)) {
    flag <- cli_args[[i]]
    value <- cli_args[[i + 1]]
    if (flag %in% c("--mode", "-m")) {
      args$mode <- value
      i <- i + 2
    } else if (flag %in% c("--input-file", "-f")) {
      args$input_file <- value
      i <- i + 2
    } else if (flag %in% c("--output-file", "-o")) {
      args$output_file <- value
      i <- i + 2
    } else if (flag == "--input-format") {
      args$input_format <- tolower(value)
      i <- i + 2
    } else {
      i <- i + 1
    }
  }

  models <- load_models()
  if (identical(args$mode, "batch")) {
    run_batch(args$input_file, args$output_file, args$input_format, models)
  } else {
    start_api(args$host, args$port, models)
  }
}

main()
