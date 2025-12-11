#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(tidyverse)
  library(tidymodels)
  library(jsonlite)
  library(plumber)
  source("scripts/bmp_helpers.R")
})

load_models <- function() {
  list(
    models_combined = read_rds(Sys.getenv("BMP_MODEL_PATH", "models/bmp_models_combined.RDS")),
    mix_ratio_models = read_rds(Sys.getenv("BMP_MIX_MODEL_PATH", "models/bmp_mix_ratio_models_combined.RDS"))
  )
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
  host <- Sys.getenv("HOST", "0.0.0.0")
  port <- as.integer(Sys.getenv("PORT", "8000"))

  models <- load_models()
  start_api(host, port, models)
}

main()
