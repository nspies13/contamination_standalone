#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  source("scripts/bmp_helpers.R")
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  message("Usage: Rscript scripts/preprocess_bmp.R <input_long_csv> <output_wide_csv> [lookback_hours=48]")
  quit(status = 1)
}

input_path <- args[[1]]
output_path <- args[[2]]
lookback <- if (length(args) >= 3) as.numeric(args[[3]]) else 48

input <- read_csv(input_path, show_col_types = FALSE, progress = FALSE)
output <- preprocessBmpData(input, lookback_hours = lookback)

write_csv(output, output_path)
message("Wrote BMP wide data to ", output_path)
