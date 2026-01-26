library(tidyverse)
library(tidymodels)
library(bonsai)
library(bundle)
library(butcher)
library(ranger)

# Usage:
# Rscript scripts/train_bmp_models.R <training_template_csv> <fluids_concentrations_csv> [output_rds_path]
# - training_template_csv: wide BMP data with prior/post columns to sample from.
# - fluids_concentrations_csv: e.g., data/fluids_concentrations.tsv
# - output_rds_path: optional, default models/bmp_models_combined.RDS

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("Usage: Rscript scripts/train_bmp_models.R <training_template.csv> <fluids_concentrations.csv> [output_rds_path]")
}

template_path <- args[[1]]
fluids_path <- args[[2]]
output_path <- ifelse(length(args) >= 3, args[[3]], "models/bmp_models_combined.RDS")
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

read_fluids <- function(path) {
  delim <- ifelse(grepl("\\.tsv$", tolower(path)), "\t", ",")
  readr::read_delim(path, delim = delim, show_col_types = FALSE)
}

data <- read_csv(template_path, show_col_types = FALSE)
fluids <- read_fluids(fluids_path)

lab_strings_bmp_no_gap <- c("sodium", "chloride", "potassium_plas", "co2_totl", "bun", "creatinine", "calcium", "glucose")
all_lab_cols <- c(
  lab_strings_bmp_no_gap,
  paste0(lab_strings_bmp_no_gap, "_prior"),
  paste0(lab_strings_bmp_no_gap, "_post")
)

make_log_delta_prior_exprs <- function(cols) {
  exprs <- map(cols, function(col) {
    prior_col <- paste0(col, "_prior")
    rlang::expr(log(0.01 + !!rlang::sym(col) / ifelse(!!rlang::sym(prior_col) == 0, 0.1, !!rlang::sym(prior_col))))
  })
  set_names(exprs, paste0(cols, "_log_delta_prior_prop"))
}

make_log_delta_post_exprs <- function(cols) {
  exprs <- map(cols, function(col) {
    post_col <- paste0(col, "_post")
    rlang::expr(log(0.01 + !!rlang::sym(post_col) / ifelse(!!rlang::sym(col) == 0, 0.1, !!rlang::sym(col))))
  })
  set_names(exprs, paste0(cols, "_log_delta_post_prop"))
}

data <- data %>%
  mutate(across(any_of(all_lab_cols), ~ suppressWarnings(as.numeric(.)))) %>%
  mutate(across(any_of(all_lab_cols), ~ ifelse(is.finite(.), ., NA_real_))) %>%
  filter(if_all(any_of(all_lab_cols), ~ !is.na(.) & . > 0)) %>%
  drop_na(any_of(all_lab_cols))

use_random_forest <- nrow(data) < 200000

simulateContaminationRow <- function(input, mix_ratio, fluid) {
  cols <- names(fluid)[which(names(fluid) %in% names(input))]

  output <-
    input %>%
    mutate(across(all_of(cols), ~(1 - mix_ratio) * . + fluid[[cur_column()]] * mix_ratio)) %>%
    select(all_of(cols), mix_ratio)

  output %>%
    mutate(across(c("sodium", "chloride", "co2_totl", "bun", "glucose"), ~ round(.))) %>%
    mutate(across(c("potassium_plas", "calcium"), ~ round(., 1))) %>%
    mutate(creatinine = round(creatinine, 2)) %>%
    mutate(label = fluid[["fluid"]])
}

makeMixRatios <- function(contam_rate = 0.1, contam_train_input, fluid_names_tar) {
  minimum_significant_contamination <- ifelse(grepl("D5", fluid_names_tar), 0.05, 0.15)
  out <- round(rbeta(contam_rate * nrow(contam_train_input), 1, 5), 2) + minimum_significant_contamination
  out[out >= 0.8] <- 0.8
  out
}

makeSimulatedBinaryTrainingData <- function(train_input, fluid_row) {
  n <- nrow(train_input)
  n_uncontam <- floor(n / 2)
  n_contam <- n - n_uncontam

  set.seed(123)
  idx <- sample(seq_len(n), n)
  idx_uncontam <- idx[1:n_uncontam]
  idx_contam <- idx[(n_uncontam + 1):n]

  input_uncontam <- train_input[idx_uncontam, ] %>%
    mutate(mix_ratio = NA, label = "Patient")

  sim_rows <- train_input[idx_contam, ]
  mix_ratios <- makeMixRatios(contam_rate = 1, contam_train_input = sim_rows, fluid_names_tar = fluid_row[["fluid"]])
  mix_ratios <- mix_ratios[seq_len(nrow(sim_rows))]

  sim_rows <- sim_rows %>%
    mutate(mix_ratio = mix_ratios, label = fluid_row[["fluid"]])

  tmp <- simulateContaminationRow(sim_rows, sim_rows$mix_ratio, fluid_row)
  sim_rows[, names(tmp)] <- tmp

  bind_rows(input_uncontam, sim_rows) %>%
    mutate(target = factor(if_else(label == "Patient", "0", "1"), levels = c("0", "1"))) %>%
    select(
      target,
      all_of(lab_strings_bmp_no_gap),
      paste0(lab_strings_bmp_no_gap, "_prior"),
      paste0(lab_strings_bmp_no_gap, "_post")
    )
}

train_one_fluid <- function(train_input, fluid_row) {
  train_input <- train_input |>
    drop_na(
      all_of(lab_strings_bmp_no_gap),
      paste0(lab_strings_bmp_no_gap, "_prior"),
      paste0(lab_strings_bmp_no_gap, "_post")
    )

  train <- makeSimulatedBinaryTrainingData(train_input, fluid_row)
  prior_exprs <- make_log_delta_prior_exprs(lab_strings_bmp_no_gap)
  post_exprs <- make_log_delta_post_exprs(lab_strings_bmp_no_gap)

  rec_retro <-
    recipe(train) |>
      update_role(target, new_role = "outcome") |>
      update_role(all_of(lab_strings_bmp_no_gap), new_role = "predictor") |>
      step_mutate(!!!prior_exprs) |>
      step_mutate(!!!post_exprs) |>
      step_pca(matches("delta_prior"), num_comp = 3, keep_original_cols = TRUE, options = list(center = TRUE, scale. = TRUE), prefix = "prior_PC") |>
      step_pca(matches("delta_post"), num_comp = 3, keep_original_cols = TRUE, options = list(center = TRUE, scale. = TRUE), prefix = "post_PC") |>
      step_pca(matches("delta"), num_comp = 3, prefix = "all_PC", options = list(center = TRUE, scale. = TRUE), keep_original_cols = TRUE)
  rec_retro$template <- rec_retro$template |> slice_head(n = 10)

  rec_realtime <-
    recipe(train) |>
      update_role(target, new_role = "outcome") |>
      update_role(all_of(lab_strings_bmp_no_gap), new_role = "predictor") |>
      step_mutate(!!!prior_exprs) |>
      step_pca(all_predictors(), num_comp = 3, keep_original_cols = TRUE, options = list(center = TRUE, scale. = TRUE), prefix = "all_PC") |>
      step_pca(matches("delta_prior"), num_comp = 3, keep_original_cols = TRUE, options = list(center = TRUE, scale. = TRUE), prefix = "prior_PC")
  rec_realtime$template <- rec_realtime$template |> slice_head(n = 10)
  
  model <- if (use_random_forest) {
    rand_forest(mode = "classification", engine = "ranger", trees = 500)
  } else {
    boost_tree(mode = "classification", engine = "lightgbm", tree_depth = 10, trees = 1000, learn_rate = 0.3, min_n = 32, loss_reduction = 0.1)
  }

  wf_realtime <- workflow(rec_realtime, model) |> add_tailor(tailor() |> adjust_equivocal_zone(value = 0.2, threshold = 0.7))
  wf_retro <- workflow(rec_retro, model) |> add_tailor(tailor() |> adjust_equivocal_zone(value = 0.2, threshold = 0.7))

  wf_fit_realtime <- wf_realtime |> fit(train) |> butcher()
  wf_fit_retro <- wf_retro |> fit(train) |> butcher()

  list(
    list(workflow = bundle(wf_fit_realtime), type = "Realtime", fluid = fluid_row[["fluid"]]),
    list(workflow = bundle(wf_fit_retro), type = "Retrospective", fluid = fluid_row[["fluid"]])
  )

}

all_models <-
  fluids |>
    (\(df) split(df, seq_len(nrow(df))))() |>
    map(~ train_one_fluid(data, .x)) |>
    flatten()

write_rds(all_models, output_path)
message("Wrote combined BMP models to: ", output_path)
