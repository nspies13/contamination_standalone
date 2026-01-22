library(tidyverse)
library(tidymodels)
library(bonsai)
library(bundle)
library(butcher)

# Usage:
# Rscript scripts/train_cbc_models.R <training_template_csv> [output_models_rds] [output_mix_rds]
# - training_template_csv: wide CBC data with prior/post columns (e.g., data/cbc_test_wide.csv)
# - output_models_rds: default models/cbc_models_combined.RDS
# - output_mix_rds: default models/cbc_mix_ratio_model.RDS

args <- commandArgs(trailingOnly = TRUE)
no_mix <- "--no-mix" %in% args
args <- args[args != "--no-mix"]
if (length(args) < 1) {
  stop("Usage: Rscript scripts/train_cbc_models.R <training_template.csv> [output_models_rds] [output_mix_rds] [--no-mix]")
}

template_path <- args[[1]]
models_out <- ifelse(length(args) >= 2, args[[2]], "models/cbc_models_combined.RDS")
mix_out <- ifelse(length(args) >= 3, args[[3]], "models/cbc_mix_ratio_model.RDS")
dir.create(dirname(models_out), recursive = TRUE, showWarnings = FALSE)
if (!no_mix) {
  dir.create(dirname(mix_out), recursive = TRUE, showWarnings = FALSE)
}

data <- readr::read_csv(template_path, show_col_types = FALSE)

lab_cols <- c("Hgb", "Plt", "WBC")
all_cols <- c(lab_cols, paste0(lab_cols, "_prior"), paste0(lab_cols, "_post"))

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

data <- data |>
  mutate(across(any_of(all_cols), ~ suppressWarnings(as.numeric(.)))) |>
  mutate(across(any_of(all_cols), ~ ifelse(is.finite(.), ., NA_real_))) |>
  filter(if_all(any_of(all_cols), ~ !is.na(.) & . > 0)) |>
  drop_na(any_of(all_cols))

use_random_forest <- nrow(data) < 200000

make_binary_training <- function(train_input) {
  n <- nrow(train_input)
  n_uncontam <- floor(n / 2)
  n_contam <- n - n_uncontam

  set.seed(123)
  idx <- sample(seq_len(n), n)
  idx_uncontam <- idx[1:n_uncontam]
  idx_contam <- idx[(n_uncontam + 1):n]

  clean <- train_input[idx_uncontam, ] |>
    mutate(mix_ratio = 0, target = factor("0", levels = c("0", "1")))

  ratios <- pmin(runif(n_contam, 0.1, 0.8), 0.8)
  contam <- train_input[idx_contam, ] |>
    mutate(mix_ratio = ratios) |>
    mutate(across(all_of(all_cols), ~ . * (1 - mix_ratio))) |>
    mutate(target = factor("1", levels = c("0", "1")))

  bind_rows(clean, contam) |>
    select(target, all_of(all_cols), mix_ratio)
}

train_class_models <- function(train_df) {
  prior_exprs <- make_log_delta_prior_exprs(lab_cols)
  post_exprs <- make_log_delta_post_exprs(lab_cols)

  rec_retro <-
    recipe(train_df |> select(-mix_ratio)) |>
    update_role(target, new_role = "outcome") |>
    update_role(all_of(lab_cols), new_role = "predictor") |>
    step_mutate(!!!prior_exprs) |>
    step_mutate(!!!post_exprs) |>
    step_pca(matches("delta_prior"), num_comp = 3, keep_original_cols = TRUE, options = list(center = TRUE, scale. = TRUE), prefix = "prior_PC") |>
    step_pca(matches("delta_post"), num_comp = 3, keep_original_cols = TRUE, options = list(center = TRUE, scale. = TRUE), prefix = "post_PC") |>
    step_pca(matches("delta"), num_comp = 3, prefix = "all_PC", options = list(center = TRUE, scale. = TRUE), keep_original_cols = TRUE)

  rec_realtime <-
    recipe(train_df |> select(-mix_ratio)) |>
    update_role(target, new_role = "outcome") |>
    update_role(all_of(lab_cols), new_role = "predictor") |>
    step_mutate(!!!prior_exprs) |>
    step_pca(all_predictors(), num_comp = 3, keep_original_cols = TRUE, options = list(center = TRUE, scale. = TRUE), prefix = "all_PC")

  model <- if (use_random_forest) {
    rand_forest(mode = "classification", engine = "ranger", trees = 500)
  } else {
    boost_tree(mode = "classification", engine = "lightgbm", trees = 1000, tree_depth = 10, learn_rate = 0.3)
  }

  wf_realtime <- workflow(rec_realtime, model)
  wf_retro <- workflow(rec_retro, model)

  list(
    list(workflow = bundle(wf_realtime |> fit(train_df |> select(-mix_ratio)) |> butcher()), type = "Realtime", fluid = "CBC"),
    list(workflow = bundle(wf_retro |> fit(train_df |> select(-mix_ratio)) |> butcher()), type = "Retrospective", fluid = "CBC")
  )
}

train_mix_model <- function(train_df) {
  prior_exprs <- make_log_delta_prior_exprs(lab_cols)
  post_exprs <- make_log_delta_post_exprs(lab_cols)

  rec <-
    recipe(data = train_df |> select(-target)) |>
    update_role(mix_ratio, new_role = "outcome") |>
    step_mutate(!!!prior_exprs) |>
    step_mutate(!!!post_exprs) |>
    step_pca(matches("delta_prior"), num_comp = 3, keep_original_cols = TRUE, options = list(center = TRUE, scale. = TRUE), prefix = "prior_PC") |>
    step_pca(matches("delta_post"), num_comp = 3, keep_original_cols = TRUE, options = list(center = TRUE, scale. = TRUE), prefix = "post_PC") |>
    step_pca(matches("delta"), num_comp = 3, prefix = "all_PC", options = list(center = TRUE, scale. = TRUE), keep_original_cols = TRUE)

  model <- boost_tree(mode = "regression", engine = "lightgbm", trees = 1000, learn_rate = 0.5)

  wf <- workflow(rec, model) |>
    fit(train_df |> select(-target)) |>
    butcher() |>
    bundle()

  wf
}

train_df <- make_binary_training(data)

models_combined <- train_class_models(train_df)
mix_workflow <- if (no_mix) NULL else train_mix_model(train_df)

write_rds(models_combined, models_out)
if (!no_mix) {
  write_rds(mix_workflow, mix_out)
}
message("Wrote CBC models to: ", models_out)
if (!no_mix) {
  message("Wrote CBC mix model to: ", mix_out)
}
