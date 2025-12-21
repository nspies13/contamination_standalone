library(tidyverse)
library(tidymodels)
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

data <- data |>
  mutate(across(any_of(all_cols), ~ suppressWarnings(as.numeric(.)))) |>
  mutate(across(any_of(all_cols), ~ ifelse(is.finite(.), ., NA_real_))) |>
  drop_na(any_of(all_cols))

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
  rec_base <-
    recipe(target ~ ., data = train_df |> select(-mix_ratio)) |>
    step_normalize(all_predictors()) |>
    step_pca(all_predictors(), num_comp = 5)

  model <- boost_tree(mode = "classification", engine = "lightgbm", trees = 500, learn_rate = 0.1)

  wf <- workflow(rec_base, model)

  wf_realtime <- wf
  wf_retro <- wf

  list(
    list(workflow = bundle(wf_realtime |> fit(train_df |> select(-mix_ratio)) |> butcher()), type = "Realtime", fluid = "CBC"),
    list(workflow = bundle(wf_retro |> fit(train_df |> select(-mix_ratio)) |> butcher()), type = "Retrospective", fluid = "CBC")
  )
}

train_mix_model <- function(train_df) {
  rec <-
    recipe(mix_ratio ~ ., data = train_df |> select(-target)) |>
    step_normalize(all_predictors()) |>
    step_pca(all_predictors(), num_comp = 5)

  model <- boost_tree(mode = "regression", engine = "lightgbm", trees = 300, learn_rate = 0.1)

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
