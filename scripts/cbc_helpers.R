preprocessCBCData <- function(data_long = read_csv("cbc_test_long.csv"), collection_interval = 48) {
  names(data_long) <- toupper(names(data_long))

  if (!"PATIENT_ID" %in% names(data_long) && "EPIC_MRN" %in% names(data_long)) {
    data_long <- data_long |> rename(PATIENT_ID = EPIC_MRN)
  }

  if (!"RESULT_VALUE_NUMERIC" %in% names(data_long) && "RESULT_VALUE" %in% names(data_long)) {
    data_long <- data_long |> rename(RESULT_VALUE_NUMERIC = RESULT_VALUE)
  }

  data_long <- data_long |>
    mutate(
      RESULT_VALUE_NUMERIC = as.numeric(RESULT_VALUE_NUMERIC),
      DRAWN_DT_TM = lubridate::ymd_hms(DRAWN_DT_TM, tz = "UTC", quiet = TRUE)
    ) |>
    filter(!is.na(DRAWN_DT_TM))

  data_wide <-
    data_long |>
    arrange(DRAWN_DT_TM) |>
    pivot_wider(id_cols = c("PATIENT_ID", "DRAWN_DT_TM"), names_from = "TASK_ASSAY", values_from = "RESULT_VALUE_NUMERIC", values_fn = last, values_fill = NA) |>
    distinct()

  data_wide |>
    group_by(PATIENT_ID) |>
    arrange(DRAWN_DT_TM) |>
    mutate(
      hours_since_prior = as.numeric(DRAWN_DT_TM - lag(DRAWN_DT_TM), units = "hours"),
      hours_to_post = as.numeric(lead(DRAWN_DT_TM) - DRAWN_DT_TM, units = "hours"),
      across(c("Hgb", "WBC", "Plt"), ~ ifelse(hours_since_prior > collection_interval, NA, lag(.)), .names = "{.col}_prior"),
      across(c("Hgb", "WBC", "Plt"), ~ ifelse(hours_to_post > collection_interval, NA, lead(.)), .names = "{.col}_post")
    ) |>
    ungroup()
}

label_pred_class <- function(x) {
  x <- as.character(x)
  case_when(
    x == "0" ~ "Real",
    x == "1" ~ "Contaminated",
    x == "[EQ]" ~ "Equivocal",
    TRUE ~ x
  )
}

makeCbcPredictions <- function(
  input_raw = read_csv("data/cbc_test_wide.csv"),
  models_combined = read_rds("models/cbc_models_combined.RDS"),
  mix_ratio_workflows = read_rds("models/cbc_mix_ratio_model.RDS")
) {
  library(tidyverse)
  library(tidymodels)

  input <- input_raw |>
    select(Hgb, Plt, WBC, Hgb_prior, Plt_prior, WBC_prior, Hgb_post, Plt_post, WBC_post) |>
    mutate(across(everything(), ~ suppressWarnings(as.numeric(.))))
  input$num_NA_realtime <- rowSums(is.na(input |> select(-matches("post"))))
  input$num_NA_retro <- rowSums(is.na(input))

  workflows <- models_combined |> map("workflow") |> map(bundle::unbundle)
  types <- models_combined |> map("type")

  preds <-
    pmap(list(workflows, types), function(workflow, type) {
      predict(workflow, input) |> set_names(paste0("pred_", type, "_CBC"))
    }) |>
    bind_cols() |>
    mutate(across(starts_with("pred_"), label_pred_class))

  probs <-
    pmap(list(workflows, types), function(workflow, type) {
      predict(workflow, input, type = "prob") |>
        select(.pred_1) |>
        set_names(paste0("prob_", type, "_CBC"))
    }) |>
    bind_cols()

  mix_ratios <- mix_ratio_workflows |>
    bundle::unbundle() |>
    predict(input) |>
    set_names("mix_ratio_CBC")

  output <- bind_cols(input, probs, preds, mix_ratios)

  output_no_NA <-
    output |>
    mutate(
      across(matches("^pred_Realtime"), ~ ifelse(num_NA_realtime > 0, NA, .)),
      across(matches("^pred_Retro"), ~ ifelse(num_NA_retro > 0, NA, .)),
      across(matches("^prob_Realtime"), ~ ifelse(num_NA_realtime > 0, NA, .)),
      across(matches("^prob_Retro"), ~ ifelse(num_NA_retro > 0, NA, .)),
      across(matches("mix_ratio"), ~ ifelse(num_NA_retro > 0, NA, .))
    ) |>
    mutate(
      across(matches("^pred_Realtime"), ~ ifelse(is.na(.) & num_NA_realtime == 0, "Equivocal", .)),
      across(matches("^pred_Retro"), ~ ifelse(is.na(.) & num_NA_retro == 0, "Equivocal", .))
    ) |>
    select(-matches("num_NA"))

  output_no_NA
  
}
