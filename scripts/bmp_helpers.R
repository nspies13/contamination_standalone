lab_strings <- c("sodium", "chloride", "potassium_plas", "co2_totl", "bun", "creatinine", "calcium", "glucose")

rectangularizeResults <- function(input, label = "", fn = last) {
  
  input <- input |> mutate(RESULT_VALUE = as.numeric(str_replace_all(RESULT_VALUE, "<|>", "")))

  data_wide <-
    input %>%
    arrange(DRAWN_DT_TM) %>%
    pivot_wider(
      id_cols = c(PATIENT_ID, DRAWN_DT_TM),
      names_from = TASK_ASSAY,
      values_from = RESULT_VALUE,
      values_fill = NA,
      values_fn = fn
    ) |> 
    janitor::clean_names()
  
  data_wide_last <-
    data_wide %>%
    group_by(patient_id, drawn_dt_tm) %>%
    fill(any_of(lab_strings), .direction = "down") %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    distinct(
      patient_id,
      drawn_dt_tm,
      sodium,
      chloride,
      potassium_plas,
      co2_totl,
      bun,
      creatinine,
      calcium,
      glucose,
      .keep_all = T
    )
  
  data_wide_last
  
}

addPrePost <- function(input, lookback_hours = 48){
  
  input = input %>% mutate(index_id = row_number())
  
  long <- input %>% pivot_longer(any_of(lab_strings), names_to = "task_assay", values_to = "result_val", values_drop_na = T)
  
  priors <- long %>% arrange(drawn_dt_tm) %>% group_by(patient_id, task_assay) %>% mutate(prior = ifelse(as.numeric(difftime(drawn_dt_tm, lag(drawn_dt_tm), units = "hours")) < lookback_hours, lag(result_val), NA)) %>% pivot_wider(id_cols = matches("_id"), names_from = "task_assay", values_from = "prior", names_glue = "{task_assay}_prior") %>% ungroup()
  posts <- long %>% arrange(drawn_dt_tm) %>% group_by(patient_id, task_assay) %>% mutate(post = ifelse(as.numeric(difftime(lead(drawn_dt_tm), drawn_dt_tm, units = "hours")) < lookback_hours, lead(result_val), NA)) %>% pivot_wider(id_cols = matches("_id"), names_from = "task_assay", values_from = "post", names_glue = "{task_assay}_post") %>% ungroup()
  
  output <- left_join(input, priors %>% select(-patient_id, -epic_id, -specimen_id), by = "index_id") %>% left_join(posts %>% select(-patient_id, -epic_id, -specimen_id), by = "index_id") %>% select(-index_id)
  
  output
  
}

makeBmpPredictions <- function(input_raw = read_csv("data/bmp_test_wide.csv"),  models_combined = read_rds("models/bmp_models_combined.RDS"), mix_ratio_models = read_rds("models/bmp_mix_ratio_models_combined.RDS")){
  
  library(tidyverse)
  library(tidymodels)
  cur_column <- dplyr::cur_column
  
  shim_xgb_load_raw <- function() {
    if (!requireNamespace("xgboost", quietly = TRUE)) {
      return(invisible())
    }

    ns <- asNamespace("xgboost")
    if (exists("xgb.load.raw", envir = ns, inherits = FALSE)) {
      orig <- get("xgb.load.raw", envir = ns, inherits = FALSE)
      unlockBinding("xgb.load.raw", ns)
      assign(
        "xgb.load.raw",
        function(raw, as_booster = TRUE, ...) {
          orig(raw, as_booster = as_booster, ...)
        },
        envir = ns
      )
      lockBinding("xgb.load.raw", ns)
    }
  }
  
  # xgboost 2.x removed the as_booster argument; patch it back for bundled models
  shim_xgb_load_raw()

  input <- input_raw |>
    select(any_of(lab_strings), matches("prior|post")) |>
    mutate(across(everything(), ~ suppressWarnings(as.numeric(.))))
  input$num_NA_realtime <- rowSums(is.na(input |> select(-matches("post"))))
  input$num_NA_retro <- rowSums(is.na(input))
  
  workflows <- models_combined |> map("workflow") |> map(bundle::unbundle)
  fluids <- models_combined |> map("fluid")
  types <- models_combined |> map("type")
  
  preds <- 
    pmap(list(workflows, fluids, types), function(workflow, fluid, type) predict(workflow, input) |> set_names(paste0("pred_", type, "_", fluid))) |> 
    bind_cols() |> 
    mutate(
      any_realtime_pred = factor(if_any(matches("pred_Realtime") & !matches("LR"), ~ . == "1")),
      any_retrospective_pred = factor(if_any(matches("pred_Retrospective") & !matches("LR"), ~ . == "1")),
      any_realtime_pred_with_LR = factor(if_any(matches("pred_Realtime"), ~ . == "1")),
      any_retrospective_pred_with_LR = factor(if_any(matches("pred_Retrospective"), ~ . == "1")))

  probs <- suppressWarnings(
    pmap(list(workflows, fluids, types), function(workflow, fluid, type) {
      prob_tbl <- predict(workflow, input, type = "prob")

      # Prefer the fluid-specific probability column; fall back to .pred_1 or the last column.
      preferred_col <- paste0(".pred_", fluid)
      prob_col <- if (preferred_col %in% names(prob_tbl)) {
        preferred_col
      } else if (".pred_1" %in% names(prob_tbl)) {
        ".pred_1"
      } else {
        tail(names(prob_tbl), 1)
      }

      prob_tbl |>
        select(all_of(prob_col)) |>
        set_names(paste0("prob_", type, "_", fluid))
    }) |> 
    bind_cols() |> 
    rowwise() |> 
    mutate(
      max_prob_fluid_realtime = str_split_i(names(cur_data())[which.max(c_across(matches("prob_Real") & !matches("LR")))], "_", 3),
      max_prob_fluid_retrospective = str_split_i(names(cur_data())[which.max(c_across(matches("prob_Retro") & !matches("LR")))], "_", 3),
      max_prob_fluid_realtime_with_LR = str_split_i(names(cur_data())[which.max(c_across(matches("prob_Real")))], "_", 3),
      max_prob_fluid_retrospective_with_LR = str_split_i(names(cur_data())[which.max(c_across(matches("prob_Retro")))], "_", 3),
      max_realtime_prob = max(c_across(matches("Realtime", ignore.case = F) & !matches("LR")), na.rm = T),
      max_retrospective_prob = max(c_across(matches("Retrospective", ignore.case = F) & !matches("LR")), na.rm = T),
      max_realtime_prob_with_LR = max(c_across(matches("Realtime", ignore.case = F)), na.rm = T),
      max_retrospective_prob_with_LR = max(c_across(matches("Retrospective", ignore.case = F)), na.rm = T)
    )
  )
  
  mix_ratios <- tryCatch({
    shim_xgb_load_raw()
    mix_ratio_workflows <- mix_ratio_models |>
      map("workflow") |>
      map(bundle::unbundle)
    mix_ratio_fluids <- mix_ratio_models |> map("fluid")

    pmap(list(mix_ratio_workflows, mix_ratio_fluids), function(workflow, fluid) predict(workflow, input) |> select(.pred) |> set_names(paste0("mix_ratio_", fluid))) |> 
      bind_cols() |> 
      rowwise() |> 
      mutate(
        max_mix_ratio = max(c_across(!matches("LR"))),
        max_mix_ratio_with_LR = max(c_across(matches("mix_ratio")))
      )
  }, error = function(e) {
    warning("Mix ratio models unavailable; returning contamination probabilities only. Details: ", conditionMessage(e))
    tibble(
      max_mix_ratio = rep(NA_real_, nrow(input)),
      max_mix_ratio_with_LR = rep(NA_real_, nrow(input))
    )
  })
  
  output <- bind_cols(input, probs, preds, mix_ratios) 
  
  output_no_NA <-
    output |>
      mutate(
        across(matches("Realtime"), ~ ifelse(num_NA_realtime > 0, NA, .)),
        across(matches("mix_ratio|Retro"), ~ifelse(num_NA_retro > 0, NA, .))) |>
      select(-matches("num_NA"))

  output_no_NA 
  
}

preprocessBmpData <- function(data_long = read_csv("data/bmp_test_long.csv"), lookback_hours = 48) {
  # Normalize column names so downstream helpers can rely on them.
  names(data_long) <- toupper(names(data_long))

  if (!"PATIENT_ID" %in% names(data_long) && "EPIC_MRN" %in% names(data_long)) {
    data_long <- data_long |> rename(PATIENT_ID = EPIC_MRN)
  }

  if (!"RESULT_VALUE" %in% names(data_long) && "RESULT_VALUE_NUMERIC" %in% names(data_long)) {
    data_long <- data_long |> rename(RESULT_VALUE = RESULT_VALUE_NUMERIC)
  }

  data_long <- data_long |>
    mutate(
      RESULT_VALUE = as.character(RESULT_VALUE),
      DRAWN_DT_TM = lubridate::ymd_hms(DRAWN_DT_TM, tz = "UTC", quiet = TRUE)
    ) |>
    filter(!is.na(DRAWN_DT_TM))

  data_long |>
    rectangularizeResults() |>
    addPrePost(lookback_hours = lookback_hours)
}
