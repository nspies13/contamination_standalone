lab_strings <- c("sodium", "chloride", "potassium_plas", "co2_totl", "bun", "creatinine", "calcium", "glucose")

normalize_assay_label <- function(x) {
  out <- tolower(as.character(x))
  out[is.na(x)] <- NA_character_
  gsub("[^a-z0-9]+", "", out)
}

bmp_synonyms <- list(
  sodium = c("sodium", "na", "na+", "sod"),
  chloride = c("chloride", "cl", "cl-", "chl", "chlor"),
  potassium_plas = c("potassium", "potassiumplasma", "potassiumplas", "potassium_plas", "k", "k+", "potas"),
  co2_totl = c("co2", "co2totl", "co2total", "co2totl", "tco2", "bicarb", "bicarbonate", "hco3", "hco3-", "carbondioxide"),
  bun = c("bun", "bloodureanitrogen", "ureanitrogen", "urean", "urea"),
  creatinine = c("creatinine", "creat", "cr", "creatinin"),
  calcium = c("calcium", "ca", "ca2+", "ca++", "cal"),
  glucose = c("glucose", "glu", "gluc", "bloodglucose", "serumglucose")
)

bmp_synonym_lookup <- unlist(
  lapply(names(bmp_synonyms), function(canon) {
    normalized <- normalize_assay_label(unique(c(canon, bmp_synonyms[[canon]])))
    setNames(rep(canon, length(normalized)), normalized)
  }),
  use.names = TRUE
)

map_bmp_assay_names <- function(labels) {
  normalized <- normalize_assay_label(labels)
  mapped <- unname(bmp_synonym_lookup[normalized])

  unmatched <- which(is.na(mapped) | mapped == "")
  if (length(unmatched) > 0) {
    valid <- unmatched[!is.na(normalized[unmatched])]
    if (length(valid) == 0) {
      return(mapped)
    }
    targets <- names(bmp_synonyms)
    targets_norm <- normalize_assay_label(targets)
    distances <- adist(normalized[valid], targets_norm)
    closest_idx <- apply(distances, 1, which.min)
    closest_dist <- apply(distances, 1, min)
    max_dist <- pmax(1, floor(nchar(normalized[valid]) * 0.25))
    mapped[valid] <- ifelse(closest_dist <= max_dist, targets[closest_idx], labels[valid])
  }

  mapped
}

map_bmp_wide_names <- function(data_wide) {
  original <- names(data_wide)
  cleaned <- janitor::make_clean_names(original)
  new_names <- cleaned

  for (i in seq_along(cleaned)) {
    suffix <- ""
    base <- cleaned[[i]]
    if (grepl("_prior$", base)) {
      suffix <- "_prior"
      base <- sub("_prior$", "", base)
    } else if (grepl("_post$", base)) {
      suffix <- "_post"
      base <- sub("_post$", "", base)
    }

    mapped_base <- map_bmp_assay_names(base)
    if (mapped_base %in% names(bmp_synonyms)) {
      new_names[[i]] <- paste0(mapped_base, suffix)
    }
  }

  names(data_wide) <- new_names
  data_wide
}

label_pred_class <- function(x) {
  
  out <- as.character(x)
  out[which(out == "0")] <- "Real"
  out[which_equivocal(x)] <- "Equivocal"
  out[which(out == "1")] <- "Contaminated"

  out

}

rectangularizeResults <- function(input, label = "", fn = last) {
  
  input <- input |>
    mutate(RESULT_VALUE = readr::parse_number(str_replace_all(RESULT_VALUE, "<|>", "")))

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
      .keep_all = TRUE
    )
  
  data_wide_last
  
}

addPrePost <- function(input, lookback_hours = 48){
  
  input = input %>% mutate(index_id = row_number())
  
  long <- input %>% pivot_longer(any_of(lab_strings), names_to = "task_assay", values_to = "result_val", values_drop_na = T)
  
  priors <- long %>% arrange(drawn_dt_tm) %>% group_by(patient_id, task_assay) %>% mutate(prior = ifelse(as.numeric(difftime(drawn_dt_tm, lag(drawn_dt_tm), units = "hours")) < lookback_hours, lag(result_val), NA)) %>% pivot_wider(id_cols = matches("_id"), names_from = "task_assay", values_from = "prior", names_glue = "{task_assay}_prior") %>% ungroup()
  posts <- long %>% arrange(drawn_dt_tm) %>% group_by(patient_id, task_assay) %>% mutate(post = ifelse(as.numeric(difftime(lead(drawn_dt_tm), drawn_dt_tm, units = "hours")) < lookback_hours, lead(result_val), NA)) %>% pivot_wider(id_cols = matches("_id"), names_from = "task_assay", values_from = "post", names_glue = "{task_assay}_post") %>% ungroup()
  
  output <- left_join(input, priors %>% select(-any_of(c("patient_id"))), by = "index_id") %>%
    left_join(posts %>% select(-any_of(c("patient_id"))), by = "index_id") %>%
    select(-index_id)
  
  output
  
}

makeBmpPredictions <- function(input_raw = read_csv("data/bmp_test_wide.csv"),  models_combined = read_rds("models/bmp_models_combined.RDS"), mix_ratio_models = read_rds("models/bmp_mix_ratio_models_combined.RDS")){
  
  library(tidyverse)
  library(tidymodels)
  cur_column <- dplyr::cur_column

  input <- input_raw |>
    select(any_of(lab_strings), matches("prior|post")) |>
    mutate(across(everything(), ~ suppressWarnings(as.numeric(.))))
  input$num_NA_realtime <- rowSums(is.na(input |> select(-matches("post"))))
  input$num_NA_retro <- rowSums(is.na(input))
  
  workflows <- models_combined |> map("workflow") |> map(bundle::unbundle)
  fluids <- models_combined |> map("fluid")
  types <- models_combined |> map("type")
  
  preds <- 
    pmap(list(workflows, fluids, types), function(workflow, fluid, type) {
      predict(workflow, input) |> set_names(paste0("pred_", type, "_", fluid))
    }) |>
    bind_cols() |>
    mutate(across(starts_with("pred_"), label_pred_class)) |>
    mutate(
      any_realtime_pred = factor(if_any(matches("pred_Realtime") & !matches("LR"), ~ coalesce(. == "Contaminated", FALSE))),
      any_retrospective_pred = factor(if_any(matches("pred_Retrospective") & !matches("LR"), ~ coalesce(. == "Contaminated", FALSE))),
      any_realtime_pred_with_LR = factor(if_any(matches("pred_Realtime"), ~ coalesce(. == "Contaminated", FALSE))),
      any_retrospective_pred_with_LR = factor(if_any(matches("pred_Retrospective"), ~ coalesce(. == "Contaminated", FALSE))))

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
        mutate(across(everything(), ~round(., 3))) |> 
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
    mix_ratio_workflows <- mix_ratio_models |>
      map("workflow") |>
      map(bundle::unbundle)
    mix_ratio_fluids <- mix_ratio_models |> map("fluid")

    pmap(list(mix_ratio_workflows, mix_ratio_fluids), function(workflow, fluid) predict(workflow, input) |> transmute(.pred = round(.pred, 3)) |> set_names(paste0("mix_ratio_", fluid))) |> 
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
  
  output <- bind_cols(input_raw, probs, preds, mix_ratios) |>
    mutate(
      num_NA_realtime = input$num_NA_realtime,
      num_NA_retro = input$num_NA_retro
    )
  
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
    mutate(
      any_realtime_pred = factor(if_any(
        matches("^pred_Realtime") & !matches("LR"),
        ~ coalesce(. == "Contaminated", FALSE)
      )),
      any_retrospective_pred = factor(if_any(
        matches("^pred_Retrospective") & !matches("LR"),
        ~ coalesce(. == "Contaminated", FALSE)
      )),
      any_realtime_pred_with_LR = factor(if_any(
        matches("^pred_Realtime"),
        ~ coalesce(. == "Contaminated", FALSE)
      )),
      any_retrospective_pred_with_LR = factor(if_any(
        matches("^pred_Retrospective"),
        ~ coalesce(. == "Contaminated", FALSE)
      ))
    ) |>
    select(-matches("num_NA"))

  output_no_NA 
  
}

preprocessBmpData <- function(data_long = read_csv("data/bmp_test_long.csv"), lookback_hours = 48) {
  # Normalize column names so downstream helpers can rely on them.
  names(data_long) <- toupper(names(data_long))

  is_long <- "TASK_ASSAY" %in% names(data_long) &&
    ("RESULT_VALUE" %in% names(data_long) || "RESULT_VALUE_NUMERIC" %in% names(data_long))

  if (!is_long) {
    return(map_bmp_wide_names(data_long))
  }

  if (!"RESULT_VALUE" %in% names(data_long) && "RESULT_VALUE_NUMERIC" %in% names(data_long)) {
    data_long <- data_long |> rename(RESULT_VALUE = RESULT_VALUE_NUMERIC)
  }

  data_long <- data_long |>
    mutate(
      TASK_ASSAY = map_bmp_assay_names(TASK_ASSAY),
      RESULT_VALUE = as.character(RESULT_VALUE),
      DRAWN_DT_TM = suppressWarnings(lubridate::parse_date_time(
        DRAWN_DT_TM,
        orders = c("ymd HMS", "ymd HM", "mdy HMS", "mdy HM", "mdy IMSp", "mdy IMp"),
        tz = "UTC"
      ))
    ) |>
    filter(!is.na(DRAWN_DT_TM))

  data_long |>
    rectangularizeResults() |>
    addPrePost(lookback_hours = lookback_hours)
}
