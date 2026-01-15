normalize_assay_label <- function(x) {
  out <- tolower(as.character(x))
  out[is.na(x)] <- NA_character_
  gsub("[^a-z0-9]+", "", out)
}

cbc_synonyms <- list(
  Hgb = c("hgb", "hb", "hemoglobin", "haemoglobin"),
  WBC = c("wbc", "whitebloodcell", "whitebloodcells", "wcc", "leukocyte", "leukocytes"),
  Plt = c("plt", "platelet", "platelets", "thrombocyte", "thrombocytes")
)

cbc_synonym_lookup <- unlist(
  lapply(names(cbc_synonyms), function(canon) {
    normalized <- normalize_assay_label(unique(c(canon, cbc_synonyms[[canon]])))
    setNames(rep(canon, length(normalized)), normalized)
  }),
  use.names = TRUE
)

map_cbc_assay_names <- function(labels) {
  normalized <- normalize_assay_label(labels)
  mapped <- unname(cbc_synonym_lookup[normalized])

  unmatched <- which(is.na(mapped) | mapped == "")
  if (length(unmatched) > 0) {
    valid <- unmatched[!is.na(normalized[unmatched])]
    if (length(valid) == 0) {
      return(mapped)
    }
    targets <- names(cbc_synonyms)
    targets_norm <- normalize_assay_label(targets)
    distances <- adist(normalized[valid], targets_norm)
    closest_idx <- apply(distances, 1, which.min)
    closest_dist <- apply(distances, 1, min)
    max_dist <- pmax(1, floor(nchar(normalized[valid]) * 0.25))
    mapped[valid] <- ifelse(closest_dist <= max_dist, targets[closest_idx], labels[valid])
  }

  mapped
}

map_cbc_wide_names <- function(data_wide) {
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

    mapped_base <- map_cbc_assay_names(base)
    if (mapped_base %in% names(cbc_synonyms)) {
      new_names[[i]] <- paste0(mapped_base, suffix)
    }
  }

  names(data_wide) <- new_names
  data_wide
}

preprocessCBCData <- function(data_long = read_csv("cbc_test_long.csv"), collection_interval = 48) {
  names(data_long) <- toupper(names(data_long))

  is_long <- "TASK_ASSAY" %in% names(data_long) &&
    ("RESULT_VALUE" %in% names(data_long) || "RESULT_VALUE_NUMERIC" %in% names(data_long))

  if (!is_long) {
    return(map_cbc_wide_names(data_long))
  }

  if (!"RESULT_VALUE" %in% names(data_long) && "RESULT_VALUE_NUMERIC" %in% names(data_long)) {
    data_long <- data_long |> rename(RESULT_VALUE = RESULT_VALUE_NUMERIC)
  }

  data_long <- data_long |>
    mutate(
      TASK_ASSAY = map_cbc_assay_names(TASK_ASSAY),
      RESULT_VALUE_NUMERIC = readr::parse_number(as.character(RESULT_VALUE)),
      DRAWN_DT_TM = suppressWarnings(lubridate::parse_date_time(
        DRAWN_DT_TM,
        orders = c("ymd HMS", "ymd HM", "mdy HMS", "mdy HM", "mdy IMSp", "mdy IMp"),
        tz = "UTC"
      ))
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
