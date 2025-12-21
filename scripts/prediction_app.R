library(shiny)
library(tidyverse)
library(tidymodels)
library(jsonlite)
library(readr)
library(DT)


app_file <- sys.frame(1)$ofile
app_dir <- if (!is.null(app_file) && nzchar(app_file)) {
  dirname(normalizePath(app_file, mustWork = FALSE))
} else {
  getwd()
}
root <- normalizePath(
  if (basename(app_dir) == "scripts") file.path(app_dir, "..") else app_dir,
  mustWork = FALSE
)

model_path <- function(env_var, default_path) {
  value <- Sys.getenv(env_var, unset = "")
  if (!nzchar(value)) {
    default_path
  } else {
    value
  }
}

open_rds_connection <- function(path) {
  header <- readBin(path, "raw", n = 6)
  if (length(header) >= 2 && identical(header[1:2], charToRaw("X\n"))) {
    return(file(path, "rb"))
  }
  if (length(header) >= 2 && identical(header[1:2], charToRaw("B\n"))) {
    return(file(path, "rb"))
  }
  if (length(header) >= 2 && identical(header[1:2], charToRaw("A\n"))) {
    return(file(path, "rb"))
  }
  if (length(header) >= 2 && header[1] == as.raw(0x1f) && header[2] == as.raw(0x8b)) {
    return(gzfile(path, "rb"))
  }
  if (length(header) >= 3 && identical(header[1:3], charToRaw("BZh"))) {
    return(bzfile(path, "rb"))
  }
  if (length(header) >= 6 && identical(as.integer(header[1:6]), c(0xFD, 0x37, 0x7A, 0x58, 0x5A, 0x00))) {
    return(xzfile(path, "rb"))
  }
  file(path, "rb")
}

safe_read_rds <- function(path) {
  if (!file.exists(path)) {
    stop(paste0("Model file not found: ", path), call. = FALSE)
  }
  tryCatch(
    {
      con <- open_rds_connection(path)
      on.exit(close(con), add = TRUE)
      readRDS(con)
    },
    error = function(e) {
      stop(paste0("Failed to read model file: ", path, " (", e$message, ")"), call. = FALSE)
    }
  )
}
source(file.path(root, "scripts", "bmp_helpers.R"))
source(file.path(root, "scripts", "cbc_helpers.R"))

load_bmp_models <- function() {
  list(
    models_combined = safe_read_rds(model_path("BMP_MODEL_PATH", file.path(root, "models/bmp_models_combined.RDS"))),
    mix_ratio_models = safe_read_rds(model_path("BMP_MIX_MODEL_PATH", file.path(root, "models/bmp_mix_ratio_models_combined.RDS")))
  )
}

load_cbc_models <- function() {
  list(
    models_combined = safe_read_rds(model_path("CBC_MODEL_PATH", file.path(root, "models/cbc_models_combined.RDS"))),
    mix_ratio_workflows = safe_read_rds(model_path("CBC_MIX_MODEL_PATH", file.path(root, "models/cbc_mix_ratio_model.RDS")))
  )
}

bmp_fluids_tbl <- read_delim(file.path(root, "data", "fluid_concentrations.tsv"), delim = "\t", show_col_types = FALSE)
bmp_fluids <- bmp_fluids_tbl$fluid

read_upload <- function(fileinfo) {
  req(fileinfo)
  ext <- tools::file_ext(fileinfo$name)
  delim <- ifelse(tolower(ext) %in% c("tsv", "txt"), "\t", ",")
  read_delim(fileinfo$datapath, delim = delim, show_col_types = FALSE)
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML(
      "
      .sidebar-section { margin-bottom: 16px; padding-bottom: 12px; border-bottom: 1px solid #e5e7eb; }
      .sidebar-section:last-child { border-bottom: 0; padding-bottom: 0; }
      .section-title { font-weight: 600; margin-bottom: 8px; }
      .button-row { display: flex; gap: 8px; align-items: center; }
      .primary-btn { background: #2563eb; color: #fff; border: none; }
      .primary-btn:hover { background: #1d4ed8; }
      .secondary-btn { background: #e5e7eb; color: #111827; border: none; }
      .secondary-btn:hover { background: #d1d5db; }
      .loader {
        border: 3px solid #e5e7eb;
        border-top: 3px solid #2563eb;
        border-radius: 50%;
        width: 28px;
        height: 28px;
        animation: spin 1s linear infinite;
        margin-right: 8px;
      }
      table.label-table { border-collapse: collapse; margin: 12px auto; }
      table.label-table th, table.label-table td { border: 1px solid #d1d5db; padding: 12px 20px; text-align: center; }
      .review-actions { display: flex; justify-content: center; gap: 8px; margin-top: 16px; }
      .input-hint { font-size: 12px; color: #6b7280; margin-top: 4px; }
      .format-badge { display: inline-block; padding: 2px 8px; border-radius: 999px; background: #e0f2fe; color: #075985; font-size: 12px; margin-top: 6px; }
      #fluids .shiny-options-group,
      #train_fluids .shiny-options-group {
        columns: 3;
        -webkit-columns: 3;
        -moz-columns: 3;
      }
      @keyframes spin { 0% { transform: rotate(0deg); } 100% { transform: rotate(360deg); } }
    "
    )),
    tags$script(HTML(
      "
      Shiny.addCustomMessageHandler('triggerDownload', function(message) {
        var el = document.getElementById(message.id);
        if (el) { el.click(); }
      });
    "
    ))
  ),
  titlePanel("FluidFlagger.ai"),
  sidebarLayout(
    sidebarPanel(
      div(
        class = "sidebar-section",
        div(class = "section-title", "Mode"),
        radioButtons(
          "mode",
          NULL,
          choices = c("Predict", "Train", "Review"),
          inline = TRUE
        ),
        radioButtons(
          "dataset",
          "Model set",
          choices = c("BMP", "CBC"),
          inline = TRUE
        )
      ),
      div(
        class = "sidebar-section",
        div(class = "section-title", "Data"),
        conditionalPanel(
          condition = "input.mode == 'Predict'",
          fluidRow(
            column(
              6,
              fileInput(
                "file_wide",
                "Upload wide CSV/TSV",
                accept = c(".csv", ".tsv", ".txt"),
                placeholder = "Row = 1 BMP"
              )
            ),
            column(
              6,
              fileInput(
                "file_long",
                "Upload long CSV/TSV",
                accept = c(".csv", ".tsv", ".txt"),
                placeholder = "Row = 1 Analyte"
              )
            )
          )
        ),
        conditionalPanel(
          condition = "input.mode == 'Train'",
          fluidRow(
            column(
              6,
              fileInput(
                "train_file_wide",
                "Upload training (wide CSV/TSV)",
                accept = c(".csv", ".tsv", ".txt"),
                placeholder = "Row = 1 BMP"
              )
            ),
            column(
              6,
              fileInput(
                "train_file_long",
                "Upload training (long CSV/TSV)",
                accept = c(".csv", ".tsv", ".txt"),
                placeholder = "Row = 1 Analyte"
              )
            )
          )
        ),
        conditionalPanel(
          condition = "input.mode == 'Review'",
          fluidRow(
            column(
              6,
              fileInput(
                "review_file_wide",
                "Upload review (wide CSV/TSV)",
                accept = c(".csv", ".tsv", ".txt"),
                placeholder = "Row = 1 BMP"
              )
            ),
            column(
              6,
              fileInput(
                "review_file_long",
                "Upload review (long CSV/TSV)",
                accept = c(".csv", ".tsv", ".txt"),
                placeholder = "Row = 1 Analyte"
              )
            )
          )
        ),
        conditionalPanel(
          condition = "input.dataset == 'BMP' && input.mode == 'Predict'",
          checkboxGroupInput(
            "fluids",
            "Fluids to include",
            choices = bmp_fluids,
            selected = bmp_fluids
          ),
          fileInput(
            "bmp_model_file",
            "Custom BMP models (combined RDS, optional)",
            accept = c(".rds", ".RDS")
          ),
          fileInput(
            "bmp_mix_model_file",
            "Custom BMP mix ratio models (RDS, optional)",
            accept = c(".rds", ".RDS")
          )
        ),
        conditionalPanel(
          condition = "input.dataset == 'CBC' && input.mode == 'Predict'",
          fileInput(
            "cbc_model_file",
            "Custom CBC models (combined RDS, optional)",
            accept = c(".rds", ".RDS")
          ),
          fileInput(
            "cbc_mix_model_file",
            "Custom CBC mix model (RDS, optional)",
            accept = c(".rds", ".RDS")
          )
        ),
        conditionalPanel(
          condition = "input.dataset == 'BMP' && input.mode == 'Train'",
          checkboxGroupInput(
            "train_fluids",
            "Fluids to include",
            choices = bmp_fluids_tbl$fluid,
            selected = bmp_fluids_tbl$fluid
          ),
          actionButton("add_fluid", "Add fluid", class = "secondary-btn")
        )
      ),
      div(
        class = "sidebar-section",
        div(class = "section-title", "Actions"),
        conditionalPanel(
          condition = "input.mode == 'Train'",
          div(
            class = "button-row",
            actionButton("train_models", "Train models", class = "primary-btn"),
            downloadButton(
              "download_trained_models",
              "Download models",
              class = "secondary-btn"
            )
          )
        ),
        conditionalPanel(
          condition = "input.mode == 'Predict'",
          div(
            class = "button-row",
            uiOutput("predict_action_ui"),
            downloadButton(
              "download",
              "Download predictions",
              class = "secondary-btn"
            )
          )
        )
      )
    ),
    mainPanel(
      uiOutput("predict_instructions"),
      uiOutput("train_instructions"),
      uiOutput("review_instructions"),
      verbatimTextOutput("status"),
      uiOutput("format_badge"),
      DTOutput("preview"),
      textOutput("preview_count"),
      tableOutput("train_preview"),
      uiOutput("review_progress"),
      uiOutput("review_table"),
      uiOutput("review_actions")
    )
  )
)

server <- function(input, output, session) {
  preds <- reactiveVal(NULL)
  predict_input <- reactiveVal(NULL)
  predict_file_name <- reactiveVal(NULL)
  predict_format <- reactiveVal(NULL)
  status_text <- reactiveVal("")
  train_fluids_tbl <- reactiveVal(bmp_fluids_tbl)
  trained_models_paths <- reactiveValues(BMP = NULL, CBC = NULL)
  trained_models_ready <- reactiveValues(BMP = FALSE, CBC = FALSE)
  trained_models_stamp <- reactiveValues(BMP = NULL, CBC = NULL)
  train_input <- reactiveVal(NULL)
  train_preview <- reactiveVal(NULL)
  review_df <- reactiveVal(NULL)
  review_index <- reactiveVal(1)
  review_save_path <- reactiveVal(NULL)
  review_file_name <- reactiveVal(NULL)
  bmp_models_cache <- reactiveVal(NULL)
  cbc_models_cache <- reactiveVal(NULL)
  clear_outputs <- function() {
    preds(NULL)
    predict_input(NULL)
    predict_file_name(NULL)
    predict_format(NULL)
    train_input(NULL)
    train_preview(NULL)
    status_text("")
    review_df(NULL)
    review_index(1)
    review_save_path(NULL)
    review_file_name(NULL)
  }
  review_specs <- list(
    BMP = list(
      analytes = lab_strings,
      display = c(
        sodium = "Na",
        chloride = "Cl",
        potassium_plas = "K",
        co2_totl = "CO2",
        bun = "BUN",
        creatinine = "Cr",
        calcium = "Ca",
        glucose = "Glu"
      ),
      round_cols = c("potassium_plas", "creatinine", "calcium")
    ),
    CBC = list(
      analytes = c("Hgb", "WBC", "Plt"),
      display = c(Hgb = "Hgb", WBC = "WBC", Plt = "Plt"),
      round_cols = character(0)
    )
  )

  run_predictions <- function(df) {
    status_text("Running Predictions...")
    showModal(modalDialog(
      div(
        style = "display:flex; align-items:center; gap:8px;",
        div(class = "loader"),
        div("Running predictions. This can take a few minutes.")
      ),
      footer = NULL,
      easyClose = FALSE
    ))
    on.exit(removeModal(), add = TRUE)
    if (input$dataset == "BMP") {
      use_defaults <- is.null(input$bmp_model_file) || is.null(input$bmp_mix_model_file)
      if (use_defaults && is.null(bmp_models_cache())) {
        status_text("Loading BMP models...")
        bmp_defaults <- tryCatch(load_bmp_models(), error = function(e) e)
        if (inherits(bmp_defaults, "error")) {
          status_text(paste("Model load error:", bmp_defaults$message))
          preds(NULL)
          return()
        }
        bmp_models_cache(bmp_defaults)
      }
      models_combined <- if (is.null(input$bmp_model_file)) {
        bmp_models_cache()$models_combined
      } else {
        tryCatch(safe_read_rds(input$bmp_model_file$datapath), error = function(e) e)
      }
      mix_models <- if (is.null(input$bmp_mix_model_file)) {
        bmp_models_cache()$mix_ratio_models
      } else {
        tryCatch(safe_read_rds(input$bmp_mix_model_file$datapath), error = function(e) e)
      }
      if (inherits(models_combined, "error")) {
        status_text(paste("Custom model error:", models_combined$message))
        preds(NULL)
        return()
      }
      if (inherits(mix_models, "error")) {
        status_text(paste("Custom mix model error:", mix_models$message))
        preds(NULL)
        return()
      }
      include <- input$fluids
      models_combined <- models_combined |> keep(~ .x$fluid %in% include)
      mix_models <- mix_models |> keep(~ .x$fluid %in% include)
      res <- tryCatch(
        makeBmpPredictions(df, models_combined = models_combined, mix_ratio_models = mix_models),
        error = function(e) e
      )
    } else {
      use_defaults <- is.null(input$cbc_model_file) || is.null(input$cbc_mix_model_file)
      if (use_defaults && is.null(cbc_models_cache())) {
        status_text("Loading CBC models...")
        cbc_defaults <- tryCatch(load_cbc_models(), error = function(e) e)
        if (inherits(cbc_defaults, "error")) {
          status_text(paste("Model load error:", cbc_defaults$message))
          preds(NULL)
          return()
        }
        cbc_models_cache(cbc_defaults)
      }
      models_combined <- if (is.null(input$cbc_model_file)) {
        cbc_models_cache()$models_combined
      } else {
        tryCatch(safe_read_rds(input$cbc_model_file$datapath), error = function(e) e)
      }
      mix_models <- if (is.null(input$cbc_mix_model_file)) {
        cbc_models_cache()$mix_ratio_workflows
      } else {
        tryCatch(safe_read_rds(input$cbc_mix_model_file$datapath), error = function(e) e)
      }
      if (inherits(models_combined, "error")) {
        status_text(paste("Custom model error:", models_combined$message))
        preds(NULL)
        return()
      }
      if (inherits(mix_models, "error")) {
        status_text(paste("Custom mix model error:", mix_models$message))
        preds(NULL)
        return()
      }
      res <- tryCatch(
        makeCbcPredictions(df, models_combined = models_combined, mix_ratio_workflows = mix_models),
        error = function(e) e
      )
    }

    if (inherits(res, "error")) {
      status_text(paste("Prediction error:", res$message))
      preds(NULL)
      return()
    }

    status_text("Predictions ready.")
    preds(res)
  }
  review_cell_html <- function(value, prior_cmp, post_cmp) {
    val_str <- if (is.na(value)) "" else as.character(value)
    html <- paste0("<div style='position:relative;'>", val_str)
    if (!is.null(prior_cmp) && !is.na(prior_cmp) && prior_cmp != 0) {
      arrow <- if (prior_cmp > 0) "&#9650;" else "&#9660;"
      color <- if (prior_cmp > 0) "cyan" else "magenta"
      html <- paste0(
        html,
        "<span style='position:absolute;top:-16px;right:-18px;color:",
        color,
        ";font-size:1.2em'>",
        arrow,
        "</span>"
      )
    }
    if (!is.null(post_cmp) && !is.na(post_cmp) && post_cmp != 0) {
      arrow <- if (post_cmp > 0) "&#9650;" else "&#9660;"
      color <- if (post_cmp > 0) "cyan" else "magenta"
      html <- paste0(
        html,
        "<span style='position:absolute;bottom:-16px;right:-18px;color:",
        color,
        ";font-size:1.2em'>",
        arrow,
        "</span>"
      )
    }
    paste0(html, "</div>")
  }

  build_review_table <- function(row, analytes, display) {
    header <- paste0(
      "<tr><th></th>",
      paste0("<th>", display[analytes], "</th>", collapse = ""),
      "</tr>"
    )
    rows <- map_chr(c("prior", "current", "post"), function(label) {
      cells <- map_chr(analytes, function(analyte) {
        if (label == "current") {
          cur <- row[[analyte]]
          prior <- row[[paste0(analyte, "_prior")]]
          post <- row[[paste0(analyte, "_post")]]
          cmp_prior <- if (!is.na(cur) && !is.na(prior)) as.integer(cur > prior) - as.integer(cur < prior) else NA_integer_
          cmp_post <- if (!is.na(cur) && !is.na(post)) as.integer(post > cur) - as.integer(post < cur) else NA_integer_
          review_cell_html(cur, cmp_prior, cmp_post)
        } else {
          val_key <- paste0(analyte, "_", label)
          val <- row[[val_key]]
          if (is.na(val)) "" else as.character(val)
        }
      })
      paste0("<tr><td><b>", label, "</b></td>", paste0("<td>", cells, "</td>", collapse = ""), "</tr>")
    })
    paste0(
      "<table class='label-table'>",
      header,
      paste0(rows, collapse = ""),
      "</table>"
    )
  }
  save_review_label <- function(label_value) {
    df <- review_df()
    if (is.null(df)) {
      status_text("Review error: upload a file first.")
      return(invisible(FALSE))
    }
    labeler <- trimws(if (is.null(input$review_labeler)) "" else input$review_labeler)
    idx <- review_index()
    if (idx > nrow(df)) {
      status_text("All entries labeled.")
      return(invisible(FALSE))
    }
    row <- df[idx, , drop = FALSE]
    out <- row
    out$label <- tools::toTitleCase(tolower(label_value))
    out$labeler <- labeler
    out$timestamp <- format(Sys.time(), tz = "UTC", usetz = TRUE)
    path <- review_save_path()
    if (is.null(path)) {
      status_text("Review error: output path not set.")
      return(invisible(FALSE))
    }
    write.table(out, path,
      sep = ",", row.names = FALSE, col.names = !file.exists(path),
      append = file.exists(path), quote = TRUE
    )
    review_index(idx + 1)
    status_text(paste("Saved label to", path))
    invisible(TRUE)
  }
  run_in_root <- function(expr) {
    old_dir <- getwd()
    on.exit(setwd(old_dir), add = TRUE)
    setwd(root)
    force(expr)
  }

  output$status <- renderText(status_text())

  observeEvent(input$mode, {
    clear_outputs()
  })

  observeEvent(input$dataset, {
    clear_outputs()
  })

  observeEvent(input$add_fluid, {
    clear_outputs()
    req(input$mode == "Train", input$dataset == "BMP")
    showModal(modalDialog(
      title = "Add fluid",
      textInput("new_fluid_name", "Fluid name"),
      lapply(lab_strings, function(analyte) {
        numericInput(paste0("new_fluid_", analyte), analyte, value = NA_real_)
      }),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_add_fluid", "Add")
      )
    ))
  })

  observeEvent(input$confirm_add_fluid, {
    clear_outputs()
    req(input$mode == "Train", input$dataset == "BMP")
    name <- trimws(if (is.null(input$new_fluid_name)) "" else input$new_fluid_name)
    if (!nzchar(name)) {
      showNotification("Fluid name is required.", type = "error")
      return()
    }
    values <- map_dbl(lab_strings, ~ input[[paste0("new_fluid_", .x)]])
    if (any(is.na(values))) {
      showNotification("All analyte concentrations are required.", type = "error")
      return()
    }
    tbl <- train_fluids_tbl()
    if (name %in% tbl$fluid) {
      showNotification("Fluid already exists.", type = "error")
      return()
    }
    new_row <- tibble::tibble(fluid = name)
    for (i in seq_along(lab_strings)) {
      new_row[[lab_strings[[i]]]] <- values[[i]]
    }
    train_fluids_tbl(bind_rows(tbl, new_row))
    removeModal()
  })

  observeEvent(train_fluids_tbl(), {
    req(input$mode == "Train", input$dataset == "BMP")
    tbl <- train_fluids_tbl()
    updateCheckboxGroupInput(session, "train_fluids",
      choices = tbl$fluid,
      selected = tbl$fluid
    )
  }, ignoreInit = TRUE)

  observeEvent(input$review_file_wide, {
    clear_outputs()
    req(input$mode == "Review")
    df <- tryCatch(read_upload(input$review_file_wide), error = function(e) e)
    if (inherits(df, "error")) {
      status_text(paste("Error reading review file:", df$message))
      review_df(NULL)
      return()
    }
    review_file_name(input$review_file_wide$name)
    spec <- review_specs[[input$dataset]]
    for (col in spec$round_cols) {
      for (suffix in c("", "_prior", "_post")) {
        name <- paste0(col, suffix)
        if (name %in% names(df)) {
          df[[name]] <- round(as.numeric(df[[name]]), 1)
        }
      }
    }
    review_df(df)
    review_index(1)
    base <- tools::file_path_sans_ext(input$review_file_wide$name)
    out_dir <- file.path(root, "results")
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    review_save_path(file.path(out_dir, paste0(base, "_labels.csv")))
    status_text("Review file loaded.")
  })

  observeEvent(input$review_file_long, {
    clear_outputs()
    req(input$mode == "Review")
    status_text("Preprocessing long-form review data...")
    showModal(modalDialog(
      div(
        style = "display:flex; align-items:center; gap:8px;",
        div(class = "loader"),
        div("Preprocessing long-form review data.")
      ),
      footer = NULL,
      easyClose = FALSE
    ))
    on.exit(removeModal(), add = TRUE)
    df <- tryCatch(read_upload(input$review_file_long), error = function(e) e)
    if (inherits(df, "error")) {
      status_text(paste("Error reading review file:", df$message))
      review_df(NULL)
      return()
    }
    wide_df <- if (input$dataset == "BMP") {
      tryCatch(preprocessBmpData(df), error = function(e) e)
    } else {
      tryCatch(preprocessCBCData(df), error = function(e) e)
    }
    if (inherits(wide_df, "error")) {
      status_text(paste("Review preprocess error:", wide_df$message))
      review_df(NULL)
      return()
    }
    review_file_name(input$review_file_long$name)
    spec <- review_specs[[input$dataset]]
    for (col in spec$round_cols) {
      for (suffix in c("", "_prior", "_post")) {
        name <- paste0(col, suffix)
        if (name %in% names(wide_df)) {
          wide_df[[name]] <- round(as.numeric(wide_df[[name]]), 1)
        }
      }
    }
    review_df(wide_df)
    review_index(1)
    base <- tools::file_path_sans_ext(input$review_file_long$name)
    out_dir <- file.path(root, "results")
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    review_save_path(file.path(out_dir, paste0(base, "_labels.csv")))
    status_text("Review file loaded.")
  })

  observeEvent(input$file_wide, {
    clear_outputs()
    req(input$mode == "Predict")
    df <- tryCatch(read_upload(input$file_wide), error = function(e) e)
    if (inherits(df, "error")) {
      status_text(paste("Error reading file:", df$message))
      preds(NULL)
      return()
    }
    predict_input(df)
    predict_file_name(input$file_wide$name)
    predict_format("wide")
    status_text("Upload complete. Click Run Predictions.")
  })

  observeEvent(input$file_long, {
    clear_outputs()
    req(input$mode == "Predict")
    status_text("Preprocessing long-form data...")
    showModal(modalDialog(
      div(
        style = "display:flex; align-items:center; gap:8px;",
        div(class = "loader"),
        div("Preprocessing long-form data.")
      ),
      footer = NULL,
      easyClose = FALSE
    ))
    on.exit(removeModal(), add = TRUE)
    df <- tryCatch(read_upload(input$file_long), error = function(e) e)
    if (inherits(df, "error")) {
      status_text(paste("Error reading file:", df$message))
      preds(NULL)
      return()
    }
    wide_df <- if (input$dataset == "BMP") {
      tryCatch(preprocessBmpData(df), error = function(e) e)
    } else {
      tryCatch(preprocessCBCData(df), error = function(e) e)
    }
    if (inherits(wide_df, "error")) {
      status_text(paste("Preprocess error:", wide_df$message))
      preds(NULL)
      return()
    }
    predict_input(wide_df)
    predict_file_name(input$file_long$name)
    predict_format("long")
    status_text("Upload complete. Click Run Predictions.")
  })

  observeEvent(input$run_predictions, {
    req(input$mode == "Predict")
    df <- predict_input()
    if (is.null(df)) {
      status_text("Prediction error: upload a file first.")
      preds(NULL)
      return()
    }
    showModal(modalDialog(
      div(
        style = "display:flex; align-items:center; gap:8px;",
        div(class = "loader"),
        div("Running predictions...")
      ),
      footer = NULL,
      easyClose = FALSE
    ))
    run_predictions(df)
  })

  observeEvent(input$train_models, {
    clear_outputs()
    req(input$mode == "Train")
    req(train_input())
    status_text("Training models...")
    showModal(modalDialog(
      div(
        style = "display:flex; align-items:center; gap:8px;",
        div(class = "loader"),
        div("Training models. This can take a few minutes.")
      ),
      footer = NULL,
      easyClose = FALSE
    ))
    on.exit(removeModal(), add = TRUE)
    trained_dataset <- input$dataset

    train_df <- train_input()
    train_file_csv <- tempfile("train_data_", fileext = ".csv")
    readr::write_csv(train_df, train_file_csv)

    train_preview(train_df)
    if (trained_dataset == "BMP") {
      tbl <- train_fluids_tbl()
      include <- input$train_fluids
      if (length(include) == 0) {
        status_text("Training error: select at least one fluid.")
        return()
      }
      tbl <- tbl |> filter(fluid %in% include)
      fluids_file <- tempfile("fluids_", fileext = ".csv")
      readr::write_csv(tbl, fluids_file)
      stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      out_file <- tempfile(paste0("bmp_models_", stamp, "_"), fileext = ".RDS")
      res <- tryCatch({
        run_in_root(system2(
          "Rscript",
          c(file.path(root, "scripts", "train_bmp_models.R"), train_file_csv, fluids_file, out_file),
          stdout = TRUE,
          stderr = TRUE
        ))
      }, error = function(e) e)
      if (inherits(res, "error")) {
        status_text(paste("Training error:", res$message))
        return()
      }
      if (!is.null(attr(res, "status")) && attr(res, "status") != 0) {
        status_text(paste("Training error:", paste(res, collapse = "\n")))
        return()
      }
      trained_models_paths$BMP <- out_file
      trained_models_ready$BMP <- TRUE
      trained_models_stamp$BMP <- stamp
      status_text("BMP models trained.")
      session$sendCustomMessage("triggerDownload", list(id = "download_trained_models"))
    } else {
      stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      out_file <- tempfile(paste0("cbc_models_", stamp, "_"), fileext = ".RDS")
      res <- tryCatch({
        run_in_root(system2(
          "Rscript",
          c(file.path(root, "scripts", "train_cbc_models.R"), train_file_csv, out_file, "--no-mix"),
          stdout = TRUE,
          stderr = TRUE
        ))
      }, error = function(e) e)
      if (inherits(res, "error")) {
        status_text(paste("Training error:", res$message))
        return()
      }
      if (!is.null(attr(res, "status")) && attr(res, "status") != 0) {
        status_text(paste("Training error:", paste(res, collapse = "\n")))
        return()
      }
      trained_models_paths$CBC <- out_file
      trained_models_ready$CBC <- TRUE
      trained_models_stamp$CBC <- stamp
      status_text("CBC models trained.")
      session$sendCustomMessage("triggerDownload", list(id = "download_trained_models"))
    }
  })

  observeEvent(input$train_file_wide, {
    clear_outputs()
    req(input$mode == "Train")
    df <- tryCatch(read_upload(input$train_file_wide), error = function(e) e)
    if (inherits(df, "error")) {
      status_text(paste("Training error:", df$message))
      train_preview(NULL)
      return()
    }
    train_input(df)
    train_preview(df)
    status_text("Training file loaded. Ready to train.")
  })

  observeEvent(input$train_file_long, {
    clear_outputs()
    req(input$mode == "Train")
    status_text("Preprocessing long-form training data...")
    df <- tryCatch(read_upload(input$train_file_long), error = function(e) e)
    if (inherits(df, "error")) {
      status_text(paste("Training error:", df$message))
      train_preview(NULL)
      return()
    }
    wide_df <- if (input$dataset == "BMP") {
      tryCatch(preprocessBmpData(df), error = function(e) e)
    } else {
      tryCatch(preprocessCBCData(df), error = function(e) e)
    }
    if (inherits(wide_df, "error")) {
      status_text(paste("Training preprocess error:", wide_df$message))
      train_preview(NULL)
      return()
    }
    train_input(wide_df)
    train_preview(wide_df)
    status_text("Training file loaded. Ready to train.")
  })

  observeEvent(input$label_real, {
    req(input$mode == "Review")
    save_review_label("Real")
  })

  observeEvent(input$label_contam, {
    req(input$mode == "Review")
    save_review_label("Contaminated")
  })

  observeEvent(input$label_equivocal, {
    req(input$mode == "Review")
    save_review_label("Equivocal")
  })

  output$preview <- renderDT({
    req(input$mode == "Predict")
    df <- if (!is.null(preds())) preds() else predict_input()
    req(df)
    display_df <- head(df, 10)
    datatable(
      display_df,
      rownames = FALSE,
      options = list(pageLength = 10, dom = "t", scrollX = TRUE)
    )
  })

  output$predict_instructions <- renderUI({
    req(input$mode == "Predict")
    if (!is.null(predict_input()) || !is.null(preds())) {
      return(NULL)
    }
    if (input$dataset == "BMP") {
      tagList(
        h4("How to run predictions"),
        tags$ol(
          tags$li("Choose BMP, then upload a wide or long file."),
          tags$li("If you upload long-form data, it will be converted to wide-form automatically."),
          tags$li("Click Run Predictions, then download the results.")
        ),
        tags$strong("Wide-form required columns:"),
        tags$p("sodium, chloride, potassium_plas, co2_totl, bun, creatinine, calcium, glucose, plus *_prior and *_post for each analyte."),
        tags$strong("Long-form required columns:"),
        tags$p("PATIENT_ID, DRAWN_DT_TM, TASK_ASSAY, RESULT_VALUE.")
      )
    } else {
      tagList(
        h4("How to run predictions"),
        tags$ol(
          tags$li("Choose CBC, then upload a wide or long file."),
          tags$li("If you upload long-form data, it will be converted to wide-form automatically."),
          tags$li("Click Run Predictions, then download the results.")
        ),
        tags$strong("Wide-form required columns:"),
        tags$p("Hgb, WBC, Plt, Hgb_prior, WBC_prior, Plt_prior, Hgb_post, WBC_post, Plt_post."),
        tags$strong("Long-form required columns:"),
        tags$p("PATIENT_ID, DRAWN_DT_TM, TASK_ASSAY, RESULT_VALUE.")
      )
    }
  })

  output$preview_count <- renderText({
    req(input$mode == "Predict")
    df <- if (!is.null(preds())) preds() else predict_input()
    if (is.null(df)) {
      return("")
    }
    paste("Loaded", nrow(df), "total rows")
  })

  output$format_badge <- renderUI({
    req(input$mode == "Predict")
    fmt <- predict_format()
    if (is.null(fmt)) {
      return(NULL)
    }
    div(class = "format-badge", paste("Detected format:", fmt))
  })

  output$predict_action_ui <- renderUI({
    req(input$mode == "Predict")
    if (is.null(predict_input())) {
      actionButton("run_predictions", "Run Predictions", class = "primary-btn", disabled = "disabled")
    } else {
      actionButton("run_predictions", "Run Predictions", class = "primary-btn")
    }
  })

  output$train_preview <- renderTable({
    req(input$mode == "Train")
    req(train_preview())
    head(train_preview(), 10)
  })

  output$train_instructions <- renderUI({
    req(input$mode == "Train")
    if (!is.null(train_input())) {
      return(NULL)
    }
    if (input$dataset == "BMP") {
      tagList(
        h4("How to train models"),
        tags$ol(
          tags$li("Choose BMP, then upload a wide or long training file."),
          tags$li("If you upload long-form data, it will be converted to wide-form automatically."),
          tags$li("Click Train models to build new workflows.")
        ),
        tags$strong("Wide-form required columns:"),
        tags$p("sodium, chloride, potassium_plas, co2_totl, bun, creatinine, calcium, glucose, plus *_prior and *_post for each analyte."),
        tags$strong("Long-form required columns:"),
        tags$p("PATIENT_ID, DRAWN_DT_TM, TASK_ASSAY, RESULT_VALUE.")
      )
    } else {
      tagList(
        h4("How to train models"),
        tags$ol(
          tags$li("Choose CBC, then upload a wide or long training file."),
          tags$li("If you upload long-form data, it will be converted to wide-form automatically."),
          tags$li("Click Train models to build new workflows.")
        ),
        tags$strong("Wide-form required columns:"),
        tags$p("Hgb, WBC, Plt, plus Hgb_prior, WBC_prior, Plt_prior and Hgb_post, WBC_post, Plt_post."),
        tags$strong("Long-form required columns:"),
        tags$p("PATIENT_ID, DRAWN_DT_TM, TASK_ASSAY, RESULT_VALUE.")
      )
    }
  })

  output$review_progress <- renderUI({
    req(input$mode == "Review")
    df <- review_df()
    if (is.null(df)) {
      return(NULL)
    }
    idx <- review_index()
    total <- nrow(df)
    div(paste0("Reviewing ", min(idx, total), " of ", total))
  })

  output$review_instructions <- renderUI({
    req(input$mode == "Review")
    if (!is.null(review_df())) {
      return(NULL)
    }
    if (input$dataset == "BMP") {
      tagList(
        h4("How to review"),
        tags$ol(
          tags$li("Upload a review CSV/TSV in wide or long form."),
          tags$li("If you upload long-form data, it will be converted to wide-form automatically."),
          tags$li("Verify the table preview, then label each row as Real, Contaminated, or Equivocal."),
          tags$li("Use the download button at any time to save your labels.")
        ),
        tags$strong("Required columns:"),
        tags$p("Wide-form: sodium, chloride, potassium_plas, co2_totl, bun, creatinine, calcium, glucose, plus *_prior and *_post for each analyte."),
        tags$p("Long-form: PATIENT_ID, DRAWN_DT_TM, TASK_ASSAY, RESULT_VALUE.")
      )
    } else {
      tagList(
        h4("How to review"),
        tags$ol(
          tags$li("Upload a review CSV/TSV in wide or long form."),
          tags$li("If you upload long-form data, it will be converted to wide-form automatically."),
          tags$li("Verify the table preview, then label each row as Real, Contaminated, or Equivocal."),
          tags$li("Use the download button at any time to save your labels.")
        ),
        tags$strong("Required columns:"),
        tags$p("Wide-form: Hgb, WBC, Plt, plus Hgb_prior, WBC_prior, Plt_prior and Hgb_post, WBC_post, Plt_post."),
        tags$p("Long-form: PATIENT_ID, DRAWN_DT_TM, TASK_ASSAY, RESULT_VALUE.")
      )
    }
  })

  output$review_table <- renderUI({
    req(input$mode == "Review")
    df <- review_df()
    if (is.null(df)) {
      return(NULL)
    }
    idx <- review_index()
    total <- nrow(df)
    if (idx > total) {
      return(div("All entries labeled."))
    }
    spec <- review_specs[[input$dataset]]
    row <- df[idx, , drop = FALSE]
    html <- build_review_table(row, spec$analytes, spec$display)
    HTML(html)
  })

  output$review_actions <- renderUI({
    req(input$mode == "Review")
    if (is.null(review_df())) {
      return(NULL)
    }
    tagList(
      div(
        class = "review-actions",
        actionButton("label_real", "Real", class = "primary-btn"),
        actionButton("label_contam", "Contaminated", class = "secondary-btn"),
        actionButton("label_equivocal", "Equivocal", class = "secondary-btn")
      ),
      div(
        class = "review-actions",
        downloadButton("download_review_labels", "Download labels", class = "secondary-btn")
      ),
      div(
        class = "review-actions",
        textInput("review_labeler", "Your name (optional)")
      )
    )
  })

  output$download <- downloadHandler(
    filename = function() {
      name <- predict_file_name()
      if (is.null(name)) {
        "predictions.csv"
      } else {
        base <- tools::file_path_sans_ext(name)
        paste0(base, "_predictions.csv")
      }
    },
    content = function(file) {
      req(preds())
      write_csv(preds(), file)
    }
  )

  output$download_trained_models <- downloadHandler(
    filename = function() {
      if (input$dataset == "BMP") {
        stamp <- trained_models_stamp$BMP
        if (is.null(stamp)) {
          "bmp_models_combined.RDS"
        } else {
          paste0("bmp_models_combined_", stamp, ".RDS")
        }
      } else {
        stamp <- trained_models_stamp$CBC
        if (is.null(stamp)) {
          "cbc_models_combined.RDS"
        } else {
          paste0("cbc_models_combined_", stamp, ".RDS")
        }
      }
    },
    content = function(file) {
      if (input$dataset == "BMP") {
        req(trained_models_ready$BMP)
        file.copy(trained_models_paths$BMP, file, overwrite = TRUE)
      } else {
        req(trained_models_ready$CBC)
        file.copy(trained_models_paths$CBC, file, overwrite = TRUE)
      }
    }
  )

  output$download_review_labels <- downloadHandler(
    filename = function() {
      name <- review_file_name()
      if (is.null(name)) {
        "review_labels.csv"
      } else {
        base <- tools::file_path_sans_ext(name)
        paste0(base, "_labels.csv")
      }
    },
    content = function(file) {
      path <- review_save_path()
      if (!is.null(path) && file.exists(path)) {
        file.copy(path, file, overwrite = TRUE)
        return()
      }
      df <- review_df()
      header <- if (is.null(df)) character(0) else names(df)
      header <- c(header, "label", "labeler", "timestamp")
      write.table(as.data.frame(matrix(nrow = 0, ncol = length(header)), stringsAsFactors = FALSE),
        file = file,
        sep = ",",
        row.names = FALSE,
        col.names = header,
        quote = TRUE
      )
    }
  )
}

shinyApp(ui, server)
