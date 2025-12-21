args <- commandArgs(trailingOnly = TRUE)
app_name <- if (length(args) >= 1 && nzchar(args[[1]])) args[[1]] else "contamination-prediction"

name <- Sys.getenv("SHINYAPPS_NAME", unset = "")
token <- Sys.getenv("SHINYAPPS_TOKEN", unset = "")
secret <- Sys.getenv("SHINYAPPS_SECRET", unset = "")

if (!nzchar(name) || !nzchar(token) || !nzchar(secret)) {
  stop("Missing SHINYAPPS_NAME/SHINYAPPS_TOKEN/SHINYAPPS_SECRET environment variables.")
}

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  install.packages("rsconnect")
}

rsconnect::setAccountInfo(name = name, token = token, secret = secret)
rsconnect::deployApp(
  appDir = ".",
  appPrimaryDoc = "scripts/prediction_app.R",
  appFiles = c(
    "scripts",
    "models",
    file.path("data", "fluid_concentrations.tsv")
  ),
  appName = app_name
)
