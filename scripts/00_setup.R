# ---------------------------------------------------------------------------
# One-off environment setup: install the packages the project needs and
# download the raw dataset (~141 MB) if it is not already present.
#
#   Rscript scripts/00_setup.R
# ---------------------------------------------------------------------------

required_packages <- c("tidyverse", "janitor", "ranger", "car", "sandwich",
                       "lmtest", "MASS", "knitr", "rmarkdown",
                       "cluster", "nnet", "pROC")
missing <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing) > 0) {
  message("Installing: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cloud.r-project.org")
} else {
  message("All packages already installed.")
}

# Optional boosting packages for scripts/06_paper_benchmark.R. The benchmark
# skips a model automatically if its package cannot be installed (LightGBM in
# particular can fail on Windows without Rtools), so a failure here is not fatal.
for (pkg in c("xgboost", "lightgbm")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    try(install.packages(pkg, repos = "https://cloud.r-project.org"), silent = TRUE)
    if (!requireNamespace(pkg, quietly = TRUE))
      message("NOTE: optional package '", pkg, "' could not be installed; ",
              "the benchmark will skip that model.")
  }
}

# Raw data ----------------------------------------------------------------
# Stack Overflow Annual Developer Survey 2025, published under CC BY-SA 4.0.
# Mirrored in the official StackExchange/Survey repository (Git LFS), which is
# what the URLs below resolve to.
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

files <- list(
  "data/raw/survey_results_public.csv" =
    "https://media.githubusercontent.com/media/StackExchange/Survey/main/packages/archive/2025/results.csv",
  "data/raw/survey_results_schema.csv" =
    "https://media.githubusercontent.com/media/StackExchange/Survey/main/packages/archive/2025/schema.csv"
)

for (dest in names(files)) {
  if (file.exists(dest) && file.size(dest) > 1e6) {
    message("Already downloaded: ", dest)
    next
  }
  message("Downloading ", dest, " (this takes a few minutes) ...")
  download.file(files[[dest]], dest, mode = "wb", quiet = FALSE)
}

message("Setup complete. Next: Rscript scripts/01_data_cleaning.R")
