# ---------------------------------------------------------------------------
# Shared project setup: packages, paths, plotting theme, random seed.
# Every script starts with: source("R/setup.R")
# ---------------------------------------------------------------------------

required_packages <- c(
  "tidyverse",  # dplyr, ggplot2, readr, tidyr, stringr, forcats, purrr, broom
  "janitor",    # clean_names(), tabyl()
  "ranger",     # random forest
  "sandwich",   # heteroskedasticity-robust standard errors
  "lmtest",     # coeftest() for the robust-SE table
  "car",        # VIF / regression diagnostics
  "knitr",      # kable() tables
  "MASS",       # ordinal (proportional-odds) logistic regression
  "rmarkdown"   # final report
)

missing <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing) > 0) {
  stop(
    "Missing packages: ", paste(missing, collapse = ", "),
    "\nRun: source(\"scripts/00_setup.R\")",
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
})

# Paths -------------------------------------------------------------------
PATHS <- list(
  # SURVEY_RAW_CSV lets a smaller extract be swapped in for a quick smoke test.
  raw_csv    = Sys.getenv("SURVEY_RAW_CSV", "data/raw/survey_results_public.csv"),
  schema_csv = "data/raw/survey_results_schema.csv",
  clean_rds  = "data/processed/survey_clean.rds",
  clean_csv  = "data/processed/survey_clean.csv",
  figures    = "outputs/figures",
  tables     = "outputs/tables",
  models     = "outputs/models"
)

for (p in c(PATHS$figures, PATHS$tables, PATHS$models, "data/processed")) {
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
}

# Reproducibility ---------------------------------------------------------
SEED <- 42
set.seed(SEED)

# Plot theme --------------------------------------------------------------
theme_survey <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title    = element_text(face = "bold", size = base_size * 1.15),
      plot.subtitle = element_text(colour = "grey35"),
      plot.caption  = element_text(colour = "grey45", size = base_size * 0.75),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
}
theme_set(theme_survey())

PALETTE <- c("#4C78A8", "#F58518", "#54A24B", "#E45756", "#72B7B2", "#B279A2")

#' Save a plot to outputs/figures and return the path invisibly.
save_fig <- function(plot, name, width = 8, height = 5, dpi = 150) {
  path <- file.path(PATHS$figures, paste0(name, ".png"))
  ggsave(path, plot, width = width, height = height, dpi = dpi, bg = "white")
  message("figure saved: ", path)
  invisible(path)
}

#' Save a data frame to outputs/tables as CSV and return the path invisibly.
save_table <- function(df, name) {
  path <- file.path(PATHS$tables, paste0(name, ".csv"))
  readr::write_csv(df, path)
  message("table saved:  ", path)
  invisible(path)
}

# Pandoc ------------------------------------------------------------------
# RStudio sets RSTUDIO_PANDOC itself, but `Rscript` does not, so rendering the
# report from the command line fails unless pandoc is found first.
if (!rmarkdown::pandoc_available()) {
  candidates <- c(
    Sys.getenv("RSTUDIO_PANDOC"),
    "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools",
    "C:/Program Files/RStudio/bin/quarto/bin/tools",
    "/usr/lib/rstudio/bin/quarto/bin/tools"
  )
  hit <- candidates[nzchar(candidates) & dir.exists(candidates)]
  if (length(hit) > 0) Sys.setenv(RSTUDIO_PANDOC = hit[1])
}
