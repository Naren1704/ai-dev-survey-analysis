# ---------------------------------------------------------------------------
# Runs the whole analysis end to end. Assumes scripts/00_setup.R has already
# installed the packages and downloaded the raw data.
#
#   Rscript scripts/run_all.R
# ---------------------------------------------------------------------------

steps <- c(
  "scripts/01_data_cleaning.R",
  "scripts/02_eda.R",
  "scripts/03_statistical_tests.R",
  "scripts/04_modeling.R",
  "scripts/05_clustering.R"
)

for (s in steps) {
  message("\n===== ", s, " =====")
  t0 <- Sys.time()
  source(s, echo = FALSE)
  message("done in ", round(difftime(Sys.time(), t0, units = "secs")), "s")
}

rmarkdown::render("reports/final_report.Rmd", quiet = TRUE)
message("\nReport: reports/final_report.html")
