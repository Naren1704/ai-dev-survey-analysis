# ---------------------------------------------------------------------------
# Cleaning and feature-engineering helpers for the Stack Overflow 2025 survey.
# Sourced by scripts/01_data_cleaning.R.
# ---------------------------------------------------------------------------

# Columns kept from the 172 in the raw file. Everything else is dropped early
# so the rest of the pipeline stays readable (and fits in memory comfortably).
KEEP_COLS <- c(
  "ResponseId", "MainBranch", "Age", "EdLevel", "Employment", "RemoteWork",
  "WorkExp", "YearsCode", "DevType", "OrgSize", "Industry", "Country",
  "ConvertedCompYearly", "JobSat", "ICorPM",
  "AISelect", "AISent", "AIAcc", "AIComplex", "AIThreat", "AIAgents",
  "AIFrustration", "AIHuman", "AIOpen",
  "LanguageHaveWorkedWith", "AIModelsHaveWorkedWith"
)

# "Years of coding" is stored as text because of two open-ended answers.
parse_years <- function(x) {
  x <- str_trim(as.character(x))
  suppressWarnings(as.numeric(case_when(
    x == "Less than 1 year"  ~ "0.5",
    x == "More than 50 years" ~ "51",
    TRUE ~ x
  )))
}

# Age is a bucket; the midpoint makes it usable as a numeric predictor while
# the original factor is kept for plots.
age_midpoint <- function(x) {
  case_when(
    x == "Under 18 years old"  ~ 16,
    x == "18-24 years old"     ~ 21,
    x == "25-34 years old"     ~ 29.5,
    x == "35-44 years old"     ~ 39.5,
    x == "45-54 years old"     ~ 49.5,
    x == "55-64 years old"     ~ 59.5,
    x == "65 years or older"   ~ 70,
    TRUE ~ NA_real_
  )
}

# Education collapsed to four ordered levels; the raw labels are too long and
# too sparse (e.g. 12 respondents with primary school only) for modelling.
collapse_education <- function(x) {
  out <- case_when(
    str_detect(x, "^Bachelor")                        ~ "Bachelor's",
    str_detect(x, "^Master")                          ~ "Master's",
    str_detect(x, "^Professional degree")             ~ "Doctorate/Professional",
    str_detect(x, "Associate|Some college|Secondary|Primary|Something else") ~ "No degree",
    TRUE ~ NA_character_
  )
  factor(out, levels = c("No degree", "Bachelor's", "Master's",
                         "Doctorate/Professional"), ordered = TRUE)
}

# Organisation size collapsed to three bands (small / medium / large).
collapse_org_size <- function(x) {
  out <- case_when(
    str_detect(x, "^Just me|^Less than 20|^2 to 19")        ~ "Small (<20)",
    str_detect(x, "^20 to 99|^100 to 499|^500 to 999")      ~ "Medium (20-999)",
    str_detect(x, "^1,000|^5,000|^10,000")                  ~ "Large (1000+)",
    TRUE ~ NA_character_
  )
  factor(out, levels = c("Small (<20)", "Medium (20-999)", "Large (1000+)"))
}

# Free-form country names to coarse regions, so country can enter a model
# without 180 dummy variables.
to_region <- function(country) {
  case_when(
    country %in% c("United States of America", "Canada") ~ "North America",
    country %in% c(
      "Germany", "United Kingdom of Great Britain and Northern Ireland",
      "France", "Netherlands", "Spain", "Italy", "Poland", "Sweden",
      "Switzerland", "Austria", "Belgium", "Denmark", "Norway", "Finland",
      "Portugal", "Ireland", "Czech Republic", "Romania", "Ukraine",
      "Russian Federation", "Greece", "Hungary", "Bulgaria", "Croatia",
      "Serbia", "Slovakia", "Slovenia", "Lithuania", "Latvia", "Estonia",
      "Turkey", "Belarus"
    ) ~ "Europe",
    country %in% c(
      "India", "China", "Japan", "Republic of Korea", "Singapore",
      "Pakistan", "Bangladesh", "Indonesia", "Viet Nam", "Philippines",
      "Malaysia", "Sri Lanka", "Nepal", "Thailand", "Taiwan",
      "Hong Kong (S.A.R.)", "Israel", "Iran, Islamic Republic of...",
      "United Arab Emirates", "Saudi Arabia"
    ) ~ "Asia & Middle East",
    country %in% c("Brazil", "Mexico", "Argentina", "Colombia", "Chile",
                   "Peru", "Uruguay", "Venezuela, Bolivarian Republic of...",
                   "Ecuador", "Costa Rica") ~ "Latin America",
    country %in% c("Australia", "New Zealand") ~ "Oceania",
    country %in% c("South Africa", "Nigeria", "Kenya", "Egypt", "Ghana",
                   "Morocco", "Tunisia", "Algeria", "Ethiopia", "Uganda")
      ~ "Africa",
    is.na(country) ~ NA_character_,
    TRUE ~ "Other"
  )
}

# AISelect -> binary adopter flag. "Planning to" counts as a non-user, because
# the question of interest is current usage.
is_ai_user <- function(ai_select) {
  case_when(
    str_starts(ai_select, "Yes") ~ 1L,
    str_starts(ai_select, "No")  ~ 0L,
    TRUE ~ NA_integer_
  )
}

# Usage intensity as an ordered factor (drops the "planning" nuance on purpose;
# that nuance lives in ai_select_raw if it is ever needed).
ai_usage_level <- function(ai_select) {
  out <- case_when(
    str_detect(ai_select, "daily")                   ~ "Daily",
    str_detect(ai_select, "weekly")                  ~ "Weekly",
    str_detect(ai_select, "monthly|infrequently")    ~ "Monthly/rarely",
    str_starts(ai_select, "No")                      ~ "Never",
    TRUE ~ NA_character_
  )
  factor(out, levels = c("Never", "Monthly/rarely", "Weekly", "Daily"),
         ordered = TRUE)
}

# Likert scales -> ordered factors plus a numeric score, so the same variable
# can be plotted as categories and correlated as a number.
ORDERED_SCALES <- list(
  ai_sentiment = c("Very unfavorable", "Unfavorable", "Indifferent",
                   "Favorable", "Very favorable"),
  ai_trust     = c("Highly distrust", "Somewhat distrust",
                   "Neither trust nor distrust", "Somewhat trust",
                   "Highly trust")
)

as_ordered_scale <- function(x, levels) {
  factor(ifelse(x %in% levels, x, NA_character_), levels = levels, ordered = TRUE)
}

#' Split a semicolon-separated multi-select column into one row per answer.
#'
#' @param df   data frame
#' @param col  column to split (unquoted)
#' @return long data frame with ResponseId and the split column
split_multiselect <- function(df, col) {
  df %>%
    select(ResponseId, {{ col }}) %>%
    filter(!is.na({{ col }}), {{ col }} != "") %>%
    separate_rows({{ col }}, sep = ";") %>%
    mutate("{{col}}" := str_trim({{ col }})) %>%
    filter({{ col }} != "")
}

#' Flag implausible / extreme salaries instead of silently dropping them.
#'
#' Keeps rows between `floor` and the `upper_q` quantile. The survey's own
#' ConvertedCompYearly contains values up to ~$60M, which are data-entry noise
#' (monthly figures reported as yearly, joke answers, etc.).
flag_salary_outlier <- function(comp, floor = 1000, upper_q = 0.99) {
  cap <- quantile(comp, upper_q, na.rm = TRUE)
  !is.na(comp) & (comp < floor | comp > cap)
}
