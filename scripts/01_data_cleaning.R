# ---------------------------------------------------------------------------
# 01 - Data cleaning and feature engineering
#
# Reads the raw survey CSV (172 columns), keeps the variables the analysis
# needs, fixes types, engineers the modelling features, and writes
# data/processed/survey_clean.rds (+ a CSV copy).
#
#   Rscript scripts/01_data_cleaning.R
# ---------------------------------------------------------------------------

source("R/setup.R")
source("R/cleaning.R")

stopifnot(file.exists(PATHS$raw_csv))

raw <- read_csv(PATHS$raw_csv, show_col_types = FALSE, progress = FALSE,
                na = c("", "NA"))
message("raw dimensions: ", nrow(raw), " x ", ncol(raw))

# --- 1. Keep the columns of interest --------------------------------------
survey <- raw %>%
  select(any_of(KEEP_COLS)) %>%
  mutate(across(where(is.character), ~ na_if(str_trim(.x), "")))

# --- 2. Types and derived features ----------------------------------------
clean <- survey %>%
  mutate(
    # experience ----------------------------------------------------------
    years_code   = parse_years(YearsCode),
    work_exp     = suppressWarnings(as.numeric(WorkExp)),
    age_group    = factor(Age, levels = c(
      "Under 18 years old", "18-24 years old", "25-34 years old",
      "35-44 years old", "45-54 years old", "55-64 years old",
      "65 years or older"), ordered = TRUE),
    age_numeric  = age_midpoint(Age),
    exp_bucket   = cut(years_code,
                       breaks = c(-Inf, 2, 5, 10, 20, Inf),
                       labels = c("0-2", "3-5", "6-10", "11-20", "20+")),

    # demographics / job context ------------------------------------------
    education    = collapse_education(EdLevel),
    org_size     = collapse_org_size(OrgSize),
    region       = factor(to_region(Country)),
    remote_work  = factor(case_when(
      str_starts(RemoteWork, "Remote")   ~ "Remote",
      str_starts(RemoteWork, "Hybrid")   ~ "Hybrid",
      str_starts(RemoteWork, "In-person") ~ "In-person",
      str_starts(RemoteWork, "Your choice") ~ "Flexible",
      TRUE ~ NA_character_)),
    # DevType is a single-choice field; only the frequent roles are kept as
    # separate levels so tests are not dominated by 1-2 person categories.
    dev_type     = fct_lump_min(factor(DevType), min = 300, other_level = "Other role"),
    industry     = fct_lump_min(factor(Industry), min = 200, other_level = "Other industry"),

    # outcomes -------------------------------------------------------------
    comp_yearly  = suppressWarnings(as.numeric(ConvertedCompYearly)),
    job_sat      = suppressWarnings(as.numeric(JobSat)),

    # AI variables ---------------------------------------------------------
    ai_select_raw  = AISelect,
    ai_user        = is_ai_user(AISelect),
    ai_usage       = ai_usage_level(AISelect),
    ai_daily       = as.integer(ai_usage == "Daily"),
    ai_sentiment   = as_ordered_scale(AISent, ORDERED_SCALES$ai_sentiment),
    ai_sent_score  = as.integer(ai_sentiment),
    ai_trust       = as_ordered_scale(AIAcc, ORDERED_SCALES$ai_trust),
    ai_trust_score = as.integer(ai_trust),
    ai_trusts      = case_when(
      ai_trust %in% c("Somewhat trust", "Highly trust")       ~ 1L,
      ai_trust %in% c("Highly distrust", "Somewhat distrust",
                      "Neither trust nor distrust")           ~ 0L,
      TRUE ~ NA_integer_),
    ai_complex_ok  = case_when(
      str_detect(AIComplex, "^Very well|^Good")               ~ 1L,
      str_detect(AIComplex, "^Bad|^Very poor|^Neither")       ~ 0L,
      TRUE ~ NA_integer_),
    ai_threat      = factor(AIThreat, levels = c("No", "I'm not sure", "Yes")),
    ai_agent_user  = case_when(
      str_starts(AIAgents, "Yes") ~ 1L,
      str_starts(AIAgents, "No")  ~ 0L,
      TRUE ~ NA_integer_),

    # multi-select counts (how broad is someone's stack?) -------------------
    n_languages    = str_count(LanguageHaveWorkedWith, ";") + 1L,
    n_ai_models    = str_count(AIModelsHaveWorkedWith, ";") + 1L
  )

# --- 3. Salary treatment ---------------------------------------------------
# Extreme values are flagged, not deleted, so the EDA can show them and the
# models can exclude them explicitly.
clean <- clean %>%
  mutate(
    comp_outlier = flag_salary_outlier(comp_yearly),
    comp_model   = if_else(comp_outlier, NA_real_, comp_yearly),
    log_comp     = log(comp_model)
  )

# --- 4. Analysis population ------------------------------------------------
# Professional developers only: students and hobbyists answer the salary and
# work-context questions differently, which would confound every comparison.
clean <- clean %>%
  filter(str_detect(MainBranch, "developer by profession|part of my work")) %>%
  select(
    ResponseId, main_branch = MainBranch,
    age_group, age_numeric, education, region, country = Country,
    remote_work, org_size, industry, dev_type,
    years_code, work_exp, exp_bucket,
    comp_yearly, comp_model, comp_outlier, log_comp, job_sat,
    ai_select_raw, ai_user, ai_usage, ai_daily,
    ai_sentiment, ai_sent_score, ai_trust, ai_trust_score, ai_trusts,
    ai_complex_ok, ai_threat, ai_agent_user,
    n_languages, n_ai_models,
    languages = LanguageHaveWorkedWith, ai_models = AIModelsHaveWorkedWith,
    ai_frustration = AIFrustration, ai_human = AIHuman, ai_open = AIOpen
  )

message("analysis population: ", nrow(clean), " respondents")

# --- 5. Missingness report -------------------------------------------------
missing_report <- clean %>%
  summarise(across(everything(), ~ mean(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "pct_missing") %>%
  arrange(desc(pct_missing)) %>%
  mutate(pct_missing = round(100 * pct_missing, 1))

save_table(missing_report, "01_missingness")
print(head(missing_report, 15))

# --- 6. Write outputs ------------------------------------------------------
saveRDS(clean, PATHS$clean_rds)
# The CSV drops the long free-text columns to stay a reasonable size.
clean %>%
  select(-ai_open, -ai_frustration, -ai_human, -languages, -ai_models) %>%
  write_csv(PATHS$clean_csv)

message("written: ", PATHS$clean_rds, " and ", PATHS$clean_csv)
