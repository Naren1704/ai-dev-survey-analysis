# ---------------------------------------------------------------------------
# 03 - Statistical inference
#
# Hypothesis tests for the research questions, each with an assumption check,
# an effect size and a confidence interval. Results land in
# outputs/tables/09_hypothesis_tests.csv and the console log.
#
#   Rscript scripts/03_statistical_tests.R
# ---------------------------------------------------------------------------

source("R/setup.R")
source("R/stats.R")

d <- readRDS(PATHS$clean_rds)

results <- list()   # one row per test, combined at the end

# ===========================================================================
# H1: AI tool adoption is not independent of developer role.
#     Chi-square test of independence.
# ===========================================================================
tab_role <- d %>%
  filter(!is.na(dev_type), !is.na(ai_user)) %>%
  count(dev_type, ai_user) %>%
  pivot_wider(names_from = ai_user, values_from = n, values_fill = 0) %>%
  column_to_rownames("dev_type") %>%
  as.matrix()

chi_role <- chisq.test(tab_role)
# Chi-square needs expected counts of at least 5 in (nearly) every cell.
cat("\nH1 - AI adoption vs developer role\n")
cat("  min expected cell count:", round(min(chi_role$expected), 1), "\n")
print(chi_role)
cat("  Cramer's V:", round(cramers_v(tab_role), 3), "\n")

save_table(as_tibble(tab_role, rownames = "dev_type"), "09a_adoption_by_role")

results$h1 <- tibble(
  hypothesis = "H1: AI adoption depends on developer role",
  test = "Chi-square test of independence",
  statistic = unname(chi_role$statistic),
  df = unname(chi_role$parameter),
  p_value = chi_role$p.value,
  effect_size = paste0("Cramer's V = ", round(cramers_v(tab_role), 3)),
  n = sum(tab_role)
)

# ===========================================================================
# H2: AI users and non-users differ in compensation.
#     Welch t-test on log salary + Wilcoxon rank-sum as a robustness check.
# ===========================================================================
comp_split <- d %>% filter(!is.na(log_comp), !is.na(ai_user))
x <- comp_split$log_comp[comp_split$ai_user == 1]
y <- comp_split$log_comp[comp_split$ai_user == 0]

# Assumption checks: normality (Shapiro on a 5,000-row sample, the test's
# maximum) and equal variance (Levene-style F test).
shapiro_x <- shapiro.test(sample(x, min(5000, length(x))))
shapiro_y <- shapiro.test(sample(y, min(5000, length(y))))
var_test  <- var.test(x, y)

t_comp <- t.test(x, y)                       # Welch: unequal variances assumed
w_comp <- suppressWarnings(wilcox.test(x, y, conf.int = TRUE))

cat("\nH2 - Compensation of AI users vs non-users (log scale)\n")
cat("  Shapiro-Wilk p (users / non-users):",
    fmt_p(shapiro_x$p.value), "/", fmt_p(shapiro_y$p.value), "\n")
cat("  Variance ratio test p:", fmt_p(var_test$p.value), "\n")
print(t_comp)
cat("  Cohen's d:", round(cohens_d(x, y), 3), "\n")
cat("  Wilcoxon p:", fmt_p(w_comp$p.value),
    " rank-biserial r:", round(rank_biserial(x, y), 3), "\n")
cat("  Median salary, users:  ",
    round(median(comp_split$comp_model[comp_split$ai_user == 1], na.rm = TRUE)), "\n")
cat("  Median salary, non-users:",
    round(median(comp_split$comp_model[comp_split$ai_user == 0], na.rm = TRUE)), "\n")

results$h2 <- tibble(
  hypothesis = "H2: Compensation differs between AI users and non-users",
  test = "Welch two-sample t-test on log(salary)",
  statistic = unname(t_comp$statistic),
  df = unname(t_comp$parameter),
  p_value = t_comp$p.value,
  effect_size = paste0("Cohen's d = ", round(cohens_d(x, y), 3),
                       "; 95% CI on log-diff [",
                       paste(round(t_comp$conf.int, 3), collapse = ", "), "]"),
  n = nrow(comp_split)
)

results$h2b <- tibble(
  hypothesis = "H2b: Same comparison without the normality assumption",
  test = "Wilcoxon rank-sum test",
  statistic = unname(w_comp$statistic),
  df = NA_real_,
  p_value = w_comp$p.value,
  effect_size = paste0("rank-biserial r = ", round(rank_biserial(x, y), 3)),
  n = nrow(comp_split)
)

# ===========================================================================
# H3: Compensation differs across education levels.
#     One-way ANOVA + Kruskal-Wallis, then Tukey HSD for the pairs.
# ===========================================================================
ed <- d %>%
  filter(!is.na(log_comp), !is.na(education)) %>%
  mutate(education = factor(education, ordered = FALSE))

aov_ed <- aov(log_comp ~ education, data = ed)
kw_ed  <- kruskal.test(log_comp ~ education, data = ed)
tukey  <- TukeyHSD(aov_ed)

cat("\nH3 - Compensation across education levels\n")
print(summary(aov_ed))
cat("  eta squared:", round(eta_squared(aov_ed), 4), "\n")
cat("  Kruskal-Wallis p:", fmt_p(kw_ed$p.value), "\n")
print(round(tukey$education, 4))

save_table(as_tibble(tukey$education, rownames = "comparison"),
           "09b_tukey_education")

aov_tab <- summary(aov_ed)[[1]]
results$h3 <- tibble(
  hypothesis = "H3: Compensation differs across education levels",
  test = "One-way ANOVA on log(salary)",
  statistic = aov_tab[1, "F value"],
  df = aov_tab[1, "Df"],
  p_value = aov_tab[1, "Pr(>F)"],
  effect_size = paste0("eta squared = ", round(eta_squared(aov_ed), 4)),
  n = nrow(ed)
)

# ===========================================================================
# H4: Trust in AI accuracy is not independent of experience bucket.
# ===========================================================================
tab_trust <- d %>%
  filter(!is.na(exp_bucket), !is.na(ai_trusts)) %>%
  count(exp_bucket, ai_trusts) %>%
  pivot_wider(names_from = ai_trusts, values_from = n, values_fill = 0) %>%
  column_to_rownames("exp_bucket") %>%
  as.matrix()

chi_trust <- chisq.test(tab_trust)
cat("\nH4 - Trust in AI accuracy vs experience\n")
print(chi_trust)
cat("  Cramer's V:", round(cramers_v(tab_trust), 3), "\n")

# Trend across the ordered experience buckets, which the chi-square ignores.
trend <- prop.trend.test(tab_trust[, "1"], rowSums(tab_trust))
cat("  Chi-square test for trend p:", fmt_p(trend$p.value), "\n")

results$h4 <- tibble(
  hypothesis = "H4: Trust in AI accuracy depends on experience",
  test = "Chi-square test of independence",
  statistic = unname(chi_trust$statistic),
  df = unname(chi_trust$parameter),
  p_value = chi_trust$p.value,
  effect_size = paste0("Cramer's V = ", round(cramers_v(tab_trust), 3)),
  n = sum(tab_trust)
)

results$h4b <- tibble(
  hypothesis = "H4b: Trust changes monotonically with experience",
  test = "Chi-square test for trend in proportions",
  statistic = unname(trend$statistic),
  df = unname(trend$parameter),
  p_value = trend$p.value,
  effect_size = NA_character_,
  n = sum(tab_trust)
)

# ===========================================================================
# H5: Job satisfaction differs between daily AI users and everyone else.
# ===========================================================================
js <- d %>% filter(!is.na(job_sat), !is.na(ai_daily))
js_x <- js$job_sat[js$ai_daily == 1]
js_y <- js$job_sat[js$ai_daily == 0]
w_js <- suppressWarnings(wilcox.test(js_x, js_y, conf.int = TRUE))

cat("\nH5 - Job satisfaction, daily AI users vs others\n")
cat("  medians:", median(js_x), "vs", median(js_y), "\n")
print(w_js)

results$h5 <- tibble(
  hypothesis = "H5: Job satisfaction differs for daily AI users",
  test = "Wilcoxon rank-sum test",
  statistic = unname(w_js$statistic),
  df = NA_real_,
  p_value = w_js$p.value,
  effect_size = paste0("rank-biserial r = ", round(rank_biserial(js_x, js_y), 3)),
  n = nrow(js)
)

# ===========================================================================
# Summary table
# ===========================================================================
# Five independent families of tests, so p-values are adjusted (Holm) to keep
# the family-wise error rate at 5%.
summary_tbl <- bind_rows(results) %>%
  mutate(
    p_adjusted = p.adjust(p_value, method = "holm"),
    significant = p_adjusted < 0.05,
    p_value = fmt_p(p_value),
    p_adjusted = fmt_p(p_adjusted),
    across(c(statistic, df), ~ round(.x, 3))
  )

save_table(summary_tbl, "09_hypothesis_tests")
print(summary_tbl, width = Inf)

message("Statistical tests complete.")
