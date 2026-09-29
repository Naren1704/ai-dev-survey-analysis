# ---------------------------------------------------------------------------
# 04 - Regression and machine learning
#
#   A. Linear regression: what predicts log(salary)?          (lm + diagnostics)
#   A2. The same model with heteroskedasticity-robust standard errors (sandwich)
#   B. Logistic regression: who uses AI tools?                (glm)
#   C. Random forest on the same target, compared on held-out data (ranger)
#   D. Ordinal logistic regression on the five-level trust scale (MASS::polr)
#
#   Rscript scripts/04_modeling.R
# ---------------------------------------------------------------------------

source("R/setup.R")
source("R/stats.R")
suppressPackageStartupMessages({
  library(ranger)
  library(car)
  library(sandwich)
  library(lmtest)
})

d <- readRDS(PATHS$clean_rds)
set.seed(SEED)

# ===========================================================================
# A. Linear regression on log(salary)
# ===========================================================================
lm_data <- d %>%
  select(log_comp, years_code, age_numeric, education, region, remote_work,
         org_size, dev_type, ai_user, n_languages) %>%
  drop_na() %>%
  mutate(education = factor(education, ordered = FALSE))

message("linear model rows: ", nrow(lm_data))

fit_lm <- lm(log_comp ~ years_code + age_numeric + education + region +
               remote_work + org_size + dev_type + ai_user + n_languages,
             data = lm_data)

cat("\n=== A. Linear regression on log(salary) ===\n")
print(summary(fit_lm))

coef_tbl <- broom::tidy(fit_lm, conf.int = TRUE) %>%
  mutate(
    # log-scale coefficients read more naturally as percent changes in salary.
    pct_effect = 100 * (exp(estimate) - 1),
    across(where(is.numeric), ~ round(.x, 4))
  )
save_table(coef_tbl, "10_lm_coefficients")

glance_tbl <- broom::glance(fit_lm)
cat("R squared:", round(glance_tbl$r.squared, 3),
    " adjusted:", round(glance_tbl$adj.r.squared, 3), "\n")

# --- Diagnostics -----------------------------------------------------------
# Multicollinearity: GVIF^(1/(2*Df)) above ~2.2 (i.e. VIF > 5) is a concern.
vif_tbl <- as_tibble(vif(fit_lm), rownames = "term") %>%
  rename(gvif = GVIF, df = Df, gvif_scaled = `GVIF^(1/(2*Df))`) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
cat("\nVariance inflation factors:\n")
print(vif_tbl)
save_table(vif_tbl, "11_lm_vif")

diag_df <- tibble(
  fitted = fitted(fit_lm),
  resid = resid(fit_lm),
  std_resid = rstandard(fit_lm)
)

resid_plot <- ggplot(diag_df, aes(fitted, resid)) +
  geom_point(alpha = 0.08, colour = PALETTE[1]) +
  geom_hline(yintercept = 0, colour = PALETTE[4]) +
  geom_smooth(se = FALSE, colour = PALETTE[2], linewidth = 0.8) +
  labs(title = "Residuals vs fitted values",
       subtitle = "A flat line means the linear specification is adequate",
       x = "Fitted log(salary)", y = "Residual")
save_fig(resid_plot, "12_lm_residuals")

qq_plot <- ggplot(diag_df, aes(sample = std_resid)) +
  stat_qq(alpha = 0.1, colour = PALETTE[1]) +
  stat_qq_line(colour = PALETTE[4]) +
  labs(title = "Normal Q-Q plot of standardised residuals",
       x = "Theoretical quantiles", y = "Sample quantiles")
save_fig(qq_plot, "13_lm_qq")

# Largest effects, excluding the intercept, for the report.
effect_plot <- coef_tbl %>%
  filter(term != "(Intercept)") %>%
  slice_max(abs(estimate), n = 15) %>%
  ggplot(aes(x = estimate, y = fct_reorder(term, estimate))) +
  geom_vline(xintercept = 0, colour = "grey60") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), colour = PALETTE[1]) +
  labs(title = "Largest salary effects",
       subtitle = "Linear model coefficients on log(salary), 95% CI",
       x = "Effect on log(salary)", y = NULL)
save_fig(effect_plot, "14_lm_effects", height = 6)

# ===========================================================================
# A2. Heteroskedasticity-robust standard errors
# ===========================================================================
# The residual plot fans out: salary variance is larger in high-paying regions
# than in low-paying ones, which violates the constant-variance assumption
# behind the ordinary standard errors. The coefficients stay unbiased, so the
# fix is HC3 sandwich errors rather than a different model. A Breusch-Pagan
# test states the problem formally.
bp <- bptest(fit_lm)
cat("\nBreusch-Pagan test for heteroskedasticity: BP =", round(bp$statistic, 1),
    ", df =", bp$parameter, ", p =", fmt_p(bp$p.value), "\n")

robust <- coeftest(fit_lm, vcov. = vcovHC(fit_lm, type = "HC3"))

robust_tbl <- tibble(
  term          = rownames(robust),
  estimate      = robust[, "Estimate"],
  se_classical  = summary(fit_lm)$coefficients[, "Std. Error"],
  se_robust     = robust[, "Std. Error"],
  p_classical   = summary(fit_lm)$coefficients[, "Pr(>|t|)"],
  p_robust      = robust[, "Pr(>|t|)"]
) %>%
  mutate(
    se_ratio = se_robust / se_classical,
    # Coefficients whose verdict changes once the errors are corrected.
    flips_at_5pct = (p_classical < 0.05) != (p_robust < 0.05),
    across(where(is.numeric), ~ round(.x, 4))
  )

save_table(robust_tbl, "21_lm_robust_se")
cat("Robust SEs are on average", round(mean(robust_tbl$se_ratio), 3),
    "times the classical ones;",
    sum(robust_tbl$flips_at_5pct), "coefficient(s) change significance.\n")

# ===========================================================================
# A3. Does dropping the non-reporters bias the salary model?
# ===========================================================================
# Half the sample skips the compensation question, and script 03 shows the
# response rate varies by region. Complete-case regression is only unbiased if
# reporting is unrelated to salary given the predictors. This refits the model
# weighted by the inverse probability of reporting: respondents from
# under-reporting groups stand in for their missing peers. If the coefficients
# barely move, the complete-case results survive the objection.
response_data <- d %>%
  select(comp_yearly, years_code, age_numeric, education, region, remote_work,
         org_size, dev_type, ai_user, n_languages) %>%
  mutate(reports = as.integer(!is.na(comp_yearly))) %>%
  select(-comp_yearly) %>%
  drop_na(-reports) %>%
  mutate(education = factor(education, ordered = FALSE))

fit_response <- glm(reports ~ years_code + age_numeric + education + region +
                      remote_work + org_size + dev_type + ai_user + n_languages,
                    data = response_data, family = binomial())

# Weights are trimmed at the 99th percentile: a handful of very small
# predicted probabilities would otherwise dominate the fit.
ipw_data <- lm_data %>%
  mutate(p_report = predict(fit_response, newdata = ., type = "response"),
         weight   = 1 / p_report,
         weight   = pmin(weight, quantile(weight, 0.99)))

fit_ipw <- lm(formula(fit_lm), data = ipw_data, weights = ipw_data$weight)

ipw_compare <- broom::tidy(fit_lm) %>%
  select(term, complete_case = estimate) %>%
  left_join(broom::tidy(fit_ipw) %>% select(term, weighted = estimate),
            by = "term") %>%
  mutate(
    difference = weighted - complete_case,
    pct_of_effect = round(100 * difference / complete_case, 1),
    across(where(is.numeric), ~ round(.x, 4))
  ) %>%
  arrange(desc(abs(difference)))

save_table(ipw_compare, "25_ipw_sensitivity")

cat("\n=== A3. Inverse-probability-weighted sensitivity check ===\n")
cat("Largest coefficient shifts when non-reporters are weighted back in:\n")
print(head(ipw_compare, 8), width = Inf)
cat("Median absolute shift:",
    round(median(abs(ipw_compare$difference), na.rm = TRUE), 4),
    "log points; R squared weighted:",
    round(summary(fit_ipw)$r.squared, 3), "\n")

# ===========================================================================
# B/C. Predicting AI tool adoption
# ===========================================================================
clf_data <- d %>%
  select(ai_user, years_code, age_numeric, education, region, remote_work,
         org_size, dev_type, job_sat, n_languages, ai_threat) %>%
  drop_na() %>%
  mutate(
    education = factor(education, ordered = FALSE),
    ai_user_f = factor(ai_user, levels = c(0, 1), labels = c("no", "yes"))
  )

message("classification rows: ", nrow(clf_data),
        " | positive class share: ",
        round(100 * mean(clf_data$ai_user), 1), "%")

# 75/25 stratified split.
train_idx <- clf_data %>%
  mutate(row = row_number()) %>%
  group_by(ai_user) %>%
  slice_sample(prop = 0.75) %>%
  pull(row)

train <- clf_data[train_idx, ]
test  <- clf_data[-train_idx, ]

# --- B. Logistic regression ------------------------------------------------
fit_glm <- glm(ai_user ~ years_code + age_numeric + education + region +
                 remote_work + org_size + dev_type + job_sat + n_languages +
                 ai_threat,
               data = train, family = binomial())

cat("\n=== B. Logistic regression: who uses AI tools? ===\n")
print(summary(fit_glm))

glm_coef <- broom::tidy(fit_glm, conf.int = TRUE, exponentiate = TRUE) %>%
  rename(odds_ratio = estimate) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))
save_table(glm_coef, "12_logit_odds_ratios")

glm_prob <- predict(fit_glm, newdata = test, type = "response")

# About 80% of respondents use AI tools, so a 0.5 cutoff labels nearly
# everyone a user. The cutoff is tuned on the training set (Youden's J) and
# then applied, unchanged, to the held-out set.
glm_cut <- best_threshold(train$ai_user,
                          predict(fit_glm, type = "response"))
cat("\nTuned decision threshold (logistic):", glm_cut, "\n")

glm_metrics <- classification_metrics(test$ai_user, glm_prob, glm_cut)
cat("Logistic regression, held-out performance:\n")
print(glm_metrics)

# --- Cross-validated AUC (5-fold) so the comparison is not one lucky split --
folds <- sample(rep(1:5, length.out = nrow(clf_data)))
cv_auc <- map_dbl(1:5, function(k) {
  tr <- clf_data[folds != k, ]; te <- clf_data[folds == k, ]
  m <- glm(formula(fit_glm), data = tr, family = binomial())
  roc_auc(te$ai_user, predict(m, newdata = te, type = "response"))
})
cat("5-fold CV AUC (logistic): mean", round(mean(cv_auc), 3),
    "sd", round(sd(cv_auc), 3), "\n")

# --- C. Random forest ------------------------------------------------------
fit_rf <- ranger(
  ai_user_f ~ years_code + age_numeric + education + region + remote_work +
    org_size + dev_type + job_sat + n_languages + ai_threat,
  data = train,
  num.trees = 500,
  probability = TRUE,
  importance = "permutation",
  seed = SEED
)

cat("\n=== C. Random forest ===\n")
print(fit_rf)

rf_prob <- predict(fit_rf, data = test)$predictions[, "yes"]
# ranger's own out-of-bag predictions give an honest in-training probability
# to tune the threshold on, without touching the test set.
rf_cut <- best_threshold(train$ai_user, fit_rf$predictions[, "yes"])
cat("\nTuned decision threshold (random forest):", rf_cut, "\n")

rf_metrics <- classification_metrics(test$ai_user, rf_prob, rf_cut)
cat("Random forest, held-out performance:\n")
print(rf_metrics)

rf_cv_auc <- map_dbl(1:5, function(k) {
  tr <- clf_data[folds != k, ]; te <- clf_data[folds == k, ]
  m <- ranger(ai_user_f ~ years_code + age_numeric + education + region +
                remote_work + org_size + dev_type + job_sat + n_languages +
                ai_threat,
              data = tr, num.trees = 300, probability = TRUE, seed = SEED)
  roc_auc(te$ai_user, predict(m, data = te)$predictions[, "yes"])
})
cat("5-fold CV AUC (random forest): mean", round(mean(rf_cv_auc), 3),
    "sd", round(sd(rf_cv_auc), 3), "\n")

# --- Model comparison ------------------------------------------------------
# Baseline: always predict the majority class.
baseline_acc <- max(mean(test$ai_user), 1 - mean(test$ai_user))

comparison <- bind_rows(
  glm_metrics %>% mutate(model = "Logistic regression", threshold = glm_cut,
                         cv_auc = mean(cv_auc)),
  rf_metrics  %>% mutate(model = "Random forest",       threshold = rf_cut,
                         cv_auc = mean(rf_cv_auc))
) %>%
  add_row(model = "Majority-class baseline", accuracy = baseline_acc, auc = 0.5) %>%
  select(model, threshold, accuracy, precision, recall, f1, auc, cv_auc,
         tp, fp, tn, fn) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

save_table(comparison, "13_model_comparison")
cat("\nModel comparison (test set):\n")
print(comparison, width = Inf)

# --- ROC curves ------------------------------------------------------------
roc_df <- bind_rows(
  roc_points(test$ai_user, glm_prob) %>% mutate(model = "Logistic regression"),
  roc_points(test$ai_user, rf_prob)  %>% mutate(model = "Random forest")
)

roc_plot <- ggplot(roc_df, aes(fpr, tpr, colour = model)) +
  geom_abline(linetype = "dashed", colour = "grey60") +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(values = PALETTE[1:2], name = NULL) +
  coord_equal() +
  labs(title = "ROC curves: predicting AI tool adoption",
       subtitle = paste0("Held-out test set, n = ", nrow(test),
                         " | AUC: logistic ", round(glm_metrics$auc, 3),
                         ", random forest ", round(rf_metrics$auc, 3)),
       x = "False positive rate", y = "True positive rate")
save_fig(roc_plot, "15_roc_curves", width = 6.5, height = 6)

# --- Feature importance ----------------------------------------------------
imp_tbl <- tibble(
  feature = names(fit_rf$variable.importance),
  importance = unname(fit_rf$variable.importance)
) %>%
  arrange(desc(importance))
save_table(imp_tbl, "14_rf_importance")

imp_plot <- imp_tbl %>%
  ggplot(aes(x = importance, y = fct_reorder(feature, importance))) +
  geom_col(fill = PALETTE[3]) +
  labs(title = "Random forest variable importance",
       subtitle = "Permutation importance for predicting AI tool adoption",
       x = "Mean decrease in accuracy", y = NULL)
save_fig(imp_plot, "16_rf_importance")

# ===========================================================================
# D. Ordinal logistic regression: what predicts trust in AI accuracy?
# ===========================================================================
# Trust is measured on a five-point ordered scale. Collapsing it to a binary
# (as the chi-square tests in script 03 do) throws away the ordering, and
# treating it as a number assumes the gaps between categories are equal.
# A proportional-odds model uses the ordering without either assumption.
ord_data <- d %>%
  select(ai_trust, years_code, age_numeric, education, region, remote_work,
         org_size, dev_type, job_sat, n_languages, ai_usage) %>%
  drop_na() %>%
  mutate(
    education = factor(education, ordered = FALSE),
    # Unordered, so each usage level gets its own interpretable coefficient
    # instead of a polynomial contrast.
    ai_usage  = factor(as.character(ai_usage),
                       levels = c("Never", "Monthly/rarely", "Weekly", "Daily"))
  )

message("ordinal model rows: ", nrow(ord_data))

fit_ord <- MASS::polr(
  ai_trust ~ years_code + age_numeric + education + region + remote_work +
    org_size + dev_type + job_sat + n_languages + ai_usage,
  data = ord_data, Hess = TRUE, method = "logistic"
)

cat("\n=== D. Ordinal logistic regression on trust in AI accuracy ===\n")
print(summary(fit_ord))

# polr reports no p-values; the Wald approximation supplies them.
ord_coef <- as_tibble(coef(summary(fit_ord)), rownames = "term") %>%
  rename(estimate = Value, se = `Std. Error`, t_value = `t value`) %>%
  mutate(
    p_value    = 2 * pnorm(abs(t_value), lower.tail = FALSE),
    odds_ratio = exp(estimate),
    conf_low   = exp(estimate - 1.96 * se),
    conf_high  = exp(estimate + 1.96 * se),
    # The last rows are the cut-points between adjacent trust levels, not
    # predictors, so they are labelled as such.
    kind = if_else(str_detect(term, "\\|"), "cut-point", "predictor"),
    across(where(is.numeric), ~ round(.x, 4))
  )
save_table(ord_coef, "22_ordinal_trust_model")

cat("\nLargest effects on trust (odds ratio, >1 = more trusting):\n")
print(ord_coef %>%
        filter(kind == "predictor") %>%
        slice_max(abs(estimate), n = 10) %>%
        select(term, odds_ratio, conf_low, conf_high, p_value),
      width = Inf)

# --- Proportional-odds assumption -----------------------------------------
# The model assumes one coefficient per predictor holds at every cut-point.
# Fitting a separate binary logit at each of the four cut-points and comparing
# the coefficients is the informal version of a Brant test (which would need
# another package).
cut_models <- map_dfr(1:4, function(k) {
  y <- as.integer(as.integer(ord_data$ai_trust) > k)
  m <- glm(y ~ years_code + age_numeric + education + region + remote_work +
             org_size + dev_type + job_sat + n_languages + ai_usage,
           data = ord_data, family = binomial())
  tibble(cutpoint = k, term = names(coef(m)), estimate = unname(coef(m)))
})

po_check <- cut_models %>%
  filter(term != "(Intercept)") %>%
  group_by(term) %>%
  summarise(
    mean_estimate = mean(estimate),
    sd_estimate   = sd(estimate),
    # A coefficient that swings more than it is large is not constant across
    # cut-points, which is what the model assumes.
    unstable      = sd_estimate > abs(mean_estimate),
    .groups = "drop"
  ) %>%
  arrange(desc(sd_estimate)) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

save_table(po_check, "23_proportional_odds_check")
cat("\nProportional-odds check:",
    sum(po_check$unstable), "of", nrow(po_check),
    "coefficients vary more across cut-points than their own size.\n")
print(head(po_check, 8))

ord_plot <- ord_coef %>%
  filter(kind == "predictor") %>%
  # Ranked by evidence (|t|), not by raw size: a sparse category such as
  # "Retired" produces a huge coefficient with a confidence interval three
  # orders of magnitude wide, which would otherwise top the chart.
  slice_max(abs(t_value), n = 14) %>%
  ggplot(aes(x = odds_ratio, y = fct_reorder(term, odds_ratio))) +
  geom_vline(xintercept = 1, colour = "grey60") +
  geom_pointrange(aes(xmin = conf_low, xmax = conf_high), colour = PALETTE[1]) +
  scale_x_log10() +
  labs(title = "What predicts trust in AI accuracy",
       subtitle = "Odds ratios with 95% CI, 14 best-evidenced effects (log scale)",
       x = "Odds ratio (>1 = more trusting)", y = NULL,
       caption = paste0("Reference levels: ",
                        levels(ord_data$region)[1], " (region), ",
                        levels(ord_data$ai_usage)[1], " (AI usage), ",
                        levels(ord_data$dev_type)[1], " (role). ",
                        "Stack Overflow Developer Survey 2025"))
save_fig(ord_plot, "22_ordinal_trust_effects", width = 9.5, height = 6)

# --- Save the fitted models so the report does not refit them --------------
saveRDS(list(lm = fit_lm, glm = fit_glm, rf = fit_rf, ordinal = fit_ord,
             comparison = comparison, test_n = nrow(test),
             bp_test = bp, ordinal_n = nrow(ord_data),
             thresholds = c(logistic = glm_cut, rf = rf_cut)),
        file.path(PATHS$models, "models.rds"))

message("Modelling complete.")
