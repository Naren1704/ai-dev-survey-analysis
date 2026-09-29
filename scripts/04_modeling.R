# ---------------------------------------------------------------------------
# 04 - Regression and machine learning
#
#   A. Linear regression: what predicts log(salary)?          (lm + diagnostics)
#   B. Logistic regression: who uses AI tools?                (glm)
#   C. Random forest on the same target, compared on held-out data (ranger)
#
#   Rscript scripts/04_modeling.R
# ---------------------------------------------------------------------------

source("R/setup.R")
source("R/stats.R")
suppressPackageStartupMessages({
  library(ranger)
  library(car)
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

# --- Save the fitted models so the report does not refit them --------------
saveRDS(list(lm = fit_lm, glm = fit_glm, rf = fit_rf,
             comparison = comparison, test_n = nrow(test),
             thresholds = c(logistic = glm_cut, rf = rf_cut)),
        file.path(PATHS$models, "models.rds"))

message("Modelling complete.")
