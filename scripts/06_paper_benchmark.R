# ---------------------------------------------------------------------------
# 06 - Paper benchmark: recent models, confusion matrices, ROC, p-values
#
# Put this file in the REPO ROOT and run:
#
#   Rscript 06_paper_benchmark.R
#
# It re-uses the project's own data (data/processed/survey_clean.rds), the same
# 75/25 stratified split and the same predictors as scripts/04_modeling.R, and
# adds recent models on top of the logistic-regression / random-forest pair:
#   Logistic regression, Random forest (ranger), XGBoost, LightGBM,
#   MLP neural network (nnet), and a soft-voting ensemble.
# (XGBoost / LightGBM are skipped automatically if they cannot be installed.)
#
# Everything the paper needs is written to  paper/ :
#   paper/figures/*.png     ROC curves, confusion matrices, copied EDA figures
#   paper/tables/*.tex      LaTeX tables + macros.tex (numbers used in the text)
#   paper/tables/*.csv      the same results as CSV
# Upload the whole  paper/  folder (plus main.tex) to Overleaf.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages(library(tidyverse))
source("R/stats.R")   # roc_auc(), roc_points(), classification_metrics(), best_threshold()

SEED <- 42
OUT  <- "paper"
FIG  <- file.path(OUT, "figures")
TAB  <- file.path(OUT, "tables")
dir.create(FIG, recursive = TRUE, showWarnings = FALSE)
dir.create(TAB, recursive = TRUE, showWarnings = FALSE)

# ---- packages ---------------------------------------------------------------
need <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    try(install.packages(pkg, repos = "https://cloud.r-project.org"), silent = TRUE)
  }
  requireNamespace(pkg, quietly = TRUE)
}
for (p in c("ranger", "pROC", "nnet")) {
  if (!need(p)) stop("Could not install required package: ", p, call. = FALSE)
}
HAS_XGB <- need("xgboost")
HAS_LGB <- need("lightgbm")
if (!HAS_XGB) message("NOTE: xgboost not available - skipped.")
if (!HAS_LGB) message("NOTE: lightgbm not available - skipped.")

# ---- helpers ----------------------------------------------------------------
tex_esc <- function(x) {
  x <- as.character(x)
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([&%$#_{}])", "\\\\\\1", x)
  x <- gsub("<", "$<$", x, fixed = TRUE)
  x <- gsub(">", "$>$", x, fixed = TRUE)
  x
}
f3 <- function(x) ifelse(is.na(x), "--", sprintf("%.3f", x))
fpv <- function(p) {
  ifelse(is.na(p), "--",
         ifelse(p < 0.001, "$<0.001$", sprintf("%.3f", p)))
}
bold_best <- function(x, higher = TRUE) {
  s <- f3(x)
  if (all(is.na(x))) return(s)
  best <- if (higher) which.max(x) else which.min(x)
  s[best] <- paste0("\\textbf{", s[best], "}")
  s
}
write_tex <- function(lines, name) {
  writeLines(lines, file.path(TAB, name))
  message("table saved:  ", file.path(TAB, name))
}

# ---- data -------------------------------------------------------------------
if (file.exists("data/processed/survey_clean.rds")) {
  d <- readRDS("data/processed/survey_clean.rds")
} else if (file.exists("data/sample/survey_clean_sample.csv")) {
  warning("Full cleaned data not found - using the 500-row SAMPLE. ",
          "Run scripts/01_data_cleaning.R first for paper-quality results.",
          call. = FALSE)
  d <- readr::read_csv("data/sample/survey_clean_sample.csv", show_col_types = FALSE)
} else {
  stop("Run  Rscript scripts/00_setup.R  and  Rscript scripts/01_data_cleaning.R  first.",
       call. = FALSE)
}

# Same predictors, same filtering as scripts/04_modeling.R (section B/C).
clf_data <- d %>%
  select(ai_user, years_code, age_numeric, education, region, remote_work,
         org_size, dev_type, job_sat, n_languages, ai_threat) %>%
  drop_na() %>%
  mutate(
    across(where(is.character), factor),
    education = factor(education, ordered = FALSE),
    ai_user_f = factor(ai_user, levels = c(0, 1), labels = c("no", "yes"))
  )

# Same stratified 75/25 split and seed as the project's modelling script.
set.seed(SEED)
train_idx <- clf_data %>%
  mutate(row = row_number()) %>%
  group_by(ai_user) %>%
  slice_sample(prop = 0.75) %>%
  pull(row)

feat_names <- c("years_code", "age_numeric", "education", "region", "remote_work",
                "org_size", "dev_type", "job_sat", "n_languages", "ai_threat")
feats <- clf_data[, feat_names]
y     <- clf_data$ai_user
X     <- model.matrix(~ ., data = feats)[, -1, drop = FALSE]
colnames(X) <- make.names(colnames(X), unique = TRUE)

tr <- train_idx
te <- setdiff(seq_len(nrow(clf_data)), tr)
ytr <- y[tr]; yte <- y[te]
message("rows: train = ", length(tr), ", test = ", length(te),
        " | positive share = ", round(100 * mean(y), 1), "%")

# ---- model zoo --------------------------------------------------------------
# Each function: fit on rows `a`, predict P(AI user) for rows `b`.
fit_predict <- function(model, a, b) {
  ya <- y[a]
  switch(model,
    "Logistic regression" = {
      m <- suppressWarnings(
        glm(ya ~ ., data = data.frame(ya = ya, X[a, , drop = FALSE]), family = binomial()))
      suppressWarnings(predict(m, newdata = data.frame(X[b, , drop = FALSE]),
                               type = "response"))
    },
    "Random forest" = {
      m <- ranger::ranger(ai_user_f ~ .,
                          data = cbind(ai_user_f = clf_data$ai_user_f[a], feats[a, ]),
                          num.trees = 500, probability = TRUE, seed = SEED)
      predict(m, data = feats[b, ])$predictions[, "yes"]
    },
    "XGBoost" = {
      m <- xgboost::xgb.train(
        params = list(objective = "binary:logistic", eval_metric = "auc",
                      eta = 0.05, max_depth = 3, subsample = 0.8,
                      colsample_bytree = 0.8, min_child_weight = 5, seed = SEED),
        data = xgboost::xgb.DMatrix(X[a, , drop = FALSE], label = ya),
        nrounds = 300, verbose = 0)
      predict(m, xgboost::xgb.DMatrix(X[b, , drop = FALSE]))
    },
    "LightGBM" = {
      m <- lightgbm::lgb.train(
        params = list(objective = "binary", learning_rate = 0.05, num_leaves = 15,
                      min_data_in_leaf = 40, feature_fraction = 0.8,
                      bagging_fraction = 0.8, bagging_freq = 1,
                      seed = SEED, verbose = -1),
        data = lightgbm::lgb.Dataset(X[a, , drop = FALSE], label = ya),
        nrounds = 300)
      predict(m, X[b, , drop = FALSE])
    },
    "MLP (nnet)" = {
      mu <- colMeans(X[a, , drop = FALSE])
      s  <- apply(X[a, , drop = FALSE], 2, sd); s[s == 0 | is.na(s)] <- 1
      Xa <- sweep(sweep(X[a, , drop = FALSE], 2, mu), 2, s, "/")
      Xb <- sweep(sweep(X[b, , drop = FALSE], 2, mu), 2, s, "/")
      set.seed(SEED)
      m <- nnet::nnet(Xa, ya, size = 8, decay = 0.1, maxit = 300,
                      MaxNWts = 100000, trace = FALSE)
      as.numeric(predict(m, Xb, type = "raw"))
    }
  )
}

base_models <- c("Logistic regression", "Random forest",
                 if (HAS_XGB) "XGBoost", if (HAS_LGB) "LightGBM", "MLP (nnet)")

# 5-fold out-of-fold predictions on the TRAIN set: used for the decision
# threshold (Youden's J) and a cross-validated AUC, never touching the test set.
set.seed(SEED)
fold <- sample(rep(1:5, length.out = length(tr)))

oof <- list(); te_prob <- list()
for (m in base_models) {
  message("fitting ", m, " ...")
  o <- numeric(length(tr))
  for (k in 1:5) {
    o[fold == k] <- fit_predict(m, tr[fold != k], tr[fold == k])
  }
  oof[[m]]     <- o
  te_prob[[m]] <- fit_predict(m, tr, te)
}

# Soft-voting ensemble of all base models.
oof[["Ensemble (soft vote)"]]     <- Reduce(`+`, oof) / length(oof)
te_prob[["Ensemble (soft vote)"]] <- Reduce(`+`, te_prob) / length(te_prob)
models <- names(te_prob)

cv_auc <- map_dbl(models, function(m)
  mean(map_dbl(1:5, ~ roc_auc(ytr[fold == .x], oof[[m]][fold == .x]))))
names(cv_auc) <- models
thr <- map_dbl(models, ~ best_threshold(ytr, oof[[.x]]))
names(thr) <- models

# ---- metrics ----------------------------------------------------------------
metrics <- map_dfr(models, function(m) {
  classification_metrics(yte, te_prob[[m]], thr[[m]]) %>%
    mutate(model = m, threshold = thr[[m]], cv_auc = cv_auc[[m]])
}) %>%
  mutate(
    n         = tp + fp + tn + fn,
    specificity = tn / (tn + fp),
    bal_acc   = (recall + specificity) / 2,
    mcc       = (tp * tn - fp * fn) /
                sqrt(as.numeric(tp + fp) * (tp + fn) * (tn + fp) * (tn + fn)),
    kappa     = {
      pe <- ((tp + fp) * (tp + fn) + (fn + tn) * (fp + tn)) / n^2
      (accuracy - pe) / (1 - pe)
    }
  ) %>%
  select(model, threshold, tn, fp, fn, tp, accuracy, precision, recall,
         specificity, f1, bal_acc, mcc, kappa, auc, cv_auc)

baseline_acc <- max(mean(yte), 1 - mean(yte))
readr::write_csv(metrics, file.path(TAB, "benchmark_metrics.csv"))
print(metrics, width = Inf)

# ---- ROC objects, DeLong CIs and pairwise tests -----------------------------
rocs <- map(models, ~ pROC::roc(yte, te_prob[[.x]], quiet = TRUE, direction = "<"))
names(rocs) <- models
ref <- "Logistic regression"

pvals <- map_dfr(models, function(m) {
  ci <- as.numeric(pROC::ci.auc(rocs[[m]], method = "delong"))
  row <- tibble(model = m, auc = as.numeric(pROC::auc(rocs[[m]])),
                lo = ci[1], hi = ci[3],
                delta = NA_real_, p_delong = NA_real_, p_mcnemar = NA_real_)
  if (m != ref) {
    row$delta    <- row$auc - as.numeric(pROC::auc(rocs[[ref]]))
    row$p_delong <- suppressWarnings(
      pROC::roc.test(rocs[[m]], rocs[[ref]], method = "delong")$p.value)
    ca <- (te_prob[[m]]   >= thr[[m]])   == (yte == 1)
    cb <- (te_prob[[ref]] >= thr[[ref]]) == (yte == 1)
    tb <- table(factor(ca, c(TRUE, FALSE)), factor(cb, c(TRUE, FALSE)))
    row$p_mcnemar <- tryCatch(mcnemar.test(tb)$p.value, error = function(e) NA_real_)
  }
  row
}) %>%
  mutate(p_holm = p.adjust(p_delong, method = "holm"))
readr::write_csv(pvals, file.path(TAB, "benchmark_pvalues.csv"))

# ---- figures ----------------------------------------------------------------
pal <- c("#4C78A8", "#F58518", "#54A24B", "#E45756", "#B279A2", "#222222")
pal <- setNames(pal[seq_along(models)], models)

theme_paper <- theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), plot.title.position = "plot",
        panel.grid.minor = element_blank(), legend.position = "bottom",
        legend.title = element_blank())

roc_df <- map_dfr(models, ~ roc_points(yte, te_prob[[.x]]) %>%
                    mutate(model = sprintf("%s (AUC = %.3f)", .x, metrics$auc[metrics$model == .x])))
op_df <- metrics %>%
  transmute(model = sprintf("%s (AUC = %.3f)", model, auc),
            fpr = fp / (fp + tn), tpr = tp / (tp + fn))
lev <- unique(roc_df$model)
roc_df$model <- factor(roc_df$model, levels = lev)
op_df$model  <- factor(op_df$model,  levels = lev)

roc_plot <- ggplot(roc_df, aes(fpr, tpr, colour = model)) +
  geom_abline(linetype = "dashed", colour = "grey60") +
  geom_line(linewidth = 0.9) +
  geom_point(data = op_df, size = 3, shape = 21, fill = "white", stroke = 1.3) +
  scale_colour_manual(values = unname(pal)) +
  coord_equal() +
  guides(colour = guide_legend(ncol = 2)) +
  labs(title = "ROC curves: predicting AI tool adoption",
       subtitle = "Test set; circles = Youden operating points",
       x = "False positive rate (1 - specificity)", y = "True positive rate (sensitivity)") +
  theme_paper
ggsave(file.path(FIG, "roc_comparison.png"), roc_plot,
       width = 7, height = 7, dpi = 300, bg = "white")

cm_long <- metrics %>%
  select(model, tn, fp, fn, tp) %>%
  pivot_longer(-model, names_to = "cell", values_to = "n") %>%
  mutate(actual    = if_else(cell %in% c("tn", "fp"), "Non-user (0)", "AI user (1)"),
         predicted = if_else(cell %in% c("tn", "fn"), "Non-user (0)", "AI user (1)")) %>%
  group_by(model, actual) %>% mutate(pct = 100 * n / sum(n)) %>% ungroup() %>%
  mutate(model = factor(model, levels = models),
         actual = factor(actual, levels = c("AI user (1)", "Non-user (0)")),
         predicted = factor(predicted, levels = c("Non-user (0)", "AI user (1)")))

cm_plot <- ggplot(cm_long, aes(predicted, actual, fill = pct)) +
  geom_tile(colour = "white", linewidth = 1) +
  geom_text(aes(label = sprintf("%d\n(%.1f%%)", n, pct),
                colour = pct > 55), size = 3.4, lineheight = 0.95) +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "black"), guide = "none") +
  scale_fill_gradient(low = "#EAF1F8", high = "#1F4E79", limits = c(0, 100), guide = "none") +
  facet_wrap(~ model, ncol = 3) +
  labs(title = "Confusion matrices on the held-out test set",
       subtitle = "Cell percentages are row-normalised (share of each actual class)",
       x = "Predicted class", y = "Actual class") +
  theme_paper + theme(strip.text = element_text(face = "bold"))
ggsave(file.path(FIG, "confusion_matrices.png"), cm_plot,
       width = 9, height = if (length(models) > 3) 6 else 3.6, dpi = 300, bg = "white")

# Re-use the project's own EDA figures in the paper.
for (f in c("04_ai_usage", "05_adoption_by_experience", "06_trust_by_experience",
            "16_rf_importance", "20_cluster_centres", "21_cluster_pca",
            "22_ordinal_trust_effects", "23_cluster_stability")) {
  src <- file.path("outputs/figures", paste0(f, ".png"))
  if (file.exists(src)) file.copy(src, file.path(FIG, basename(src)), overwrite = TRUE)
}

# ---- LaTeX tables -----------------------------------------------------------
# 1) Confusion-matrix counts
cm_tex <- c(
  "\\begin{tabular}{lccccc}", "\\toprule",
  "\\textbf{Model} & \\textbf{Thr.} & \\textbf{TN} & \\textbf{FP} & \\textbf{FN} & \\textbf{TP} \\\\",
  "\\midrule",
  with(metrics, sprintf("%s & %.2f & %d & %d & %d & %d \\\\",
                        tex_esc(model), threshold, tn, fp, fn, tp)),
  "\\bottomrule", "\\end{tabular}")
write_tex(cm_tex, "cm_counts.tex")

# 2) Classification metrics (best value per column in bold)
mt <- metrics
met_tex <- c(
  "\\begin{tabular}{lccccccccc}", "\\toprule",
  paste0("\\textbf{Model} & \\textbf{Acc.} & \\textbf{Prec.} & \\textbf{Rec.} & ",
         "\\textbf{Spec.} & \\textbf{F1} & \\textbf{MCC} & $\\boldsymbol{\\kappa}$ & ",
         "\\textbf{AUC} & \\textbf{CV-AUC} \\\\"),
  "\\midrule",
  paste0(tex_esc(mt$model), " & ", bold_best(mt$accuracy), " & ", bold_best(mt$precision),
         " & ", bold_best(mt$recall), " & ", bold_best(mt$specificity), " & ",
         bold_best(mt$f1), " & ", bold_best(mt$mcc), " & ", bold_best(mt$kappa),
         " & ", bold_best(mt$auc), " & ", bold_best(mt$cv_auc), " \\\\"),
  "\\midrule",
  sprintf("Majority-class baseline & %s & -- & -- & -- & -- & 0.000 & 0.000 & 0.500 & -- \\\\",
          f3(baseline_acc)),
  "\\bottomrule", "\\end{tabular}")
write_tex(met_tex, "metrics.tex")

# 3) Model-vs-model p-values (DeLong on AUC, McNemar on errors)
pv_tex <- c(
  "\\begin{tabular}{lccccc}", "\\toprule",
  paste0("\\textbf{Model} & \\textbf{AUC [95\\% CI]} & $\\boldsymbol{\\Delta}$\\textbf{AUC} & ",
         "\\textbf{DeLong $p$} & \\textbf{Holm $p$} & \\textbf{McNemar $p$} \\\\"),
  "\\midrule",
  with(pvals, sprintf("%s & %.3f [%.3f, %.3f] & %s & %s & %s & %s \\\\",
                      tex_esc(model), auc, lo, hi,
                      ifelse(is.na(delta), "ref.", sprintf("%+.3f", delta)),
                      ifelse(is.na(p_delong), "--", fpv(p_delong)),
                      ifelse(is.na(p_holm), "--", fpv(p_holm)),
                      ifelse(is.na(p_mcnemar), "--", fpv(p_mcnemar)))),
  "\\bottomrule", "\\end{tabular}")
write_tex(pv_tex, "model_pvalues.tex")

# 4) Hypothesis-test p-values from the project's own script 03
ht_path <- "outputs/tables/09_hypothesis_tests.csv"
if (file.exists(ht_path)) {
  ht <- readr::read_csv(ht_path, show_col_types = FALSE)
  labs <- c(H1 = "Adoption vs.\\ developer role", 
            H2 = "Salary: AI users vs.\\ non-users (Welch $t$)",
            H2b = "Salary: same, Wilcoxon rank-sum",
            H3 = "Salary across education (ANOVA)",
            H4 = "Trust vs.\\ experience ($\\chi^2$)",
            H4b = "Trust vs.\\ experience (trend test)",
            H5 = "Job satisfaction, daily AI users (Wilcoxon)",
            H6 = "Salary response vs.\\ region ($\\chi^2$)")
  id <- sub(":.*$", "", ht$hypothesis)
  pstr <- function(s) ifelse(grepl("<", s), "$<0.001$", s)
  ht_tex <- c(
    "\\begin{tabular}{p{3.9cm}rrp{2.5cm}cc}", "\\toprule",
    "\\textbf{Hypothesis} & \\textbf{Stat.} & \\textbf{df} & \\textbf{Effect size} & \\textbf{$p$} & \\textbf{Holm $p$} \\\\",
    "\\midrule",
    sprintf("%s: %s & %s & %s & %s & %s & %s \\\\",
            id, unname(labs[id]),
            sapply(ht$statistic, function(v) formatC(v, format = "f", digits = ifelse(abs(v) >= 1000, 0, 1), big.mark = ",")),
            ifelse(is.na(ht$df), "--", formatC(ht$df, format = "f", digits = 0, big.mark = ",")),
            ifelse(is.na(ht$effect_size), "--",
                   tex_esc(sub(";.*$", "", ht$effect_size))),
            pstr(ht$p_value), pstr(ht$p_adjusted)),
    "\\bottomrule", "\\end{tabular}")
  write_tex(ht_tex, "hypothesis_pvalues.tex")
} else {
  warning("outputs/tables/09_hypothesis_tests.csv missing - run scripts/03_statistical_tests.R",
          call. = FALSE)
}

# 5) Macros so the paper text always matches the numbers
best <- metrics %>% slice_max(auc, n = 1, with_ties = FALSE)
bp   <- pvals %>% filter(model == best$model)
get  <- function(m, col) metrics[[col]][metrics$model == m]
mac  <- function(name, val) sprintf("\\newcommand{\\%s}{%s}", name, val)
write_tex(c(
  mac("nTrain", format(length(tr), big.mark = "{,}")),
  mac("nTest", format(length(te), big.mark = "{,}")),
  mac("posShare", sprintf("%.1f", 100 * mean(y))),
  mac("baselineAcc", sprintf("%.3f", baseline_acc)),
  mac("bestModel", tex_esc(best$model)),
  mac("bestAUC", sprintf("%.3f", best$auc)),
  mac("bestAUCci", sprintf("%.3f--%.3f", bp$lo, bp$hi)),
  mac("bestAcc", sprintf("%.3f", best$accuracy)),
  mac("bestFone", sprintf("%.3f", best$f1)),
  mac("bestRecall", sprintf("%.3f", best$recall)),
  mac("bestSpec", sprintf("%.3f", best$specificity)),
  mac("bestMCC", sprintf("%.3f", best$mcc)),
  mac("bestDeltaAUC", ifelse(is.na(bp$delta), "0.000", sprintf("%+.3f", bp$delta))),
  mac("bestDeltaP", ifelse(is.na(bp$p_delong), "--", gsub("\\$", "", fpv(bp$p_delong)))),
  mac("lrAUC", sprintf("%.3f", get("Logistic regression", "auc"))),
  mac("lrAcc", sprintf("%.3f", get("Logistic regression", "accuracy"))),
  mac("rfAUC", sprintf("%.3f", get("Random forest", "auc"))),
  mac("rfAcc", sprintf("%.3f", get("Random forest", "accuracy"))),
  mac("nModels", length(models))
), "macros.tex")

message("\nDONE. Upload the '", OUT, "' folder (and main.tex) to Overleaf.")
