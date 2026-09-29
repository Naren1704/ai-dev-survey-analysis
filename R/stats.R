# ---------------------------------------------------------------------------
# Small statistics helpers: effect sizes and a dependency-free ROC-AUC.
# Base R covers the tests themselves (chisq.test, t.test, aov, kruskal.test).
# ---------------------------------------------------------------------------

#' Cramer's V for a contingency table (effect size for chi-square).
cramers_v <- function(tbl) {
  chi <- suppressWarnings(chisq.test(tbl)$statistic)
  n <- sum(tbl)
  unname(sqrt(chi / (n * (min(dim(tbl)) - 1))))
}

#' Cohen's d for two independent samples (pooled SD).
cohens_d <- function(x, y) {
  x <- x[!is.na(x)]; y <- y[!is.na(y)]
  nx <- length(x); ny <- length(y)
  s <- sqrt(((nx - 1) * var(x) + (ny - 1) * var(y)) / (nx + ny - 2))
  (mean(x) - mean(y)) / s
}

#' Rank-biserial correlation: effect size for the Wilcoxon rank-sum test.
rank_biserial <- function(x, y) {
  x <- x[!is.na(x)]; y <- y[!is.na(y)]
  u <- suppressWarnings(wilcox.test(x, y)$statistic)
  2 * unname(u) / (length(x) * length(y)) - 1
}

#' Eta squared for a one-way ANOVA fit.
eta_squared <- function(fit) {
  tab <- summary(fit)[[1]]
  tab[1, "Sum Sq"] / sum(tab[, "Sum Sq"])
}

#' ROC-AUC via the Mann-Whitney U identity (no pROC dependency).
#'
#' @param truth 0/1 vector of observed labels
#' @param prob  predicted probability of class 1
roc_auc <- function(truth, prob) {
  pos <- prob[truth == 1]; neg <- prob[truth == 0]
  if (!length(pos) || !length(neg)) return(NA_real_)
  r <- rank(c(pos, neg))
  (sum(r[seq_along(pos)]) - length(pos) * (length(pos) + 1) / 2) /
    (length(pos) * length(neg))
}

#' Points of an ROC curve, for plotting.
roc_points <- function(truth, prob) {
  ord <- order(prob, decreasing = TRUE)
  t <- truth[ord]
  tibble(
    fpr = cumsum(t == 0) / sum(t == 0),
    tpr = cumsum(t == 1) / sum(t == 1)
  ) %>%
    add_row(fpr = 0, tpr = 0, .before = 1)
}

#' Confusion matrix plus accuracy / precision / recall / F1 at a threshold.
classification_metrics <- function(truth, prob, threshold = 0.5) {
  pred <- as.integer(prob >= threshold)
  tp <- sum(pred == 1 & truth == 1); fp <- sum(pred == 1 & truth == 0)
  tn <- sum(pred == 0 & truth == 0); fn <- sum(pred == 0 & truth == 1)
  precision <- tp / (tp + fp)
  recall    <- tp / (tp + fn)
  tibble(
    accuracy  = (tp + tn) / length(truth),
    precision = precision,
    recall    = recall,
    f1        = 2 * precision * recall / (precision + recall),
    auc       = roc_auc(truth, prob),
    tp = tp, fp = fp, tn = tn, fn = fn
  )
}

#' Format a p-value for tables/reports.
fmt_p <- function(p) ifelse(p < 0.001, "< 0.001", sprintf("%.3f", p))

#' Threshold that maximises Youden's J (sensitivity + specificity - 1).
#'
#' The default 0.5 cutoff is useless on an imbalanced target: the model simply
#' predicts the majority class for almost everyone. The threshold is chosen on
#' the training set and then applied to the held-out set.
best_threshold <- function(truth, prob) {
  grid <- seq(0.05, 0.95, by = 0.01)
  j <- map_dbl(grid, function(t) {
    pred <- as.integer(prob >= t)
    tpr <- sum(pred == 1 & truth == 1) / sum(truth == 1)
    fpr <- sum(pred == 1 & truth == 0) / sum(truth == 0)
    tpr - fpr
  })
  grid[which.max(j)]
}
