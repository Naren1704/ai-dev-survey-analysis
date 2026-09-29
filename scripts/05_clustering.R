# ---------------------------------------------------------------------------
# 05 - Unsupervised learning: developer personas
#
# PCA on the numeric AI-attitude and experience variables, then k-means on the
# scaled features. k is chosen with an elbow plot and an average silhouette
# width (both computed in base R / the recommended `cluster` package).
#
#   Rscript scripts/05_clustering.R
# ---------------------------------------------------------------------------

source("R/setup.R")
suppressPackageStartupMessages(library(cluster))

d <- readRDS(PATHS$clean_rds)
set.seed(SEED)

# --- Feature matrix --------------------------------------------------------
feat <- d %>%
  select(ResponseId, years_code, age_numeric, job_sat, n_languages,
         ai_sent_score, ai_trust_score, ai_daily, ai_agent_user,
         ai_complex_ok) %>%
  drop_na()

message("clustering rows: ", nrow(feat))

X <- feat %>% select(-ResponseId) %>% scale()

# --- PCA -------------------------------------------------------------------
pca <- prcomp(X, center = FALSE, scale. = FALSE)   # X is already standardised
var_explained <- pca$sdev^2 / sum(pca$sdev^2)

scree <- tibble(pc = seq_along(var_explained),
                var = var_explained,
                cum = cumsum(var_explained)) %>%
  slice_head(n = 9)
save_table(scree %>% mutate(across(where(is.numeric), ~ round(.x, 4))),
           "15_pca_variance")

scree_plot <- scree %>%
  ggplot(aes(x = factor(pc), y = var)) +
  geom_col(fill = PALETTE[1]) +
  geom_line(aes(y = cum, group = 1), colour = PALETTE[2], linewidth = 0.8) +
  geom_point(aes(y = cum), colour = PALETTE[2]) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "PCA: variance explained",
       subtitle = "Bars: individual components. Line: cumulative.",
       x = "Principal component", y = "Variance explained")
save_fig(scree_plot, "17_pca_scree")

loadings <- as_tibble(pca$rotation[, 1:3], rownames = "variable") %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
save_table(loadings, "16_pca_loadings")
cat("\nPCA loadings (first three components):\n")
print(loadings)

# --- Choosing k ------------------------------------------------------------
ks <- 2:8
# A sample keeps the silhouette computation (O(n^2)) tractable.
sil_sample <- sample(nrow(X), min(2000, nrow(X)))
dist_sample <- dist(X[sil_sample, ])

k_search <- map_dfr(ks, function(k) {
  km <- kmeans(X, centers = k, nstart = 25, iter.max = 50)
  sil <- silhouette(km$cluster[sil_sample], dist_sample)
  tibble(k = k,
         wss = km$tot.withinss,
         silhouette = mean(sil[, "sil_width"]))
})
save_table(k_search %>% mutate(across(where(is.numeric), ~ round(.x, 4))),
           "17_kmeans_k_search")
print(k_search)

elbow_plot <- k_search %>%
  ggplot(aes(k, wss)) +
  geom_line(colour = PALETTE[1]) + geom_point(colour = PALETTE[1], size = 2) +
  scale_x_continuous(breaks = ks) +
  labs(title = "Elbow plot", subtitle = "Total within-cluster sum of squares",
       x = "Number of clusters (k)", y = "Within-cluster SS")
save_fig(elbow_plot, "18_kmeans_elbow", width = 6.5, height = 4.5)

sil_plot <- k_search %>%
  ggplot(aes(k, silhouette)) +
  geom_line(colour = PALETTE[2]) + geom_point(colour = PALETTE[2], size = 2) +
  scale_x_continuous(breaks = ks) +
  labs(title = "Average silhouette width",
       subtitle = "Higher is better; computed on a 2,000-row sample",
       x = "Number of clusters (k)", y = "Average silhouette width")
save_fig(sil_plot, "19_kmeans_silhouette", width = 6.5, height = 4.5)

best_k <- k_search$k[which.max(k_search$silhouette)]
message("k chosen by silhouette: ", best_k)

# --- Final clustering ------------------------------------------------------
km <- kmeans(X, centers = best_k, nstart = 50, iter.max = 100)
feat$cluster <- factor(km$cluster)

# Cluster profiles in the original units, which is what makes them readable.
profiles <- feat %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    across(c(years_code, age_numeric, job_sat, n_languages, ai_sent_score,
             ai_trust_score, ai_daily, ai_agent_user, ai_complex_ok),
           ~ round(mean(.x), 2)),
    .groups = "drop"
  ) %>%
  mutate(share = round(100 * n / sum(n), 1), .after = n)

save_table(profiles, "18_cluster_profiles")
cat("\nCluster profiles (means in original units):\n")
print(profiles, width = Inf)

# Standardised profile heatmap: which features define each cluster.
centre_plot <- as_tibble(km$centers) %>%
  mutate(cluster = factor(row_number())) %>%
  pivot_longer(-cluster, names_to = "feature", values_to = "z") %>%
  ggplot(aes(x = cluster, y = feature, fill = z)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = sprintf("%.2f", z)), size = 3) +
  scale_fill_gradient2(low = PALETTE[4], mid = "white", high = PALETTE[1],
                       midpoint = 0) +
  labs(title = paste0("Developer personas: k-means with k = ", best_k),
       subtitle = "Cluster centres in standard deviations from the mean",
       x = "Cluster", y = NULL, fill = "z-score")
save_fig(centre_plot, "20_cluster_centres", height = 5.5)

# Clusters projected on the first two principal components.
pca_plot <- tibble(pc1 = pca$x[, 1], pc2 = pca$x[, 2], cluster = feat$cluster) %>%
  slice_sample(n = min(6000, nrow(feat))) %>%
  ggplot(aes(pc1, pc2, colour = cluster)) +
  geom_point(alpha = 0.35, size = 1) +
  scale_colour_manual(values = PALETTE) +
  labs(title = "Clusters in principal component space",
       subtitle = paste0("PC1 and PC2 explain ",
                         round(100 * sum(var_explained[1:2]), 1),
                         "% of total variance (sampled points)"),
       x = paste0("PC1 (", round(100 * var_explained[1], 1), "%)"),
       y = paste0("PC2 (", round(100 * var_explained[2], 1), "%)"),
       colour = "Cluster")
save_fig(pca_plot, "21_cluster_pca", width = 7, height = 5.5)

# --- Do the personas differ on variables not used to build them? -----------
# A sanity check: salary and region were excluded from the feature matrix.
validation <- d %>%
  inner_join(feat %>% select(ResponseId, cluster), by = "ResponseId") %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    median_salary = round(median(comp_model, na.rm = TRUE)),
    pct_remote = round(100 * mean(remote_work == "Remote", na.rm = TRUE), 1),
    pct_large_org = round(100 * mean(org_size == "Large (1000+)", na.rm = TRUE), 1),
    pct_ai_threat_yes = round(100 * mean(ai_threat == "Yes", na.rm = TRUE), 1),
    .groups = "drop"
  )
save_table(validation, "19_cluster_validation")
cat("\nCluster validation on held-out variables:\n")
print(validation, width = Inf)

kw_salary <- kruskal.test(comp_model ~ cluster,
                          data = d %>% inner_join(feat %>% select(ResponseId, cluster),
                                                  by = "ResponseId"))
cat("\nKruskal-Wallis, salary across clusters: p =",
    format.pval(kw_salary$p.value, digits = 3), "\n")

# Cluster assignment saved so the report and any follow-up share the labels.
saveRDS(list(kmeans = km, pca = pca, best_k = best_k,
             assignments = feat %>% select(ResponseId, cluster),
             profiles = profiles),
        file.path(PATHS$models, "clustering.rds"))

message("Clustering complete.")
