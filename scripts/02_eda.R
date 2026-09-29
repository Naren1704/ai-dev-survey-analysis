# ---------------------------------------------------------------------------
# 02 - Exploratory data analysis
#
# Distributions, group comparisons and correlations. Writes PNGs to
# outputs/figures and CSV summaries to outputs/tables.
#
#   Rscript scripts/02_eda.R
# ---------------------------------------------------------------------------

source("R/setup.R")
source("R/cleaning.R")

d <- readRDS(PATHS$clean_rds)

# --- Overview --------------------------------------------------------------
overview <- tibble(
  metric = c("Respondents", "Countries", "Median years coding",
             "Median salary (USD, outliers removed)", "AI tool users (%)",
             "Daily AI users (%)", "Trust AI accuracy (%)"),
  value = c(
    nrow(d),
    n_distinct(d$country, na.rm = TRUE),
    median(d$years_code, na.rm = TRUE),
    median(d$comp_model, na.rm = TRUE),
    round(100 * mean(d$ai_user, na.rm = TRUE), 1),
    round(100 * mean(d$ai_daily, na.rm = TRUE), 1),
    round(100 * mean(d$ai_trusts, na.rm = TRUE), 1)
  )
)
save_table(overview, "02_overview")
print(overview)

# --- Missingness -----------------------------------------------------------
miss_plot <- d %>%
  summarise(across(everything(), ~ mean(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "pct") %>%
  filter(pct > 0) %>%
  ggplot(aes(x = pct, y = fct_reorder(variable, pct))) +
  geom_col(fill = PALETTE[1]) +
  scale_x_continuous(labels = scales::percent) +
  labs(title = "Missing values by variable",
       subtitle = "Only variables with at least one missing value",
       x = "Share missing", y = NULL,
       caption = "Stack Overflow Developer Survey 2025")
save_fig(miss_plot, "01_missingness", height = 6)

# --- Salary distribution ---------------------------------------------------
salary_plot <- d %>%
  filter(!is.na(comp_model)) %>%
  ggplot(aes(x = comp_model)) +
  geom_histogram(bins = 60, fill = PALETTE[1], colour = "white") +
  scale_x_log10(labels = scales::dollar) +
  labs(title = "Annual compensation is log-normal",
       subtitle = paste0("Outliers removed (below $1k or above the 99th percentile); n = ",
                         sum(!is.na(d$comp_model))),
       x = "Compensation (USD, log scale)", y = "Respondents",
       caption = "Stack Overflow Developer Survey 2025")
save_fig(salary_plot, "02_salary_distribution")

salary_region <- d %>%
  filter(!is.na(comp_model), !is.na(region)) %>%
  ggplot(aes(x = fct_reorder(region, comp_model, median), y = comp_model)) +
  geom_boxplot(fill = PALETTE[1], alpha = 0.7, outlier.alpha = 0.15) +
  scale_y_log10(labels = scales::dollar) +
  coord_flip() +
  labs(title = "Compensation by region", x = NULL,
       y = "Compensation (USD, log scale)",
       caption = "Stack Overflow Developer Survey 2025")
save_fig(salary_region, "03_salary_by_region")

# --- AI adoption -----------------------------------------------------------
usage_plot <- d %>%
  filter(!is.na(ai_usage)) %>%
  count(ai_usage) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(x = ai_usage, y = pct)) +
  geom_col(fill = PALETTE[2]) +
  geom_text(aes(label = scales::percent(pct, accuracy = 0.1)), vjust = -0.4) +
  scale_y_continuous(labels = scales::percent, expand = expansion(c(0, 0.1))) +
  labs(title = "How often developers use AI tools", x = NULL, y = "Share",
       caption = "Stack Overflow Developer Survey 2025")
save_fig(usage_plot, "04_ai_usage")

adoption_exp <- d %>%
  filter(!is.na(exp_bucket), !is.na(ai_user)) %>%
  group_by(exp_bucket) %>%
  summarise(adoption = mean(ai_user), n = n(), .groups = "drop") %>%
  mutate(se = sqrt(adoption * (1 - adoption) / n))
save_table(adoption_exp, "03_adoption_by_experience")

adoption_plot <- adoption_exp %>%
  ggplot(aes(x = exp_bucket, y = adoption)) +
  geom_col(fill = PALETTE[3]) +
  geom_errorbar(aes(ymin = adoption - 1.96 * se, ymax = adoption + 1.96 * se),
                width = 0.2, colour = "grey30") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "AI adoption by years of experience",
       subtitle = "Share using AI tools, with 95% confidence intervals",
       x = "Years of coding experience", y = "AI tool users",
       caption = "Stack Overflow Developer Survey 2025")
save_fig(adoption_plot, "05_adoption_by_experience")

# --- Trust -----------------------------------------------------------------
trust_exp <- d %>%
  filter(!is.na(ai_trust), !is.na(exp_bucket)) %>%
  count(exp_bucket, ai_trust) %>%
  group_by(exp_bucket) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()
save_table(trust_exp, "04_trust_by_experience")

trust_plot <- trust_exp %>%
  ggplot(aes(x = exp_bucket, y = pct, fill = ai_trust)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  # A diverging red-to-green scale, so the ordinal meaning is readable without
  # consulting the legend.
  scale_fill_brewer(palette = "RdYlGn", name = NULL) +
  labs(title = "Trust in AI accuracy by experience",
       x = "Years of coding experience", y = "Share of respondents",
       caption = "Stack Overflow Developer Survey 2025") +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE, reverse = TRUE))
save_fig(trust_plot, "06_trust_by_experience")

# Sentiment vs trust: the two AI attitude scales are related but not identical.
sent_trust <- d %>%
  filter(!is.na(ai_sentiment), !is.na(ai_trust)) %>%
  count(ai_sentiment, ai_trust) %>%
  group_by(ai_sentiment) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  ggplot(aes(x = ai_sentiment, y = ai_trust, fill = pct)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = scales::percent(pct, accuracy = 1)), size = 3) +
  scale_fill_gradient(low = "white", high = PALETTE[1], labels = scales::percent) +
  labs(title = "Sentiment towards AI vs trust in its accuracy",
       subtitle = "Row percentages within each sentiment level",
       x = "Sentiment", y = "Trust in accuracy", fill = "Share",
       caption = "Stack Overflow Developer Survey 2025") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
save_fig(sent_trust, "07_sentiment_vs_trust", height = 6)

# --- Correlations ----------------------------------------------------------
num_vars <- d %>%
  select(years_code, work_exp, age_numeric, log_comp, job_sat,
         ai_sent_score, ai_trust_score, n_languages, n_ai_models)

cor_mat <- cor(num_vars, use = "pairwise.complete.obs")
save_table(as_tibble(cor_mat, rownames = "variable"), "05_correlations")

cor_plot <- as_tibble(cor_mat, rownames = "var1") %>%
  pivot_longer(-var1, names_to = "var2", values_to = "r") %>%
  ggplot(aes(x = var1, y = var2, fill = r)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = sprintf("%.2f", r)), size = 2.8) +
  scale_fill_gradient2(low = PALETTE[4], mid = "white", high = PALETTE[1],
                       midpoint = 0, limits = c(-1, 1)) +
  labs(title = "Correlation between numeric variables",
       subtitle = "Pearson r, pairwise complete observations",
       x = NULL, y = NULL,
       caption = "Stack Overflow Developer Survey 2025") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))
save_fig(cor_plot, "08_correlation_heatmap", height = 6.5)

# --- Multi-select: languages and AI models ---------------------------------
top_languages <- split_multiselect(d, languages) %>%
  count(languages, sort = TRUE) %>%
  slice_head(n = 15)
save_table(top_languages, "06_top_languages")

lang_plot <- top_languages %>%
  ggplot(aes(x = n, y = fct_reorder(languages, n))) +
  geom_col(fill = PALETTE[1]) +
  labs(title = "Most used programming languages", x = "Respondents", y = NULL,
       caption = "Multi-select question; Stack Overflow Developer Survey 2025")
save_fig(lang_plot, "09_top_languages")

top_models <- split_multiselect(d, ai_models) %>%
  count(ai_models, sort = TRUE) %>%
  slice_head(n = 15)
save_table(top_models, "07_top_ai_models")

model_plot <- top_models %>%
  ggplot(aes(x = n, y = fct_reorder(ai_models, n))) +
  geom_col(fill = PALETTE[2]) +
  labs(title = "Most used AI models", x = "Respondents", y = NULL,
       caption = "Multi-select question; Stack Overflow Developer Survey 2025")
save_fig(model_plot, "10_top_ai_models")

# --- What frustrates developers about AI tools -----------------------------
# AIFrustration is a "select all that apply" question, so it needs splitting
# before it can be counted.
frustrations <- split_multiselect(d, ai_frustration) %>%
  count(ai_frustration, sort = TRUE) %>%
  mutate(pct = n / sum(!is.na(d$ai_frustration)))
save_table(frustrations, "20_ai_frustrations")

frustration_plot <- frustrations %>%
  slice_head(n = 12) %>%
  ggplot(aes(x = pct, y = fct_reorder(str_wrap(ai_frustration, 45), pct))) +
  geom_col(fill = PALETTE[4]) +
  scale_x_continuous(labels = scales::percent) +
  labs(title = "What frustrates developers about AI tools",
       subtitle = "Share of respondents who answered the question (select all that apply)",
       x = "Share of respondents", y = NULL,
       caption = "Stack Overflow Developer Survey 2025")
save_fig(frustration_plot, "10b_ai_frustrations", height = 6)

# --- Free text: which skills survive better AI? ----------------------------
# AIOpen asks: "Looking ahead 3-5 years, what skills do you believe will remain
# valuable for developers even as AI tools become more capable?"
# A deliberately simple bag-of-words count (no extra text-mining dependency):
# split on non-letters, drop stop words, count.
STOPWORDS <- c(
  "the", "and", "that", "for", "with", "you", "are", "but", "not", "its",
  "this", "have", "has", "was", "were", "they", "them", "their", "from",
  "what", "when", "which", "would", "could", "should", "there", "here",
  "than", "then", "too", "very", "just", "get", "got", "can", "cant",
  "dont", "doesnt", "also", "into", "about", "out", "all", "any", "use",
  "using", "used", "like", "more", "most", "some", "much", "many", "our",
  "your", "his", "her", "who", "how", "why", "one", "two", "will",
  "because", "been", "being", "does", "did", "had", "she", "him", "yes",
  "only", "other", "over", "such", "time", "make", "makes", "need",
  "needs", "even", "still", "way", "lot", "really", "actually"
)

comments <- d %>%
  filter(!is.na(ai_open), str_length(ai_open) > 20) %>%
  pull(ai_open)

ai_words <- comments %>%
  str_to_lower() %>%
  str_split("[^a-z]+") %>%
  unlist() %>%
  keep(~ str_length(.x) > 2 && !(.x %in% STOPWORDS)) %>%
  tibble(word = .) %>%
  count(word, sort = TRUE) %>%
  slice_head(n = 25)
save_table(ai_words, "08_ai_open_top_words")

words_plot <- ai_words %>%
  ggplot(aes(x = n, y = fct_reorder(word, n))) +
  geom_col(fill = PALETTE[6]) +
  labs(title = "Skills developers expect to stay valuable as AI improves",
       subtitle = paste0("Most frequent words in the free-text answers; n = ",
                         length(comments), " responses"),
       x = "Occurrences", y = NULL,
       caption = "Stop words removed; Stack Overflow Developer Survey 2025")
save_fig(words_plot, "11_ai_open_words", height = 6.5)

message("EDA complete.")
