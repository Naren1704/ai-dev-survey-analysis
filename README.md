# Do developers trust AI? Adoption, attitudes and pay

A complex data analysis in **R** of the
[Stack Overflow Annual Developer Survey 2025](https://survey.stackoverflow.co/)
— 49,191 raw responses, 172 columns, 44,682 professional developers after
cleaning, from 175 countries.

The project covers the full analysis pipeline: cleaning and feature
engineering, exploratory analysis, hypothesis testing with effect sizes,
regression with diagnostics, supervised machine learning with cross-validation,
and unsupervised clustering — ending in a rendered R Markdown report.

**Report:** [`reports/final_report.html`](reports/final_report.html)
(open it in a browser after cloning).

---

## Research questions

| # | Question | Method |
|---|----------|--------|
| RQ1 | Does AI tool adoption depend on developer role and experience? | Chi-square test, logistic regression |
| RQ2 | Do AI users earn more than non-users? | Welch t-test, Wilcoxon, linear regression |
| RQ3 | Does trust in AI accuracy change with experience? | Chi-square, test for trend in proportions |
| RQ4 | Can AI adoption be predicted from professional attributes? | Six-model benchmark: logistic regression, random forest, XGBoost, LightGBM, MLP, ensemble; ROC/AUC, DeLong and McNemar tests |
| RQ5 | Are there distinct developer "personas" in AI attitudes? | k-means, silhouette, resampling stability |
| RQ6 | What predicts *how much* someone trusts AI, on the full five-point scale? | Ordinal (proportional-odds) logistic regression |

## Key findings

1. **Adoption is near-universal; trust is not.** 79.4% of professional
   developers use AI tools and 48.1% use them daily, but only **32.4% trust
   the accuracy** of what those tools produce.
2. **Adoption and trust both fall with experience.** Adoption drops from
   **88.5%** (0–2 years of experience) to **72.7%** (20+ years); the chi-square
   test for trend is significant at p < 0.001 (Cramer's V = 0.124 for trust).
3. **AI users earn slightly *less*, not more** — median $76,097 vs $81,685
   (Welch t on log salary, p < 0.001, **Cohen's d = −0.16**, a small effect).
   The direction is the opposite of the popular assumption, and the gap is not
   present once experience, region and the other covariates are controlled.
4. **Pay is driven by geography, experience and education.** The linear model
   on log(salary) reaches **R² = 0.443**; a doctorate is worth about +49% over
   no degree (Tukey HSD, p < 0.001), while AI use adds almost nothing.
5. **Adoption is hard to predict.** Across six benchmarked models (logistic
   regression, random forest, XGBoost, LightGBM, MLP, soft-vote ensemble) the
   best held-out **AUC is 0.666** (XGBoost; logistic regression 0.656). After
   Holm correction no model is reliably better than logistic regression, and
   MCC is below 0.19 for every model. Adoption is mostly not explained by the
   ten professional attributes available.
6. **Trust is driven by usage, and eroded by experience.** An ordinal
   proportional-odds model on the full five-point trust scale (n = 19,112)
   puts daily users at **11.2× the odds** (95% CI 10.3–12.1) of a higher trust
   rating than respondents who do not currently use AI tools, while each extra
   year of coding multiplies those odds by **0.969** — about 27% lower odds
   across a decade. Cause may run both ways; the model cannot separate them.
7. **The complaint is precision, not capability.** 67% name *"AI solutions that
   are almost right, but not quite"* as their main frustration and 46% say
   debugging AI-generated code costs more time than it saves. Asked which
   skills survive better AI, the free-text answers converge on understanding,
   problem solving, architecture and debugging — judgement, not typing.
8. **Two camps, fuzzy border, stable line.** k-means splits developers into an
   **AI-embracing** cluster (54.5%, 87% daily users, mean trust 3.5/5) and a
   **sceptical** cluster (45.5%, 24% daily users, mean trust 1.95/5). Silhouette
   width is only 0.184 — the groups are a continuum, not separate blobs — but
   the partition reproduces at **adjusted Rand index 0.998** across 25 runs on
   80% subsamples. Sceptics earn more (median $83,668 vs $73,686,
   Kruskal-Wallis p < 2e-16) on a variable the clustering never saw.

### Robustness

Results were attacked before they were reported:

- **Heteroskedasticity.** Breusch-Pagan rejects constant variance
  (BP = 1043.6, p < 0.001), so every salary conclusion is stated against HC3
  sandwich standard errors — on average 1.13× the classical ones.
- **Missing salaries.** Only 52.1% report pay, and response rate varies by
  region (χ², p < 0.001, Cramer's V = 0.168). Refitting with inverse-probability
  weights moves coefficients by a **median of 0.0036 log points** — the
  complete-case results hold.
- **Proportional odds.** The ordinal model's assumption was checked by
  refitting a binary logit at each of the four cut-points; 10 of 46
  coefficients vary more than their own size (four sparse role categories and
  six near-zero terms), while usage frequency, years of coding, age and the
  main region contrasts are stable.
- **Cluster reality.** Silhouette says weak separation, adjusted Rand says high
  stability. Both are reported rather than the flattering one.

| | |
|---|---|
| ![AI usage](outputs/figures/04_ai_usage.png) | ![Adoption by experience](outputs/figures/05_adoption_by_experience.png) |
| ![Trust by experience](outputs/figures/06_trust_by_experience.png) | ![Cluster centres](outputs/figures/20_cluster_centres.png) |

## How to run

Requires **R ≥ 4.3**. From the project root:

```bash
Rscript scripts/00_setup.R     # installs packages, downloads the data (~141 MB)
Rscript scripts/run_all.R      # runs every step and renders the report
```

Or step by step:

```bash
Rscript scripts/01_data_cleaning.R      # raw CSV  -> data/processed/survey_clean.rds
Rscript scripts/02_eda.R                # 12 figures, 8 summary tables
Rscript scripts/03_statistical_tests.R  # 8 hypothesis tests, Holm-adjusted
Rscript scripts/04_modeling.R           # lm + robust SE + IPW, glm vs RF, ordinal model
Rscript scripts/05_clustering.R         # k-means + resampling stability (PCA for display)
Rscript scripts/06_paper_benchmark.R    # six-model benchmark -> paper/tables, paper/figures
```

Scripts 02 to 06 each read `data/processed/survey_clean.rds` written by
script 01, so any of them can be re-run on its own. `SEED = 42` is set in `R/setup.R`, so splits, folds
and cluster assignments reproduce exactly.

## Repository layout

```
ai-dev-survey-analysis/
├── R/                        # reusable functions, sourced by the scripts
│   ├── setup.R               #   packages, paths, plot theme, seed
│   ├── cleaning.R            #   recoding, feature engineering, outlier rules
│   └── stats.R               #   effect sizes, ROC-AUC, adjusted Rand, thresholds
├── scripts/                  # the analysis pipeline, run in order
│   ├── 00_setup.R            #   install packages + download raw data
│   ├── 01_data_cleaning.R
│   ├── 02_eda.R
│   ├── 03_statistical_tests.R
│   ├── 04_modeling.R
│   ├── 05_clustering.R
│   ├── 06_paper_benchmark.R  #   six-model benchmark, writes paper/ tables + figures
│   └── run_all.R
├── data/
│   ├── raw/                  # downloaded CSVs (not committed, see its README)
│   ├── processed/            # survey_clean.rds / .csv (regenerated)
│   └── sample/               # 500-row sample of the cleaned data (committed)
├── outputs/
│   ├── figures/              # 24 PNGs
│   ├── tables/               # 29 CSV result tables
│   └── models/               # fitted model objects (not committed)
├── paper/
│   ├── main.tex              # IEEE-format paper (compile on Overleaf or with pdflatex)
│   ├── figures/              # written by scripts/06_paper_benchmark.R
│   └── tables/               # LaTeX tables, macros.tex and CSVs written by the same script
└── reports/
    ├── final_report.Rmd
    └── final_report.html
```

## Methods in detail

**Cleaning** (`01_data_cleaning.R`). 172 raw columns are narrowed to 26, and
feature engineering yields the 38 analysis columns. `YearsCode` is text (because of "Less than 1 year" and
"More than 50 years") and becomes numeric; `Age` buckets become midpoints;
education (8 levels), organisation size (9) and country (175) collapse to 4, 3
and 7 categories so that tests have adequate cell counts. Salaries below
$1,000 or above the 99th percentile are **flagged, not deleted**, and modelling
uses `log(salary)` because the distribution is log-normal. Semicolon-separated
multi-select columns are counted for breadth (`n_languages`, `n_ai_models`) and
split into one row per answer with `separate_rows()` for the frequency charts.
The sample is restricted to respondents who are developers by profession or who
write code as part of their work (44,682 of 49,191).

**Inference** (`03_statistical_tests.R`). Eight tests addressing six
hypotheses: chi-square with Cramer's V, Welch t-test with Cohen's d and a
Wilcoxon robustness check, one-way ANOVA with eta-squared plus Tukey HSD, a
chi-square test for trend, and a test of whether salary is missing at random.
Assumptions are checked where they apply (minimum expected cell count for the
role test; Shapiro-Wilk and a variance-ratio test for the salary comparison)
and all eight p-values are **Holm-adjusted** together.

**Modelling** (`04_modeling.R`). Four models:

- *A.* Linear model on log(salary) with residual, Q-Q and VIF diagnostics.
- *A2.* The same model with HC3 sandwich standard errors, because Breusch-Pagan
  rejects constant variance.
- *A3.* Inverse-probability weighting for salary non-response, as a sensitivity
  check on the complete-case fit.
- *B/C.* Logistic regression against a random forest for predicting AI
  adoption, compared on a stratified 75/25 split *and* 5-fold cross-validated
  AUC. Because 81% of respondents are AI users, the decision threshold is tuned
  on the training set with Youden's J instead of being left at 0.5 — otherwise
  both models simply predict the majority class.
- *E.* (`06_paper_benchmark.R`) The same split and predictors extended to six
  models: logistic regression, random forest, XGBoost, LightGBM, an MLP
  (`nnet`) and a soft-vote ensemble. Thresholds come from out-of-fold training
  predictions (Youden's J); models are compared with DeLong's test on AUC
  (Holm-adjusted) and McNemar's test on errors. Results are written to `paper/`.
- *D.* Ordinal (proportional-odds) logistic regression on the full five-point
  trust scale, with the proportional-odds assumption checked by refitting a
  binary logit at each cut-point.

**Clustering** (`05_clustering.R`). k-means on nine standardised AI-attitude
and experience features, with k chosen by average silhouette width (computed on
a 2,000-row subsample) and an elbow plot as a cross-check; the final fit uses
50 random starts. PCA is used only for the scree plot and the 2-D view of the
clusters, not as input to k-means. Two validity checks follow: 25 runs on 80%
subsamples scored by adjusted Rand index against the full solution, and a
comparison on median salary, remote work, large-organisation share and the
AI-threat answer — variables deliberately **excluded** from the clustering
features.

## Limitations

* Self-selected sample of survey respondents; about 73% of those with a known
  region are in Europe or North America — this does not generalise to all
  developers.
* Cross-sectional data: every result is an association, never a causal effect.
  AI users earning less is a property of who adopts AI, not an effect of AI.
* `ConvertedCompYearly` is self-reported and exchange-rate dependent.
* Collapsing 175 countries into 7 regions loses real variation; it buys models
  that fit and tests with adequate cell counts.
* With n in the tens of thousands, nearly everything is statistically
  significant, so effect sizes carry the interpretation.

## Data source and licence

Stack Overflow Annual Developer Survey 2025 — <https://survey.stackoverflow.co/>,
released under **CC BY-SA 4.0**. The raw files are downloaded by
`scripts/00_setup.R` (see [`data/raw/README.md`](data/raw/README.md)) and are
not committed because the main CSV is ~141 MB.

Analysis code in this repository: MIT.
