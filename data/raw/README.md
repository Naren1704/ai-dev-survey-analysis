# Raw data

The raw survey files are **not committed** (the main CSV is ~141 MB, above
GitHub's file limit). Download them with:

```r
Rscript scripts/00_setup.R
```

which writes:

| File | Size | Description |
|------|------|-------------|
| `survey_results_public.csv` | ~141 MB | One row per respondent, 172 columns |
| `survey_results_schema.csv` | ~90 KB | Question text for every column |

Source: Stack Overflow Annual Developer Survey 2025 —
<https://survey.stackoverflow.co/> (mirrored in the official
[StackExchange/Survey](https://github.com/StackExchange/Survey) repository).

Licence: CC BY-SA 4.0.
