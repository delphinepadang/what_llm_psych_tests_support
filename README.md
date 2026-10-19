# Data and code: Do large language models respond like human participants?

This repository contains the data, analysis code and figures for the manuscript

> **Do large language models respond like human participants? Evidence from social attitude and mental-state measures in two languages.**
> [Authors]. Submitted to *Humanities and Social Sciences Communications*, Collection "Large language models in psychological science".

Twenty-seven instruction-tuned large language models (LLMs) answered three attitude questionnaires:
- the short form of the SDO₇ scale, SDO₇(s);
- SDO₆, in English and in Chinese;
- the Very Short Authoritarianism scale (VSA).

Twenty-one of them also took the revised Reading the Mind in the Eyes Test (RMET), with and without definitions of the response words. Every item was presented five times in separate requests without conversation history.

The analyses ask three things:
- whether the models' scores resemble human data;
- whether they differ between English and Chinese;
- whether individual answers and the conclusions drawn from them repeat across identical administrations.

## Repository structure

```
Results/                     Raw response data, one row per API call
Analysis_script/             Analysis code, run in numerical order
Analysis_output/             Statistical outputs
  ├── stats_log.txt          Full log of every statistic, section by section (script 02)
  ├── Stats_README.md        Index of the tables in Stats_tables/
  ├── Stats_tables/          Every table in the log as CSV (S01–S19)
  ├── Reframing_checks/      Summary statistics vs answer changes (R1–R6, script 04)
  ├── Additional_checks/     Two-run disagreement and language sensitivity (script 05)
  ├── Report_outputs/        Descriptives and the earlier-vs-later release comparison (script 06)
  ├── Figures/               Analysis and supplementary figures (scripts 03 and 04)
  └── analysis_results.rds   Objects read by 03_figures.R
Manuscript_figures/          Figs 1–4 of the manuscript, PNG (600 dpi) and PDF (script 08)
  └── source_data/           The numbers plotted in each figure panel, as CSV
Notebooks/                   Data-collection notebooks
requirements.txt             Python packages for scripts 06 and 08
```

## Data (`Results/`)

| File | Contents | Used for |
|---|---|---|
| `SDO_VSA_Results_20260830.csv` | Attitude questionnaires, 30 August 2026: 28 endpoints × 46 items (SDO₇(s) 8, SDO₆ English 16, SDO₆ Chinese 16, VSA 6) × 5 runs | Main analyses |
| `RMET_Results_20260912.csv` | RMET, 11–12 September 2026: 22 endpoints × 36 items × 2 conditions × 5 runs | Main analyses |
| `SDO_VSA_Results.csv` | SDO₇(s) and VSA, 14 June 2026 (24 endpoints, 5 runs) | Supplementary comparison over time only |
| `RMET_Results.csv` | RMET, 14 June 2026 (18 endpoints, 1 run per condition) | Supplementary comparison over time only |

Claude Sonnet 4 returned an API error on every call in both main collections. It is excluded in the analysis code, leaving 27 attitude endpoints and 21 RMET endpoints.

**Columns, attitude files:**
- `question_id`, `question_text`: item identifier and wording as presented.
- `instrument` (`SDO` = SDO₇(s), `SDO6`, `VSA`) and `language` (`en`, `zh`).
- `item_no`.
- `model_label`: access route and serving name, e.g. `azure-gpt-4.1`.
- `repeat`: run 0–4.
- `value`: the numeric answer.
- `label`: the label the model returned.
- `raw_response`: the full model output.
- `parse_error`, `timestamp`.

**Columns, RMET files:**
- `question_id`.
- `image`: stimulus file name only; the photographs are not included.
- `options`, `correct_answer`.
- `definitions_shown`: condition.
- `model_label`, `repeat`.
- `choice`, `is_correct`.
- `raw_response`, `parse_error`, `timestamp`.

> **Note on keying.** In the attitude files, the `reverse_scored` and `value_recoded` columns are correct for SDO₆ only. The analysis code applies the published keys itself:
> - SDO₇(s) items 3, 4, 7 and 8 are reverse-keyed (8 − x).
> - SDO₆ items 9–16 are reverse-keyed (8 − x).
> - VSA items 1, 4 and 5 are reverse-keyed (sign reversal).

## Code (`Analysis_script/`)

| Script | What it does | Writes to |
|---|---|---|
| `02_statistical_analysis.R` | All statistical tests | `Analysis_output/stats_log.txt`, `Stats_tables/`, `analysis_results.rds` |
| `03_figures.R` | Analysis and supplementary figures | `Analysis_output/Figures/` |
| `04_reframing_checks.R` | Summary statistics (ICC, Krippendorff's α, coefficient of variation) compared with answer changes; single-run conclusions (R1–R6) | `Analysis_output/Reframing_checks/`, `Figures/FigR1_*`, `FigR2_*` |
| `05_two_run_disagreement_and_language_sensitivity.py` | Two-run disagreement; the language shift without Llama 3.1 8B | `Analysis_output/Additional_checks/` |
| `06_report_descriptives.py` | Descriptive tables; comparison of earlier and later releases from the same developers | `Analysis_output/Report_outputs/` |
| `08_manuscript_figures.py` | **Figs 1–4 of the manuscript** and their source data | `Manuscript_figures/` |

## Reproducing the analyses

**Requirements:**
- R 4.6.0, with these packages:
  - dplyr, tidyr, readr, purrr, stringr, tibble, forcats;
  - lme4, lmerTest, afex, ordinal, metafor, psych, pwr, performance, cluster;
  - ggplot2, ggrepel, patchwork, ragg, systemfonts.
- Python 3.8 or later, with the packages in `requirements.txt`. Script 05 needs only the standard library.

The R package versions used are listed under SESSION INFORMATION at the end of `Analysis_output/stats_log.txt`.

Run from the repository root, in order:

```r
source("Analysis_script/02_statistical_analysis.R")   # statistics
source("Analysis_script/03_figures.R")                # analysis figures
source("Analysis_script/04_reframing_checks.R")       # R1–R6 checks (needed by script 08)
```
```bash
pip install -r requirements.txt
python3 Analysis_script/05_two_run_disagreement_and_language_sensitivity.py
python3 Analysis_script/06_report_descriptives.py
python3 Analysis_script/08_manuscript_figures.py        # Figs 1–4 -> Manuscript_figures/
```

Script 08 needs only the two main data files and `Analysis_output/Reframing_checks/R3_agreement_by_model.csv`, which is included, so it can be run on its own. It prints the numbers quoted in the text next to each figure, so they can be checked.

**Notes on reproducibility:**
- **Seed.** Random procedures use seed 20260922, with 2,000 bootstrap and 5,000 permutation resamples.
- **Ties.** Values that enter ties (ranks, signs, thresholds) are rounded to 10 decimal places, so results do not depend on the computer's floating-point precision.
- **Wilcoxon tests.** R 4.6.0 handles zero differences in Wilcoxon signed-rank tests differently from earlier R versions, so a few Wilcoxon p-values can differ under other versions. The reported values come from R 4.6.0.
- **Cross-check.** Script 04 cross-checks its headline numbers against the outputs of script 02 (R1).

## Where each manuscript result comes from

| Manuscript | Source in this repository |
|---|---|
| Fig. 1 (study design) | `Manuscript_figures/Fig1_study_design` |
| Fig. 2 (English vs Chinese SDO₆) | `Manuscript_figures/Fig2_language`; data in `source_data/Fig2a_*.csv`, `Fig2b_*.csv` |
| Fig. 3a (agreement vs items changed) | `Manuscript_figures/Fig3_answer_changes`; per-model α from `Reframing_checks/R3_agreement_by_model.csv` |
| Fig. 3b (changes by first answer) | `Manuscript_figures/Fig3_answer_changes`; `source_data/Fig3b_change_by_first_answer.csv`. Changes are counted only in runs 2–5, so the grouping by run-1 answer cannot create them through regression to the mean; the all-five-runs version is in the same file. |
| Fig. 4 (RMET accuracy vs consistency) | `Manuscript_figures/Fig4_rmet`; `source_data/Fig4_rmet_by_model.csv` |
| Table 1 (what each level of claim supported) | `Additional_checks/two_run_disagreement_summary.csv`; `Reframing_checks/R4a_classification_summary.csv`, `R4b_rank_summary.csv`, `R4c_*.csv`; `Stats_tables/S02_rmet_vs_human.csv` |
| Attitude scores and human comparison | `Report_outputs/table2_attitude_by_instrument.csv`; `Stats_tables/S13_language_descriptives.csv` |
| Answers changed (20.7%) and two-run disagreement (11.4%) | `Additional_checks/two_run_disagreement_summary.csv` |
| ICCs, α ≥ .80, coefficient of variation | `Reframing_checks/R6_table2_summary_vs_change.csv`, `R3_agreement_summary.csv`, `R2_cv_summary.csv` |
| Conclusions from single runs | `Reframing_checks/R4a_*.csv`, `R4b_*.csv`, `R4c_*.csv` |
| Language analyses | `Stats_tables/S13_*.csv`; `Additional_checks/language_shift_sensitivity.csv`, `chinese_label_check.csv` |
| RMET accuracy, definitions, answer changes, item criteria | `Stats_tables/S02_*.csv`, `S03_*.csv`, `S04_*.csv`, `S05_rmet_items.csv` |
| Earlier vs later releases (Supplementary Results) | `Report_outputs/generation_comparison.csv` |
| Test registry with multiplicity adjustment | `Stats_tables/S19_test_registry.csv`, `S19_registry_summary.csv` |

## Data collection (`Notebooks/`)

| Notebook | Collection |
|---|---|
| `LLM Benchmarks with Chinese.ipynb` | Attitude questionnaires, August 2026 (main data): SDO₇(s), SDO₆ in English and Chinese, and VSA, with parallel requests |
| `LLM Benchmarks.ipynb` | Attitude questionnaires, June 2026 |
| `LLM Benchmarks - Images.ipynb` | RMET, June 2026. The code run for the September collection was not archived; the same procedure and settings are assumed. |

The notebooks show how prompts were built, how requests were sent and how responses were parsed. API keys and internal storage paths have been removed. No temperature, top-p or seed was set, so endpoints ran at provider defaults. Text-only endpoints were excluded from the RMET.

## Materials not included

- **RMET photographs.** The revised RMET photographs (Baron-Cohen et al., 2001) are not redistributed; the data files contain stimulus file names only.
- **Questionnaire sources.** The questionnaire items are reproduced in the data files as presented to the models:
  - English items: Pratto et al. (1994), Ho et al. (2015) and Bizumic and Duckitt (2018).
  - Chinese SDO₆ items: Lin (2017). The favour–oppose response labels were translated for this study.

## Licence and citation

[Choose a licence, for example CC BY 4.0 for the data and MIT for the code, and add a LICENSE file.]

Please cite the article once it is published. [Add citation or DOI.]
