# Data and code: repeatability of LLM responses to psychological questionnaires

This repository contains the data, analysis code and analysis outputs for the manuscript

> *[Title as submitted]* — [Authors]. Submitted to *Humanities and Social Sciences Communications*, Collection "Large language models in psychological science".

Twenty-seven instruction-tuned large language models (LLMs) answered three attitude questionnaires:
- the short form of the SDO₇ scale;
- SDO₆, in English and in Chinese;
- the Very Short Authoritarianism scale (VSA).

Each item was presented five times in separate requests without conversation history. Twenty-one models also completed the revised Reading the Mind in the Eyes Test (RMET) five times, with and without definitions of the response options. The analyses compare the summary statistics used to argue that LLM answers are stable (intraclass correlations, Krippendorff's α, the coefficient of variation) with how often individual answers change between identical requests.

## Repository structure

```
Results/                      Raw response data (one row per API call)
Analysis_script/              Analysis code (run in numerical order)
Analysis_output/              Outputs used in the manuscript
  ├── stats_log.txt           Full log of every statistic, section by section
  ├── Stats_README.md         Index of the tables in Stats_tables/
  ├── Stats_tables/           Every table in the log as CSV (S01–S19)
  ├── Reframing_checks/       Summary-statistic checks (R1–R6) and their log
  ├── Additional_checks/      Two-run disagreement and language sensitivity
  ├── Figures/                All figures (PDF and PNG)
  └── analysis_results.rds    Objects read by 03_figures.R
Notebooks/                    Data-collection notebooks (June 2026 versions; see below)
```

## Data (`Results/`)

| File | Contents | Used for |
|---|---|---|
| `SDO_VSA_Results_20260830.csv` | Attitude questionnaires, 30 August 2026: 28 endpoints × 46 items (SDO₇(s) 8, SDO₆ English 16, SDO₆ Chinese 16, VSA 6) × 5 runs | Main analyses |
| `RMET_Results_20260912.csv` | RMET, 11–12 September 2026: 22 endpoints × 36 items × 2 conditions × 5 runs | Main analyses |
| `SDO_VSA_Results.csv` | SDO₇(s) and VSA, 14 June 2026 (24 endpoints, 5 runs) | Supplementary comparison over time only |
| `RMET_Results.csv` | RMET, 14 June 2026 (18 endpoints, 1 run per condition) | Supplementary comparison over time only |

Claude Sonnet 4 returned an API error on every call in both main collections and is excluded in the analysis code, leaving 27 attitude and 21 RMET endpoints.

**Columns, attitude files:**
- `question_id`, `question_text`: item identifier and wording as presented.
- `instrument` (`SDO` = SDO₇(s), `SDO6`, `VSA`) and `language` (`en`, `zh`).
- `item_no`.
- `model_label`: access route and serving name, e.g. `azure-gpt-4.1`, `databricks-claude-opus-4-5`.
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

> **Note on keying.** In the attitude files, the `reverse_scored` and `value_recoded` columns are correct for SDO₆ only. The analysis code applies the published keys itself: SDO₇(s) items 3, 4, 7, 8 reverse-keyed (8 − x); VSA items 1, 4, 5 reverse-keyed (sign reversal); SDO₆ items 9–16 reverse-keyed.

## Reproducing the analyses

Requirements: R 4.6.0 and Python ≥ 3.8 (standard library only, for script 05).

R packages:
- dplyr, tidyr, readr, purrr, stringr, tibble, forcats;
- lme4, lmerTest, afex, ordinal, metafor, psych, pwr, performance, cluster;
- ggplot2, ggrepel, patchwork, ragg, systemfonts.

The versions used are listed under SESSION INFORMATION at the end of `Analysis_output/stats_log.txt` (e.g. lme4 2.0-6, lmerTest 3.2-1, metafor 5.2-1, psych 2.6.5, afex 1.5-1).

Run from the repository root, in order:

```r
source("Analysis_script/02_statistical_analysis.R")   # all statistics -> Analysis_output/stats_log.txt, Stats_tables/, analysis_results.rds
source("Analysis_script/03_figures.R")                # figures -> Analysis_output/Figures/
source("Analysis_script/04_reframing_checks.R")       # R1–R6 checks, Table 1 of the manuscript, FigR1, FigR2
```
```bash
python3 Analysis_script/05_two_run_disagreement_and_language_sensitivity.py   # -> Analysis_output/Additional_checks/
```

Notes on reproducibility:
- **Seed.** Random procedures use seed 20260922, with 2,000 bootstrap and 5,000 permutation resamples.
- **Ties.** Values that enter ties (ranks, signs, thresholds) are rounded to 10 decimal places, so results do not depend on the computer's floating-point precision.
- **R version and Wilcoxon tests.** R 4.6.0 and earlier R versions handle zero differences differently in Wilcoxon signed-rank tests, so a small number of Wilcoxon p-values can differ under other R versions. The reported values come from R 4.6.0.
- **Cross-check.** Script 04 cross-checks its headline numbers against the 02 outputs (R1).
- **Language shift in script 05.** The all-model shift from script 05 (0.312) differs from 02's (0.313) in the third decimal.

## Where each manuscript result comes from

| Manuscript | Source in this repository |
|---|---|
| Table 1 (summary statistics vs answer changes) | `Reframing_checks/R6_table2_summary_vs_change.csv`; two-run disagreement from `Additional_checks/two_run_disagreement_summary.csv` |
| Table 2 (conclusions from single runs) | `Reframing_checks/R4a_*.csv`, `R4b_*.csv`; `Stats_tables/S07_rank_stability.csv` |
| Table 3 (runs and models needed) | `Stats_tables/S08_d_study_runs_needed.csv`, `S18_models_for_precision.csv` |
| Table 4 (RMET) | `Stats_tables/S02_rmet_accuracy_overall.csv`, `S04_rmet_instability_overall.csv`, `S04_rmet_aggregate_reliability.csv` |
| Fig. 3 | `Figures/FigR1_alpha_vs_items_changed` |
| Fig. 4 | `Figures/Fig04_instability_by_scale_position` |
| Fig. 5 | `Figures/FigR2_rank_range_across_runs` |
| Fig. 6 | `Figures/Fig05_language_models_and_items` |
| CV and α ≥ .80 checks | `Reframing_checks/R2_*.csv`, `R3_*.csv` |
| Language analyses | `Stats_tables/S13_*.csv`, `Reframing_checks/R4c_*.csv`, `Additional_checks/language_shift_sensitivity.csv`, `Additional_checks/chinese_label_check.csv` |
| Test registry (Supplementary Table S9) | `Stats_tables/S19_test_registry.csv`, `S19_registry_summary.csv` |
| Supplementary Figs S1–S15 | `Figures/` (Fig02, Fig03, Fig06, FigS2, Fig07, Fig01, Fig09, Fig10, Fig12, Fig08, FigS1, Fig11, FigS3, FigS4, Fig13, in that order) |

Figs 1 and 2 of the manuscript are conceptual diagrams and are not produced by this code.

## Data collection

The notebooks in `Notebooks/` are the data-collection code from the June 2026 collections:
- `LLM Benchmarks.ipynb`: attitude questionnaires;
- `LLM Benchmarks - Images.ipynb`: RMET.

They show how prompts were built, how requests were sent and how responses were parsed. API keys and internal storage paths have been removed. The June notebooks set no temperature, top-p or seed, so endpoints ran at provider defaults. The code executed for the August and September collections was not archived; the same settings are assumed for those collections. The RMET notebook excludes text-only endpoints.

## Materials not included

- **RMET photographs.** The revised RMET (Baron-Cohen et al., 2001) photographs are not redistributed; the data files contain stimulus file names only.
- **Questionnaire sources.** The questionnaire items are reproduced in the data files as presented to the models. Their sources are Pratto et al. (1994), Ho et al. (2015) and Bizumic and Duckitt (2018). The Chinese SDO₆ items come from Lin (2017), with favour–oppose response labels translated for this study.

## Licence and citation

[Choose a licence, e.g. CC BY 4.0 for data and MIT for code, and add a LICENSE file.]
Please cite the article once published. [Add citation or DOI.]
