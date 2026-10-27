# Data and code: Do large language models respond like human participants?

This folder holds the data, analysis code and figures for the manuscript

> **Do large language models respond like human participants? Stability and language effects in responses to social attitude and mental-state measures.**
> Submitted to *Humanities and Social Sciences Communications*, collection "Large language models in psychological science".

**The study in brief**
- 27 large language models (LLMs) answered three attitude questionnaires:
  - two social dominance orientation scales (SDO7(s), and SDO6 in English and in Chinese);
  - the Very Short Authoritarianism scale (VSA).
- 21 of these models also took the revised Reading the Mind in the Eyes Test (RMET), with and without word definitions.
- Every item was asked five times, each time as a separate request with no conversation history.

---

## What is in this folder

```
README.md                    This file
requirements.txt             Python packages needed
Results/                     The data: every answer the models gave (one row per request)
Notebooks/                   The code used to send the questions to the models
Analysis_script/             The analysis code, numbered in the order to run it
Analysis_output/             Everything the analysis code produces (tables and logs)
Figures/
  ├── Main_figures/          Figures 1–4 of the manuscript
  └── Supplementary_figures/ Supplementary Figures S1–S13
```

If you only want to check a number in the paper, start with
`Analysis_output/stats_log.txt`. It lists every statistic in the order the analysis runs.
The table "Where to find each result" below says where everything else is.

---

## The data (`Results/`)

| File | What it contains | Used for |
|---|---|---|
| `SDO_VSA_Results_20260830.csv` | Attitude questionnaires, 30 August 2026: 28 models × 46 items × 5 runs | Main analyses |
| `RMET_Results_20260912.csv` | RMET, 11–12 September 2026: 22 models × 36 items × 2 conditions × 5 runs | Main analyses |
| `SDO_VSA_Results.csv` | SDO7(s) and VSA, June 2026 (earlier session) | Supplementary Fig. S2 only |
| `RMET_Results.csv` | RMET, June 2026 (earlier session) | Supplementary Fig. S2 only |

One model, Claude Sonnet 4, returned an error on every request. The analysis code removes it, which leaves 27 models for the attitude scales and 21 for the RMET.

**Main columns**
- `model_label`: which model answered.
- `repeat`: which of the five runs (0–4).
- `question_text`, or `image` for the RMET: the item. Only the file names of the RMET photographs are included.
- `value` (attitude) or `choice` and `is_correct` (RMET): the answer.
- `raw_response`: the model's full reply.
- `timestamp`: when the reply was received.

**Note on reverse-scored items.** In the attitude file, the `reverse_scored` and `value_recoded` columns are correct for SDO6 only. The analysis code applies the published scoring keys itself:
- SDO7(s) items 3, 4, 7 and 8, and SDO6 items 9–16, are scored as 8 − answer.
- VSA items 1, 4 and 5 have their sign reversed.

---

## How the data were collected (`Notebooks/`)

| Notebook | Collection |
|---|---|
| `LLM Benchmarks with Chinese.ipynb` | Attitude questionnaires, August 2026, in English and Chinese (main data) |
| `LLM Benchmarks.ipynb` | Attitude questionnaires, June 2026 |
| `LLM Benchmarks - Images.ipynb` | RMET, June 2026. The September run used the same procedure; its code was not saved separately. |

The notebooks show:
- the exact prompts;
- how requests were sent;
- how replies were turned into answers.

API keys and storage paths have been removed. No temperature, top-p or random seed was set, so every model ran at its provider's default settings.

---

## How to reproduce the results

**You need:**
- **R 4.6.0.** The package versions used are listed at the end of `Analysis_output/stats_log.txt`.
- **Python 3.8 or later**, with the packages in `requirements.txt`.
- **Quarto**, only for script 10.

**Steps.** Run everything from this folder (the repository root), in this order:

```r
source("Analysis_script/02_statistical_analysis.R")   # all statistical tests
source("Analysis_script/03_figures.R")                # Supplementary Figs S1, S2, S4–S7, S9–S13
source("Analysis_script/04_reframing_checks.R")       # reliability statistics vs answer changes; single-run checks
```
```bash
pip install -r requirements.txt
python3 Analysis_script/05_two_run_disagreement_and_language_sensitivity.py
python3 Analysis_script/06_report_descriptives.py      # descriptive tables; earlier vs later model generations
python3 Analysis_script/07_report_figures.py           # Supplementary Fig. S8
python3 Analysis_script/08_manuscript_figures.py       # Figures 2–4
python3 Analysis_script/11_supplementary_fig_s3.py     # Supplementary Fig. S3
quarto render Analysis_script/10_sdo6_language_tests.qmd --output-dir ../Analysis_output/Language_comparison
```

**Where new figures are written.** Scripts 03, 07 and 08 write to `Analysis_output/Figures/` and `Manuscript_figures/` under the scripts' own working names. The copies in `Figures/` are the final versions used in the paper, renamed to match its numbering. The table below gives the working name of each one.

**Random numbers.** Random procedures use seed 20260922, with 2,000 bootstrap and 5,000 permutation resamples, so results repeat exactly.

---

## Where to find each result

### Main figures (`Figures/Main_figures/`)

| Figure | File | Made by |
|---|---|---|
| Fig. 1 Study design | `Fig1_study_design.png` | Drawn in Canva (no code) |
| Fig. 2 English vs Chinese SDO6 | `Fig2_language.png` | Script 08 (working name `Fig2_language_simple`) |
| Fig. 3 Reliability vs answer changes | `Fig3_answer_changes.png` | Script 08 |
| Fig. 4 RMET accuracy and consistency | `Fig4_rmet.png` | Script 08 |

### Supplementary figures (`Figures/Supplementary_figures/`)

| Figure | File | Made by (working name) |
|---|---|---|
| S1 Answers across the five runs | `FigS01_run_order.png` | Script 03 (`FigS1_run_order`) |
| S2 Answers given months apart | `FigS02_june_vs_later_answers.png` | Script 03 (`Fig11_longitudinal_drift`) |
| S3 How SDO6 answers changed between languages | `FigS03_language_mechanism.png` | Script 11 |
| S4 Answers at an end of the scale, by language | `FigS04_scale_endpoints_by_language.png` | Script 03 (`FigS2_extreme_responding_by_language`) |
| S5 Model-level effects of definitions and language | `FigS05_model_level_effects.png` | Script 03 (`Fig07_forest_definitions_and_language`) |
| S6 Answer change by scale position | `FigS06_change_by_scale_position.png` | Script 03 (`Fig04_instability_by_scale_position`) |
| S7 Answer changes by model and scale | `FigS07_change_by_model_and_scale.png` | Script 03 (`Fig02_instability_heatmap`) |
| S8 Earlier vs later model generations | `FigS08_earlier_vs_later_generations.png` | Script 07 (`Report_Fig5_older_vs_newer`) |
| S9 RMET accuracy and answer changes by model | `FigS09_rmet_by_model.png` | Script 03 (`Fig01_rmet_accuracy_and_stability`) |
| S10 RMET item responses | `FigS10_rmet_items.png` | Script 03 (`Fig09_rmet_items_consensus`) |
| S11 Which RMET items models get wrong together | `FigS11_rmet_error_agreement.png` | Script 03 (`Fig10_rmet_error_consistency`) |
| S12 Correlations across models | `FigS12_correlations_across_models.png` | Script 03 (`FigS3_cross_instrument_correlations`) |
| S13 SDO subscale scores | `FigS13_sdo_subscales.png` | Script 03 (`Fig12_subscale_divergence`) |

### Supplementary tables (built from these files in `Analysis_output/`)

| Table | Source files |
|---|---|
| S1 Prompts and items | `Notebooks/` (prompts) and `Results/` (item wording as asked) |
| S2 Models | `Stats_tables/S01_nonscored_by_model.csv`, `Report_outputs/generation_table.csv` |
| S3 Design and completeness | `Stats_tables/S01_design.csv`, `S07_instability_by_instrument.csv` |
| S4 Every statistical test | `Stats_tables/S19_test_registry.csv` (test, sample size, statistic, exact and adjusted P) |
| S5 Language differences by item | `Stats_tables/S13_language_by_item.csv`, `S13_dif_robust_linking.csv` |
| S6 Answer stability by scale | `Stats_tables/S07_reliability_by_aggregation_level.csv`, `S10_*.csv`; `Additional_checks/two_run_disagreement_*.csv`; `Reframing_checks/R2_*`, `R3_*`, `R6_*` |
| S7 Earlier vs later generations | `Report_outputs/generation_comparison.csv` |
| S8 Conclusions from single runs | `Reframing_checks/R4a_*`, `R4b_*`, `R4c_*` |
| S9 RMET by model | `Stats_tables/S02_rmet_accuracy_by_model.csv`, `S04_rmet_reliability_by_model.csv` |
| S10 SDO subscales | `Stats_tables/S11_subscale_summary.csv`, `S11_rank_reversals.csv`, `S12_internal_structure.csv` |

**A note on numbering.** The files in `Analysis_output/Stats_tables/` start with S01–S19. These numbers are the sections of the analysis log; they are *not* the Supplementary Table numbers in the paper. `Analysis_output/Stats_README.md` describes each of these files.

### Other key results

| Result in the paper | File |
|---|---|
| SDO6 higher in Chinese (paired t test, Section 3.2) | `Analysis_output/Language_comparison/primary_paired_t_test.csv` |
| Share of items changing across runs; two-run disagreement | `Analysis_output/Additional_checks/two_run_disagreement_summary.csv` |
| Language result without Llama 3.1 8B | `Analysis_output/Additional_checks/language_shift_sensitivity.csv` |
| RMET item selection criteria applied to model answers (Section 3.5) | `Analysis_output/Stats_tables/S05_rmet_items.csv` (column `retained`) |
| Attitude scores by model and scale | `Analysis_output/Report_outputs/scale_scores_by_model.csv` |

---

## Materials not included

- **RMET photographs.** These belong to the published test (Baron-Cohen et al., 2001) and are not shared here. The data files give file names only.
- **Questionnaire items.** The items appear in the data files exactly as asked. Sources:
  - SDO6: Pratto et al., 1994; Chinese version from Lin, 2017.
  - SDO7(s): Ho et al., 2015.
  - VSA: Bizumic and Duckitt, 2018.
