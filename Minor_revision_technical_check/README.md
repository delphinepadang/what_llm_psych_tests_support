# Minor revision (technical check), HSSC, November 2026

This folder holds the checks behind the minor revision requested at the HSSC
technical check (Submission ID dec96048-eb1b-4977-a973-b7bc7dd040f7).

The original analysis pipeline (`Analysis_script/`, `Analysis_output/`) is
**unchanged**. It produced the results in the submitted manuscript. This folder
only recomputes, from the raw files in `Results/`, the numbers that were added
or corrected during the revision. Each one is compared with the value printed
in the final manuscript.

## Contents

| File | What it is |
|---|---|
| `recomputed_values.qmd` | Quarto notebook. Each section is one claim, with its manuscript line numbers, the code (folded) and a reported-versus-recomputed check. |
| `recomputed_values.html` | The rendered notebook. Open it in a browser; no R needed. |
| `outputs/check_summary.csv` | All checks in one table (73 rows; all agree). |
| `outputs/sdo6_model_scores_by_language.csv` | Each model's English and Chinese SDO6 score and difference. |
| `outputs/rmet_scores_by_model.csv` | Each model's RMET accuracy and score out of 36. |
| `outputs/developer_mixed_models.csv` | Developer-adjusted estimates of the language difference. |

## What is checked

| Notebook section | Manuscript location (final PDF) |
|---|---|
| Data and exclusions: 6,210 and 7,560 calls; Claude Sonnet 4 excluded | Methods 2.3, lines 264–265 |
| English versus Chinese SDO6: SDs, mean difference and CI, exact P, d<sub>z</sub>, Wilcoxon | Results 3.2, lines 382–385 |
| Robustness: without items 2, 4, 8 (and 2, 3, 4, 8); developer mixed models | Results 3.2, lines 409–413; SI Notes "Language analyses"; Table S4 |
| RMET: 74%, 26.7 of 36, SD 3.7, Welch difference from the adult mean | Results 3.1, lines 362–364 |
| Answer change (21%) and two-run disagreement (11%), with bootstrap CIs | Results 3.3, lines 425–426 |
| Models in the generation comparison (10 earlier, 9 later, GPT-OSS excluded) | Results 3.3, lines 471–474 |
| Pratto et al. (2013) SDs rescaled to 1–7 | Results 3.1, lines 370–372 |
| Which tests are one-sided (read from the code) | Methods, lines 322–328 |

## How to rerun

Open `recomputed_values.qmd` in RStudio and click **Render**, or run this from
the repository root:

```bash
quarto render Minor_revision_technical_check/recomputed_values.qmd
```

Packages: `dplyr`, `tidyr`, `readr`, `lme4`, `knitr`. Rendering takes about a
minute, mostly for the bootstrap.

## Two notes from the rerun

1. **Wilcoxon P without items 2, 4 and 8.** The round-2 draft of the SI gave
   5.5 × 10⁻⁶, from a routine that ignores tied ranks. The exact value that
   allows for ties is **5.3 × 10⁻⁶**. Use 5.3 × 10⁻⁶ in the SI Notes and in
   Table S4. The main text is unaffected: for all 16 items both methods give
   1.5 × 10⁻⁷.
2. **Bootstrap limits.** The 13% to 30% interval comes from the original
   script (12.99 to 30.06, from 2,000 resamples). With 2,000 resamples the
   upper limit moves between about 29.4 and 30.1 depending on the random
   seed. The notebook uses 20,000 resamples, which gives stable limits (12.7
   to 29.9). Both round to the printed 13% to 30%.
