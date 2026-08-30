# Build notes — NeurIPS 2026 TAE submission

## Files

- `main.tex` — the manuscript
- `references.bib` — 33 entries, all cited, none orphaned, all corrections applied

## To compile

Put `neurips_2026.sty` in the same directory, then:

```
pdflatex main && bibtex main && pdflatex main && pdflatex main
```

Figures expected in the working directory:

| File | Where | Status |
|---|---|---|
| `fig1_scores.pdf` | §3.1, body | needed |
| `fig3_facets.pdf` | §3.2, body | needed |
| `fig2_stability.pdf` | Appendix B | needed |
| `fig4_item_matrix.pdf` | Appendix C | needed |

Your existing PNGs will work if you change the extensions in the four
`\includegraphics` calls, but PDF is better for a print submission.

## Page count

Body estimates at roughly **8.8 pages** against a limit of 8 (references and
appendices excluded). My estimator carries about ±0.7 pages of error, so the real
render may come in at 8.1 or at 9.5. **Compile before you cut further.**

If it renders over 8, delete in this order. Each is self-contained and none
removes a load-bearing claim:

1. **§3.3, the paragraph beginning "Between-model variance on the SDO is also
   concentrated"** (~90 words). A nice detail; nothing later depends on it. Move
   to an appendix if you want to keep it.
2. **§4, the paragraph beginning "Why the assumption does not transfer"**
   (~180 words). Compress to three sentences: the VSA total is validated on
   humans, Einwiller and Sühr both find the structure fails on models, so our
   result is expected rather than surprising.
3. **§1, the paragraph beginning "This is what makes the constructs
   interesting"** (~170 words). The prior-work survey can lose Hartmann, Motoki
   and Rozado down to a single parenthetical citation list.
4. **Move Figure 3 to the appendix** (~0.3 page). Last resort — it is the figure
   that shows the reversal, and it should stay in the body if at all possible.

If it renders under 8, restore Figure 4 (`fig:items`) to §3.3 first. It carries
the wording argument visually and it is the strongest of the four.

## Before you upload

- [ ] Set `\workshoptitle` — already set to the TAE title, confirm the exact
      string against the CFP
- [ ] Create the anonymous repository mirror and put the link in supplementary
      material, **not** in the PDF body
- [ ] OpenReview profile IDs from all four authors
- [ ] Check PDF metadata, not just the text — Quarto and LaTeX both write author
      fields into file properties
- [ ] Confirm Wang Table 1 per-cell $\kappa$ values against the open-access PDF
      (see `edit_sheet_addendum.md` §D1). The manuscript no longer prints any
      per-cell value, so this is now a check rather than a blocker.

## What changed from your draft

Every correction from the three review passes is applied. The substantive ones:

**Data corrections.** The SDO replication in §3.3 now reports the subdimension
contrast in the correct direction, with the keying-matched decomposition added.
Table 1 row 1 is filled in (18/24, 1 reversal, 5 ties). The §3.5 $\alpha$ counts
are corrected and the unequal replication $n$ is disclosed. The
aggression–conventionalism $p$ is .000087.

**Method corrections.** All 24 models are now described as served through Azure
or Databricks rather than vendor APIs. The Wang temperature contrast is rewritten
so it no longer presents a prompt-and-temperature comparison as a temperature
comparison. §2.2 no longer uses "keyed" in two senses in one sentence.

**Citations.** Sühr corrected (title and authors). Barmettler retitled to the v3
title and the "inverting" claim softened to what the paper reports. Feng moved
out of the convergence group. Ożegalska-Łukasik added. Urban's TODO resolved.
Einwiller's predictive-validity claim now carries the $r = 0.01$ / $r = 0.65$
split.

**Framing.** A paragraph in §1 now states the AuAu overlap explicitly — same
instrument, same facets, seventeen models — and says what is different about our
question. §3.4 uses the divergent facet ordering in AuAu as cross-study evidence
for the rate-versus-mean claim rather than leaving it as an unaddressed
contradiction. §5 cites Einwiller's VSA reliability finding alongside Bizumic and
Duckitt's.

**Housekeeping.** Scratch header, `(file fig 3)`, `fig4_item_matrix.png` and
`(Figure 2 A.B)` removed. Figure 1's caption no longer contradicts §3.1. The
Method's pre-registration subsection folded into the analysis paragraph, since it
duplicated the Limitations entry.

**Structural cuts made for length.** The dual-process exposition in §1 is about
half its former size; the instrument-selection rationale, the sampling-parameter
justification, and the Limitations section are each tightened; Figures 2 and 4
moved to appendices. No claim was removed, only compressed. If you want any of
the longer versions back, they are in the previous draft.

## One judgement call I made for you

Your §1 had three paragraphs of Duckitt exposition establishing that decomposing
an aggregate recovers structure the aggregate concealed. That is a lovely
argument and it is the intellectual spine of the paper. I cut it to two shorter
paragraphs because at a measurement workshop the reviewers will not need
convincing that SDO and RWA are distinct constructs — they will need convincing
that your aggregation finding is real. The space is better spent on §3.3 and
§3.5. If Jie disagrees, the material is easy to restore and the cut list above
gives you somewhere else to find the space.
