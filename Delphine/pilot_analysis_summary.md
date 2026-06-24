# Pilot Analysis — Progress Summary

**Project:** Evolution of theory of mind and psychological attributes in LLMs
**Prepared for:** Weekly meeting / project record
**Status:** Pilot data analysed (RMET + attitudes). Single-repetition RMET; 5-repetition attitudes.

---

## 1. What was analysed

Three pilot datasets from Ming's collection:

- **RMET_Results.csv** — Reading the Mind in the Eyes Test. 18 models × 36 eye-images × 2 conditions (word definitions shown vs. not). 1 repetition. The theory-of-mind task.
- **New_model_results.csv** — SDO (social dominance) + RWA/VSA (authoritarianism) attitude scales. 24 models, 5 repetitions, including Eastern models (Qwen) and open models (Llama, Gemma, GPT-OSS).
- **SDO_RWA_result_gpt_and_claude.csv** — earlier GPT+Claude-only single-rep slice, now superseded.

Models were grouped into families (GPT, Claude, Qwen, etc.) and ordered by release chronology *within* each family. Generation is not treated as comparable across families.

---

## 2. Headline findings

### Theory of mind (RMET)

1. **High overall accuracy (71%).** Well above the 25% chance level (four options per item).
2. **Frontier models clearly beat small ones.** GPT and Claude frontier models scored 0.60–0.83; Gemma (0.46) and Llama-4-maverick (0.54) sat far below. Capability tier matters more than anything else here.
3. **"Newer" is NOT better — and in GPT it is significantly worse.** Within families, accuracy does not climb across versions. The oldest GPT models tested (gpt-4.1, gpt-4o) scored highest (0.82, 0.83); a mixed-effects model gave a significant negative version trend (coefficient −0.156, p = 0.04). Claude opus showed a U-shape, dipping mid-series then recovering at opus-4-8 (0.83).
4. **Word definitions made no difference** (71.5% without vs. 71.0% with). A clean null on that experimental condition.

### Attitudes (SDO + RWA)

5. **Models lean low-dominance and anti-authoritarian** — expected from aligned models. Scores cluster at the low end of both scales.
6. **The outliers are small and open models.** Llama-3-1-8b was the most dominance-oriented (3.42 of 7) and the most erratic; the Eastern Qwen-35-122b also scored relatively high (2.46).
7. **No systematic version trend in attitudes** within GPT (r ≈ 0.15).
8. **Consistency tracks capability tier.** Using the 5 repetitions: frontier models (e.g. Claude opus) answered near-identically every time (SD ≈ 0), while small models varied wildly (Llama-3-1-8b SD > 2). This mirrors the RMET frontier-vs-small gap — two different tasks, the same underlying pattern.
9. **Refusals and parse failures concentrate in open/Eastern models** on the most politically loaded items (gpt-oss refused one item outright; Qwen produced unparseable answers on three). This is itself a finding worth reporting.

---

## 3. How to frame this honestly

The instinctive headline — "models are evolving/improving" — is **not supported** by the pilot. The accurate and more interesting story is:

> Across both tasks, capability tier (frontier vs. small) is the dominant axis, not version recency. Within families, performance is flat or wobbly, and GPT's RMET accuracy significantly *declines* across versions. Frontier models are also far more consistent. This engages directly with the published debate about whether LLM theory of mind is genuine or "superficial and unstable" (Chen et al. 2025 survey).

A declining-with-version finding is striking and publishable — *provided* it is hedged properly (see caveats).

---

## 4. Caveats (must accompany any claim)

- **RMET has only one repetition.** Some apparent dips (e.g. gpt-5.2 at 0.625) could be noise. The significant GPT trend leans on a few points and may not survive replication. This is the single strongest argument for the full study's 5–10 repetitions.
- **Small item count** (36 items per condition) → wide confidence intervals.
- **Possible contamination.** RMET is a famous instrument and may sit in training data; high scores could reflect memorisation rather than inference.
- **The East/West axis rests on one Eastern family (Qwen).** No ecosystem-level claims are possible yet — DeepSeek and additional Eastern models are needed.
- **VSA values exceeded the documented ±3 range** (some ±4 appeared). Minor data-quality issue to raise with Ming.

---

## 5. Questions for the team

**For Ming (modelling/data):**
- Confirm the intended VSA scale range — some responses fell outside ±3.
- Confirm version/release ordering for non-GPT/Claude models before we treat them as ordered.
- Repetition target for the full RMET run — the single-rep limitation is the main blocker on the ToM conclusion.

**For Kangning (psychology):**
- Is the definitions-vs-no-definitions condition still worth keeping, given it had zero effect in the pilot?
- Are the reverse-coding groupings for SDO/RWA correct as applied (SDO q3/q4/q7/q8 and VSA q1/q4/q5 reversed)?
- How should refusals on loaded items be treated — as missing, or as a substantive response category?

---

## 6. Suggested next steps

1. Re-run RMET with 5–10 repetitions so the version trend can be confirmed or dismissed.
2. Add Eastern models (DeepSeek + more Qwen) before any East/West framing.
3. Begin the Chinese/English administration that is the project's larger aim.
4. Draft the Introduction around the "capability tier, not recency" and "accuracy vs. stability" themes — both are well supported and align with the survey's framing.

---

## Files produced

- `rmet_evolution.R` — RMET analysis + evolution plot
- `attitude_evolution.R` — SDO/RWA analysis with reverse-coding + consistency
- `rmet_accuracy_by_model.csv`, `attitude_*_by_model.csv` — tidy result tables
