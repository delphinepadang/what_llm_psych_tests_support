# additional_checks.R — supplements verify_section3_stats.R (7 July 2026)
# Purpose: (1) truly exact Mann–Whitney p-values (base wilcox.test CANNOT give
# exact p with ties — it silently falls back to a normal approximation, so the
# caption claim "exact p-values" is currently inaccurate for these data);
# (2) Holm correction across the four dispersion contrasts; (3) mean–dispersion
# coupling (floor-compression check); (4) cross-scale dispersion correlation;
# (5) VSA facet means. Adjust column names to match your CSVs.

library(tidyverse)
library(coin)   # install.packages("coin") — exact Wilcoxon WITH ties via permutation

d <- read_csv("attitude_consistency_by_model.csv") |>
  left_join(read_csv("attitude_sdo_by_model.csv"), by = "model") |>
  left_join(read_csv("attitude_rwa_by_model.csv"), by = "model")
# expected columns after join (rename as needed):
#   model, access (flagship/open-weight), family, sdo_mean, vsa_mean, sdo_disp, vsa_disp

d <- d |> mutate(access = factor(access, levels = c("flagship", "open-weight")))
restr <- d |> filter(family == "Claude" | access == "open-weight")  # provider-restricted set (9 + 8)

# ---- 1. Exact-with-ties Mann–Whitney (coin uses the exact permutation null) ----
exact_mwu <- function(y, g) {
  t <- coin::wilcox_test(y ~ g, distribution = "exact")
  c(p_exact = coin::pvalue(t))
}
p <- c(
  full_sdo  = exact_mwu(d$sdo_disp,  d$access),
  full_vsa  = exact_mwu(d$vsa_disp,  d$access),
  restr_sdo = exact_mwu(restr$sdo_disp, droplevels(factor(ifelse(restr$family == "Claude", "flagship", "open-weight")))),
  restr_vsa = exact_mwu(restr$vsa_disp, droplevels(factor(ifelse(restr$family == "Claude", "flagship", "open-weight"))))
)
print(round(p, 4))
# Cross-check against Python enumeration (7 Jul 2026, from Table 2 values):
#   full SDO .2573 | full VSA .0556 | restricted SDO .2085 | restricted VSA .0202
# Note full-sample VSA: exact = .056, i.e. slightly LARGER than the reported .052
# (which was the tie-corrected normal approximation).

# ---- 2. Holm across the four contrasts ----
print(round(p.adjust(p, method = "holm"), 4))
# Expected: restricted VSA .0808; full VSA .1667; both SDO .4170 — none < .05.

# ---- 3. Mean–dispersion coupling (floor-compression check) ----
cor.test(d$sdo_mean, d$sdo_disp, method = "spearman", exact = FALSE)  # rho ≈ +.46
cor.test(d$vsa_mean, d$vsa_disp, method = "spearman", exact = FALSE)  # rho ≈ -.07
# Permutation p for the SDO coupling (ties make asymptotic p rough):
perm_p <- function(x, y, B = 1e5) {
  obs <- abs(cor(rank(x), rank(y)))
  mean(replicate(B, abs(cor(rank(x), rank(sample(y)))) >= obs - 1e-12)
)}
perm_p(d$sdo_mean, d$sdo_disp)  # ≈ .027 in the Python run

# ---- 4. Does stability generalise across scales within a model? ----
cor.test(d$sdo_disp, d$vsa_disp, method = "spearman", exact = FALSE)  # rho ≈ +.28, ns

# ---- 5. VSA facet means (submission q1R,q2; conventionalism q3,q4R; aggression q5R,q6) ----
# From the item-level keyed means (Appendix B): should reproduce
#   submission -1.56 | conventionalism -1.11 | aggression +0.22
item <- read_csv("PATH_TO_ITEM_LEVEL_KEYED_MEANS.csv")  # cols: question_id, mean_keyed
item |>
  mutate(facet = case_when(
    question_id %in% c("vsa-q1", "vsa-q2") ~ "submission",
    question_id %in% c("vsa-q3", "vsa-q4") ~ "conventionalism",
    question_id %in% c("vsa-q5", "vsa-q6") ~ "aggression")) |>
  group_by(facet) |> summarise(mean = mean(mean_keyed))

# ---- 6. Optional robustness metric (floor-insensitive): modal-response share ----
# From the raw item-level responses: per item, share of the 5 reps equal to the
# item's modal response; average over items per model. Report alongside SD-based
# dispersion — it is bounded [0.2, 1], invariant to where the mean sits on the scale.
