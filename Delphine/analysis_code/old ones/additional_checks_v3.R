# additional_checks_v3.R — v2 with corrected expectation comments (8 July 2026)
# Code unchanged from v2, which ran clean on Delphine's machine 7 July; only the
# comments are corrected, after the exact-p-value dispute was resolved in the
# manuscript's favour. Replaces additional_checks.R. Fixes vs v1:
#   (1) join key is `m`, not `model`
#   (2) no `access` column exists in the CSVs — derived from model IDs, with assertions
#   (3) `family` appears in more than one file — join on `m` only, rename on select
#   (4) RWA score column name auto-detected; hard stop if not found
#   (5) dependency-free full-enumeration check added, so coin's exact p is verified
#       by a second, independent implementation inside R (mirrors the Python check)
# The "package 'coin' was built under R 4.6.1" warning is harmless.
#
# CONVENTION NOTE (supersedes the stale expectations corrected below): base R's
# wilcox.test DOES compute exact p-values with ties from R 4.6, using the
# doubled one-sided-tail two-sided convention (verified empirically 7 July: no
# ties warning; doubled enumeration reproduces .05227127 and .02097902). coin's
# exact values (.0514 / .0202) use the symmetric-distance convention. Both are
# exact; they differ by convention, not correctness. The manuscript reports the
# base-R values (.26 / .052 / .21 / .021).

library(tidyverse)
library(coin)

# ---- 0. Load and join on `m` -------------------------------------------------
cons <- read_csv("attitude_consistency_by_model.csv", show_col_types = FALSE)
sdo  <- read_csv("attitude_sdo_by_model.csv",         show_col_types = FALSE)
rwa  <- read_csv("attitude_rwa_by_model.csv",         show_col_types = FALSE)

vsa_col <- intersect(c("RWA", "VSA", "rwa", "vsa", "rwa_mean", "vsa_mean",
                       "mean_rwa", "mean_vsa"), names(rwa))
stopifnot("RWA/VSA score column not found - run names(rwa) and add it to the candidate list" =
            length(vsa_col) == 1)

d <- cons |>
  left_join(sdo |> select(m, ecosystem, sdo_mean = SDO), by = "m") |>
  left_join(rwa |> select(m, vsa_mean = all_of(vsa_col)), by = "m")

stopifnot(nrow(d) == 24, !anyNA(d$sdo_mean), !anyNA(d$vsa_mean))

# ---- 1. Derive access type from model identifiers ----------------------------
d <- d |>
  mutate(
    is_claude = str_starts(m, "claude"),
    access = factor(if_else(is_claude | (str_starts(m, "gpt") & !str_detect(m, "oss")),
                            "flagship", "open-weight"),
                    levels = c("flagship", "open-weight")))

stopifnot(sum(d$access == "flagship") == 16,
          sum(d$access == "open-weight") == 8,
          sum(d$is_claude) == 9)

# Eyeball check: these should be the 8 open-weight systems
d |> filter(access == "open-weight") |> pull(m) |> print()

# Sanity vs manuscript 3.2 medians — expect SDO .06/.23 and VSA .07/.74.
# If these do NOT match, the consistency_* columns are not the dispersion
# quantity as assumed: stop and check before going further.
d |> group_by(access) |>
  summarise(across(c(consistency_sdo, consistency_vsa), median)) |> print()

# ---- 2. Exact Mann-Whitney (ties handled exactly by coin) ---------------------
exact_p <- function(df, y, g) {
  coin::pvalue(coin::wilcox_test(reformulate(g, response = y),
                                 data = df, distribution = "exact"))
}

restr <- d |>
  filter(is_claude | access == "open-weight") |>
  mutate(group = factor(if_else(is_claude, "claude", "open-weight")))
stopifnot(nrow(restr) == 17)

p <- c(
  full_sdo  = exact_p(d,     "consistency_sdo", "access"),
  full_vsa  = exact_p(d,     "consistency_vsa", "access"),
  restr_sdo = exact_p(restr, "consistency_sdo", "group"),
  restr_vsa = exact_p(restr, "consistency_vsa", "group"))
print(round(p, 4))
# Expected (coin, symmetric-tail convention; confirmed on the raw CSVs 7 July,
# coin == enumeration to 7 decimals):
#   full_sdo .2573 | full_vsa .0514 | restr_sdo .2085 | restr_vsa .0202
# (The earlier .0556 expectation here was an artifact of enumerating from
# Table-2-rounded inputs rather than the raw CSVs. The manuscript's .052 is the
# base-R doubled-tail exact value, .05227127 — see the convention note above.)

# ---- 2b. Dependency-free ground truth: full enumeration in base R -------------
# coin's exact two-sided p is P(|T - mu| >= |t_obs - mu|); this enumerates the
# same quantity directly, no packages. Full sample takes ~10-20 s (C(24,8) sets).
enum_exact_p <- function(y, in_group2) {
  r   <- rank(y)                          # midranks
  n2  <- sum(in_group2); N <- length(y)
  mu  <- n2 * (N + 1) / 2
  obs <- sum(r[in_group2])
  cs  <- colSums(matrix(r[combn(N, n2)], nrow = n2))
  mean(abs(cs - mu) >= abs(obs - mu) - 1e-9)
}
enum_exact_p(d$consistency_vsa,     d$access == "open-weight")       # expect .0514
enum_exact_p(restr$consistency_vsa, restr$group == "open-weight")    # expect .0202
# (coin and the enumeration must agree exactly.)

# ---- 3. Holm across the four contrasts ----------------------------------------
print(round(p.adjust(p, method = "holm"), 4))
# Expected: full_sdo .4170 | full_vsa .1542 | restr_sdo .4170 | restr_vsa .0808
# (Under the manuscript's base-R convention the Holm set is .084 / .157 / ~.41 /
#  ~.41 — computed at full precision in verify_section6.R, section 6.6.)
# i.e. nothing survives correction across the four tests.

# ---- 4. Mean-dispersion coupling (floor-compression check) --------------------
cor.test(d$sdo_mean, d$consistency_sdo, method = "spearman", exact = FALSE) # rho ~ +.46
cor.test(d$vsa_mean, d$consistency_vsa, method = "spearman", exact = FALSE) # rho ~ -.07

perm_p_spearman <- function(x, y, B = 1e5) {
  obs  <- abs(cor(rank(x), rank(y)))
  sims <- replicate(B, abs(cor(rank(x), rank(sample(y)))))
  mean(sims >= obs - 1e-12)
}
set.seed(2026)
perm_p_spearman(d$sdo_mean, d$consistency_sdo)   # expect ~ .026 (seed 2026)

# ---- 5. Cross-scale stability --------------------------------------------------
cor.test(d$consistency_sdo, d$consistency_vsa, method = "spearman", exact = FALSE)
# rho ~ +.28, ns — stability does not strongly generalise across scales within model

# ---- 6. VSA facet means ---------------------------------------------------------
# Standalone from the Appendix B keyed item means; swap in the raw item-level file
# to reproduce from source when convenient.
tibble(question_id = paste0("vsa-q", 1:6),
       mean_keyed  = c(-0.98, -2.14, -1.08, -1.13, 0.22, 0.21)) |>
  mutate(facet = case_when(
    question_id %in% c("vsa-q1", "vsa-q2") ~ "submission",
    question_id %in% c("vsa-q3", "vsa-q4") ~ "conventionalism",
    TRUE                                   ~ "aggression")) |>
  group_by(facet) |>
  summarise(mean = round(mean(mean_keyed), 3)) |> print()
# Expected: aggression +0.215 | conventionalism -1.105 | submission -1.560
