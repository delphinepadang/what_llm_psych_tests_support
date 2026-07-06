# verify_section3_stats.R
# ---------------------------------------------------------------------------
# Reproduces, in manuscript order, every statistic reported in §3 of
# MS_AAI_intro_methods_references.md from the three model-level aggregate CSVs.
# Expected values (as reported in the draft, computed independently in
# Python/scipy) appear as comments next to each block — run and diff.
#
# Run from the directory containing the three CSVs:
#   attitude_sdo_by_model.csv, attitude_rwa_by_model.csv,
#   attitude_consistency_by_model.csv
#
# NOTE on tests: R's wilcox.test will warn "cannot compute exact p-value with
# ties" — expected. With ties it uses the normal approximation with continuity
# correction, which is what scipy used, so p-values should match to the
# precision printed. R's W is the same statistic as the U reported in §3.2
# (first-listed group: flagship / Claude).
# ---------------------------------------------------------------------------

library(tidyverse)

sdo <- read_csv("attitude_sdo_by_model.csv", show_col_types = FALSE)
rwa <- read_csv("attitude_rwa_by_model.csv", show_col_types = FALSE)
con <- read_csv("attitude_consistency_by_model.csv", show_col_types = FALSE)

d <- sdo |>
  inner_join(rwa |> select(m, RWA), by = "m") |>
  inner_join(con |> select(m, consistency_sdo, consistency_vsa), by = "m") |>
  mutate(
    access   = if_else(family %in% c("Claude", "GPT"), "flagship", "open-weight"),
    provider = if_else(family == "GPT", "Azure", "Databricks")
  )

stopifnot(nrow(d) == 24)   # 24 models, per Table 1

rb <- function(W, n1, n2) 2 * W / (n1 * n2) - 1   # rank-biserial from U/W

# ===========================================================================
# §3.1 Dispositional placement
# ===========================================================================
cat("\n== §3.1 SDO: grand mean 1.64, median 1.49, range 1.00–3.42, all 24 < 4 ==\n")
d |> summarise(mean = mean(SDO), median = median(SDO),
               min = min(SDO), max = max(SDO), n_below_mid = sum(SDO < 4)) |> print()

cat("\n== §3.1 VSA: grand mean −0.82, median −0.77, range −1.80–0.20, 21 of 24 < 0 ==\n")
d |> summarise(mean = mean(RWA), median = median(RWA),
               min = min(RWA), max = max(RWA), n_below_mid = sum(RWA < 0)) |> print()

cat("\n== §3.1 the three at/above the VSA midpoint (GPT-4o +0.20; GPT-OSS-20B +0.13; Qwen3.5 0.00) ==\n")
d |> filter(RWA >= 0) |> select(m, RWA) |> print()

cat("\n== §3.1 family means: SDO 1.45 (Gemma) – 2.18 (Llama); VSA −1.34 (Llama) – −0.38 (GPT-OSS) ==\n")
d |> group_by(family) |>
  summarise(SDO = mean(SDO), VSA = mean(RWA), .groups = "drop") |>
  arrange(SDO) |> print()

cat("\n== §3.1 extremes: min SDO qwen3-next (1.00), max llama-3.1-8b (3.42) ==\n")
d |> slice_min(SDO, n = 1) |> select(m, SDO) |> print()
d |> slice_max(SDO, n = 1) |> select(m, SDO) |> print()

# ===========================================================================
# §3.2 Response stability
# ===========================================================================
cat("\n== §3.2 dispersion ranges: SDO 0–1.40, VSA 0–2.19 ==\n")
d |> summarise(across(c(consistency_sdo, consistency_vsa),
                      list(min = min, max = max))) |> print()

cat("\n== §3.2 '14 of 24 identical on ≥1 scale; none on both' ==\n")
d |> summarise(
  zero_on_at_least_one = sum(consistency_sdo == 0 | consistency_vsa == 0),
  zero_on_both         = sum(consistency_sdo == 0 & consistency_vsa == 0)
) |> print()

cat("\n== §3.2 medians by access: SDO 0.06 vs 0.23; VSA 0.08 vs 0.75 ==\n")
d |> group_by(access) |>
  summarise(med_sdo = median(consistency_sdo),
            med_vsa = median(consistency_vsa), .groups = "drop") |> print()

cat("\n== §3.2 full-sample tests: SDO U = 45.5, p = .26; VSA U = 32.5, p = .055 ==\n")
w1 <- wilcox.test(consistency_sdo ~ access, data = d)
print(w1); cat("rank-biserial r =", round(rb(w1$statistic, 16, 8), 2), " (manuscript: −0.29)\n")
w2 <- wilcox.test(consistency_vsa ~ access, data = d)
print(w2); cat("rank-biserial r =", round(rb(w2$statistic, 16, 8), 2), " (manuscript: −0.49)\n")

cat("\n== §3.2 Databricks-restricted: SDO U = 23.0, p = .21, r = −.36; VSA U = 12.5, p = .024, r = −.65 ==\n")
db <- d |> filter(provider == "Databricks") |>
  mutate(grp = if_else(family == "Claude", "claude", "open-weight"))
stopifnot(sum(db$grp == "claude") == 9, sum(db$grp == "open-weight") == 8)
w3 <- wilcox.test(consistency_sdo ~ grp, data = db)
print(w3); cat("rank-biserial r =", round(rb(w3$statistic, 9, 8), 2), "\n")
w4 <- wilcox.test(consistency_vsa ~ grp, data = db)
print(w4); cat("rank-biserial r =", round(rb(w4$statistic, 9, 8), 2), "\n")

cat("\n== §3.2 structure: top-3 VSA dispersion all open-weight; Claude max 0.23; GPT SDO 0–0.30, VSA 0–0.85 ==\n")
d |> slice_max(consistency_vsa, n = 3) |> select(m, access, consistency_vsa) |> print()
d |> slice_max(consistency_sdo, n = 4) |> select(m, access, consistency_sdo) |> print()
d |> filter(family == "Claude") |>
  summarise(claude_max_disp = max(pmax(consistency_sdo, consistency_vsa))) |> print()
d |> filter(family == "GPT") |>
  summarise(sdo_min = min(consistency_sdo), sdo_max = max(consistency_sdo),
            vsa_min = min(consistency_vsa), vsa_max = max(consistency_vsa)) |> print()

# ===========================================================================
# §3.3 Ecosystem (descriptive)
# ===========================================================================
cat("\n== §3.3 the two Qwen models ==\n")
d |> filter(ecosystem == "Eastern") |>
  select(m, SDO, RWA, consistency_sdo, consistency_vsa) |> print()

# ===========================================================================
# §3.4 Version trends (release order per Table 1)
# ===========================================================================
orders <- list(
  `Claude-Opus`   = c("claude-opus-4-1","claude-opus-4-5","claude-opus-4-6",
                      "claude-opus-4-7","claude-opus-4-8"),
  `Claude-Sonnet` = c("claude-sonnet-4","claude-sonnet-4-5","claude-sonnet-4-6"),
  GPT             = c("gpt-4o","gpt-4.1","gpt-5","gpt-5.1","gpt-5.2","gpt-5.4","gpt-5.5"),
  Llama           = c("meta-llama-3-1-8b-instruct","meta-llama-3-3-70b-instruct",
                      "llama-4-maverick"),
  Qwen            = c("qwen3-next-80b-a3b-instruct","qwen35-122b-a10b")
)
cat("\n== §3.4 within-family sequences (compare to the arrows in the text) ==\n")
di <- d |> column_to_rownames("m")
for (fam in names(orders)) {
  seqs <- orders[[fam]]
  cat(sprintf("%-14s SDO: %s\n", fam,
              paste(round(di[seqs, "SDO"], 2), collapse = " -> ")))
  cat(sprintf("%-14s VSA: %s\n", fam,
              paste(round(di[seqs, "RWA"], 2), collapse = " -> ")))
}

# ===========================================================================
# §2.4 missingness identification (denominator check)
# SDO max n = 8 items x 5 reps = 40; VSA max n = 6 x 5 = 30.
# A mean of integer responses times the true n must be an integer.
# Expected: qwen35-122b-a10b consistent ONLY with n = 37 (3 missing);
#           gpt-oss-120b consistent with n = {36, 39} — n = 39 (1 missing)
#           is the value compatible with the recorded total of 4 missing.
# ===========================================================================
consistent_n <- function(mean_val, n_max) {
  ns <- (n_max - 4):n_max
  ns[abs(mean_val * ns - round(mean_val * ns)) < 1e-6]
}
cat("\n== §2.4 models whose SDO mean is NOT consistent with the full n = 40 ==\n")
for (i in seq_len(nrow(d))) {
  ns <- consistent_n(d$SDO[i], 40)
  if (!(40 %in% ns)) cat(sprintf("  %-30s consistent n: %s\n", d$m[i],
                                 paste(ns, collapse = ", ")))
}

# ===========================================================================
# Table 2 cross-check + session info
# ===========================================================================
d |>
  mutate(m = factor(m, levels = unname(unlist(orders)) |>
                      c("gpt-oss-20b","gpt-oss-120b","gemma-3-12b",
                        "claude-haiku-4-5") |> unique())) |>
  arrange(m) |>
  select(m, family, ecosystem, access, SDO, RWA,
         consistency_sdo, consistency_vsa) |>
  mutate(across(where(is.numeric), ~ round(.x, 2))) |>
  write_csv("table2_check.csv")
cat("\nWrote table2_check.csv — diff against Table 2 in the manuscript.\n")

cat("\n== sessionInfo(): paste the R version + package versions into §2.5 ==\n")
sessionInfo()
