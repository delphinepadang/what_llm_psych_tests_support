# verify_section3_stats_v2.R — v2, 8 July 2026
# ---------------------------------------------------------------------------
# Reproduces, in manuscript order, every statistic reported in §3 of
# MS_AAI_manuscript.qmd from the three model-level aggregate CSVs, then
# (new in v2) the Holm / floor-coupling / enumeration supplement transplanted
# from verify_section6.R §6.6.
#
# v2 changes (computation paths unchanged unless noted):
#   (1) Header test note corrected: from R 4.6, wilcox.test computes the exact
#       conditional distribution WITH ties (no warning; the v1 note predated
#       the 7 July resolution — this script's own output prints "Wilcoxon rank
#       sum exact test"). Two-sided p = twice the smaller one-sided tail,
#       verified against the full enumeration at the end of this script.
#   (2) Rank-biserial flipped to the manuscript's standardized convention:
#       positive = higher open-weight dispersion (r = .29 / .49 / .36 / .65).
#   (3) Stale expected values in section headers corrected
#       (.052 not .055; .021 not .024; median display .07 / .74 not .08 / .75).
#   (4) table2_check.csv write retained, with a provenance comment: the file
#       is the rounded (2dp) diff-target for the rendered Table 2 by design.
#   (5) §6.6 supplement appended (Holm, floor-coupling, doubled-tail
#       enumeration with a hard assertion on the §2.5 exactness claim).
#
# Run: Rscript verify_section3_stats_v2.R   (data_summaries auto-discovered;
#      or set AAI_DATA_DIR / pass the folder as the first argument).
# Requires R >= 4.6 and tidyverse. Hard assertions for every quantity live in
# verify_section6.R; this script prints against expected-value comments, per
# its original idiom. R's W is the U reported in §3.2 (first-listed group:
# flagship / Claude).
# ---------------------------------------------------------------------------

if (getRversion() < "4.6")
  stop("R >= 4.6 required: wilcox.test computes exact p-values with ties only ",
       "from 4.6; on older versions the same calls silently fall back to a ",
       "normal approximation and the expected values below will not reproduce.")

library(tidyverse)

# ---- data location (standard path block — identical across the R scripts) -----
# Reads inputs from, and writes outputs to, the `data_summaries` folder that sits
# next to `analysis_code/`. Resolution order:
#   (1) env var AAI_DATA_DIR  (2) first command-line arg  (3) a `data_summaries`
#   folder found next to this script or one level up. Nothing to edit to run from
#   inside analysis_code/ or from the project root; override only if you move data.
.aai_script_dir <- function() {
  fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(fa)) return(normalizePath(dirname(sub("^--file=", "", fa[[1]]))))
  for (i in seq_len(sys.nframe())) {
    of <- sys.frame(i)$ofile
    if (!is.null(of)) return(normalizePath(dirname(of)))
  }
  normalizePath(getwd())
}
.aai_find_data <- function() {
  env <- Sys.getenv("AAI_DATA_DIR", "")
  if (nzchar(env)) return(normalizePath(env))
  a <- commandArgs(trailingOnly = TRUE)
  if (length(a) >= 1 && nzchar(a[[1]])) return(normalizePath(a[[1]]))
  for (cand in c(file.path(.aai_script_dir(), "data_summaries"),
                 file.path(.aai_script_dir(), "..", "data_summaries"),
                 file.path(getwd(), "data_summaries"))) {
    if (dir.exists(cand)) return(normalizePath(cand))
  }
  stop("Could not find the 'data_summaries' folder. Set AAI_DATA_DIR, pass it as ",
       "the first argument, or run from inside the project.", call. = FALSE)
}
DATA_DIR <- .aai_find_data()
in_path  <- function(f) file.path(DATA_DIR, f)
out_path <- function(f) file.path(DATA_DIR, f)
message("[paths] data_summaries -> ", DATA_DIR)
# -------------------------------------------------------------------------------

sdo <- read_csv(in_path("attitude_sdo_by_model.csv"), show_col_types = FALSE)
rwa <- read_csv(in_path("attitude_rwa_by_model.csv"), show_col_types = FALSE)
con <- read_csv(in_path("attitude_consistency_by_model.csv"), show_col_types = FALSE)

d <- sdo |>
  inner_join(rwa |> select(m, RWA), by = "m") |>
  inner_join(con |> select(m, consistency_sdo, consistency_vsa), by = "m") |>
  mutate(
    access   = if_else(family %in% c("Claude", "GPT"), "flagship", "open-weight"),
    provider = if_else(family == "GPT", "Azure", "Databricks")
  )

stopifnot(nrow(d) == 24)   # 24 models, per Table 1

# Rank-biserial from U/W — v2: manuscript's standardized convention,
# positive = higher open-weight dispersion.
rb <- function(W, n1, n2) 1 - 2 * W / (n1 * n2)

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

cat("\n== §3.2 medians by access: SDO 0.06 vs 0.23; VSA 0.07 vs 0.74 ==\n")
d |> group_by(access) |>
  summarise(med_sdo = median(consistency_sdo),
            med_vsa = median(consistency_vsa), .groups = "drop") |> print()

cat("\n== §3.2 full-sample tests: SDO U = 45.5, p = .26; VSA U = 32.5, p = .052 ==\n")
w1 <- wilcox.test(consistency_sdo ~ access, data = d)
print(w1); cat("rank-biserial r =", round(rb(w1$statistic, 16, 8), 2), " (manuscript: 0.29)\n")
w2 <- wilcox.test(consistency_vsa ~ access, data = d)
print(w2); cat("rank-biserial r =", round(rb(w2$statistic, 16, 8), 2), " (manuscript: 0.49)\n")

cat("\n== §3.2 Databricks-restricted: SDO U = 23.0, p = .21, r = .36; VSA U = 12.5, p = .021, r = .65 ==\n")
db <- d |> filter(provider == "Databricks") |>
  mutate(grp = if_else(family == "Claude", "claude", "open-weight"))
stopifnot(sum(db$grp == "claude") == 9, sum(db$grp == "open-weight") == 8)
w3 <- wilcox.test(consistency_sdo ~ grp, data = db)
print(w3); cat("rank-biserial r =", round(rb(w3$statistic, 9, 8), 2), " (manuscript: 0.36)\n")
w4 <- wilcox.test(consistency_vsa ~ grp, data = db)
print(w4); cat("rank-biserial r =", round(rb(w4$statistic, 9, 8), 2), " (manuscript: 0.65)\n")

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
# Table 2 cross-check
# table2_check.csv is written here BY DESIGN at 2dp display precision: it is
# the diff-target for the rendered Table 2, not an analysis source (see
# DEPOSIT_README). The manuscript's tbl-results reads it for display; every
# statistic, everywhere, is computed from the full-precision files.
# ===========================================================================
d |>
  mutate(m = factor(m, levels = unname(unlist(orders)) |>
                      c("gpt-oss-20b","gpt-oss-120b","gemma-3-12b",
                        "claude-haiku-4-5") |> unique())) |>
  arrange(m) |>
  select(m, family, ecosystem, access, SDO, RWA,
         consistency_sdo, consistency_vsa) |>
  mutate(across(where(is.numeric), ~ round(.x, 2))) |>
  write_csv(out_path("table2_check.csv"))
cat("\nWrote", out_path("table2_check.csv"), "— diff against Table 2 in the manuscript.\n")

# ===========================================================================
# §3.2 supplement (transplanted 8 July 2026 from verify_section6.R §6.6):
# Holm across the four contrasts, floor-coupling, and the doubled-tail
# enumeration behind the §2.5 exactness claim.
# ===========================================================================
p4 <- c(full_sdo = w1$p.value, full_vsa = w2$p.value,
        restr_sdo = w3$p.value, restr_vsa = w4$p.value)
cat("\n== Holm across the four contrasts ==\n")
print(round(p.adjust(p4, method = "holm"), 4))
# Expected: full_sdo .4220 | full_vsa .1568 | restr_sdo .4220 | restr_vsa .0839
# Reported in §3.2 (B2): .42 / .157 / .42 / .084 — nothing survives correction.

cat("\n== Floor-coupling (mean vs dispersion), §3.2 ==\n")
cat(sprintf("rho(SDO mean, SDO dispersion) = %.4f  # expect .4549 -> reported .45\n",
            cor(d$SDO, d$consistency_sdo, method = "spearman")))
cat(sprintf("rho(VSA mean, VSA dispersion) = %.4f  # expect -.0577 -> reported -.06\n",
            cor(d$RWA, d$consistency_vsa, method = "spearman")))
perm_p_spearman <- function(x, y, B = 1e5) {
  obs  <- abs(cor(rank(x), rank(y)))
  sims <- replicate(B, abs(cor(rank(x), rank(sample(y)))))
  mean(sims >= obs - 1e-12)
}
set.seed(2026)
cat(sprintf("permutation p (seed 2026, B = 1e5) = %.5f  # expect ~ .026\n",
            perm_p_spearman(d$SDO, d$consistency_sdo)))

cat("\n== Exactness check: doubled-tail enumeration must equal wilcox.test ==\n")
enum_doubled <- function(y, in_g2) {
  r <- rank(y); n2 <- sum(in_g2)
  cs <- colSums(matrix(r[combn(length(y), n2)], nrow = n2))
  obs <- sum(r[in_g2])
  min(1, 2 * min(mean(cs >= obs - 1e-9), mean(cs <= obs + 1e-9)))
}
ow_l <- d$access == "open-weight"
cl_v <- d |> filter(family == "Claude")
ow_v <- d |> filter(access == "open-weight")
stopifnot(nrow(cl_v) == 9, nrow(ow_v) == 8)
ed <- c(full_sdo  = enum_doubled(d$consistency_sdo, ow_l),
        full_vsa  = enum_doubled(d$consistency_vsa, ow_l),
        restr_sdo = enum_doubled(c(cl_v$consistency_sdo, ow_v$consistency_sdo),
                                 rep(c(FALSE, TRUE), c(9, 8))),
        restr_vsa = enum_doubled(c(cl_v$consistency_vsa, ow_v$consistency_vsa),
                                 rep(c(FALSE, TRUE), c(9, 8))))
print(round(ed, 6))
# Expected: .260279 | .052271 | .211024 | .020979
# (.05227127 and .02097902 are the values named in §2.5 / B1.)
stopifnot(max(abs(p4 - ed)) < 1e-9)          # the §2.5 exactness claim, asserted
cat("enumeration == wilcox.test on all four contrasts (< 1e-9)\n")

cat("\n== sessionInfo(): paste the R version + package versions into §2.5 ==\n")
sessionInfo()
