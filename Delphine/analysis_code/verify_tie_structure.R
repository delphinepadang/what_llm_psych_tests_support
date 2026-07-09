# verify_tie_structure.R — 9 July 2026
# ---------------------------------------------------------------------------
# WHY THIS SCRIPT EXISTS (the sixth precision-class artifact, and the deepest):
#
# The by-model construct means and dispersions carry MATHEMATICALLY EXACT ties.
# Every mean is a rational number (sum of integers / n); every flagship-median
# dispersion is an exact multiple of sqrt(0.2) (a single one-point deviation in
# five otherwise-identical integer responses gives per-item SD = sqrt(0.2)
# EXACTLY, regardless of which integers — SD is translation-invariant). So:
#
#   SDO dispersion, true tie groups:  0 (x8) | sqrt(.2)/8 (x5, all Claude)
#                                     | sqrt(.03)... = 0.0684653 (x3)
#                                     | 0.1677051 (x2: gpt-4o = 3*sqrt(.2)/8
#                                       and gpt-5.1 = sqrt(1.8)/8 — EQUAL,
#                                       since sqrt(1.8) = 3*sqrt(.2))
#   SDO mean, true tie groups:        1.375 (x2) | 1.45 (x2) | 1.725 (x3)
#   VSA mean, true tie groups:        five exact pairs (-1.8, -7/6, -1.1,
#                                     -2/3, -8/15)
#   VSA dispersion, true tie groups:  0 (x6) | sqrt(.2)/6 (x5)
#
# Exact rank statistics are DISCONTINUOUS in ties: the exact conditional
# Wilcoxon p and Spearman rho jump when a tie breaks. Any pipeline that
# recomputes these aggregates through a float path (R sd()/mean(), pandas)
# perturbs identical quantities by ~1 ulp via translation noise — e.g.
# sd(c(1,1,1,1,2)) and sd(c(3,3,3,3,4)) are mathematically equal but differ
# in the last bit — and each regeneration breaks the ties DIFFERENTLY. This
# is what happened between the 7 and 8 July file vintages, and it is why
# E52/E53/E55 chased moving targets:
#
#   quantity                     true value      8 July "correction" (artifact)
#   Holm, both SDO contrasts     .4112 -> .41    .42        (E52)
#   rho(SDO mean, SDO disp)      .455639 -> .46  .45        (E53)
#   rho(VSA mean, VSA disp)      -.067185 -> -.07  -.06     (E53)
#   permutation p (SDO coupling) .0264 -> .026   .027       (E55)
#
# All four revert to the 7 July originals, which were computed on tie-intact
# data. The four exact p-values and their display forms are UNCHANGED:
# .25905576 / .05227127 / .20559441 / .02097902 -> .26 / .052 / .21 / .021.
# (The deposit-vintage .260279/.211024 do not reproduce from the raw data
# under any tie treatment; they are file-vintage artifacts. U statistics are
# unchanged by the tie breaks — 45.5/32.5/23.0/12.5 — which is why no
# existing check caught this.)
#
# THE FIX used here and recommended everywhere ranks are taken:
#   round(x, 12) before ranking. Ulp noise is ~1e-17; the smallest TRUE gap
#   between distinct dispersion values is 0.01256 (printed below). Twelve
#   decimals is therefore ten orders of magnitude of safety on either side.
#
# Runtime: ~2-4 min (two C(24,8) enumerations + B = 4e6 permutation draws).
# Requires R >= 4.6 and tidyverse. All expectations verified 9 July 2026 in
# exact rational arithmetic (Python Fractions + 60-digit Decimal) and by
# doubled-tail enumeration on the raw item-level file.
# ---------------------------------------------------------------------------

if (getRversion() < "4.6")
  stop("R >= 4.6 required (exact wilcox.test with ties).")

suppressPackageStartupMessages(library(tidyverse))

# ---- data location (standard path block — identical across the R scripts) -----
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
in_path <- function(f) file.path(DATA_DIR, f)
message("[paths] data_summaries -> ", DATA_DIR)
# -------------------------------------------------------------------------------

.results <- list()
check <- function(label, cond) {
  ok <- isTRUE(cond)
  .results[[length(.results) + 1]] <<- list(label = label, ok = ok)
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", label))
  invisible(ok)
}

# ---- raw, keyed, tie-restored aggregates ---------------------------------------
raw <- read_csv(in_path("SDO_VSA_Results.csv"), show_col_types = FALSE,
                col_types = cols(parse_error = "c", raw_response = "c",
                                 .default = col_guess())) |>
  mutate(m = str_remove(model, "^databricks-"))
sdo_rev <- c("sdo-q3", "sdo-q4", "sdo-q7", "sdo-q8")
vsa_rev <- c("vsa-q1", "vsa-q4", "vsa-q5")
raw <- raw |>
  mutate(keyed = case_when(
    category == "sdo" & question_id %in% sdo_rev ~ 8 - value,
    category == "sdo" ~ value,
    category == "vsa" & question_id %in% vsa_rev ~ -value,
    TRUE ~ value))

TIE_DP <- 12   # tie-restoration precision: ulp noise ~1e-17 << 1e-12 << true gaps

agg <- raw |> filter(!is.na(keyed)) |>
  group_by(m, category, question_id) |>
  summarise(sd_i = sd(keyed), .groups = "drop") |>
  group_by(m, category) |>
  summarise(disp = round(mean(sd_i), TIE_DP), .groups = "drop") |>
  left_join(raw |> filter(!is.na(keyed)) |> group_by(m, category) |>
              summarise(mn = round(mean(keyed), TIE_DP), .groups = "drop"),
            by = c("m", "category"))
wide <- agg |>
  pivot_wider(names_from = category, values_from = c(disp, mn)) |>
  mutate(family = case_when(str_detect(m, "claude") ~ "Claude",
                            str_starts(m, "gpt-oss") ~ "GPT-OSS",
                            str_starts(m, "gpt") ~ "GPT",
                            str_detect(m, "qwen") ~ "Qwen",
                            str_detect(m, "gemma") ~ "Gemma",
                            str_detect(m, "llama") ~ "Llama"),
         access = if_else(family %in% c("Claude", "GPT"), "flagship", "open-weight")) |>
  arrange(m)   # E55 protocol: deterministic row order before any seeded draw
stopifnot(nrow(wide) == 24, sum(wide$access == "flagship") == 16)

# ---- 1. the true tie structure, demonstrated ------------------------------------
cat("\n---- 1. true tie structure (after 12-dp tie restoration) ----\n")
gap <- wide |> pull(disp_sdo) |> unique() |> sort() |> diff() |> min()
cat(sprintf("    smallest TRUE gap between distinct SDO dispersion values: %.5f\n", gap))
check("1.0 tie-restoration margin: min true gap > 1e-6 >> 1e-12 rounding", gap > 1e-6)

tie_n <- function(v) sort(as.integer(table(v)[table(v) > 1]))
check("1.1 SDO dispersion tie groups are {2, 3, 5, 8}",
      identical(tie_n(wide$disp_sdo), c(2L, 3L, 5L, 8L)))
check("1.2 VSA dispersion tie groups are {5, 6}",
      identical(tie_n(wide$disp_vsa), c(5L, 6L)))
check("1.3 SDO mean tie groups are {2, 2, 3}",
      identical(tie_n(wide$mn_sdo), c(2L, 2L, 3L)))
check("1.4 VSA mean tie groups are five exact pairs",
      identical(tie_n(wide$mn_vsa), c(2L, 2L, 2L, 2L, 2L)))
check("1.5 the radical-equal pair: gpt-4o (3*sqrt(.2)/8) == gpt-5.1 (sqrt(1.8)/8)",
      { d <- wide |> filter(m %in% c("gpt-4o", "gpt-5.1")) |> pull(disp_sdo)
        d[1] == d[2] && abs(d[1] - 3 * sqrt(0.2) / 8) < 1e-9 })

# ---- 2. the four exact tests on the TRUE structure -------------------------------
cat("\n---- 2. exact tests, Holm ----\n")
fl <- wide |> filter(access == "flagship");  ow <- wide |> filter(access == "open-weight")
cl <- wide |> filter(family == "Claude")
p4 <- c(
  full_sdo  = wilcox.test(fl$disp_sdo, ow$disp_sdo, exact = TRUE)$p.value,
  full_vsa  = wilcox.test(fl$disp_vsa, ow$disp_vsa, exact = TRUE)$p.value,
  restr_sdo = wilcox.test(cl$disp_sdo, ow$disp_sdo, exact = TRUE)$p.value,
  restr_vsa = wilcox.test(cl$disp_vsa, ow$disp_vsa, exact = TRUE)$p.value)
cat("    exact p (8dp): ", paste(sprintf("%s=%.8f", names(p4), p4), collapse = "  "), "\n")
check("2.1 true exact p = .25905576 / .05227127 / .20559441 / .02097902",
      max(abs(p4 - c(0.25905576, 0.05227127, 0.20559441, 0.02097902))) < 5e-8)
check("2.2 display forms unchanged: .26 / .052 / .21 / .021",
      round(p4[[1]], 2) == 0.26 && round(p4[[2]], 3) == 0.052 &&
        round(p4[[3]], 2) == 0.21 && round(p4[[4]], 3) == 0.021)

enum_doubled <- function(y, in_g2) {
  r <- rank(y); n2 <- sum(in_g2)
  cs <- colSums(matrix(r[combn(length(y), n2)], nrow = n2))
  obs <- sum(r[in_g2])
  min(1, 2 * min(mean(cs >= obs - 1e-9), mean(cs <= obs + 1e-9)))
}
ed <- c(
  full_sdo  = enum_doubled(c(fl$disp_sdo, ow$disp_sdo), rep(c(FALSE, TRUE), c(16, 8))),
  restr_sdo = enum_doubled(c(cl$disp_sdo, ow$disp_sdo), rep(c(FALSE, TRUE), c(9, 8))))
check("2.3 wilcox.test == doubled-tail enumeration on the two SDO contrasts (< 1e-9)",
      abs(p4[["full_sdo"]] - ed[["full_sdo"]]) < 1e-9 &&
        abs(p4[["restr_sdo"]] - ed[["restr_sdo"]]) < 1e-9)

holm <- p.adjust(p4, method = "holm")
cat("    Holm (6dp):    ", paste(sprintf("%s=%.6f", names(holm), holm), collapse = "  "), "\n")
check("2.4 Holm = .411189 / .156814 / .411189 / .083916 -> B2 carries .41, NOT .42",
      max(abs(holm - c(0.411189, 0.156814, 0.411189, 0.083916))) < 5e-7)
check("2.5 nothing survives correction (unchanged conclusion)", all(holm > 0.05))

# ---- 3. coupling (B3) on the TRUE structure ---------------------------------------
cat("\n---- 3. floor-coupling ----\n")
rho_sdo <- cor(wide$mn_sdo, wide$disp_sdo, method = "spearman")
rho_vsa <- cor(wide$mn_vsa, wide$disp_vsa, method = "spearman")
cat(sprintf("    rho(SDO mean, SDO disp) = %.6f | rho(VSA mean, VSA disp) = %.6f\n",
            rho_sdo, rho_vsa))
check("3.1 rho_SDO = .455639 -> B3 carries .46, NOT .45", abs(rho_sdo - 0.455639) < 5e-6)
check("3.2 rho_VSA = -.067185 -> B3 carries -.07, NOT -.06", abs(rho_vsa + 0.067185) < 5e-6)

perm_p <- function(x, y, B = 4e6, seed = 2026) {
  set.seed(seed)
  rx <- rank(x) - mean(rank(x)); ry <- rank(y) - mean(rank(y))
  obs <- abs(sum(rx * ry)); n <- length(ry); hits <- 0L
  for (b in seq_len(B))
    if (abs(sum(rx * ry[sample.int(n)])) >= obs - 1e-9) hits <- hits + 1L
  hits / B
}
pp <- perm_p(wide$mn_sdo, wide$disp_sdo)
cat(sprintf("    permutation p (m-sorted, seed 2026, B = 4e6) = %.5f\n", pp))
# Pooled reference on true ranks: .026443, MC SE 6.6e-5 (B = 6e6, 9 July) —
# 95% CI [.0263, .0266], below the .0265 display boundary: reports as .026.
check("3.3 permutation p within .0264 +/- .0006 -> B3 carries .026, NOT .027",
      abs(pp - 0.0264) < 6e-4)

# ---- 4. diagnostics: YOUR local by-model files -------------------------------------
cat("\n---- 4. file-vintage diagnostics (informational; no checks) ----\n")
diag_files <- c("attitude_consistency_by_model.csv",
                "attitude_sdo_by_model.csv", "attitude_rwa_by_model.csv")
if (all(file.exists(in_path(diag_files)))) {
  consf <- read_csv(in_path(diag_files[1]), show_col_types = FALSE)
  sdof  <- read_csv(in_path(diag_files[2]), show_col_types = FALSE)
  rwaf  <- read_csv(in_path(diag_files[3]), show_col_types = FALSE)
  probe <- function(vals, ms, label) {
    v <- vals[match(ms, names(vals))]
    cat(sprintf("    %-34s %s\n", label,
                if (length(unique(v)) == 1) "TIED (as it should be)"
                else sprintf("*** BROKEN into %d values (spread %.2e) ***",
                             length(unique(v)), diff(range(v)))))
  }
  cv <- setNames(consf$consistency_sdo, consf$m)
  probe(cv, c("claude-opus-4-7","claude-opus-4-8","claude-sonnet-4",
              "claude-sonnet-4-5","claude-sonnet-4-6"), "SDO disp 5-way (sqrt(.2)/8):")
  probe(cv, c("claude-haiku-4-5","gemma-3-12b","gpt-5.4"),   "SDO disp 3-way:")
  probe(cv, c("gpt-4o","gpt-5.1"),                            "SDO disp radical pair:")
  probe(setNames(consf$consistency_vsa, consf$m),
        c("claude-opus-4-5","claude-opus-4-6","claude-sonnet-4-5",
          "gpt-5.2","meta-llama-3-3-70b-instruct"),           "VSA disp 5-way (sqrt(.2)/6):")
  probe(setNames(sdof$SDO, sdof$m),
        c("claude-sonnet-4-5","claude-sonnet-4-6","gpt-5.2"), "SDO mean 3-way (1.725):")
  rv <- setNames(rwaf$RWA, rwaf$m)
  probe(rv, c("claude-opus-4-6","meta-llama-3-3-70b-instruct"), "VSA mean pair (-1.8):")
  probe(rv, c("claude-opus-4-8","meta-llama-3-1-8b-instruct"),  "VSA mean pair (-2/3):")
  probe(rv, c("gpt-4.1","qwen3-next-80b-a3b-instruct"),         "VSA mean pair (-1.1):")
  cat("    Any BROKEN line above means that file, used as an analysis source,\n",
      "    silently shifts the exact tests / Spearman ranks. Regenerate the\n",
      "    by-model CSVs with round(., 12) applied at write time (one mutate in\n",
      "    attitude_evolution.R), or keep computing from raw as this script does.\n", sep = "")
} else cat("    (by-model CSVs not found next to the raw file; skipped)\n")

# ---- tally ---------------------------------------------------------------------
fails <- sum(!vapply(.results, function(x) x$ok, logical(1)))
cat(sprintf("\n==== %d checks run, %d failed ====\n", length(.results), fails))
if (fails == 0) {
  cat("R side confirms the exact-arithmetic values. Next steps:\n",
      "  1. python3 apply_tie_corrections.py <path-to-qmd>   (B2 .42->.41; B3 .45->.46,\n",
      "     -.06->-.07, .027->.026, header note) — 5 assert-verified edits\n",
      "  2. Re-correct the deposit scripts' expectations (verify_section6.R v3: the\n",
      "     Holm-.42, rho-.45/-.06 and perm-window checks; verify_section3_stats_v2.R:\n",
      "     the .260279/.211024/.4220/.4549/-.0577 comments)\n",
      "  3. Regenerate the by-model CSVs tie-stably (round 12dp at write) before deposit;\n",
      "     note the convention in DEPOSIT_README\n", sep = "")
} else {
  stop("Some checks failed. If §1 fails, this raw CSV differs from the one verified ",
       "on 9 July — stop and reconcile raw-file provenance first.")
}
