# verify_section6.R 
#
# Covers: 6.1 (11.7% authoritarian-side share + per-item counts), 6.2 (VSA facet
# means for B4), 6.3 (SDO-D/E subdimension means — decision support for
# OPTIONAL 4.1), 6.4 (item-level -> Table 2 reconciliation + B8 single-deviation
# check), 6.5 (display-rounding of the four §3.2 medians), 6.6 (fold-in block
# for verify_section3_stats.R: four exact tests, Holm, floor-coupling, plus the
# optional full-enumeration evidence behind B1).
#
# Usage, from the manuscript folder:   Rscript verify_section6.R [data_dir]
# data_dir defaults to "data", matching the qmd. Requires R >= 4.6, tidyverse.
#
# Data transforms below replicate the qmd setup chunk VERBATIM (same file, same
# key derivation, same reverse-keying) so this script verifies the manuscript's
# own pipeline, not a parallel one.
#
# If everything passes, the [VERIFY-R] tags at these four locations can be
# cleared: fig-vsa-items caption | DRAFT-VOICE A1 (11.7% + law-and-order
# concentration) | DRAFT-VOICE B4 (facet means) | DRAFT-VOICE B8 (medians).

if (getRversion() < "4.6")
  stop("R >= 4.6 required: wilcox.test computes exact p-values with ties only ",
       "from 4.6; on older versions the same calls silently fall back to a ",
       "normal approximation and the expectations below will fail.")

suppressPackageStartupMessages(library(tidyverse))

args <- commandArgs(trailingOnly = TRUE)
data_dir <- if (length(args) >= 1) args[1] else "data"

# ---- check ledger -------------------------------------------------------------
.results <- list()
check <- function(label, cond) {
  ok <- isTRUE(cond)
  .results[[length(.results) + 1]] <<- list(label = label, ok = ok)
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", label))
  invisible(ok)
}

# ---- data: replicate the qmd setup chunk --------------------------------------
raw <- read_csv(file.path(data_summaries, "SDO_VSA_Results.csv"), show_col_types = FALSE)
t2  <- read_csv(file.path(data_summaries, "table2_check.csv"),    show_col_types = FALSE)

raw <- raw |> mutate(m = str_remove(model, "^databricks-"))
stopifnot(setequal(unique(raw$m), t2$m))
stopifnot(sum(t2$access == "flagship") == 16, sum(t2$access == "open-weight") == 8)

sdo_rev <- c("sdo-q3", "sdo-q4", "sdo-q7", "sdo-q8")
vsa_rev <- c("vsa-q1", "vsa-q4", "vsa-q5")
raw <- raw |>
  mutate(keyed = case_when(
    category == "sdo" & question_id %in% sdo_rev ~ 8 - value,
    category == "sdo" ~ value,
    category == "vsa" & question_id %in% vsa_rev ~ -value,
    TRUE ~ value))

# ================================================================================
# 6.1 — authoritarian-side share and per-item counts (fig-vsa-items cap, A1)
# ================================================================================
cat("\n---- 6.1 authoritarian-side share ----\n")
v <- raw |> filter(category == "vsa", !is.na(keyed))
check("6.1 VSA administrations n = 720 (24 models x 6 items x 5 reps, no missing)",
      nrow(v) == 720)

share <- 100 * mean(v$keyed > 0)
cat(sprintf("    share = %.6f%%  (manuscript: 11.7%%)\n", share))
check("6.1 share rounds to 11.7%", round(share, 1) == 11.7)

item_tab <- v |>
  group_by(question_id) |>
  summarise(n = n(), auth_n = sum(keyed > 0),
            pct = 100 * mean(keyed > 0),
            mean_keyed = mean(keyed), .groups = "drop")
print(item_tab)
check("6.1 per-item authoritarian-side counts are {0, 2, 4, 5, 34, 39}",
      identical(sort(item_tab$auth_n), c(0L, 2L, 4L, 5L, 34L, 39L)))
lo <- item_tab |> filter(question_id %in% c("vsa-q5", "vsa-q6"))
check("6.1 law-and-order concentration: q5 + q6 carry 73 of the 84 (A1 claim)",
      sum(lo$auth_n) == 73 && sum(item_tab$auth_n) == 84)
check("6.1 item means match the rendered Appendix B values",
      identical(round(item_tab$mean_keyed[order(item_tab$question_id)], 2),
                c(-0.98, -2.14, -1.08, -1.13, 0.22, 0.21)))

# ================================================================================
# 6.2 — VSA facet means (B4)
# ================================================================================
cat("\n---- 6.2 VSA facet means ----\n")
facet_map <- tribble(
  ~question_id, ~facet,
  "vsa-q1", "submission",        # defy authority (R)
  "vsa-q2", "submission",        # discipline, follow leaders
  "vsa-q3", "conventionalism",   # God's laws
  "vsa-q4", "conventionalism",   # premarital sex (R)
  "vsa-q5", "aggression",        # no tougher government (R)
  "vsa-q6", "aggression")        # crack down on troublemakers

facets <- v |>
  left_join(facet_map, by = "question_id") |>
  group_by(facet) |>
  summarise(mean_keyed = mean(keyed), n = n(), .groups = "drop")
print(facets)
fget <- function(f) facets$mean_keyed[facets$facet == f]
check("6.2 submission mean = -1.56 (B4)",        round(fget("submission"), 2)      == -1.56)
check("6.2 conventionalism mean = -1.11 (B4)",   round(fget("conventionalism"), 2) == -1.11)
check("6.2 aggression mean = +0.22 (B4)",        round(fget("aggression"), 2)      ==  0.22)

n_agg_pos <- v |> left_join(facet_map, by = "question_id") |>
  filter(facet == "aggression") |> group_by(m) |>
  summarise(mk = mean(keyed), .groups = "drop") |> summarise(n = sum(mk > 0)) |> pull(n)
cat(sprintf("    (context for the B4 voice pass: %d of 24 models sit authoritarian-side\n",
            n_agg_pos), "    on the aggression facet)\n")

# ================================================================================
# 6.3 — SDO-D vs SDO-E subdimension means (decision support for OPTIONAL 4.1)
# ================================================================================
cat("\n---- 6.3 SDO subdimensions (no prior expectations; feeds OPTIONAL 4.1) ----\n")
s <- raw |> filter(category == "sdo")

# Missingness accounting first (ties to Table 4 / A3b: all four in SDO-D items)
avail <- s |> filter(!is.na(keyed)) |> count(question_id)
shortfall <- avail |> mutate(missing = 120L - n) |> filter(missing > 0)
print(shortfall)
check("6.3 exactly 4 missing SDO administrations, on q1 (x2), q2 (x1), q3 (x1) - all SDO-D",
      sum(shortfall$missing) == 4 &&
        identical(sort(shortfall$question_id), c("sdo-q1", "sdo-q2", "sdo-q3")) &&
        identical(shortfall$missing[order(shortfall$question_id)], c(2L, 1L, 1L)))

sub_map <- tibble(question_id = paste0("sdo-q", 1:8),
                  subdim = rep(c("SDO-D", "SDO-E"), each = 4))
sd_grand <- s |> filter(!is.na(keyed)) |>
  left_join(sub_map, by = "question_id") |>
  group_by(subdim) |>
  summarise(mean_keyed = mean(keyed), n = n(), .groups = "drop")
print(sd_grand)

sd_model <- s |> filter(!is.na(keyed)) |>
  left_join(sub_map, by = "question_id") |>
  group_by(m, subdim) |>
  summarise(mean_keyed = mean(keyed), .groups = "drop") |>
  pivot_wider(names_from = subdim, values_from = mean_keyed) |>
  mutate(gap_D_minus_E = `SDO-D` - `SDO-E`) |>
  arrange(desc(gap_D_minus_E))
print(sd_model, n = 24)
cat(sprintf("    Spearman rho(D, E) across models = %.3f;  models with E > D: %d of 24\n",
            cor(sd_model$`SDO-D`, sd_model$`SDO-E`, method = "spearman"),
            sum(sd_model$`SDO-E` > sd_model$`SDO-D`)))
cat("    (If OPTIONAL 4.1 becomes yes, these are the numbers the paragraph reports.)\n")

# ================================================================================
# 6.4 — item-level -> Table 2 reconciliation, and the B8 single-deviation check
# ================================================================================
cat("\n---- 6.4 dispersion reconciliation + B8 ----\n")
per_item_sd <- raw |> filter(!is.na(keyed)) |>
  group_by(m, category, question_id) |>
  summarise(sd_i = sd(keyed), n_i = n(), .groups = "drop")
disp_ms <- per_item_sd |>
  group_by(m, category) |>
  summarise(dispersion = mean(sd_i),
            items_varying = sum(sd_i > 1e-12), .groups = "drop")

cmp <- disp_ms |>
  pivot_wider(names_from = category, values_from = c(dispersion, items_varying)) |>
  left_join(t2 |> select(m, consistency_sdo, consistency_vsa), by = "m")
maxdiff <- max(abs(cmp$dispersion_sdo - cmp$consistency_sdo),
               abs(cmp$dispersion_vsa - cmp$consistency_vsa))
cat(sprintf("    max |recomputed - table2_check| = %.2e\n", maxdiff))
check("6.4 item-level recomputation matches table2_check.csv (< 1e-6)", maxdiff < 1e-6)
# If this FAILS only for qwen35-122b-a10b / gpt-oss-120b on the SDO, the
# difference is the missing-administration denominator convention - stop and
# reconcile before trusting anything downstream.

one_dev <- c(sdo = sqrt(0.2) / 8, vsa = sqrt(0.2) / 6)   # 0.0559017 / 0.0745356
fl <- t2 |> filter(access == "flagship")
med <- c(sdo = median(fl$consistency_sdo), vsa = median(fl$consistency_vsa))
cat(sprintf("    flagship medians: SDO %.10f (target %.10f)\n", med["sdo"], one_dev["sdo"]))
cat(sprintf("                      VSA %.10f (target %.10f)\n", med["vsa"], one_dev["vsa"]))
check("6.4/B8 flagship SDO median == sqrt(0.2)/8", abs(med["sdo"] - one_dev["sdo"]) < 1e-6)
check("6.4/B8 flagship VSA median == sqrt(0.2)/6", abs(med["vsa"] - one_dev["vsa"]) < 1e-6)

tgt <- disp_ms |>
  filter((category == "sdo" & abs(dispersion - one_dev["sdo"]) < 1e-6) |
         (category == "vsa" & abs(dispersion - one_dev["vsa"]) < 1e-6))
pat_ok <- nrow(tgt) > 0
for (i in seq_len(nrow(tgt))) {
  mi <- tgt$m[i]; ci <- tgt$category[i]
  vi <- per_item_sd |> filter(m == mi, category == ci, sd_i > 1e-12)
  if (nrow(vi) != 1) {
    cat(sprintf("    %s / %s: %d varying items - PATTERN MISMATCH\n", mi, toupper(ci), nrow(vi)))
    pat_ok <- FALSE
    next
  }
  vals <- raw |> filter(m == mi, category == ci,
                        question_id == vi$question_id[1], !is.na(keyed)) |> pull(keyed)
  tb <- table(vals)
  ok <- length(tb) == 2 && setequal(as.integer(tb), c(1L, 4L)) &&
        diff(range(as.numeric(names(tb)))) == 1
  cat(sprintf("    %s / %s: item %s, keyed responses {%s} -> %s\n",
              mi, toupper(ci), vi$question_id[1], paste(sort(vals), collapse = ","),
              if (ok) "single one-point deviation" else "PATTERN MISMATCH"))
  pat_ok <- pat_ok && ok
}
check("6.4/B8 every model-scale at the one-deviation value shows the 4+1 adjacent pattern",
      pat_ok)

# ================================================================================
# 6.5 — display rounding of the four §3.2 medians
# ================================================================================
cat("\n---- 6.5 median display digits ----\n")
gm <- t2 |> group_by(access) |>
  summarise(sdo = median(consistency_sdo), vsa = median(consistency_vsa), .groups = "drop")
for (i in seq_len(nrow(gm)))
  cat(sprintf("    %-11s SDO %.10f | VSA %.10f\n", gm$access[i], gm$sdo[i], gm$vsa[i]))
gv <- function(a, s) gm[[s]][gm$access == a]
check("6.5 SDO medians display as 0.06 / 0.23 under round(., 2)",
      round(gv("flagship", "sdo"), 2) == 0.06 && round(gv("open-weight", "sdo"), 2) == 0.23)
check("6.5 VSA medians display as 0.07 / 0.74 under round(., 2)",
      round(gv("flagship", "vsa"), 2) == 0.07 && round(gv("open-weight", "vsa"), 2) == 0.74)
# If the 0.74 check fails, §3.2's "0.74" changes to the printed value - that is
# the whole point of this item; the full-precision print above settles it.

# ================================================================================
# 6.6 — FOLD-IN BLOCK for verify_section3_stats.R
#       (self-contained from here down given t2; copy this whole section)
# ================================================================================
cat("\n---- 6.6 fold-in: four exact tests, Holm, coupling ----\n")
fl_d <- t2 |> filter(access == "flagship")
ow_d <- t2 |> filter(access == "open-weight")
cl_d <- t2 |> filter(family == "Claude")
stopifnot(nrow(cl_d) == 9, nrow(ow_d) == 8)

# R >= 4.6: exact with ties; two-sided p = twice the smaller one-sided tail (B1).
p4 <- c(
  full_sdo  = wilcox.test(fl_d$consistency_sdo, ow_d$consistency_sdo, exact = TRUE)$p.value,
  full_vsa  = wilcox.test(fl_d$consistency_vsa, ow_d$consistency_vsa, exact = TRUE)$p.value,
  restr_sdo = wilcox.test(cl_d$consistency_sdo, ow_d$consistency_sdo, exact = TRUE)$p.value,
  restr_vsa = wilcox.test(cl_d$consistency_vsa, ow_d$consistency_vsa, exact = TRUE)$p.value)
cat("    exact p (6dp): ", paste(sprintf("%s=%.6f", names(p4), p4), collapse = "  "), "\n")
check("6.6 p-values reproduce the reported .259 / .052 / .206 / .021 at 3dp",
      identical(unname(round(p4, 3)), c(0.259, 0.052, 0.206, 0.021)))

holm <- p.adjust(p4, method = "holm")
cat("    Holm (6dp):    ", paste(sprintf("%s=%.6f", names(holm), holm), collapse = "  "), "\n")
check("6.6 Holm restr_vsa rounds to .084 (B2)", round(holm[["restr_vsa"]], 3) == 0.084)
check("6.6 Holm full_vsa rounds to .157 (B2; .156 was a double-rounding artifact)",
      round(holm[["full_vsa"]], 3) == 0.157)
check("6.6 both SDO Holm values round to .41 at 2dp (B2 keeps these at 2dp)",
      round(holm[["full_sdo"]], 2) == 0.41 && round(holm[["restr_sdo"]], 2) == 0.41)
check("6.6 nothing survives correction (all Holm > .05)", all(holm > 0.05))
cat("    (If B2 should carry the SDO pair at 3dp, use the 6dp values above.)\n")

# Floor-coupling (B3): Spearman + seeded Monte-Carlo permutation p (B = 1e5).
rho_sdo <- cor(t2$SDO, t2$consistency_sdo, method = "spearman")
rho_vsa <- cor(t2$RWA, t2$consistency_vsa, method = "spearman")
cat(sprintf("    rho(SDO mean, SDO disp) = %.4f | rho(VSA mean, VSA disp) = %.4f\n",
            rho_sdo, rho_vsa))
check("6.6 SDO coupling rho rounds to .46 (B3)",  round(rho_sdo, 2) ==  0.46)
check("6.6 VSA coupling rho rounds to -.07 (B3)", round(rho_vsa, 2) == -0.07)

perm_p_spearman <- function(x, y, B = 1e5) {          # identical to additional_checks_v2
  obs  <- abs(cor(rank(x), rank(y)))
  sims <- replicate(B, abs(cor(rank(x), rank(sample(y)))))
  mean(sims >= obs - 1e-12)
}
set.seed(2026)
p_perm <- perm_p_spearman(t2$SDO, t2$consistency_sdo)
cat(sprintf("    permutation p (seed 2026, B = 1e5) = %.4f  (B3 reports .026)\n", p_perm))
check("6.6 permutation p within Monte-Carlo tolerance of .026", abs(p_perm - 0.026) < 0.003)

# Optional: full-enumeration evidence behind B1 ("verified against an independent
# full-enumeration implementation"). ~30-60 s for the two full-sample calls.
run_enumeration <- TRUE
if (run_enumeration) {
  enum_doubled <- function(y, in_g2) {                # base R's two-sided convention
    r <- rank(y); n2 <- sum(in_g2)
    cs <- colSums(matrix(r[combn(length(y), n2)], nrow = n2))
    obs <- sum(r[in_g2])
    min(1, 2 * min(mean(cs >= obs - 1e-9), mean(cs <= obs + 1e-9)))
  }
  ed <- c(
    full_sdo  = enum_doubled(t2$consistency_sdo, t2$access == "open-weight"),
    full_vsa  = enum_doubled(t2$consistency_vsa, t2$access == "open-weight"),
    restr_sdo = enum_doubled(c(cl_d$consistency_sdo, ow_d$consistency_sdo),
                             rep(c(FALSE, TRUE), c(9, 8))),
    restr_vsa = enum_doubled(c(cl_d$consistency_vsa, ow_d$consistency_vsa),
                             rep(c(FALSE, TRUE), c(9, 8))))
  cat("    enumeration:   ", paste(sprintf("%s=%.6f", names(ed), ed), collapse = "  "), "\n")
  check("6.6/B1 wilcox.test == doubled-tail enumeration (all four, < 1e-9)",
        max(abs(p4 - ed)) < 1e-9)
}

# ---- tally ---------------------------------------------------------------------
fails <- sum(!vapply(.results, function(x) x$ok, logical(1)))
cat(sprintf("\n==== %d checks run, %d failed ====\n", length(.results), fails))
if (fails == 0) {
  cat("All §6 expectations reproduced from the item-level file.\n",
      "-> Clear the [VERIFY-R] tags at: fig-vsa-items caption; DRAFT-VOICE A1 (x2);\n",
      "   DRAFT-VOICE B4; DRAFT-VOICE B8.\n",
      "-> Transplant the 6.6 fold-in block into verify_section3_stats.R for the deposit.\n",
      "-> 6.3 output above is the decision material for OPTIONAL 4.1 (with Kangning).\n", sep = "")
} else {
  stop("Some expectations failed - investigate before clearing any [VERIFY-R] tag.")
}
