# verify_section6.R — v3 (8 July 2026, late evening). pre_jie_worklist §6 in one session.
#
# v2 changes after the first real-data run:
#   (a) INPUT-SOURCE FIX: dispersion and construct scores now come from the
#       full-precision files (attitude_consistency/sdo/rwa_by_model.csv);
#       table2_check.csv turned out to store dispersions at 2dp DISPLAY
#       precision (the rendered-table check target, not an analysis source).
#       v1 ran 6.4/6.5/6.6 against it, producing five spurious fails.
#   (b) New check 6.0 documents that file relationship explicitly.
#   (c) B4 conventionalism expectation corrected to -1.10 (the -1.11 was a
#       rounded-input artifact; fixed in the qmd 8 July).
#   (d) Embeds the shared data_summaries path block; prints read_csv problems().
#
# v3 changes after the second real-data run (8 July, evening):
#   (e) Three expectations corrected — they were contaminated by run 1's rounded
#       file: the p-value check now targets the manuscript display values
#       (.26/.052/.21/.021; full precision .2603/.05227/.2110/.02098), and the
#       SDO Holm pair rounds to .42, not .41.
#   (f) Coupling expectations re-based on the deposit-state files, which
#       reconcile with the raw item-level data to machine precision:
#       rho = .4549 (-> .45) and -.0577 (-> -.06). The 7 July .456/-.067 came
#       from an earlier vintage of attitude_sdo/rwa_by_model.csv.
#   (g) New check 6.4b: construct-score files reconcile with raw.
#
# The [VERIFY-R] tags this script covers were already cleared in the qmd on
# 8 July after the first run; this v2 exists so the DEPOSIT carries a script
# whose asserts all pass. On all-PASS: transplant the 6.6 fold-in block into
# verify_section3_stats.R, take the 6.3 output to the OPTIONAL 4.1 decision
# with Kangning, and settle worklist 7.5 (table2_check regeneration).
#
# Usage: Rscript verify_section6.R   (data_summaries auto-discovered; or set
#        AAI_DATA_DIR / pass the folder as the first argument)
# Requires R >= 4.6 and tidyverse.

if (getRversion() < "4.6")
  stop("R >= 4.6 required: wilcox.test computes exact p-values with ties only ",
       "from 4.6; on older versions the same calls silently fall back to a ",
       "normal approximation and the expectations below will fail.")

suppressPackageStartupMessages(library(tidyverse))

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

# ---- check ledger -------------------------------------------------------------
.results <- list()
check <- function(label, cond) {
  ok <- isTRUE(cond)
  .results[[length(.results) + 1]] <<- list(label = label, ok = ok)
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", label))
  invisible(ok)
}

# ---- data ----------------------------------------------------------------------
# Item level: replicate the qmd setup chunk VERBATIM.
raw <- read_csv(in_path("SDO_VSA_Results.csv"), show_col_types = FALSE,
                col_types = cols(parse_error = "c", raw_response = "c",
                                 .default = col_guess()))
pr <- problems(raw)
if (nrow(pr) > 0) {
  cat("NOTE: read_csv reported parsing issues in SDO_VSA_Results.csv — inspect:\n")
  print(pr)
}
raw <- raw |> mutate(m = str_remove(model, "^databricks-"))

sdo_rev <- c("sdo-q3", "sdo-q4", "sdo-q7", "sdo-q8")
vsa_rev <- c("vsa-q1", "vsa-q4", "vsa-q5")
raw <- raw |>
  mutate(keyed = case_when(
    category == "sdo" & question_id %in% sdo_rev ~ 8 - value,
    category == "sdo" ~ value,
    category == "vsa" & question_id %in% vsa_rev ~ -value,
    TRUE ~ value))

# Model level: metadata from table2_check; ANALYSIS VALUES from the
# full-precision files (v2 input-source fix).
t2   <- read_csv(in_path("table2_check.csv"),                  show_col_types = FALSE)
cons <- read_csv(in_path("attitude_consistency_by_model.csv"), show_col_types = FALSE)
sdo  <- read_csv(in_path("attitude_sdo_by_model.csv"),         show_col_types = FALSE)
rwa  <- read_csv(in_path("attitude_rwa_by_model.csv"),         show_col_types = FALSE)

vsa_col <- intersect(c("RWA", "VSA", "rwa", "vsa", "rwa_mean", "vsa_mean",
                       "mean_rwa", "mean_vsa"), names(rwa))
stopifnot("RWA/VSA score column not found - run names(rwa) and add it to the candidate list" =
            length(vsa_col) == 1)

dat <- t2 |> select(m, family, ecosystem, access) |>
  left_join(cons |> select(m, consistency_sdo, consistency_vsa), by = "m") |>
  left_join(sdo  |> select(m, sdo_mean = SDO),                   by = "m") |>
  left_join(rwa  |> select(m, vsa_mean = all_of(vsa_col)),       by = "m")

stopifnot(setequal(unique(raw$m), dat$m), nrow(dat) == 24, !anyNA(dat))
stopifnot(sum(dat$access == "flagship") == 16, sum(dat$access == "open-weight") == 8)

# ================================================================================
# 6.0 — file relationship (the 8 July root cause, made explicit)
# ================================================================================
cat("\n---- 6.0 table2_check.csv vs full-precision files ----\n")
rel <- dat |> left_join(t2 |> select(m, t2_csdo = consistency_sdo, t2_cvsa = consistency_vsa,
                                     t2_SDO = SDO, t2_RWA = RWA), by = "m")
check("6.0 table2_check dispersion columns == round(full precision, 2)",
      max(abs(rel$t2_csdo - round(rel$consistency_sdo, 2)),
          abs(rel$t2_cvsa - round(rel$consistency_vsa, 2))) < 1e-9)
cat(sprintf("    score-column precision (informational): SDO max |t2 - full| = %.2e, |t2 - round2(full)| = %.2e\n",
            max(abs(rel$t2_SDO - rel$sdo_mean)), max(abs(rel$t2_SDO - round(rel$sdo_mean, 2)))))
cat(sprintf("                                            RWA max |t2 - full| = %.2e, |t2 - round2(full)| = %.2e\n",
            max(abs(rel$t2_RWA - rel$vsa_mean)), max(abs(rel$t2_RWA - round(rel$vsa_mean, 2)))))
# (Worklist 7.5: decide whether to regenerate table2_check.csv at full precision
#  or keep it, documented, as the rendered-table check target.)

# ================================================================================
# 6.1 — authoritarian-side share and per-item counts   [verified 8 July, run 1]
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
# 6.2 — VSA facet means (B4)   [verified 8 July; conventionalism -1.10]
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
for (i in seq_len(nrow(facets)))
  cat(sprintf("    %-16s %+.6f  (n = %d)\n", facets$facet[i], facets$mean_keyed[i], facets$n[i]))
fget <- function(f) facets$mean_keyed[facets$facet == f]
check("6.2 submission mean = -1.56 (B4)",      round(fget("submission"), 2)      == -1.56)
check("6.2 conventionalism mean = -1.10 (B4; -1.11 was a rounded-input artifact)",
      round(fget("conventionalism"), 2) == -1.10)
check("6.2 aggression mean = +0.22 (B4)",      round(fget("aggression"), 2)      ==  0.22)

n_agg_pos <- v |> left_join(facet_map, by = "question_id") |>
  filter(facet == "aggression") |> group_by(m) |>
  summarise(mk = mean(keyed), .groups = "drop") |> summarise(n = sum(mk > 0)) |> pull(n)
cat(sprintf("    (context for the B4 voice pass: %d of 24 models sit authoritarian-side\n",
            n_agg_pos), "    on the aggression facet)\n")

# ================================================================================
# 6.3 — SDO-D vs SDO-E subdimensions (OPTIONAL 4.1 / candidate B13 numbers)
# ================================================================================
cat("\n---- 6.3 SDO subdimensions ----\n")
s <- raw |> filter(category == "sdo")

avail <- s |> filter(!is.na(keyed)) |> count(question_id)
shortfall <- avail |> mutate(missing = 120L - n) |> filter(missing > 0)
print(shortfall)
check("6.3 exactly 4 missing SDO administrations, on q1 (x2), q2 (x1), q3 (x1) - all SDO-D",
      sum(shortfall$missing) == 4 &&
        identical(sort(shortfall$question_id), c("sdo-q1", "sdo-q2", "sdo-q3")) &&
        identical(shortfall$missing[order(shortfall$question_id)], c(2L, 1L, 1L)))

mrows <- raw |> filter(!is.na(parse_error))
check("6.3b parse_error marks exactly the four Table 4 administrations",
      nrow(mrows) == 4 && all(is.na(mrows$value)) &&
        identical(sort(paste(mrows$m, mrows$question_id, mrows$`repeat`)),
                  sort(c("qwen35-122b-a10b sdo-q1 1", "qwen35-122b-a10b sdo-q1 4",
                         "qwen35-122b-a10b sdo-q3 0", "gpt-oss-120b sdo-q2 3"))))

sub_map <- tibble(question_id = paste0("sdo-q", 1:8),
                  subdim = rep(c("SDO-D", "SDO-E"), each = 4))
sd_grand <- s |> filter(!is.na(keyed)) |>
  left_join(sub_map, by = "question_id") |>
  group_by(subdim) |>
  summarise(mean_keyed = mean(keyed), n = n(), .groups = "drop")
print(sd_grand)
check("6.3 grand means reproduce the 8 July run (D = 1.19, E = 2.08)",
      round(sd_grand$mean_keyed[sd_grand$subdim == "SDO-D"], 2) == 1.19 &&
        round(sd_grand$mean_keyed[sd_grand$subdim == "SDO-E"], 2) == 2.08)

sd_model <- s |> filter(!is.na(keyed)) |>
  left_join(sub_map, by = "question_id") |>
  group_by(m, subdim) |>
  summarise(mean_keyed = mean(keyed), .groups = "drop") |>
  pivot_wider(names_from = subdim, values_from = mean_keyed) |>
  mutate(gap_D_minus_E = `SDO-D` - `SDO-E`) |>
  arrange(desc(gap_D_minus_E))
print(sd_model, n = 24)
check("6.3 23 of 24 models score higher on E than D (candidate B13 claim)",
      sum(sd_model$`SDO-E` > sd_model$`SDO-D`) == 23)
cat(sprintf("    Spearman rho(D, E) across models = %.3f\n",
            cor(sd_model$`SDO-D`, sd_model$`SDO-E`, method = "spearman")))

# ================================================================================
# 6.4 — item-level reconciliation + B8   [verified 8 July via full-precision file]
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
  left_join(dat |> select(m, consistency_sdo, consistency_vsa), by = "m")
maxdiff <- max(abs(cmp$dispersion_sdo - cmp$consistency_sdo),
               abs(cmp$dispersion_vsa - cmp$consistency_vsa))
cat(sprintf("    max |recomputed - attitude_consistency_by_model| = %.2e\n", maxdiff))
check("6.4 item-level recomputation matches the full-precision dispersion file (< 1e-6)",
      maxdiff < 1e-6)

rec <- raw |> filter(!is.na(keyed)) |> group_by(m, category) |>
  summarise(mk = mean(keyed), .groups = "drop") |>
  pivot_wider(names_from = category, values_from = mk) |>
  left_join(dat |> select(m, sdo_mean, vsa_mean), by = "m")
check("6.4b construct-score files reconcile with the raw item-level data (< 1e-6)",
      max(abs(rec$sdo - rec$sdo_mean), abs(rec$vsa - rec$vsa_mean)) < 1e-6)

one_dev <- c(sdo = sqrt(0.2) / 8, vsa = sqrt(0.2) / 6)   # 0.0559017 / 0.0745356
fl <- dat |> filter(access == "flagship")
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
check("6.4/B8 ten model-scales sit at exactly the one-deviation value (8 July run)",
      nrow(tgt) == 10)

# ================================================================================
# 6.5 — display rounding of the four §3.2 medians (FULL PRECISION this time)
# ================================================================================
cat("\n---- 6.5 median display digits ----\n")
gm <- dat |> group_by(access) |>
  summarise(sdo = median(consistency_sdo), vsa = median(consistency_vsa), .groups = "drop")
for (i in seq_len(nrow(gm)))
  cat(sprintf("    %-11s SDO %.10f | VSA %.10f\n", gm$access[i], gm$sdo[i], gm$vsa[i]))
gv <- function(a, s) gm[[s]][gm$access == a]
check("6.5 SDO medians display as 0.06 / 0.23 under round(., 2)",
      round(gv("flagship", "sdo"), 2) == 0.06 && round(gv("open-weight", "sdo"), 2) == 0.23)
check("6.5 VSA medians display as 0.07 / 0.74 under round(., 2)",
      round(gv("flagship", "vsa"), 2) == 0.07 && round(gv("open-weight", "vsa"), 2) == 0.74)
owv <- gv("open-weight", "vsa")
if (abs(owv - 0.745) < 5e-4)
  cat("    NOTE: the open-weight VSA median sits at the .745 display boundary. R's round()\n",
      "    gives 0.74 (consistent with Table 2's own render-time rounding); strict half-up\n",
      "    hand-rounding gives 0.75. Voice-pass decision: keep 0.74 (consistent) or write\n",
      "    0.745 in §3.2 to sidestep the boundary entirely.\n", sep = "")

# ================================================================================
# 6.6 — FOLD-IN BLOCK for verify_section3_stats.R
#       (self-contained from here down given `dat`; copy this whole section)
# ================================================================================
cat("\n---- 6.6 fold-in: four exact tests, Holm, coupling ----\n")
fl_d <- dat |> filter(access == "flagship")
ow_d <- dat |> filter(access == "open-weight")
cl_d <- dat |> filter(family == "Claude")
stopifnot(nrow(cl_d) == 9, nrow(ow_d) == 8)

# R >= 4.6: exact with ties; two-sided p = twice the smaller one-sided tail (B1).
p4 <- c(
  full_sdo  = wilcox.test(fl_d$consistency_sdo, ow_d$consistency_sdo, exact = TRUE)$p.value,
  full_vsa  = wilcox.test(fl_d$consistency_vsa, ow_d$consistency_vsa, exact = TRUE)$p.value,
  restr_sdo = wilcox.test(cl_d$consistency_sdo, ow_d$consistency_sdo, exact = TRUE)$p.value,
  restr_vsa = wilcox.test(cl_d$consistency_vsa, ow_d$consistency_vsa, exact = TRUE)$p.value)
cat("    exact p (6dp): ", paste(sprintf("%s=%.6f", names(p4), p4), collapse = "  "), "\n")
check("6.6 p-values reproduce the reported .26 / .052 / .21 / .021 (manuscript display precision)",
      round(p4[["full_sdo"]], 2) == 0.26 && round(p4[["full_vsa"]], 3) == 0.052 &&
        round(p4[["restr_sdo"]], 2) == 0.21 && round(p4[["restr_vsa"]], 3) == 0.021)
check("6.6/B1 full-sample VSA p = .05227127 (the 7 July session value)",
      abs(p4[["full_vsa"]] - 0.05227127) < 5e-8)
check("6.6/B1 restricted VSA p = .02097902 (the 7 July session value)",
      abs(p4[["restr_vsa"]] - 0.02097902) < 5e-8)

holm <- p.adjust(p4, method = "holm")
cat("    Holm (6dp):    ", paste(sprintf("%s=%.6f", names(holm), holm), collapse = "  "), "\n")
check("6.6 Holm restr_vsa rounds to .084 (B2)", round(holm[["restr_vsa"]], 3) == 0.084)
check("6.6 Holm full_vsa rounds to .157 (B2; .156 was a double-rounding artifact)",
      round(holm[["full_vsa"]], 3) == 0.157)
check("6.6 both SDO Holm values round to .42 at 2dp (B2; the earlier .41 came from the rounded file)",
      round(holm[["full_sdo"]], 2) == 0.42 && round(holm[["restr_sdo"]], 2) == 0.42)
check("6.6 nothing survives correction (all Holm > .05)", all(holm > 0.05))
cat("    (If B2 should carry the SDO pair at 3dp, use the 6dp values above.)\n")

# Floor-coupling (B3): Spearman + seeded Monte-Carlo permutation p (B = 1e5).
rho_sdo <- cor(dat$sdo_mean, dat$consistency_sdo, method = "spearman")
rho_vsa <- cor(dat$vsa_mean, dat$consistency_vsa, method = "spearman")
cat(sprintf("    rho(SDO mean, SDO disp) = %.4f | rho(VSA mean, VSA disp) = %.4f\n",
            rho_sdo, rho_vsa))
check("6.6 SDO coupling rho rounds to .45 (B3; deposit-state files)",  round(rho_sdo, 2) ==  0.45)
check("6.6 VSA coupling rho rounds to -.06 (B3; deposit-state files)", round(rho_vsa, 2) == -0.06)

perm_p_spearman <- function(x, y, B = 1e5) {          # identical to additional_checks_v3
  obs  <- abs(cor(rank(x), rank(y)))
  sims <- replicate(B, abs(cor(rank(x), rank(sample(y)))))
  mean(sims >= obs - 1e-12)
}
set.seed(2026)
p_perm <- perm_p_spearman(dat$sdo_mean, dat$consistency_sdo)
cat(sprintf("    permutation p (seed 2026, B = 1e5) = %.5f  (observed .0262–.0265; B3 reports .026)\n",
            p_perm))
check("6.6 permutation p within tolerance of .026 (observed .0262-.0265 across runs/machines)",
      abs(p_perm - 0.0263) < 0.002)

# Full-enumeration evidence behind B1 ("verified against an independent
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
    full_sdo  = enum_doubled(dat$consistency_sdo, dat$access == "open-weight"),
    full_vsa  = enum_doubled(dat$consistency_vsa, dat$access == "open-weight"),
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
  cat("All \u00a76 expectations reproduced from the full-precision files.\n",
      "-> The qmd's [VERIFY-R] tags were already cleared 8 July; nothing further there.\n",
      "-> Transplant the 6.6 fold-in block into verify_section3_stats.R for the deposit.\n",
      "-> 6.3 output above = the OPTIONAL 4.1 / candidate-B13 numbers (with Kangning).\n",
      "-> Settle worklist 7.5: regenerate table2_check.csv at full precision, or keep\n",
      "   it documented as the rendered-table check target (see check 6.0).\n", sep = "")
} else {
  stop("Some expectations failed - investigate before relying on the affected values.")
}
