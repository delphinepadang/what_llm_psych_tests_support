# ==============================================================================
# MS_AAI_ToM  |  04_reframing_checks.R
# Checks for the reframed paper: do the summary statistics used in the
# literature show that an individual model's answers reproduce across
# identical runs?
# ------------------------------------------------------------------------------
# Adds four analyses to 02_statistical_analysis.R (SDO/VSA, August 2026 data):
#
#   R1  Cross-check. Recomputes 02's headline numbers from the raw CSV and
#       compares them with the tables 02 wrote (Stats_tables/S01, S07, S13).
#   R2  Coefficient of variation (CV) of scale scores across runs: the
#       statistic Rozado (2024, PLOS ONE) used to argue that run-to-run
#       variability "is not too concerning" (median CV 8.03%). Computed under
#       the analysed coding and with the same scale shifted to start at 1 or 0,
#       next to the share of each model's items that changed.
#   R3  Per-model Krippendorff's alpha across runs (items = units, runs =
#       coders): the analogue of the intra-prompt stability score of Barrie,
#       Palaiologou & Tornberg (arXiv 2407.02039), read against their 0.80
#       benchmark, next to the share and size of item changes.
#   R4  One-shot test: would a single run have led to a different conclusion?
#       (a) side of the scale midpoint and third of the scale range,
#       (b) highest-scoring models and rank ranges across runs,
#       (c) the English-vs-Chinese SDO6 result re-estimated from each run alone.
#   R6  Manuscript Table 2 (summary statistics vs answers that changed) and
#       two figures: per-model alpha against items changed (FigR1) and each
#       model's rank in every run (FigR2), drawn with 03's design system.
#
# Data preparation is not re-implemented. The script borrows the definitions
# it needs (prep_sdo, reverse keys, scale bounds, exclusion constants,
# icc_suite, kripp_alpha, paired_suite, meta_re, meta_summary) from
# 02_statistical_analysis.R by parsing that file, so both scripts always
# prepare the data identically. Figures borrow 03's design system (fonts,
# colours, theme, save_fig) the same way. If a borrowed name changes in 02 or
# 03, this script stops and says which name it could not find.
#
# Inputs   Results/SDO_VSA_Results_20260830.csv
#          Analysis_script/02_statistical_analysis.R   (definitions borrowed)
#          Analysis_script/03_figures.R                (figure design borrowed)
#          Analysis_output/Stats_tables/S01_design.csv, S07_*.csv,
#            S13_language_meta.csv  (R1 compares against these; run 02 first)
# Outputs  Analysis_output/Reframing_checks/R1_*.csv ... R6_*.csv
#          Analysis_output/Reframing_checks/reframing_log.txt
#          Analysis_output/Figures/FigR1_*.pdf/.png, FigR2_*.pdf/.png
# Run      after 02 (and 03): Rscript Analysis_script/04_reframing_checks.R
#          or source() it in RStudio. Runtime: under a minute.
#          If it stops with an error, run close_log4() to release the log file.
# ==============================================================================


# ------------------------------------------------------------------------------
# 0. SETUP
# ------------------------------------------------------------------------------

PROJECT_ROOT <- path.expand("~/Documents/research_projects/AI Bias paper/MS_AAI_ToM")

# Same fallback as 02: the working directory, or its parent if run from inside
# Analysis_script/.
if (!dir.exists(PROJECT_ROOT)) {
  wd <- normalizePath(getwd())
  PROJECT_ROOT <- if (basename(wd) == "Analysis_script") dirname(wd) else wd
}

DIR_RESULTS <- file.path(PROJECT_ROOT, "Results")
DIR_TABLES  <- file.path(PROJECT_ROOT, "Analysis_output", "Stats_tables")
DIR_R4      <- file.path(PROJECT_ROOT, "Analysis_output", "Reframing_checks")
SCRIPT_02   <- file.path(PROJECT_ROOT, "Analysis_script", "02_statistical_analysis.R")
SCRIPT_03   <- file.path(PROJECT_ROOT, "Analysis_script", "03_figures.R")
DIR_FIG     <- file.path(PROJECT_ROOT, "Analysis_output", "Figures")

if (!dir.exists(DIR_RESULTS)) {
  stop("Cannot find the Results folder at: ", DIR_RESULTS,
       "\nEdit PROJECT_ROOT at the top of this script.", call. = FALSE)
}
if (!file.exists(SCRIPT_02)) {
  stop("Cannot find 02_statistical_analysis.R at: ", SCRIPT_02, call. = FALSE)
}
if (!dir.exists(DIR_R4)) dir.create(DIR_R4, recursive = TRUE)

# Reference values from the two papers whose stability arguments are tested here
ROZADO_MEDIAN_CV <- 8.03   # Rozado (2024): median CV of test scores over 10 retakes (%)
PSS_BENCHMARK    <- 0.80   # Barrie et al.: heuristic reference point for prompt stability
CHANGE_FLAGS     <- c(20, 30)  # % of a model's items that changed, used to flag cases

# Packages ---------------------------------------------------------------------
PKGS <- c("dplyr", "tidyr", "readr", "purrr", "stringr", "tibble", "metafor",
          "ggplot2", "ggrepel")
missing_pkgs <- PKGS[!vapply(PKGS, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs)) {
  message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs)
}
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(purrr)
  library(stringr); library(tibble)
})
options(width = 200, stringsAsFactors = FALSE, dplyr.summarise.inform = FALSE,
        scipen = 6)

# Borrow definitions from 02 ---------------------------------------------------
BORROW <- c("FILES", "SEED", "CI_LEVEL", "ALPHA", "N_RUNS", "MAX_API_FAIL",
            "MODEL_LOOKUP", "DEVELOPER_LEVELS", "make_model_meta",
            "classify_outcome", "read_results",
            "SCALE_BOUNDS", "REVERSE_KEYS", "SUBSCALE_MAP", "INSTR_LEVELS",
            "prep_sdo", "icc_suite", "kripp_alpha", "paired_suite",
            "meta_re", "meta_summary", "TIE_DIGITS", "tie_round")

#' Evaluate only the top-level assignments `name <- value` in a script whose
#' name is listed, in file order. Nothing else in that script runs (no data
#' loading, no models, no log, table or figure writing).
borrow_from <- function(file, names, env = globalenv()) {
  ex  <- parse(file, keep.source = FALSE)
  lhs <- vapply(ex, function(e) {
    is_assign <- is.call(e) &&
      (identical(e[[1]], as.name("<-")) || identical(e[[1]], as.name("=")))
    if (is_assign && is.name(e[[2]])) as.character(e[[2]]) else NA_character_
  }, character(1))
  absent <- setdiff(names, lhs)
  if (length(absent)) {
    stop("Not found as top-level assignments in ", basename(file), ": ",
         paste(absent, collapse = ", "),
         "\nUpdate this script's borrow list or use the current version of that script.",
         call. = FALSE)
  }
  for (i in which(lhs %in% names)) eval(ex[[i]], envir = env)
  invisible(names)
}
borrow_from(SCRIPT_02, BORROW)
set.seed(SEED)

# Logging and reporting helpers ------------------------------------------------
close_log4 <- function() {
  while (sink.number() > 0) sink()
  if (exists("LOG4_CON", envir = .GlobalEnv)) {
    con <- get("LOG4_CON", envir = .GlobalEnv)
    try(if (inherits(con, "connection") && isOpen(con)) close(con), silent = TRUE)
  }
  invisible(TRUE)
}
close_log4()
LOG4_FILE <- file.path(DIR_R4, "reframing_log.txt")
LOG4_CON  <- file(LOG4_FILE, open = "wt", encoding = "UTF-8")
sink(LOG4_CON, split = TRUE)

RULE_MAJOR <- strrep("=", 90)
RULE_MINOR <- strrep("-", 90)
section <- function(id, title) {
  cat("\n\n", RULE_MAJOR, "\n", id, ".  ", title, "\n", RULE_MAJOR, "\n", sep = "")
}
subhead <- function(title) cat("\n", title, "\n", RULE_MINOR, "\n", sep = "")
say  <- function(...) cat("  ", ..., "\n", sep = "")
note <- function(...) {
  txt <- paste0(..., collapse = "")
  cat(paste0("  ", strwrap(paste("Note.", txt), width = 86), collapse = "\n"), "\n", sep = "")
}
f2 <- function(x, d = 2) ifelse(is.na(x), "NA", formatC(x, format = "f", digits = d))
fp <- function(p) {
  ifelse(is.na(p), "p = NA",
         ifelse(p < .001, "p < .001", paste0("p = ", sub("^0", "", sprintf("%.3f", p)))))
}

#' Print a table to the log and write it to Reframing_checks/.
write_tab <- function(df, file, caption, digits = 3, max_rows = 60) {
  subhead(caption)
  out <- as.data.frame(df)
  num <- vapply(out, is.numeric, logical(1))
  out[num] <- lapply(out[num], function(x) round(x, digits))
  print(utils::head(out, max_rows), row.names = FALSE, right = FALSE)
  if (nrow(out) > max_rows) {
    cat("  ... ", nrow(out) - max_rows, " more rows in ", file, "\n", sep = "")
  }
  readr::write_csv(as.data.frame(df), file.path(DIR_R4, file), na = "")
  invisible(df)
}

cat(RULE_MAJOR, "\n", sep = "")
cat("MS_AAI_ToM  |  Reframing checks (04_reframing_checks.R)\n")
cat("Generated : ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n", sep = "")
cat("Root      : ", PROJECT_ROOT, "\n", sep = "")
cat("Data      : Results/", FILES$sdo_main, "\n", sep = "")
cat("Borrowed from 02: ", paste(BORROW, collapse = ", "), "\n", sep = "")
cat(RULE_MAJOR, "\n", sep = "")


# ------------------------------------------------------------------------------
# R0. DATA (same preparation and exclusion rule as 02)
# ------------------------------------------------------------------------------

section("R0", "Data (same preparation and exclusion rule as 02)")

sdo_raw <- prep_sdo(read_results(FILES$sdo_main), "Aug 2026 (5 repeats)")

api_share <- sdo_raw %>%
  group_by(model_label) %>%
  summarise(n_calls = n(), api_fail = mean(outcome == "api_error"), .groups = "drop")
excluded <- api_share$model_label[api_share$api_fail > MAX_API_FAIL]

MODELS <- make_model_meta(sdo_raw$model_label)

sdo_all <- sdo_raw %>% filter(!model_label %in% excluded)          # all calls
sdo_ok  <- sdo_all %>% filter(outcome == "scored") %>%               # scored only
  left_join(MODELS %>% select(model_label, display, developer, origin, open_weight),
            by = "model_label")
sdo_complete <- sdo_ok %>%                                           # pairs with all runs
  group_by(model_label, inst_lang, item_no) %>% filter(n() == N_RUNS) %>% ungroup()

say("Models analysed: ", n_distinct(sdo_ok$model_label),
    " (excluded for API failure above ", 100 * MAX_API_FAIL, "% of calls: ",
    if (length(excluded)) paste(MODELS$display[MODELS$model_label %in% excluded],
                                collapse = ", ") else "none", ")")
say("Scored responses: ", nrow(sdo_ok), "; model-item pairs with all ", N_RUNS, " runs: ",
    nrow(distinct(sdo_complete, model_label, inst_lang, item_no)))

# One row per model-item pair (five runs)
pairs <- sdo_complete %>%
  group_by(model_label, display, inst_lang, instrument, language, item_no, keying,
           scale_min, scale_max, span, midpoint) %>%
  summarise(n_values = n_distinct(value_key),
            range    = max(value_key) - min(value_key),
            mean_key = mean(value_key),
            at_floor = all(value_key == scale_min[1]),
            .groups = "drop") %>%
  mutate(changed = n_values > 1, change_2pt = range >= 2)

# One row per model x instrument x run: the scale score (mean of recoded items,
# items complete in all runs), as 02's full-scale level. tie_round() (from 02)
# makes models with identical scores tie on every computer: without extra
# (long double) precision two equal means can differ in the last binary digit,
# which turns ties into spurious rank reversals and midpoint flips.
scores <- sdo_complete %>%
  group_by(model_label, display, inst_lang, scale_min, scale_max, span, midpoint, run) %>%
  summarise(score = tie_round(mean(value_key)), n_items = n(), .groups = "drop")

# One row per model x instrument: agreement across runs and item changes
per_model <- sdo_complete %>%
  group_by(model_label, display, inst_lang) %>%
  group_modify(function(d, key) {
    M <- d %>% select(item_no, run, value_key) %>%
      pivot_wider(names_from = run, values_from = value_key) %>% arrange(item_no)
    M <- as.matrix(M[, -1])
    tibble(n_items        = nrow(M),
           all_identical  = length(unique(as.vector(M))) == 1,
           profile_icc    = icc_suite(M)$icc1,
           alpha_interval = kripp_alpha(M, "interval"),
           alpha_nominal  = kripp_alpha(M, "nominal"))
  }) %>% ungroup() %>%
  left_join(pairs %>%
              group_by(model_label, inst_lang) %>%
              summarise(n_changed      = sum(changed),
                        pct_changed    = 100 * mean(changed),
                        pct_change_2pt = 100 * mean(change_2pt),
                        pct_changed_by_1pt_only =
                          if (any(changed)) 100 * mean(range[changed] == 1) else NA_real_,
                        max_range      = max(range),
                        pct_at_floor   = 100 * mean(at_floor),
                        .groups = "drop"),
            by = c("model_label", "inst_lang"))


# ------------------------------------------------------------------------------
# R1. CROSS-CHECK AGAINST THE TABLES WRITTEN BY 02
# ------------------------------------------------------------------------------

section("R1", "Cross-check against the tables written by 02")
note("Each quantity is recomputed here from the raw CSV with this script's own ",
     "code (only the data-preparation rules are shared with 02) and compared ",
     "with the value in Stats_tables. 'match' means the two agree to 1e-6.")

read_02 <- function(file) {
  p <- file.path(DIR_TABLES, file)
  if (!file.exists(p)) {
    say("Missing ", file, " (run 02 first): this comparison is skipped.")
    return(NULL)
  }
  readr::read_csv(p, show_col_types = FALSE, progress = FALSE)
}

CHECKS <- list()
add_check <- function(quantity, instrument, value_02, value_04) {
  CHECKS[[length(CHECKS) + 1]] <<- tibble(
    quantity = quantity, instrument = instrument,
    value_02 = as.numeric(value_02), value_04 = as.numeric(value_04))
}

# (i) Design after exclusions: models and calls (S01) ---------------------------
s01 <- read_02("S01_design.csv")
if (!is.null(s01)) {
  d04 <- sdo_all %>% group_by(unit = as.character(inst_lang)) %>%
    summarise(models = n_distinct(model_label), calls = n(), .groups = "drop")
  cmp <- s01 %>% filter(dataset == "SDO/VSA (Aug 2026)") %>%
    inner_join(d04, by = "unit", suffix = c("_02", "_04"))
  for (i in seq_len(nrow(cmp))) {
    add_check("Models after exclusions", cmp$unit[i], cmp$models_02[i], cmp$models_04[i])
    add_check("Calls after exclusions", cmp$unit[i], cmp$calls_02[i], cmp$calls_04[i])
  }
}

# (ii) Model-item pairs that changed (S07 Table S7.1) ---------------------------
inst_04 <- bind_rows(
  pairs %>% group_by(instrument = as.character(inst_lang)) %>%
    summarise(n_pairs = n(), pct_changed = 100 * mean(changed),
              pct_change_2pt = 100 * mean(change_2pt), .groups = "drop"),
  pairs %>% summarise(instrument = "All SDO/VSA", n_pairs = n(),
                      pct_changed = 100 * mean(changed),
                      pct_change_2pt = 100 * mean(change_2pt)))
s07_1 <- read_02("S07_instability_by_instrument.csv")
if (!is.null(s07_1)) {
  cmp <- inner_join(s07_1, inst_04, by = "instrument", suffix = c("_02", "_04"))
  for (i in seq_len(nrow(cmp))) {
    add_check("Model-item pairs (n)", cmp$instrument[i], cmp$n_pairs_02[i], cmp$n_pairs_04[i])
    add_check("Pairs changed (%)", cmp$instrument[i], cmp$pct_changed_02[i], cmp$pct_changed_04[i])
    add_check("Pairs changed by 2+ points (%)", cmp$instrument[i],
              cmp$pct_change_2pt_02[i], cmp$pct_change_2pt_04[i])
  }
}

# (iii) Item-level ICC(1,1), raw vs recoded (S07 Table S7.6) --------------------
icc_04 <- map_dfr(INSTR_LEVELS, function(il) {
  dl <- sdo_complete %>% filter(inst_lang == il)
  Mr <- dl %>% select(model_label, item_no, run, value_raw) %>%
    pivot_wider(names_from = run, values_from = value_raw) %>% { as.matrix(.[, -(1:2)]) }
  Mk <- dl %>% select(model_label, item_no, run, value_key) %>%
    pivot_wider(names_from = run, values_from = value_key) %>% { as.matrix(.[, -(1:2)]) }
  tibble(instrument = il,
         icc_raw = icc_suite(Mr)$icc1, icc_recoded = icc_suite(Mk)$icc1,
         alpha_raw = kripp_alpha(Mr, "interval"), alpha_recoded = kripp_alpha(Mk, "interval"))
})
s07_6 <- read_02("S07_icc_raw_vs_recoded.csv")
if (!is.null(s07_6)) {
  cmp <- inner_join(s07_6, icc_04, by = "instrument")
  for (i in seq_len(nrow(cmp))) {
    add_check("Item-level ICC(1,1), raw responses", cmp$instrument[i],
              cmp$icc1_single_raw[i], cmp$icc_raw[i])
    add_check("Item-level ICC(1,1), reverse keys corrected", cmp$instrument[i],
              cmp$icc1_single_recoded[i], cmp$icc_recoded[i])
  }
}

# (iv) Full-scale reliability (S07 Table S7.2, level 3) -------------------------
scale_rel_04 <- map_dfr(INSTR_LEVELS, function(il) {
  M <- scores %>% filter(inst_lang == il) %>% select(model_label, run, score) %>%
    pivot_wider(names_from = run, values_from = score) %>% { as.matrix(.[, -1]) }
  tibble(instrument = il, icc1 = icc_suite(M)$icc1,
         pct_units_changed = 100 * mean(apply(M, 1, function(r) diff(range(r)) > 1e-9)))
})
s07_2 <- read_02("S07_reliability_by_aggregation_level.csv")
if (!is.null(s07_2)) {
  cmp <- s07_2 %>% filter(level == "3 full-scale score") %>%
    inner_join(scale_rel_04, by = "instrument", suffix = c("_02", "_04"))
  for (i in seq_len(nrow(cmp))) {
    add_check("Full-scale ICC(1,1)", cmp$instrument[i], cmp$icc1_02[i], cmp$icc1_04[i])
    add_check("Models whose scale score changed (%)", cmp$instrument[i],
              cmp$pct_units_changed_02[i], cmp$pct_units_changed_04[i])
  }
}

# (v) Rank-order agreement of scale scores between runs (S07 Table S7.3) --------
rank_04 <- map_dfr(INSTR_LEVELS, function(il) {
  S <- scores %>% filter(inst_lang == il) %>% select(model_label, run, score) %>%
    pivot_wider(names_from = run, values_from = score) %>% { as.matrix(.[, -1]) }
  rp <- utils::combn(ncol(S), 2)
  res <- apply(rp, 2, function(cc) {
    a <- S[, cc[1]]; b <- S[, cc[2]]
    sa <- sign(outer(a, a, "-")); sb <- sign(outer(b, b, "-"))
    up <- upper.tri(sa)
    c(disc = mean((sa * sb)[up] < 0),
      tau = suppressWarnings(stats::cor(a, b, method = "kendall")))
  })
  tibble(instrument = il, mean_tau = mean(res["tau", ]),
         pct_reversed = 100 * mean(res["disc", ]))
})
s07_3 <- read_02("S07_rank_stability.csv")
if (!is.null(s07_3)) {
  cmp <- inner_join(s07_3, rank_04, by = "instrument")
  for (i in seq_len(nrow(cmp))) {
    add_check("Mean Kendall tau between runs (scale scores)", cmp$instrument[i],
              cmp$mean_kendall_tau_between_runs[i], cmp$mean_tau[i])
    add_check("Model pairs reversed between runs (%)", cmp$instrument[i],
              cmp$pct_model_pairs_reversed[i], cmp$pct_reversed[i])
  }
}

# (vi) English vs Chinese meta-analysis on all runs (S13 Table S13.3) -----------
# Same construction as 02: per-item means over runs, ZH - EN per item, one
# effect per model (mean and SE of the item differences), REML + Knapp-Hartung.
# Item differences are passed through tie_round(), as in 02.
lang_effects <- function(d) {
  d %>% select(model_label, item_no, subscale, inst_lang, v) %>%
    pivot_wider(names_from = inst_lang, values_from = v) %>%
    mutate(dd = tie_round(`SDO6-ZH` - `SDO6-EN`)) %>%
    group_by(model_label) %>%
    summarise(k_items = sum(!is.na(dd)), yi = mean(dd, na.rm = TRUE),
              sei = sd(dd, na.rm = TRUE) / sqrt(k_items), .groups = "drop") %>%
    mutate(sei = if_else(sei > 0, sei, 0.5 / k_items))
}
lang_model_scores <- function(d) {      # model-level paired comparison, as 02
  d %>% group_by(model_label, language) %>%
    summarise(score = mean(value_key), .groups = "drop") %>%
    pivot_wider(names_from = language, values_from = score) %>%
    filter(!is.na(en), !is.na(zh))
}
sdo6_ok <- sdo_ok %>% filter(instrument == "SDO6")
st_all  <- sdo6_ok %>%
  group_by(model_label, inst_lang, item_no, subscale) %>%
  summarise(v = mean(value_key), .groups = "drop") %>% lang_effects()
ms_all  <- meta_summary(meta_re(st_all$yi, st_all$sei))
s13 <- read_02("S13_language_meta.csv")
if (!is.null(s13)) {
  add_check("ZH - EN meta-analysis: models (k)", "SDO6", s13$k[1], ms_all$k)
  add_check("ZH - EN meta-analysis: pooled shift", "SDO6", s13$estimate[1], ms_all$estimate)
  add_check("ZH - EN meta-analysis: CI lower", "SDO6", s13$ci_low[1], ms_all$ci_low)
  add_check("ZH - EN meta-analysis: CI upper", "SDO6", s13$ci_high[1], ms_all$ci_high)
}

checks <- bind_rows(CHECKS)
if (nrow(checks)) {
  checks <- checks %>% mutate(abs_diff = abs(value_02 - value_04), match = abs_diff < 1e-6)
  write_tab(checks, "R1_cross_check.csv",
            "Table R1.1. Headline numbers: 02's tables vs recomputed here", digits = 6, max_rows = 80)
}

# (vii) Per-model instability and profile ICC (S07 Table S7.4) ------------------
s07_4 <- read_02("S07_instability_by_model.csv")
if (!is.null(s07_4)) {
  pm_cmp <- s07_4 %>% select(display, inst_lang, n_pairs, pct_changed, profile_icc) %>%
    full_join(per_model %>% transmute(display, inst_lang = as.character(inst_lang),
                                      n_pairs_04 = n_items, pct_changed_04 = pct_changed,
                                      profile_icc_04 = profile_icc),
              by = c("display", "inst_lang")) %>%
    mutate(match = coalesce(n_pairs == n_pairs_04, FALSE) &
             coalesce(abs(pct_changed - pct_changed_04) < 1e-6, FALSE) &
             (coalesce(abs(profile_icc - profile_icc_04) < 1e-6, FALSE) |
                (is.na(profile_icc) & is.na(profile_icc_04))))
  readr::write_csv(pm_cmp, file.path(DIR_R4, "R1_cross_check_per_model.csv"), na = "")
  say("Per-model rows (Table S7.4) that match: ", sum(pm_cmp$match), " of ", nrow(pm_cmp),
      "  [details: R1_cross_check_per_model.csv]")
}
if (nrow(checks)) {
  n_bad <- sum(!checks$match)
  if (n_bad == 0) {
    say("All ", nrow(checks), " headline checks match 02's tables.")
  } else {
    say("WARNING: ", n_bad, " of ", nrow(checks), " headline checks do NOT match 02's tables.")
    print(as.data.frame(filter(checks, !match)), row.names = FALSE, right = FALSE)
    note("A mismatch usually means 02's tables are older than 02's code or data: ",
         "re-run 02, then this script. If only the VSA rank-stability rows differ, ",
         "the tables were written by a version of 02 without tie_round(): on computers ",
         "where R's long double has no extra precision (as on Apple-silicon Macs), VSA ",
         "scores that should tie then differ in the last binary digit and ties are ",
         "counted as reversals.")
  }
}


# ------------------------------------------------------------------------------
# R2. COEFFICIENT OF VARIATION OF SCALE SCORES (Rozado 2024)
# ------------------------------------------------------------------------------

section("R2", "Coefficient of variation of scale scores across runs (Rozado 2024)")
note("Rozado (2024) summarised run-to-run variability as the CV (SD / mean x 100) ",
     "of each model's test score across 10 retakes, median 8.03%, and concluded the ",
     "variability was 'not too concerning'. Here the CV is computed for each model's ",
     "scale score across its 5 runs. The SD does not depend on how the scale is ",
     "numbered but the mean does, so the same answers are scored three ways: as ",
     "analysed (SDO 1 to 7, VSA -4 to +4), shifted to start at 1, and shifted to ",
     "start at 0. The run-to-run SD as % of the scale range is an origin-free ",
     "alternative.")

cv_of <- function(s, m) ifelse(s == 0, 0, ifelse(m <= 0, NA_real_, 100 * s / m))

cv_model <- scores %>%
  group_by(model_label, display, inst_lang, scale_min, scale_max, span) %>%
  summarise(mean_score = mean(score), sd_score = sd(score), .groups = "drop") %>%
  mutate(cv_as_analysed = cv_of(sd_score, mean_score),                 # NA if mean <= 0
         cv_from_one    = cv_of(sd_score, mean_score - scale_min + 1),
         cv_from_zero   = cv_of(sd_score, mean_score - scale_min),
         sd_pct_range   = 100 * sd_score / span) %>%
  left_join(per_model %>% select(model_label, inst_lang, n_items, pct_changed, pct_change_2pt),
            by = c("model_label", "inst_lang"))

write_tab(cv_model %>% select(-model_label) %>% arrange(inst_lang, desc(pct_changed)),
          "R2_cv_by_model.csv",
          "Table R2.1. Scale-score CV across 5 runs, by model (three codings)")

cv_sum_fun <- function(d) {
  d %>% summarise(
    models                         = n(),
    # reported only when the CV is defined for every model (not for VSA's -4..+4 coding)
    median_cv_as_analysed          = if (any(is.na(cv_as_analysed))) NA_real_ else median(cv_as_analysed),
    models_cv_undefined_as_analysed = sum(is.na(cv_as_analysed)),
    median_cv_from_one             = median(cv_from_one, na.rm = TRUE),
    median_cv_from_zero            = median(cv_from_zero, na.rm = TRUE),
    max_cv_from_zero               = max(cv_from_zero, na.rm = TRUE),
    median_sd_pct_range            = median(sd_pct_range),
    models_cv_at_or_below_8.03     = sum(cv_from_one <= ROZADO_MEDIAN_CV, na.rm = TRUE),
    of_which_items_changed_20plus  = sum(cv_from_one <= ROZADO_MEDIAN_CV & pct_changed >= CHANGE_FLAGS[1], na.rm = TRUE),
    of_which_items_changed_30plus  = sum(cv_from_one <= ROZADO_MEDIAN_CV & pct_changed >= CHANGE_FLAGS[2], na.rm = TRUE),
    median_pct_items_changed       = median(pct_changed),
    max_pct_items_changed          = max(pct_changed))
}
cv_summary <- bind_rows(
  cv_model %>% group_by(instrument = as.character(inst_lang)) %>% cv_sum_fun() %>% ungroup(),
  cv_model %>% cv_sum_fun() %>% mutate(instrument = "All model-instrument combinations"))
write_tab(cv_summary, "R2_cv_summary.csv",
          "Table R2.2. Median CV by instrument, and models that look stable by CV yet changed items")
note("The 8.03% comparison uses the 1-based coding (identical to the analysed coding ",
     "for SDO; 1 to 9 for VSA, whose analysed coding is centred on 0 and can give a ",
     "zero or negative mean, which makes the CV undefined). Rozado's tests, his 10 ",
     "retakes and his randomised prompt wrappers differ from this design, so the ",
     "comparison shows how the statistic behaves; it does not replicate his numbers.")


# ------------------------------------------------------------------------------
# R3. PER-MODEL AGREEMENT ACROSS RUNS VS THE 0.80 BENCHMARK
# ------------------------------------------------------------------------------

section("R3", "Per-model agreement across runs vs the 0.80 benchmark (prompt stability score analogue)")
note("For each model and instrument, Krippendorff's alpha is computed with the ",
     "model's items as units and its 5 runs as coders, as the intra-prompt stability ",
     "score does for repeated runs of one prompt. Interval alpha is the primary ",
     "version (nominal alpha, which counts every change as a full disagreement, is ",
     "shown as a sensitivity check). alpha is undefined when every response is ",
     "identical: such a model is perfectly repeatable, but the coefficient cannot ",
     "say so.")

pm <- per_model %>%
  mutate(alpha_defined = !is.na(alpha_interval),
         passes_080    = alpha_defined & alpha_interval >= PSS_BENCHMARK)

write_tab(pm %>% select(display, inst_lang, n_items, all_identical, alpha_interval,
                        alpha_nominal, profile_icc, n_changed, pct_changed,
                        pct_changed_by_1pt_only, max_range, pct_at_floor) %>%
            arrange(inst_lang, desc(pct_changed)),
          "R3_agreement_by_model.csv",
          "Table R3.1. Per-model alpha across runs and item changes")

r3_sum_fun <- function(d) {
  d %>% summarise(
    combinations             = n(),
    alpha_undefined_all_same = sum(all_identical),
    alpha_defined            = sum(alpha_defined),
    pass_080                 = sum(passes_080),
    pass_and_changed_20plus  = sum(passes_080 & pct_changed >= CHANGE_FLAGS[1]),
    pass_and_changed_30plus  = sum(passes_080 & pct_changed >= CHANGE_FLAGS[2]),
    fail_but_changed_under20 = sum(alpha_defined & !passes_080 & pct_changed < CHANGE_FLAGS[1]),
    pass_080_nominal         = sum(!is.na(alpha_nominal) & alpha_nominal >= PSS_BENCHMARK),
    median_alpha_interval    = median(alpha_interval, na.rm = TRUE),
    median_alpha_nominal     = median(alpha_nominal, na.rm = TRUE))
}
r3_summary <- bind_rows(
  pm %>% group_by(instrument = as.character(inst_lang)) %>% r3_sum_fun() %>% ungroup(),
  pm %>% r3_sum_fun() %>% mutate(instrument = "All"))
write_tab(r3_summary, "R3_agreement_summary.csv",
          "Table R3.2. Models that pass the 0.80 benchmark, and how many of them still changed items")

flagged <- pm %>% filter(passes_080, pct_changed >= CHANGE_FLAGS[1]) %>%
  arrange(desc(pct_changed))
write_tab(flagged %>% select(display, inst_lang, n_items, alpha_interval, alpha_nominal,
                             n_changed, pct_changed, pct_changed_by_1pt_only, max_range),
          "R3_pass_but_changed.csv",
          paste0("Table R3.3. alpha >= ", PSS_BENCHMARK, " but ", CHANGE_FLAGS[1],
                 "%+ of items changed: how big were the changes?"))
if (nrow(flagged)) {
  ch <- pairs %>% semi_join(flagged, by = c("model_label", "inst_lang")) %>% filter(changed)
  say("In these ", nrow(flagged), " model-instrument combinations, ", nrow(ch),
      " item responses changed across runs: ", sum(ch$range == 1), " by 1 point only, ",
      sum(ch$range >= 2), " by 2+ points (largest change: ", max(ch$range), " points).")
}
note("fail_but_changed_under20 counts the opposite error: models below 0.80 even though ",
     "fewer than 20% of their items changed. This happens when a model's answers are ",
     "similar across items (little between-item spread), so a single change weighs heavily.")


# ------------------------------------------------------------------------------
# R4. ONE-SHOT TEST: WOULD A SINGLE RUN CHANGE THE CONCLUSION?
# ------------------------------------------------------------------------------

section("R4", "One-shot test: would a single run have led to a different conclusion?")
note("Each of the 5 runs is treated as if it were the only one a researcher had ",
     "collected. 02 already reports rank-order agreement between runs (Table S7.3); ",
     "this section adds classification, extremes and the language comparison.")

# (a) Classification: side of the midpoint and third of the scale range ---------
third_of <- function(score, lo, hi) {
  cut((score - lo) / (hi - lo), breaks = c(-Inf, 1/3, 2/3, Inf),
      labels = c("lower third", "middle third", "upper third"))
}
cls <- scores %>%
  mutate(side  = case_when(score > midpoint ~ "above midpoint",
                           score < midpoint ~ "below midpoint",
                           TRUE ~ "at midpoint"),
         third = as.character(third_of(score, scale_min, scale_max)))
cls_model <- cls %>%
  group_by(model_label, display, inst_lang) %>%
  summarise(min_score = min(score), max_score = max(score),
            sides = paste(sort(unique(side)), collapse = " / "),
            thirds = paste(sort(unique(third)), collapse = " / "),
            side_flips = n_distinct(side) > 1, third_flips = n_distinct(third) > 1,
            .groups = "drop")
write_tab(cls_model %>% filter(side_flips | third_flips) %>% select(-model_label),
          "R4a_classification_flips.csv",
          "Table R4.1. Models whose classification differs between runs")
cls_summary <- cls_model %>%
  group_by(instrument = as.character(inst_lang)) %>%
  summarise(models = n(),
            below_midpoint_every_run = sum(sides == "below midpoint"),
            above_midpoint_every_run = sum(sides == "above midpoint"),
            side_of_midpoint_flips   = sum(side_flips),
            third_of_range_flips     = sum(third_flips), .groups = "drop")
write_tab(cls_summary, "R4a_classification_summary.csv",
          "Table R4.2. Classification by a single run: how often it flips")

# (b) Highest-scoring model and rank ranges ---------------------------------------
ranked <- scores %>%
  group_by(inst_lang, run) %>%
  mutate(rank_high = rank(-score, ties.method = "min")) %>% ungroup()
extremes <- ranked %>%
  group_by(instrument = as.character(inst_lang), run) %>%
  summarise(top_model         = paste(sort(display[score == max(score)]), collapse = "; "),
            top_score         = max(score),
            runner_up_gap     = max(score) - sort(score, decreasing = TRUE)[2],
            models_tied_at_bottom = sum(score == min(score)),
            bottom_score      = min(score), .groups = "drop") %>%
  mutate(run = run + 1)
write_tab(extremes, "R4b_extremes_by_run.csv",
          "Table R4.3. Highest-scoring model in each run (and ties at the bottom)")
rank_span <- ranked %>%
  group_by(model_label, display, inst_lang, span) %>%
  summarise(best_rank = min(rank_high), worst_rank = max(rank_high),
            min_score = min(score), max_score = max(score), .groups = "drop") %>%
  mutate(rank_span = worst_rank - best_rank,
         score_range_pct = 100 * (max_score - min_score) / span) %>%
  select(-span)
rank_summary <- rank_span %>%
  group_by(instrument = as.character(inst_lang)) %>%
  summarise(models = n(),
            same_top_model_every_run = n_distinct(extremes$top_model[extremes$instrument == first(instrument)]) == 1,
            median_rank_span = median(rank_span), max_rank_span = max(rank_span),
            models_moving_3plus_places = sum(rank_span >= 3),
            models_moving_5plus_places = sum(rank_span >= 5),
            median_score_range_pct_of_movers_5plus =
              if (any(rank_span >= 5)) median(score_range_pct[rank_span >= 5]) else NA_real_,
            .groups = "drop")
write_tab(rank_summary, "R4b_rank_summary.csv",
          "Table R4.4. How far models move in the ranking between runs (rank 1 = highest score)")
write_tab(rank_span %>% select(-model_label) %>% arrange(inst_lang, desc(rank_span)),
          "R4b_rank_span_by_model.csv",
          "Table R4.5. Best and worst rank of each model across the 5 runs")
note("Ties share the best available rank, so models tied at the floor share the same ",
     "(lowest) rank; with many models at the floor, the 'least dominant model' is ",
     "usually a tie rather than a single model. Because most scale scores are bunched ",
     "near the floor, a small change in score can move a model several places: ",
     "score_range_pct gives the size of the score change (as % of the scale range) ",
     "behind each rank movement.")

# (c) English vs Chinese SDO6, re-estimated from each run alone -------------------
runs <- sort(unique(sdo6_ok$run))
lang_row <- function(d_long, label) {
  ls <- lang_model_scores(d_long)
  ps <- paired_suite(ls$zh - ls$en)
  st <- d_long %>% transmute(model_label, item_no, subscale, inst_lang, v = value_key) %>%
    group_by(model_label, inst_lang, item_no, subscale) %>%
    summarise(v = mean(v), .groups = "drop") %>% lang_effects()
  ms <- meta_summary(meta_re(st$yi, st$sei))
  tibble(analysis = label, models = ps$n,
         paired_mean_diff = ps$mean_diff, paired_ci_low = ps$ci_low,
         paired_ci_high = ps$ci_high, paired_p = ps$p_t, wilcoxon_p = ps$p_wilcoxon,
         models_higher_in_zh = ps$n_positive, models_lower_in_zh = ps$n_negative,
         meta_k = ms$k, meta_shift = ms$estimate, meta_ci_low = ms$ci_low,
         meta_ci_high = ms$ci_high, meta_p = ms$p, meta_I2 = ms$I2)
}
lang_by_run <- bind_rows(
  lang_row(sdo6_ok, "All 5 runs (as in 02)"),
  map_dfr(runs, function(r) lang_row(filter(sdo6_ok, run == r), paste0("Run ", r + 1, " only"))))
write_tab(lang_by_run, "R4c_language_by_run.csv",
          "Table R4.6. ZH - EN SDO6 shift (scale points), all runs vs each run on its own")
single <- filter(lang_by_run, analysis != "All 5 runs (as in 02)")

# Per-model direction of the language shift in each run
lang_sign <- map_dfr(runs, function(r) {
  lang_model_scores(filter(sdo6_ok, run == r)) %>%
    transmute(model_label, run = r + 1, diff = tie_round(zh - en))
}) %>%
  group_by(model_label) %>%
  summarise(runs = n(), runs_zh_higher = sum(diff > 0), runs_zh_lower = sum(diff < 0),
            runs_no_difference = sum(diff == 0), min_diff = min(diff), max_diff = max(diff),
            .groups = "drop") %>%
  mutate(same_direction_every_run = runs_zh_higher == runs | runs_zh_lower == runs) %>%
  left_join(MODELS %>% select(model_label, display), by = "model_label") %>%
  select(display, everything(), -model_label) %>%
  arrange(same_direction_every_run, desc(runs_zh_lower))
write_tab(lang_sign, "R4c_language_direction_by_model.csv",
          "Table R4.7. Direction of each model's ZH - EN difference in each single run")
say("ZH > EN and significant at alpha = ", ALPHA, ": meta-analysis in ",
    sum(single$meta_p < ALPHA & single$meta_shift > 0), " of ", nrow(single),
    " single runs; paired t-test in ", sum(single$paired_p < ALPHA & single$paired_mean_diff > 0),
    " of ", nrow(single), ".")
say("Single-run shift ranges from ", f2(min(single$meta_shift), 3), " to ",
    f2(max(single$meta_shift), 3), " (all runs: ", f2(lang_by_run$meta_shift[1], 3), ").")
say("Models whose ZH - EN difference has the same sign in all ", length(runs), " runs: ",
    sum(lang_sign$same_direction_every_run), " of ", nrow(lang_sign), ".")
note("The pooled language result can hold in every run while individual models' ",
     "differences change sign between runs; claims about a particular model need ",
     "more than one run.")


# ------------------------------------------------------------------------------
# R5. KEY NUMBERS FOR THE MANUSCRIPT
# ------------------------------------------------------------------------------

section("R5", "Key numbers for the manuscript")
all_cv <- filter(cv_summary, instrument == "All model-instrument combinations")
all_r3 <- filter(r3_summary, instrument == "All")
say("R1  Cross-check: ", if (nrow(checks)) paste0(sum(checks$match), " of ", nrow(checks),
                                                  " headline numbers match 02") else "skipped", ".")
say("R2  Median scale-score CV across all model-instrument combinations: ",
    f2(all_cv$median_cv_from_one), "% (1-based coding) vs ", f2(all_cv$median_cv_from_zero),
    "% (0-based coding) for the same answers. ", all_cv$models_cv_at_or_below_8.03,
    " combinations sit at or below Rozado's 8.03%; ", all_cv$of_which_items_changed_20plus,
    " of them changed 20%+ of their items.")
say("R3  ", all_r3$pass_080, " of ", all_r3$alpha_defined, " combinations with a defined alpha reach ",
    PSS_BENCHMARK, " (", all_r3$alpha_undefined_all_same, " more had identical answers throughout); ",
    all_r3$pass_and_changed_20plus, " of those passing changed 20%+ of items, ",
    all_r3$pass_and_changed_30plus, " changed 30%+.")
say("R4  Side of the midpoint flips for ", sum(cls_summary$side_of_midpoint_flips),
    " model-instrument combinations; third of the range flips for ",
    sum(cls_summary$third_of_range_flips), ". Highest-scoring model is the same in every run for ",
    sum(rank_summary$same_top_model_every_run), " of ", nrow(rank_summary), " instruments; ",
    sum(rank_summary$models_moving_5plus_places), " model-instrument combinations move 5+ places. ",
    "ZH > EN holds in ", sum(single$meta_p < ALPHA & single$meta_shift > 0), " of ", nrow(single),
    " single runs, but only ", sum(lang_sign$same_direction_every_run), " of ", nrow(lang_sign),
    " models keep the same direction in every run.")

# ------------------------------------------------------------------------------
# R6. MANUSCRIPT TABLE 2 AND FIGURES
# ------------------------------------------------------------------------------

section("R6", "Manuscript Table 2 and figures")

table2 <- tibble(instrument = INSTR_LEVELS) %>%
  left_join(icc_04 %>% select(instrument, icc_item_raw = icc_raw, icc_item_recoded = icc_recoded),
            by = "instrument") %>%
  left_join(scale_rel_04 %>% select(instrument, icc_scale = icc1), by = "instrument") %>%
  left_join(r3_summary %>% transmute(instrument, median_alpha_per_model = median_alpha_interval,
                                      pct_models_alpha_080 = 100 * pass_080 / alpha_defined),
            by = "instrument") %>%
  left_join(cv_summary %>% transmute(instrument, median_cv_1based = median_cv_from_one,
                                      median_cv_0based = median_cv_from_zero),
            by = "instrument") %>%
  left_join(inst_04 %>% transmute(instrument, n_pairs, pct_pairs_changed = pct_changed,
                                   pct_pairs_changed_2plus = pct_change_2pt),
            by = "instrument")
write_tab(table2, "R6_table2_summary_vs_change.csv",
          "Table R6.1 (manuscript Table 2). Summary statistics vs answers that changed, by instrument")

if (!file.exists(SCRIPT_03)) {
  say("03_figures.R not found, so the figures were skipped.")
} else {
  borrow_from(SCRIPT_03, c("DPI", "HAS_RAGG", "HAS_CAIRO", "pick_font", "FONT", "INK", "MUTED",
                           "GRID", "INSTR_COLS", "INSTR_LABEL", "theme_pub", "save_fig"))
  if (!dir.exists(DIR_FIG)) dir.create(DIR_FIG, recursive = TRUE)
  suppressPackageStartupMessages({ library(ggplot2); library(ggrepel) })
  theme_set(theme_pub())
  inst_fac <- function(x) {
    factor(unname(INSTR_LABEL[as.character(x)]), levels = unname(INSTR_LABEL[INSTR_LEVELS]))
  }

  # FigR1: per-model agreement against items changed -----------------------------
  r1_dat <- pm %>% filter(alpha_defined) %>%
    mutate(instrument = inst_fac(inst_lang),
           flagged = passes_080 & pct_changed >= CHANGE_FLAGS[1],
           label = if_else(flagged, display, ""))   # "" = no label, but labels avoid the point
  x_min <- min(0, floor(10 * min(r1_dat$alpha_interval)) / 10)
  x_breaks <- seq(-1, 1, 0.2); x_breaks <- x_breaks[x_breaks >= x_min - 1e-9]
  quad_lab <- tibble(instrument = factor(levels(r1_dat$instrument)[1], levels = levels(r1_dat$instrument)),
                     x = 1, y = 100,
                     txt = paste0("\u03b1 \u2265 ", sprintf("%.2f", PSS_BENCHMARK), " yet \u2265 ",
                                  CHANGE_FLAGS[1], "% of items changed"))
  fig_r1 <- ggplot(r1_dat, aes(alpha_interval, pct_changed)) +
    annotate("rect", xmin = PSS_BENCHMARK, xmax = Inf, ymin = CHANGE_FLAGS[1], ymax = Inf,
             fill = "grey95") +
    geom_vline(xintercept = PSS_BENCHMARK, colour = MUTED, linewidth = 0.35, linetype = "22") +
    geom_hline(yintercept = CHANGE_FLAGS[1], colour = MUTED, linewidth = 0.35, linetype = "22") +
    geom_point(data = function(d) filter(d, !flagged), colour = "grey65", size = 1.5, alpha = 0.7) +
    geom_point(data = function(d) filter(d, flagged), aes(colour = instrument), size = 2.1) +
    geom_text_repel(aes(label = label), size = 2.3, colour = INK, family = FONT, seed = SEED,
                    min.segment.length = 0, segment.colour = "grey60", segment.size = 0.25,
                    box.padding = 0.4, point.padding = 0.25, force = 3, max.overlaps = Inf) +
    geom_text(data = quad_lab, aes(x = x, y = y, label = txt), inherit.aes = FALSE,
              hjust = 1, vjust = 1, size = 2.3, colour = MUTED, family = FONT) +
    facet_wrap(~ instrument, nrow = 2) +
    scale_colour_manual(values = INSTR_COLS, guide = "none") +
    scale_x_continuous("Agreement across the five runs (Krippendorff's \u03b1, interval)",
                       limits = c(x_min, 1), breaks = x_breaks,
                       labels = function(x) sprintf("%.1f", x)) +
    scale_y_continuous("Items whose answer changed across the five runs (%)",
                       limits = c(0, 100), breaks = seq(0, 100, 25),
                       labels = function(x) paste0(x, "%"))
  save_fig(fig_r1, "FigR1_alpha_vs_items_changed", 7.2, 5.4)
  say("FigR1: ", nrow(r1_dat), " model-instrument combinations plotted; ", sum(!pm$alpha_defined),
      " with identical answers in every run (alpha undefined) are not shown.")

  # FigR2: each model's rank in each run ------------------------------------------
  r2_dat <- ranked %>%
    left_join(rank_span %>% select(model_label, inst_lang, rank_span),
              by = c("model_label", "inst_lang")) %>%
    group_by(model_label, inst_lang) %>% mutate(med_rank = median(rank_high)) %>% ungroup() %>%
    mutate(instrument = inst_fac(inst_lang), moved = rank_span >= 5,
           row_id = paste(display, inst_lang, sep = "___"),
           col_key = if_else(moved, as.character(instrument), "not moved"))
  row_levels <- r2_dat %>% distinct(row_id, instrument, med_rank, rank_span) %>%
    arrange(instrument, desc(med_rank), desc(rank_span)) %>% pull(row_id)
  r2_dat <- r2_dat %>% mutate(row_id = factor(row_id, levels = unique(row_levels)))
  seg <- r2_dat %>% group_by(row_id, instrument, col_key) %>%
    summarise(lo = min(rank_high), hi = max(rank_high), .groups = "drop")
  n_models <- n_distinct(r2_dat$model_label)
  # Direct label for the colour encoding, in the top-right corner of the first panel
  first_inst <- levels(r2_dat$instrument)[levels(r2_dat$instrument) %in% r2_dat$instrument][1]
  key_lab <- tibble(instrument = factor(first_inst, levels = levels(r2_dat$instrument)),
                    row_id = factor(tail(row_levels[row_levels %in% r2_dat$row_id[r2_dat$instrument == first_inst]], 1),
                                    levels = levels(r2_dat$row_id)),
                    txt = "Coloured: rank moved 5 or more\nplaces between runs")
  fig_r2 <- ggplot(r2_dat, aes(y = row_id)) +
    geom_segment(data = seg, aes(x = lo, xend = hi, yend = row_id, colour = col_key),
                 linewidth = 0.5) +
    geom_point(aes(x = rank_high, colour = col_key), size = 1.1) +
    geom_text(data = key_lab, aes(x = n_models, y = row_id, label = txt), inherit.aes = FALSE,
              hjust = 1, vjust = 1, size = 2.3, colour = MUTED, family = FONT, lineheight = 0.95) +
    facet_wrap(~ instrument, nrow = 2, scales = "free_y") +
    scale_colour_manual(values = c(INSTR_COLS, `not moved` = "grey70"), guide = "none") +
    scale_x_continuous(paste0("Rank among ", n_models, " models in each run (1 = highest score)"),
                       limits = c(1, n_models), breaks = c(1, seq(5, n_models, 5))) +
    scale_y_discrete(NULL, labels = function(x) sub("___.*$", "", x)) +
    theme(axis.text.y = element_text(size = rel(0.72)), panel.grid.major.y = element_blank())
  save_fig(fig_r2, "FigR2_rank_range_across_runs", 7.2, 8.4)
  say("FigR2: coloured rows are models whose rank moved 5 or more places between runs.")
}

cat("\n", RULE_MAJOR, "\n", sep = "")
cat("R version: ", R.version.string, "\n", sep = "")
cat("Outputs  : ", DIR_R4, "\n", sep = "")
cat(RULE_MAJOR, "\n", sep = "")
close_log4()
message("Done. Tables and log written to: ", DIR_R4)
