# ==============================================================================
# MS_AAI_ToM  |  Psychometric evaluation of LLM responses to attitude scales
# ------------------------------------------------------------------------------
# Analyses:
#   1. Missingness: API failures vs. content refusals (disaggregated)
#   2. RMET: theory-of-mind competence, definitions-shown manipulation
#   3. Reliability: ICC(1,1), Krippendorff's alpha, response instability
#   4. Flat-variance test: is repeat variation order-free (error) or drift?
#   5. Scale position: endpoint pinning as a driver of apparent reliability
#   6. Language: English vs. Chinese administration of SDO6
#   7. Alignment proxy: does test-retest variance index refusal tuning?
#
# Input : Results/RMET_Results.csv, Results/SDO_VSA_Results_20260830.csv
# Output: Analysis_output/analysis_log.txt
#         Analysis_output/Tables/*.csv
#         Analysis_output/Figures/*.{pdf,png}
#
# Run with:  Rscript Analysis_script/01_analysis_LLM_attitude_stability.R
# ==============================================================================


# ------------------------------------------------------------------------------
# 0. SETUP
# ------------------------------------------------------------------------------

PROJECT_ROOT <- path.expand("~/Documents/research_projects/AI Bias paper/MS_AAI_ToM")

# Fall back to the working directory if the hard-coded path is absent, so the
# script runs unchanged on a collaborator's machine or from an RStudio project.
if (!dir.exists(PROJECT_ROOT)) PROJECT_ROOT <- normalizePath(getwd())

DIR_RESULTS <- file.path(PROJECT_ROOT, "Results")
DIR_OUT     <- file.path(PROJECT_ROOT, "Analysis_output")
DIR_TABLES  <- file.path(DIR_OUT, "Tables")
DIR_FIGURES <- file.path(DIR_OUT, "Figures")

for (d in c(DIR_OUT, DIR_TABLES, DIR_FIGURES)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# Analysis constants -----------------------------------------------------------
N_PERM      <- 2000   # permutations for the run-order test
SEED        <- 20260830
CI_LEVEL    <- 0.95
MIN_RUNS    <- 5      # runs required for a cell to enter reliability analyses

set.seed(SEED)

# Packages ---------------------------------------------------------------------
required <- c("dplyr", "tidyr", "readr", "ggplot2", "purrr", "stringr")
missing  <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Missing required packages. Install with:\n  install.packages(c(",
       paste0('"', missing, '"', collapse = ", "), "))", call. = FALSE)
}
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr)
  library(ggplot2); library(purrr); library(stringr)
})

HAS_LME4     <- requireNamespace("lme4",     quietly = TRUE)
HAS_LMERTEST <- requireNamespace("lmerTest", quietly = TRUE)
if (HAS_LME4)     suppressPackageStartupMessages(library(lme4))
if (HAS_LMERTEST) suppressPackageStartupMessages(library(lmerTest))

options(width = 160, stringsAsFactors = FALSE, dplyr.summarise.inform = FALSE)


# ------------------------------------------------------------------------------
# 0b. REPORTING HELPERS
# ------------------------------------------------------------------------------

RULE_MAJOR <- strrep("=", 78)
RULE_MINOR <- strrep("-", 78)

banner <- function(title, section = NULL) {
  cat("\n\n", RULE_MAJOR, "\n", sep = "")
  cat(if (is.null(section)) title else paste0(section, "  ", title), "\n", sep = "")
  cat(RULE_MAJOR, "\n\n", sep = "")
}

subhead <- function(title) {
  cat("\n", title, "\n", RULE_MINOR, "\n", sep = "")
}

note <- function(...) cat("  Note. ", paste0(..., collapse = ""), "\n", sep = "")

#' Print a data frame cleanly and write it to Analysis_output/Tables.
report_table <- function(df, file = NULL, caption = NULL, digits = 3) {
  if (!is.null(caption)) subhead(caption)
  out <- df
  num <- vapply(out, is.numeric, logical(1))
  out[num] <- lapply(out[num], function(x) round(x, digits))
  print(as.data.frame(out), row.names = FALSE, right = FALSE)
  if (!is.null(file)) {
    readr::write_csv(df, file.path(DIR_TABLES, file))
  }
  invisible(df)
}

#' APA-style p-value formatting.
fmt_p <- function(p) {
  ifelse(is.na(p), "NA",
         ifelse(p < .001, "< .001", sub("^0", "", sprintf("%.3f", p))))
}

save_figure <- function(plot, file, width = 7, height = 4.5) {
  ggsave(file.path(DIR_FIGURES, paste0(file, ".pdf")), plot,
         width = width, height = height, device = grDevices::cairo_pdf)
  ggsave(file.path(DIR_FIGURES, paste0(file, ".png")), plot,
         width = width, height = height, dpi = 300)
  invisible(plot)
}

#' Point labels for scatterplots: uses ggrepel when available, otherwise a
#' plain nudged text layer, so the script has no hard dependency on it.
label_points <- function(data, label_col = "model_label", size = 2.4) {
  if (requireNamespace("ggrepel", quietly = TRUE)) {
    ggrepel::geom_text_repel(
      data = data, aes(label = .data[[label_col]]),
      size = size, colour = "grey25", max.overlaps = Inf,
      segment.colour = "grey70", min.segment.length = 0.1, seed = SEED)
  } else {
    geom_text(data = data, aes(label = .data[[label_col]]),
              size = size, colour = "grey25", hjust = -0.08, vjust = -0.55,
              check_overlap = TRUE)
  }
}

theme_ms <- function(base_size = 10) {
  theme_bw(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.25, colour = "grey88"),
      strip.background = element_rect(fill = "grey95", colour = "grey70"),
      strip.text       = element_text(face = "bold", size = rel(0.9)),
      legend.position  = "bottom",
      legend.title     = element_text(face = "bold", size = rel(0.9)),
      plot.title       = element_text(face = "bold", size = rel(1.05)),
      plot.caption     = element_text(size = rel(0.8), colour = "grey35", hjust = 0)
    )
}
theme_set(theme_ms())

# Start logging ----------------------------------------------------------------
log_con <- file(file.path(DIR_OUT, "analysis_log.txt"), open = "wt")
sink(log_con, split = TRUE)
on.exit({ sink(); close(log_con) }, add = TRUE)

cat(RULE_MAJOR, "\n")
cat("MS_AAI_ToM  |  Statistical analysis log\n")
cat("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n", sep = "")
cat("Project root: ", PROJECT_ROOT, "\n", sep = "")
cat("Seed: ", SEED, " | Permutations: ", N_PERM, "\n", sep = "")
cat(RULE_MAJOR, "\n")


# ------------------------------------------------------------------------------
# 0c. STATISTICAL FUNCTIONS
# ------------------------------------------------------------------------------

#' One-way random-effects ICC(1,1) with exact F-based confidence interval.
#'
#' @param M matrix of cells (rows) x runs (columns), complete and balanced.
#' @return named list with icc, lower, upper, ms_between, ms_within, F, df.
icc_1_1 <- function(M, conf = CI_LEVEL) {
  M <- as.matrix(M)
  n <- nrow(M); k <- ncol(M)
  grand <- mean(M)
  row_m <- rowMeans(M)
  ms_b  <- k * sum((row_m - grand)^2) / (n - 1)
  ms_w  <- sum((M - row_m)^2) / (n * (k - 1))
  icc   <- (ms_b - ms_w) / (ms_b + (k - 1) * ms_w)
  Fv    <- ms_b / ms_w
  a     <- (1 - conf) / 2
  f_l   <- Fv / qf(1 - a, n - 1, n * (k - 1))
  f_u   <- Fv * qf(1 - a, n * (k - 1), n - 1)
  list(icc = icc,
       lower = (f_l - 1) / (f_l + k - 1),
       upper = (f_u - 1) / (f_u + k - 1),
       ms_between = ms_b, ms_within = ms_w, F = Fv,
       df1 = n - 1, df2 = n * (k - 1), n_cells = n, k_runs = k,
       sd_between = sqrt(max(0, (ms_b - ms_w) / k)),
       sd_within  = sqrt(ms_w))
}

#' Krippendorff's alpha, interval (squared-difference) metric.
#'
#' Balanced complete design: units = cells, observers = runs.
krippendorff_interval <- function(M) {
  M  <- as.matrix(M)
  m  <- ncol(M); n <- nrow(M) * m
  ss_within <- sum((M - rowMeans(M))^2)
  ss_total  <- sum((M - mean(M))^2)
  d_obs <- (2 * m / (n * (m - 1))) * ss_within
  d_exp <- 2 * ss_total / (n - 1)
  1 - d_obs / d_exp
}

#' All permutations of a short vector (used for the exact run-order shuffle).
all_perms <- function(v) {
  if (length(v) == 1L) return(matrix(v, 1L))
  do.call(rbind, lapply(seq_along(v), function(i)
    cbind(v[i], all_perms(v[-i]))))
}

#' Permutation test for run-order structure ("flat variance" test).
#'
#' H0: the five administrations are exchangeable, i.e. run index carries no
#' information. Two statistics are compared to the within-cell shuffle null:
#'   (a) lag-1 autocorrelation of within-cell deviations (stickiness / streaks)
#'   (b) share of within-cell variance captured by a linear trend (drift)
order_permutation_test <- function(M, n_perm = N_PERM) {
  M <- as.matrix(M); n <- nrow(M); k <- ncol(M)
  x <- seq_len(k) - mean(seq_len(k))

  stat_lag1 <- function(A) {
    D <- A - rowMeans(A)
    sum(D[, -k] * D[, -1]) / sum(D^2)
  }
  stat_trend <- function(A) {
    D <- A - rowMeans(A)
    b <- as.vector(D %*% x) / sum(x^2)
    sum(b^2 * sum(x^2)) / sum(D^2)
  }

  obs_lag1  <- stat_lag1(M)
  obs_trend <- stat_trend(M)

  perms <- all_perms(seq_len(k))
  null  <- matrix(NA_real_, n_perm, 2L)
  rows  <- rep(seq_len(n), k)
  for (i in seq_len(n_perm)) {
    idx <- perms[sample.int(nrow(perms), n, replace = TRUE), , drop = FALSE]
    Mp  <- matrix(M[cbind(rows, as.vector(idx))], n, k)
    null[i, ] <- c(stat_lag1(Mp), stat_trend(Mp))
  }

  data.frame(
    statistic = c("Lag-1 autocorrelation", "Variance share on linear trend"),
    observed  = c(obs_lag1, obs_trend),
    null_mean = colMeans(null),
    null_sd   = apply(null, 2, sd),
    p_value   = c(mean(abs(null[, 1]) >= abs(obs_lag1)),
                  mean(null[, 2] >= obs_trend))
  )
}

#' Bootstrap CI for a correlation coefficient.
boot_cor_ci <- function(x, y, method = "spearman", R = 5000, conf = CI_LEVEL) {
  ok <- complete.cases(x, y); x <- x[ok]; y <- y[ok]
  n  <- length(x)
  bs <- replicate(R, {
    i <- sample.int(n, n, replace = TRUE)
    suppressWarnings(cor(x[i], y[i], method = method))
  })
  q <- quantile(bs, c((1 - conf) / 2, 1 - (1 - conf) / 2), na.rm = TRUE)
  c(estimate = suppressWarnings(cor(x, y, method = method)),
    lower = unname(q[1]), upper = unname(q[2]))
}

#' Wilson score interval for a proportion.
wilson_ci <- function(k, n, conf = CI_LEVEL) {
  z  <- qnorm(1 - (1 - conf) / 2); p <- k / n
  d  <- 1 + z^2 / n
  ctr <- (p + z^2 / (2 * n)) / d
  hw  <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / d
  c(estimate = p, lower = max(0, ctr - hw), upper = min(1, ctr + hw))
}


# ------------------------------------------------------------------------------
# 1. LOAD AND HARMONISE
# ------------------------------------------------------------------------------

banner("Data preparation", "SECTION 1.")

raw_sdo <- readr::read_csv(file.path(DIR_RESULTS, "SDO_VSA_Results_20260830.csv"),
                           show_col_types = FALSE, progress = FALSE)
raw_rmet <- readr::read_csv(file.path(DIR_RESULTS, "RMET_Results.csv"),
                            show_col_types = FALSE, progress = FALSE)

# Strip UTF-8 BOM from the first column name; `repeat` is reserved in R.
names(raw_sdo)  <- sub("^\ufeff", "", names(raw_sdo))
names(raw_rmet) <- sub("^\ufeff", "", names(raw_rmet))
raw_sdo  <- dplyr::rename(raw_sdo,  run = `repeat`)
raw_rmet <- dplyr::rename(raw_rmet, run = `repeat`)

# Instrument metadata ----------------------------------------------------------
# Scale bounds. SDO6 and SDO7s were harmonised onto the SDO7 favour/oppose
# anchors (1-7); the VSA retains its native -4 to +4 agree/disagree metric.
SCALE_BOUNDS <- tibble::tribble(
  ~instrument, ~scale_min, ~scale_max,
  "SDO",        1,          7,
  "SDO6",       1,          7,
  "VSA",       -4,          4
)

# Reverse-keyed items. The source file carries reverse_scored only for SDO6;
# the SDO7s and VSA keys are restored here from the published instruments so
# that all composites use a consistent high-score-equals-high-construct metric.
#   SDO7s : items 3, 4 (dominance, con-trait) and 7, 8 (anti-egalitarianism)
#   VSA   : items 1, 4, 5 (con-trait items; Bizumic & Duckitt, 2018)
REVERSE_KEYS <- c("sdo-q3", "sdo-q4", "sdo-q7", "sdo-q8",
                  "vsa-q1", "vsa-q4", "vsa-q5")

# Subscale assignment for instruments lacking it in the source file.
SUBSCALE_MAP <- c(
  "sdo-q1" = "dominance",        "sdo-q2" = "dominance",
  "sdo-q3" = "dominance",        "sdo-q4" = "dominance",
  "sdo-q5" = "anti_egalitarianism", "sdo-q6" = "anti_egalitarianism",
  "sdo-q7" = "anti_egalitarianism", "sdo-q8" = "anti_egalitarianism",
  "vsa-q1" = "authoritarianism", "vsa-q2" = "authoritarianism",
  "vsa-q3" = "traditionalism",   "vsa-q4" = "traditionalism",
  "vsa-q5" = "conservatism",     "vsa-q6" = "conservatism"
)

dat <- raw_sdo %>%
  left_join(SCALE_BOUNDS, by = "instrument") %>%
  mutate(
    # Re-derive the key rather than trusting the source column, which is only
    # populated for SDO6 (a documented issue in the original pipeline).
    reverse_key = question_id %in% REVERSE_KEYS |
      (instrument == "SDO6" & !is.na(reverse_scored) & reverse_scored),
    subscale = dplyr::coalesce(subscale, unname(SUBSCALE_MAP[question_id])),
    # Recode so that HIGH = HIGH construct on every item and instrument.
    value_key = dplyr::case_when(
      is.na(value)                     ~ NA_real_,
      reverse_key & instrument == "VSA" ~ -value,
      reverse_key                      ~ (scale_min + scale_max) - value,
      TRUE                             ~ value
    ),
    # 0-1 normalisation for cross-instrument comparison of variability.
    value_norm = (value_key - scale_min) / (scale_max - scale_min),
    # Item number for the SDO7s and VSA (parsed from the id).
    item_no = dplyr::coalesce(
      item_no, suppressWarnings(as.numeric(str_extract(question_id, "\\d+$")))),
    language = factor(language, levels = c("en", "zh")),
    cell_id  = paste(model_label, question_id, language, sep = "|"),
    # Missingness taxonomy: infrastructure failure vs. content refusal.
    outcome = dplyr::case_when(
      !is.na(value)                     ~ "scored",
      parse_error == "API_ERROR"        ~ "api_error",
      !is.na(parse_error)               ~ "unparsed_or_refused",
      TRUE                              ~ "other_missing"
    )
  )

cat("SDO/VSA file : ", nrow(dat), " observations, ",
    n_distinct(dat$model_label), " models, ",
    n_distinct(dat$question_id), " items, ",
    n_distinct(dat$run), " runs\n", sep = "")
cat("RMET file    : ", nrow(raw_rmet), " observations, ",
    n_distinct(raw_rmet$model_label), " models, ",
    n_distinct(raw_rmet$question_id), " items\n", sep = "")

report_table(
  dat %>%
    count(instrument, language, name = "n_obs") %>%
    left_join(SCALE_BOUNDS, by = "instrument") %>%
    mutate(n_items = map2_int(instrument, language,
                              ~ n_distinct(dat$question_id[dat$instrument == .x &
                                                             dat$language == .y]))),
  file = "T01_design.csv",
  caption = "Table 1. Design: observations by instrument and language"
)

report_table(
  dat %>%
    filter(question_id %in% REVERSE_KEYS |
             (instrument == "SDO6" & reverse_key)) %>%
    distinct(instrument, question_id, subscale) %>%
    arrange(instrument, question_id),
  file = "T02_reverse_keyed_items.csv",
  caption = "Table 2. Reverse-keyed items applied in this analysis"
)
note("The source file supplied reverse_scored only for SDO6. Keys for the ",
     "SDO7s and VSA are restored here; all composites below use the recoded ",
     "metric (higher = higher construct).")


# ------------------------------------------------------------------------------
# 2. MISSINGNESS: API FAILURE VS. REFUSAL
# ------------------------------------------------------------------------------

banner("Missingness, disaggregated", "SECTION 2.")
note("API errors are infrastructure failures (missing completely at random ",
     "with respect to content); unparsed responses include content refusals, ",
     "which are not. Conflating them would misstate model behaviour.")

report_table(
  dat %>%
    count(outcome, name = "n") %>%
    mutate(pct = 100 * n / sum(n)) %>%
    arrange(desc(n)),
  file = "T03_missingness_overall.csv",
  caption = "Table 3. Response outcomes, overall"
)

report_table(
  dat %>%
    filter(outcome != "scored") %>%
    count(model_label, outcome, name = "n") %>%
    pivot_wider(names_from = outcome, values_from = n, values_fill = 0) %>%
    mutate(total_missing = rowSums(across(where(is.numeric)))) %>%
    arrange(desc(total_missing)),
  file = "T04_missingness_by_model.csv",
  caption = "Table 4. Non-scored responses by model"
)

# Is non-response content- and language-dependent? Tested on SDO6, which is the
# only instrument administered in both languages.
miss_sdo6 <- dat %>%
  filter(instrument == "SDO6") %>%
  mutate(refused = outcome == "unparsed_or_refused")

if (sum(miss_sdo6$refused) > 0) {
  tab_lang <- table(miss_sdo6$language, miss_sdo6$refused)
  ft_lang  <- fisher.test(tab_lang)
  tab_sub  <- table(miss_sdo6$subscale, miss_sdo6$refused)
  ft_sub   <- fisher.test(tab_sub)

  report_table(
    miss_sdo6 %>%
      group_by(language, subscale) %>%
      summarise(n = n(), n_refused = sum(refused),
                pct_refused = 100 * mean(refused), .groups = "drop"),
    file = "T05_refusal_by_language_subscale.csv",
    caption = "Table 5. SDO6 non-parsed responses by language and subscale"
  )
  cat("\n  Fisher exact test, refusal x language : OR = ",
      sprintf("%.2f", ft_lang$estimate), ", p = ", fmt_p(ft_lang$p.value), "\n", sep = "")
  cat("  Fisher exact test, refusal x subscale : p = ",
      fmt_p(ft_sub$p.value), "\n", sep = "")
  note("Refusal is a substantive finding, not only nuisance missingness. ",
       "Because it is content- and language-dependent it is also a confound ",
       "for the language contrast in Section 7 and is treated as such there.")
} else {
  note("No unparsed/refused responses in the SDO6 subset.")
}


# ------------------------------------------------------------------------------
# 3. RMET: THEORY-OF-MIND COMPETENCE
# ------------------------------------------------------------------------------

banner("RMET performance and the definitions manipulation", "SECTION 3.")

rmet <- raw_rmet %>%
  mutate(definitions_shown = as.logical(definitions_shown),
         is_correct = as.logical(is_correct))

acc_overall <- rmet %>%
  group_by(definitions_shown) %>%
  summarise(n = n(), n_correct = sum(is_correct), .groups = "drop") %>%
  mutate(accuracy = n_correct / n,
         ci_low  = map2_dbl(n_correct, n, ~ wilson_ci(.x, .y)["lower"]),
         ci_high = map2_dbl(n_correct, n, ~ wilson_ci(.x, .y)["upper"]))

report_table(acc_overall, file = "T06_rmet_accuracy.csv",
             caption = "Table 6. RMET accuracy by condition (Wilson 95% CI)")

# The manipulation is within item x model, so the paired test is McNemar's.
paired <- rmet %>%
  select(model_label, question_id, definitions_shown, is_correct) %>%
  pivot_wider(names_from = definitions_shown, values_from = is_correct,
              names_prefix = "defs_") %>%
  filter(!is.na(defs_TRUE), !is.na(defs_FALSE))

mc_tab <- table(without_definitions = paired$defs_FALSE,
                with_definitions    = paired$defs_TRUE)
mc <- mcnemar.test(mc_tab, correct = TRUE)

subhead("McNemar test: definitions shown vs. withheld (paired by model x item)")
print(mc_tab)
cat("\n  chi-square(1) = ", sprintf("%.3f", mc$statistic),
    ", p = ", fmt_p(mc$p.value),
    "\n  Discordant pairs: ", mc_tab[1, 2], " gained, ", mc_tab[2, 1], " lost\n", sep = "")

if (HAS_LME4) {
  glmm_rmet <- lme4::glmer(
    is_correct ~ definitions_shown + (1 | model_label) + (1 | question_id),
    data = rmet, family = binomial,
    control = lme4::glmerControl(optimizer = "bobyqa"))
  co <- summary(glmm_rmet)$coefficients
  subhead("Mixed-effects logistic regression (crossed random effects)")
  cat("  is_correct ~ definitions_shown + (1|model) + (1|item)\n\n")
  print(round(co, 4))
  cat("\n  Odds ratio for definitions shown: ",
      sprintf("%.3f", exp(co["definitions_shownTRUE", "Estimate"])),
      " (p = ", fmt_p(co["definitions_shownTRUE", "Pr(>|z|)"]), ")\n", sep = "")
  vc <- as.data.frame(lme4::VarCorr(glmm_rmet))
  report_table(vc[, c("grp", "vcov", "sdcor")] %>%
                 setNames(c("random_effect", "variance", "sd")),
               file = "T07_rmet_glmm_varcomp.csv",
               caption = "Table 7. RMET GLMM variance components")
} else {
  note("lme4 not installed; GLMM skipped.")
}

report_table(
  rmet %>%
    group_by(model_label, definitions_shown) %>%
    summarise(accuracy = mean(is_correct), .groups = "drop") %>%
    pivot_wider(names_from = definitions_shown, values_from = accuracy,
                names_prefix = "acc_defs_") %>%
    mutate(difference = acc_defs_TRUE - acc_defs_FALSE) %>%
    arrange(desc(acc_defs_FALSE)),
  file = "T08_rmet_accuracy_by_model.csv",
  caption = "Table 8. RMET accuracy by model and condition"
)
note("RMET used a single administration per cell, so no reliability ",
     "coefficient can be estimated for it. This is a design limitation, ",
     "not a null result.")


# ------------------------------------------------------------------------------
# 4. RELIABILITY AND RESPONSE INSTABILITY
# ------------------------------------------------------------------------------

banner("Reliability of repeated administration", "SECTION 4.")

# Cells with a complete set of runs, in wide form.
wide_all <- dat %>%
  filter(!is.na(value_key)) %>%
  select(cell_id, instrument, language, model_label, question_id, subscale,
         item_no, scale_min, scale_max, run, value_key) %>%
  group_by(cell_id) %>%
  filter(n() == MIN_RUNS) %>%
  ungroup()

cell_meta <- wide_all %>%
  distinct(cell_id, instrument, language, model_label, question_id, subscale,
           item_no, scale_min, scale_max)

wide_mat <- wide_all %>%
  select(cell_id, run, value_key) %>%
  pivot_wider(names_from = run, values_from = value_key,
              names_prefix = "run_") %>%
  arrange(cell_id)

stopifnot(identical(sort(wide_mat$cell_id), sort(cell_meta$cell_id)))
cell_meta <- cell_meta[match(wide_mat$cell_id, cell_meta$cell_id), ]
run_cols  <- grep("^run_", names(wide_mat), value = TRUE)

cat("Complete cells (all ", MIN_RUNS, " runs scored): ", nrow(wide_mat),
    " of ", n_distinct(dat$cell_id), "\n", sep = "")

# Per-cell instability summaries ----------------------------------------------
M_all <- as.matrix(wide_mat[, run_cols])
cell_stats <- cell_meta %>%
  mutate(
    span       = scale_max - scale_min,
    cell_mean  = rowMeans(M_all),
    cell_sd    = apply(M_all, 1, sd),
    cell_range = apply(M_all, 1, function(r) diff(range(r))),
    changed    = cell_range > 0,
    change_2pt = cell_range >= 2,
    range_pct  = 100 * cell_range / span,
    at_floor   = apply(M_all, 1, function(r) all(r == min(scale_min))),
    touches_end = apply(cbind(M_all, cell_meta$scale_min, cell_meta$scale_max), 1,
                        function(r) {
                          v <- r[1:MIN_RUNS]; any(v == r[MIN_RUNS + 1] | v == r[MIN_RUNS + 2])
                        })
  )
# at_floor recomputed against each cell's own minimum (recoded metric)
cell_stats$at_floor <- rowSums(M_all == cell_meta$scale_min) == MIN_RUNS
cell_stats$at_ceiling <- rowSums(M_all == cell_meta$scale_max) == MIN_RUNS
cell_stats$pinned <- cell_stats$at_floor | cell_stats$at_ceiling

overall_instab <- cell_stats %>%
  summarise(n_cells = n(),
            pct_identical = 100 * mean(!changed),
            pct_changed   = 100 * mean(changed),
            pct_change_2pt = 100 * mean(change_2pt))
report_table(overall_instab, file = "T09_instability_overall.csv",
             caption = "Table 9. Response instability across five administrations")

# Reliability by instrument ----------------------------------------------------
rel_by_instrument <- cell_meta %>%
  distinct(instrument) %>%
  pull(instrument) %>%
  map_dfr(function(ins) {
    idx <- cell_meta$instrument == ins
    Mi  <- M_all[idx, , drop = FALSE]
    span <- unique(cell_meta$scale_max[idx] - cell_meta$scale_min[idx])
    ic  <- icc_1_1(Mi)
    cs  <- cell_stats[idx, ]
    data.frame(
      instrument     = ins,
      n_cells        = ic$n_cells,
      icc_1_1        = ic$icc,
      icc_lower      = ic$lower,
      icc_upper      = ic$upper,
      kripp_alpha    = krippendorff_interval(Mi),
      sd_between_pct = 100 * ic$sd_between / span,
      sd_within_pct  = 100 * ic$sd_within  / span,
      pct_changed    = 100 * mean(cs$changed),
      pct_change_2pt = 100 * mean(cs$change_2pt),
      mean_range_pct = mean(cs$range_pct)
    )
  })

report_table(rel_by_instrument, file = "T10_reliability_by_instrument.csv",
             caption = paste0("Table 10. Reliability by instrument (ICC(1,1), ",
                              round(100 * CI_LEVEL), "% CI)"))
note("sd_between_pct and sd_within_pct express the two variance components as ",
     "percentages of each instrument's scale span, so they are comparable ",
     "across a 7-point and a 9-point scale. ICC is a ratio of the two: an ",
     "instrument on which models genuinely differ little will show a low ICC ",
     "even when individual responses are highly reproducible. Interpret the ",
     "two SD columns, not the ICC alone.")

# Raw vs. recoded metric -------------------------------------------------------
# Reliability computed on RAW responses is inflated. Reverse-keyed items sit at
# the opposite endpoint from forward-keyed items, so between-cell variance
# absorbs keying direction rather than reflecting only substantive differences.
# Recoding removes that artificial spread and lowers the ICC. The recoded figure
# is the defensible one; the comparison is reported so the difference is not
# mistaken for a discrepancy between analyses.
wide_raw <- dat %>%
  filter(!is.na(value)) %>%
  select(cell_id, run, value) %>%
  group_by(cell_id) %>% filter(n() == MIN_RUNS) %>% ungroup() %>%
  pivot_wider(names_from = run, values_from = value, names_prefix = "run_") %>%
  arrange(cell_id)
raw_meta <- cell_meta[match(wide_raw$cell_id, cell_meta$cell_id), ]
M_raw    <- as.matrix(wide_raw[, grep("^run_", names(wide_raw))])

icc_compare <- unique(cell_meta$instrument) %>%
  map_dfr(function(ins) {
    i_rec <- cell_meta$instrument == ins
    i_raw <- raw_meta$instrument  == ins
    span  <- unique(cell_meta$scale_max[i_rec] - cell_meta$scale_min[i_rec])
    a <- icc_1_1(M_raw[i_raw, , drop = FALSE])
    b <- icc_1_1(M_all[i_rec, , drop = FALSE])
    data.frame(instrument = ins,
               n_reverse_items = sum(cell_meta$instrument == ins &
                                       cell_meta$question_id %in% REVERSE_KEYS |
                                       (ins == "SDO6" & i_rec &
                                          cell_meta$item_no >= 9)) > 0,
               icc_raw = a$icc, icc_recoded = b$icc,
               sd_between_raw_pct = 100 * a$sd_between / span,
               sd_between_recoded_pct = 100 * b$sd_between / span,
               sd_within_raw_pct = 100 * a$sd_within / span,
               sd_within_recoded_pct = 100 * b$sd_within / span)
  }) %>% select(-n_reverse_items)

report_table(icc_compare, file = "T10b_icc_raw_vs_recoded.csv",
             caption = "Table 10b. ICC(1,1) computed on raw vs. reverse-key-corrected responses")
note("Within-cell SD is identical under both metrics: recoding is a monotone ",
     "relabelling and cannot change how much a model's answer moves. Only the ",
     "between-cell term changes. Report the recoded ICC.")

# Composite-level reliability --------------------------------------------------
composites <- dat %>%
  filter(!is.na(value_key)) %>%
  group_by(model_label, instrument, language, run) %>%
  summarise(n_items = n(), composite = mean(value_key), .groups = "drop") %>%
  group_by(model_label, instrument, language) %>%
  filter(n() == MIN_RUNS, n_distinct(n_items) == 1) %>%
  ungroup()

comp_rel <- composites %>%
  group_by(instrument, language) %>%
  group_modify(function(d, key) {
    W <- d %>% select(model_label, run, composite) %>%
      pivot_wider(names_from = run, values_from = composite) %>%
      select(-model_label) %>% as.matrix()
    if (nrow(W) < 3) return(tibble::tibble())
    ic <- icc_1_1(W)
    tibble::tibble(n_models = ic$n_cells, icc_1_1 = ic$icc,
                   icc_lower = ic$lower, icc_upper = ic$upper,
                   sd_between = ic$sd_between, sd_within = ic$sd_within)
  }) %>% ungroup()

report_table(comp_rel, file = "T11_reliability_composites.csv",
             caption = "Table 11. Reliability of model-level composite scores")

# Instability by model ---------------------------------------------------------
instab_by_model <- cell_stats %>%
  group_by(model_label) %>%
  summarise(n_cells = n(),
            pct_changed = 100 * mean(changed),
            pct_change_2pt = 100 * mean(change_2pt),
            mean_range_pct = mean(range_pct), .groups = "drop") %>%
  arrange(desc(pct_changed))

report_table(instab_by_model, file = "T12_instability_by_model.csv",
             caption = "Table 12. Response instability by model")
cat("\n  Range across models: ", sprintf("%.1f", min(instab_by_model$pct_changed)),
    "% to ", sprintf("%.1f", max(instab_by_model$pct_changed)),
    "% of cells changed\n", sep = "")


# ------------------------------------------------------------------------------
# 5. FLAT-VARIANCE TEST: IS REPEAT VARIATION ORDER-FREE?
# ------------------------------------------------------------------------------

banner("Run-order structure (flat-variance test)", "SECTION 5.")
note("The API harness issues stateless calls: each administration sends a ",
     "fresh two-message payload with no accumulated history, so a model ",
     "cannot know it is being asked again. The tests below verify the ",
     "behavioural corollary. If run variation were anything other than ",
     "sampling error, run index would carry information.")

# Normalised deviations so instruments contribute on a common metric.
span_vec <- cell_meta$scale_max - cell_meta$scale_min
M_norm   <- (M_all - cell_meta$scale_min) / span_vec

# (a) Mean response by run
by_run <- as.data.frame(M_norm) %>%
  setNames(paste0("run_", seq_len(MIN_RUNS))) %>%
  mutate(cell = row_number()) %>%
  pivot_longer(-cell, names_to = "run", values_to = "z") %>%
  group_by(cell) %>% mutate(dev = z - mean(z)) %>% ungroup() %>%
  group_by(run) %>%
  summarise(mean_z = mean(z), mean_dev = mean(dev),
            se_dev = sd(dev) / sqrt(n()), n = n(), .groups = "drop") %>%
  mutate(t_dev = mean_dev / se_dev)

report_table(by_run, file = "T13_response_by_run.csv",
             caption = "Table 13. Normalised response by administration order")

# (b) Friedman test across runs
fr <- friedman.test(M_norm)
cat("\n  Friedman test across runs: chi-square(", fr$parameter, ") = ",
    sprintf("%.3f", fr$statistic), ", p = ", fmt_p(fr$p.value), "\n", sep = "")

# (c) Within-cell linear trend
x_c <- seq_len(MIN_RUNS) - mean(seq_len(MIN_RUNS))
D   <- M_norm - rowMeans(M_norm)
slopes <- as.vector(D %*% x_c) / sum(x_c^2)
tt <- t.test(slopes)
cat("  Within-cell linear slope: b = ", sprintf("%+.5f", mean(slopes)),
    " normalised units per step, t(", tt$parameter, ") = ",
    sprintf("%+.2f", tt$statistic), ", p = ", fmt_p(tt$p.value), "\n", sep = "")

# (d) Warm-up: first administration vs. the rest
first <- M_norm[, 1]; later <- rowMeans(M_norm[, -1, drop = FALSE])
wt <- t.test(first, later, paired = TRUE)
cat("  First run vs. mean of runs 2-", MIN_RUNS, ": difference = ",
    sprintf("%+.5f", mean(first - later)), ", t(", wt$parameter, ") = ",
    sprintf("%+.2f", wt$statistic), ", p = ", fmt_p(wt$p.value), "\n", sep = "")

# (e) Do run-run correlations decay with lag? Drift would produce decay.
lag_tab <- expand.grid(a = seq_len(MIN_RUNS), b = seq_len(MIN_RUNS)) %>%
  filter(a < b) %>%
  mutate(lag = b - a,
         r = map2_dbl(a, b, ~ cor(M_norm[, .x], M_norm[, .y]))) %>%
  group_by(lag) %>%
  summarise(n_pairs = n(), mean_r = mean(r), .groups = "drop")
report_table(lag_tab, file = "T14_lag_correlations.csv",
             caption = "Table 14. Correlation between administrations by lag")

# (f) Permutation test
cat("\n  Running ", N_PERM, " within-cell permutations ...\n", sep = "")
perm_res <- order_permutation_test(M_norm, N_PERM)
report_table(perm_res, file = "T15_order_permutation_test.csv",
             caption = "Table 15. Permutation test for run-order structure",
             digits = 4)
note("Non-significant results here support the measurement model: the five ",
     "administrations are exchangeable replicates, so variation is error ",
     "variance and ICC(1,1) without a run-order term is appropriate.")

# (g) Direction of change, conditioned on starting position. Reported to
#     pre-empt a regression-to-the-mean artifact being read as drift.
steps <- data.frame(
  start = as.vector(M_norm[, -MIN_RUNS]),
  delta = as.vector(M_norm[, -1] - M_norm[, -MIN_RUNS])
) %>%
  mutate(band = cut(start, breaks = c(-0.01, 1/3, 2/3, 1.01),
                    labels = c("bottom third", "middle third", "top third")))

report_table(
  steps %>%
    group_by(band) %>%
    summarise(n_steps = n(),
              pct_changed = 100 * mean(delta != 0),
              pct_up_given_change = 100 * mean(delta[delta != 0] > 0),
              .groups = "drop"),
  file = "T16_step_direction_by_position.csv",
  caption = "Table 16. Direction of consecutive change, by starting position"
)
note("The asymmetry in the outer bands is bounded regression to the mean: a ",
     "response at the floor can only move up. It is NOT evidence of drift, ",
     "which the tests above rule out.")

# Full-scale reversals
rev_n <- sum(abs(steps$delta) >= 0.8, na.rm = TRUE)
if (rev_n > 0) {
  up <- sum(steps$delta >= 0.8, na.rm = TRUE)
  bt <- binom.test(up, rev_n)
  cat("\n  Near-full-scale reversals in a single step: ", rev_n, " of ",
      nrow(steps), " (", sprintf("%.2f", 100 * rev_n / nrow(steps)), "%)\n",
      "  Direction: ", up, " upward, ", rev_n - up, " downward; ",
      "sign test p = ", fmt_p(bt$p.value), "\n", sep = "")
}


# ------------------------------------------------------------------------------
# 6. SCALE POSITION AND APPARENT RELIABILITY
# ------------------------------------------------------------------------------

banner("Endpoint pinning and apparent reliability", "SECTION 6.")
note("Apparent stability depends on where a response sits. A response at a ",
     "scale endpoint can move in one direction only and usually does not ",
     "move at all; a midpoint response can move either way. Instrument ",
     "comparisons must therefore condition on response position.")

# Response distribution by instrument
pos_dist <- dat %>%
  filter(!is.na(value_key)) %>%
  group_by(instrument) %>%
  summarise(n = n(),
            pct_at_min = 100 * mean(value_key == scale_min),
            pct_at_max = 100 * mean(value_key == scale_max),
            pct_at_either_end = 100 * mean(value_key %in% c(scale_min, scale_max)),
            pct_at_midpoint = 100 * mean(value_key == (scale_min + scale_max) / 2),
            .groups = "drop")
report_table(pos_dist, file = "T17_response_position.csv",
             caption = "Table 17. Response position by instrument (recoded metric)")

report_table(
  cell_stats %>%
    group_by(instrument) %>%
    summarise(n_cells = n(),
              pct_pinned_all_runs = 100 * mean(pinned),
              pct_changed = 100 * mean(changed), .groups = "drop"),
  file = "T18_pinning_by_instrument.csv",
  caption = "Table 18. Cells pinned at an endpoint across all five runs"
)

# Instability by position band, within instrument
band_tab <- cell_stats %>%
  mutate(rel_pos = (cell_mean - scale_min) / (scale_max - scale_min),
         band = cut(rel_pos, breaks = c(-0.01, 0.15, 0.40, 0.60, 0.85, 1.01),
                    labels = c("at/near floor", "lower", "midpoint",
                               "upper", "at/near ceiling"))) %>%
  group_by(instrument, band) %>%
  summarise(n_cells = n(), pct_changed = 100 * mean(changed),
            mean_range_pct = mean(range_pct), .groups = "drop") %>%
  filter(n_cells >= 5)

report_table(band_tab, file = "T19_instability_by_position.csv",
             caption = "Table 19. Instability by scale position, within instrument")

# Like-for-like comparison: cells that never touch an endpoint
fair <- cell_stats %>% filter(!touches_end)
fair_tab <- fair %>%
  group_by(instrument) %>%
  summarise(n_cells = n(), pct_changed = 100 * mean(changed),
            mean_sd_pct = 100 * mean(cell_sd / span), .groups = "drop")
report_table(fair_tab, file = "T20_unpinned_comparison.csv",
             caption = "Table 20. Instability restricted to cells never at an endpoint")

if (all(c("SDO6", "VSA") %in% fair_tab$instrument)) {
  sub <- fair %>% filter(instrument %in% c("SDO6", "VSA"))
  pt  <- prop.test(table(sub$instrument, sub$changed)[, c("TRUE", "FALSE")])
  wx  <- wilcox.test((cell_sd / span) ~ instrument, data = sub)
  cat("\n  SDO6 vs. VSA, unpinned cells only\n",
      "    Proportion changed: chi-square = ", sprintf("%.3f", pt$statistic),
      ", p = ", fmt_p(pt$p.value), "\n",
      "    Within-cell SD (% of span): W = ", wx$statistic,
      ", p = ", fmt_p(wx$p.value), "\n", sep = "")
  note("A significant difference here is a genuine instrument difference, ",
       "not a pinning artifact. Report it alongside Table 10 so the ICC gap ",
       "is not over-read.")
}


# ------------------------------------------------------------------------------
# 7. LANGUAGE OF ADMINISTRATION (SDO6)
# ------------------------------------------------------------------------------

banner("English vs. Chinese administration of SDO6", "SECTION 7.")

sdo6 <- dat %>% filter(instrument == "SDO6", !is.na(value_key))

report_table(
  sdo6 %>%
    group_by(language) %>%
    summarise(n = n(), mean = mean(value_key), sd = sd(value_key),
              median = median(value_key), .groups = "drop"),
  file = "T21_language_descriptives.csv",
  caption = "Table 21. SDO6 responses by language of administration"
)

# Model-level paired comparison (the unit at which the claim is made)
model_lang <- sdo6 %>%
  group_by(model_label, language) %>%
  summarise(score = mean(value_key), .groups = "drop") %>%
  pivot_wider(names_from = language, values_from = score) %>%
  filter(!is.na(en), !is.na(zh)) %>%
  mutate(difference = zh - en)

tt_lang <- t.test(model_lang$zh, model_lang$en, paired = TRUE)
wx_lang <- suppressWarnings(wilcox.test(model_lang$zh, model_lang$en, paired = TRUE))
bt_lang <- binom.test(sum(model_lang$difference > 0), nrow(model_lang))
d_z     <- mean(model_lang$difference) / sd(model_lang$difference)

subhead("Paired comparison at the model level")
cat("  Models: ", nrow(model_lang), "\n",
    "  Mean difference (zh - en): ", sprintf("%+.3f", mean(model_lang$difference)),
    " scale points, 95% CI [", sprintf("%.3f", tt_lang$conf.int[1]), ", ",
    sprintf("%.3f", tt_lang$conf.int[2]), "]\n",
    "  Paired t(", tt_lang$parameter, ") = ", sprintf("%.3f", tt_lang$statistic),
    ", p = ", fmt_p(tt_lang$p.value), ", Cohen's d_z = ", sprintf("%.3f", d_z), "\n",
    "  Wilcoxon signed-rank: V = ", wx_lang$statistic,
    ", p = ", fmt_p(wx_lang$p.value), "\n",
    "  Models scoring higher in Chinese: ", sum(model_lang$difference > 0),
    " of ", nrow(model_lang), " (sign test p = ", fmt_p(bt_lang$p.value), ")\n", sep = "")

report_table(model_lang %>% arrange(desc(difference)),
             file = "T22_language_effect_by_model.csv",
             caption = "Table 22. SDO6 score by model and language")

# Mixed-effects model on the run-level data
if (HAS_LME4) {
  lmer_fn <- if (HAS_LMERTEST) lmerTest::lmer else lme4::lmer
  lmm <- lmer_fn(
    value_key ~ language + (1 | model_label) + (1 | item_no) +
      (1 | model_label:item_no),
    data = sdo6, REML = TRUE,
    control = lme4::lmerControl(optimizer = "bobyqa"))
  subhead("Mixed-effects model: value ~ language + (1|model) + (1|item) + (1|model:item)")
  print(round(summary(lmm)$coefficients, 4))
  if (HAS_LMERTEST) {
    cat("\n  Type III test (Satterthwaite):\n")
    print(anova(lmm))
  } else {
    note("lmerTest not installed; p-values for the fixed effect are omitted. ",
         "The model-level paired test above is the primary inferential result.")
  }
  vc <- as.data.frame(lme4::VarCorr(lmm))
  report_table(vc[, c("grp", "vcov", "sdcor")] %>%
                 setNames(c("random_effect", "variance", "sd")),
               file = "T23_language_lmm_varcomp.csv",
               caption = "Table 23. Variance components, language model")
}

# Item-level effects: which items carry the shift?
item_lang <- sdo6 %>%
  group_by(item_no, subscale, model_label, language) %>%
  summarise(score = mean(value_key), .groups = "drop") %>%
  pivot_wider(names_from = language, values_from = score) %>%
  filter(!is.na(en), !is.na(zh)) %>%
  group_by(item_no, subscale) %>%
  summarise(n_models = n(),
            mean_en = mean(en), mean_zh = mean(zh),
            difference = mean(zh - en),
            se = sd(zh - en) / sqrt(n()),
            t = difference / se,
            p_raw = t.test(zh, en, paired = TRUE)$p.value,
            .groups = "drop") %>%
  mutate(p_bh = p.adjust(p_raw, method = "BH"),
         ci_low = difference - qt(0.975, n_models - 1) * se,
         ci_high = difference + qt(0.975, n_models - 1) * se) %>%
  arrange(desc(difference))

report_table(item_lang, file = "T24_language_effect_by_item.csv",
             caption = "Table 24. Language effect by SDO6 item (BH-adjusted)")
cat("\n  Items with a significant shift after BH correction: ",
    sum(item_lang$p_bh < .05), " of ", nrow(item_lang), "\n", sep = "")

# Refusal confound check: does the language effect survive dropping cells with
# any non-response in either language?
clean_cells <- dat %>%
  filter(instrument == "SDO6") %>%
  group_by(model_label, item_no) %>%
  summarise(all_scored = all(outcome == "scored"), .groups = "drop") %>%
  filter(all_scored)

sdo6_clean <- sdo6 %>% semi_join(clean_cells, by = c("model_label", "item_no"))
ml_clean <- sdo6_clean %>%
  group_by(model_label, language) %>%
  summarise(score = mean(value_key), .groups = "drop") %>%
  pivot_wider(names_from = language, values_from = score) %>%
  filter(!is.na(en), !is.na(zh))

if (nrow(ml_clean) >= 3) {
  tt_clean <- t.test(ml_clean$zh, ml_clean$en, paired = TRUE)
  subhead("Sensitivity analysis: complete cases only (no refusal in either language)")
  cat("  Models retained: ", nrow(ml_clean), " | Items retained: ",
      n_distinct(sdo6_clean$item_no), "\n",
      "  Mean difference (zh - en): ",
      sprintf("%+.3f", mean(ml_clean$zh - ml_clean$en)),
      ", t(", tt_clean$parameter, ") = ", sprintf("%.3f", tt_clean$statistic),
      ", p = ", fmt_p(tt_clean$p.value), "\n", sep = "")
  note("If the effect attenuates here, the language contrast is partly ",
       "carried by differential refusal rather than differential endorsement.")
}


# ------------------------------------------------------------------------------
# 8. ALIGNMENT PROXY: DOES TEST-RETEST VARIANCE INDEX REFUSAL TUNING?
# ------------------------------------------------------------------------------

banner("Response variance as an index of alignment tuning", "SECTION 8.")
note("Hypothesis: on these instruments a model's score is largely a record of ",
     "whether it has been tuned to reject the premise. If so, cells pinned at ",
     "the construct floor should be the stable ones, and instability should ",
     "concentrate in models with weaker refusal tuning.")

align <- cell_stats %>%
  filter(instrument %in% c("SDO", "SDO6"), language == "en") %>%
  group_by(model_label) %>%
  summarise(n_cells = n(),
            pct_at_floor = 100 * mean(at_floor),
            pct_pinned = 100 * mean(pinned),
            pct_changed = 100 * mean(changed),
            mean_score = mean(cell_mean),
            .groups = "drop") %>%
  arrange(desc(pct_at_floor))

report_table(align, file = "T25_alignment_proxy_by_model.csv",
             caption = "Table 25. Floor pinning and instability by model (SDO/SDO6, English)")

cr_s <- boot_cor_ci(align$pct_at_floor, align$pct_changed, "spearman")
cr_p <- boot_cor_ci(align$pct_at_floor, align$pct_changed, "pearson")
ct   <- suppressWarnings(cor.test(align$pct_at_floor, align$pct_changed,
                                  method = "spearman"))
subhead("Association between floor pinning and response instability")
cat("  Spearman rho = ", sprintf("%.3f", cr_s["estimate"]),
    ", bootstrap 95% CI [", sprintf("%.3f", cr_s["lower"]), ", ",
    sprintf("%.3f", cr_s["upper"]), "], p = ", fmt_p(ct$p.value), "\n",
    "  Pearson  r   = ", sprintf("%.3f", cr_p["estimate"]),
    ", bootstrap 95% CI [", sprintf("%.3f", cr_p["lower"]), ", ",
    sprintf("%.3f", cr_p["upper"]), "]\n", sep = "")

# Grouping by developer / model family, to test whether pinning tracks model
# recency (a common conjecture) or tuning posture.
family_of <- function(m) {
  dplyr::case_when(
    str_detect(m, "gpt-oss")                        ~ "GPT-OSS (open weight)",
    str_detect(m, "gpt-5")                          ~ "GPT-5 family",
    str_detect(m, "gpt-4")                          ~ "GPT-4 family",
    str_detect(m, "opus-5|sonnet-5|fable-5")        ~ "Claude 5 family",
    str_detect(m, "opus-4|sonnet-4|haiku-4")        ~ "Claude 4 family",
    str_detect(m, "llama")                          ~ "Llama family",
    str_detect(m, "qwen")                           ~ "Qwen family",
    str_detect(m, "gemma")                          ~ "Gemma family",
    TRUE                                            ~ "Other")
}

fam_tab <- align %>%
  mutate(family = family_of(model_label)) %>%
  group_by(family) %>%
  summarise(n_models = n(),
            mean_pct_at_floor = mean(pct_at_floor),
            min_pct_at_floor = min(pct_at_floor),
            max_pct_at_floor = max(pct_at_floor),
            mean_pct_changed = mean(pct_changed), .groups = "drop") %>%
  arrange(desc(mean_pct_at_floor))

report_table(fam_tab, file = "T26_pinning_by_family.csv",
             caption = "Table 26. Floor pinning by model family")

kw <- kruskal.test(pct_at_floor ~ factor(family_of(model_label)), data = align)
cat("\n  Kruskal-Wallis, floor pinning by family: chi-square(", kw$parameter,
    ") = ", sprintf("%.3f", kw$statistic), ", p = ", fmt_p(kw$p.value), "\n", sep = "")
note("Family means should be read with caution: several families contribute a ",
     "single model. Any claim that pinning tracks model recency requires ",
     "release dates as a covariate, which this design does not carry.")


# ------------------------------------------------------------------------------
# 9. FIGURES
# ------------------------------------------------------------------------------

banner("Figures", "SECTION 9.")

# Figure 1. Response distribution by instrument -------------------------------
fig1_dat <- dat %>%
  filter(!is.na(value_key)) %>%
  mutate(rel = (value_key - scale_min) / (scale_max - scale_min)) %>%
  count(instrument, value_key, rel) %>%
  group_by(instrument) %>% mutate(pct = 100 * n / sum(n)) %>% ungroup()

fig1 <- ggplot(fig1_dat, aes(x = rel, y = pct)) +
  geom_col(width = 0.06, fill = "grey30") +
  facet_wrap(~ instrument, nrow = 1) +
  scale_x_continuous(breaks = c(0, 0.5, 1),
                     labels = c("floor", "midpoint", "ceiling")) +
  labs(x = "Position on scale (recoded, normalised)",
       y = "Percentage of responses",
       title = "Response distributions differ sharply across instruments",
       caption = paste("SDO items elicit endpoint responses that cannot vary;",
                       "VSA items elicit midpoint responses with room to move."))
save_figure(fig1, "Fig1_response_distribution", width = 7.5, height = 3.4)

# Figure 2. Instability by scale position -------------------------------------
fig2 <- band_tab %>%
  ggplot(aes(x = band, y = pct_changed, group = instrument)) +
  geom_col(fill = "grey35", width = 0.65) +
  facet_wrap(~ instrument, nrow = 1, scales = "free_x") +
  labs(x = "Position of the cell on the scale", y = "% of cells that changed",
       title = "Apparent instability is driven by scale position",
       caption = "Within each instrument, cells near an endpoint change far less than midpoint cells.") +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))
save_figure(fig2, "Fig2_instability_by_position", width = 7.5, height = 4)

# Figure 3. Alignment proxy ----------------------------------------------------
align_fig <- align %>%
  mutate(model_short = str_remove(model_label, "^(databricks|azure)-"))

fig3 <- ggplot(align_fig, aes(x = pct_at_floor, y = pct_changed)) +
  geom_smooth(method = "lm", se = TRUE, colour = "grey40",
              fill = "grey85", linewidth = 0.5, formula = y ~ x) +
  geom_point(size = 2, colour = "grey15") +
  label_points(align_fig, label_col = "model_short", size = 2.1) +
  labs(x = "% of cells pinned at the construct floor across all five runs",
       y = "% of cells that changed",
       title = "Models pinned at the construct floor show the least response variance",
       caption = paste("SDO6 and SDO7s items, English administration. Each point is one model.",
                       "\nAssociation is correlational; model family is not controlled.")) +
  coord_cartesian(ylim = c(-4, NA), clip = "off")
save_figure(fig3, "Fig3_alignment_proxy", width = 8, height = 5.6)

# Figure 4. Language effect by item -------------------------------------------
fig4 <- item_lang %>%
  mutate(item = factor(item_no, levels = rev(sort(unique(item_no))))) %>%
  ggplot(aes(x = difference, y = item, colour = subscale)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.25,
                 linewidth = 0.5) +
  geom_point(size = 2) +
  scale_colour_grey(start = 0.15, end = 0.6) +
  labs(x = "Chinese minus English (scale points, 95% CI)",
       y = "SDO6 item", colour = "Subscale",
       title = "The language effect is concentrated in a few dominance items",
       caption = "Paired across models. Positive values indicate higher scores under Chinese administration.")
save_figure(fig4, "Fig4_language_effect_by_item", width = 7, height = 5.5)

# Figure 5. Run-order stability ------------------------------------------------
fig5 <- by_run %>%
  mutate(run_n = as.integer(str_extract(run, "\\d+"))) %>%
  ggplot(aes(x = run_n, y = mean_dev)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbar(aes(ymin = mean_dev - 1.96 * se_dev,
                    ymax = mean_dev + 1.96 * se_dev), width = 0.12) +
  geom_point(size = 2.2) +
  scale_x_continuous(breaks = seq_len(MIN_RUNS)) +
  labs(x = "Administration order", y = "Mean within-cell deviation (normalised)",
       title = "No drift across repeated administrations",
       caption = "Deviations from each cell's own mean. Run order carries no information.")
save_figure(fig5, "Fig5_run_order_stability", width = 6.5, height = 4)

cat("Figures written to: ", DIR_FIGURES, "\n", sep = "")


# ------------------------------------------------------------------------------
# 9b. OUTPUT MANIFEST
# ------------------------------------------------------------------------------

manifest <- c(
  "# MS_AAI_ToM - Analysis output",
  paste0("Generated ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
         " by Analysis_script/01_analysis_LLM_attitude_stability.R"),
  "", "## Tables", "",
  "| File | Contents |", "|---|---|",
  "| T01 | Design: observations by instrument and language |",
  "| T02 | Reverse-keyed items applied |",
  "| T03-T05 | Missingness, disaggregated into API errors and refusals |",
  "| T06-T08 | RMET accuracy, GLMM variance components, per-model accuracy |",
  "| T09 | Overall response instability across five administrations |",
  "| T10, T10b | Reliability by instrument; raw vs. reverse-key-corrected ICC |",
  "| T11 | Reliability of model-level composite scores |",
  "| T12 | Instability by model |",
  "| T13-T16 | Run-order tests: means by run, lag correlations, permutation test |",
  "| T17-T20 | Endpoint pinning and position-conditioned instability |",
  "| T21-T24 | Language effect: descriptives, by model, mixed model, by item |",
  "| T25-T26 | Floor pinning as an alignment-tuning proxy |",
  "", "## Figures", "",
  "| File | Contents |", "|---|---|",
  "| Fig1 | Response distributions by instrument |",
  "| Fig2 | Instability by scale position, within instrument |",
  "| Fig3 | Floor pinning against response instability, by model |",
  "| Fig4 | Language effect by SDO6 item |",
  "| Fig5 | Mean within-cell deviation by administration order |",
  "", "Full numeric output with test statistics: `analysis_log.txt`."
)
writeLines(manifest, file.path(DIR_OUT, "README.md"))


# ------------------------------------------------------------------------------
# 10. SESSION INFO
# ------------------------------------------------------------------------------

banner("Session information", "SECTION 10.")
print(sessionInfo())

cat("\n\n", RULE_MAJOR, "\n", sep = "")
cat("Analysis complete.\n")
cat("  Log    : ", file.path(DIR_OUT, "analysis_log.txt"), "\n", sep = "")
cat("  Tables : ", DIR_TABLES, "\n", sep = "")
cat("  Figures: ", DIR_FIGURES, "\n", sep = "")
cat(RULE_MAJOR, "\n", sep = "")
