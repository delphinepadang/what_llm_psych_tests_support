# ==============================================================================
# MS_AAI_ToM  |  02_statistical_analysis.R
# Statistical analysis of LLM responses to human psychometric instruments
# ------------------------------------------------------------------------------
# This script runs every statistical test for the paper and writes NO figures.
# Figures are produced by 03_figures.R, which reads analysis_results.rds.
#
# Inputs (Results/)
#   RMET_Results_20260912.csv     RMET, 5 repeats x 2 conditions (Sep 2026)   main
#   RMET_Results.csv              RMET, 1 run x 2 conditions (Jun 2026)       drift
#   SDO_VSA_Results_20260830.csv  SDO7(s), SDO6 EN+ZH, VSA, 5 repeats (Aug)   main
#   SDO_VSA_Results.csv           SDO7(s), VSA, 5 repeats (Jun 2026)          drift
#
# Outputs (Analysis_output/)
#   stats_log.txt                 complete numeric log, section by section
#   Stats_tables/S##_*.csv        every table printed in the log
#   Stats_tables/S19_test_registry.csv
#                                 every hypothesis test in one table, with
#                                 BH-adjusted p-values within each test family
#   analysis_results.rds          data objects consumed by 03_figures.R
#   Stats_README.md               manifest of tables and sections
#
# Sections
#    1  Data preparation and quality audit (API failures vs refusals)
#    2  RMET accuracy and the human benchmark (Welch, TOST equivalence)
#    3  RMET definitions manipulation (GLMM, paired tests, TOST, RM-ANOVA,
#       random-effects meta-analysis)
#    4  RMET reliability across five repeats (instability, Krippendorff's
#       alpha, sitting-level ICC, developer comparisons)
#    5  RMET items: difficulty, stochastic vs systematic error, Baron-Cohen
#       retention criteria, foil concentration
#    6  RMET between-model error consistency and clustering
#    7  SDO/VSA reliability at three levels of aggregation
#    8  Generalizability theory and D-study (how many repeats are needed)
#    9  Exchangeability of repeats (flat-variance tests)
#   10  Scale position and apparent stability
#   11  Subdimensions, rank reversals and response styles
#   12  Structural validity (coherence, internal consistency, factorability)
#   13  English vs Chinese SDO6 (CLMM, meta-analysis, DIF, extremity)
#   14  Longitudinal drift (June vs August/September)
#   15  Alignment proxy, with mechanical-confound and discriminant tests
#   16  Model profiles (PCA, clustering, cluster stability)
#   17  Cross-instrument correlations
#   18  Sensitivity power and sample-size planning
#   19  Test registry and multiplicity control
#
# Terminology: a "model-item pair" is one model's five responses to one item
# (for the RMET, within one definitions condition). "Changed" means the five
# responses to that pair were not all identical.
#
# Run:   Rscript Analysis_script/02_statistical_analysis.R
#        (or source() it in RStudio; if it stops with an error, run
#        close_log() to release the log file)
# Runtime: roughly 10-20 minutes on a laptop, most of it in the mixed models.
#
# Numerical ties: wherever a tie matters, values are rounded to 10 digits once
# (see tie_round() under SETUP), so ranks, signs, counts and rank-based tests
# are the same on every computer (Apple-silicon Macs have no extended
# precision; Intel/Linux machines do). Mixed-model estimates can still differ
# in late decimals between computers and package versions.
# ==============================================================================


# ------------------------------------------------------------------------------
# 0. SETUP
# ------------------------------------------------------------------------------

PROJECT_ROOT <- path.expand("~/Documents/research_projects/AI Bias paper/MS_AAI_ToM")

# Fall back to the working directory (or its parent, if run from inside
# Analysis_script/) so the script also runs on a collaborator's machine.
if (!dir.exists(PROJECT_ROOT)) {
  wd <- normalizePath(getwd())
  PROJECT_ROOT <- if (basename(wd) == "Analysis_script") dirname(wd) else wd
}

DIR_RESULTS <- file.path(PROJECT_ROOT, "Results")
DIR_OUT     <- file.path(PROJECT_ROOT, "Analysis_output")
DIR_TABLES  <- file.path(DIR_OUT, "Stats_tables")

if (!dir.exists(DIR_RESULTS)) {
  stop("Cannot find the Results folder at: ", DIR_RESULTS,
       "\nEdit PROJECT_ROOT at the top of this script.", call. = FALSE)
}
for (d in c(DIR_OUT, DIR_TABLES)) if (!dir.exists(d)) dir.create(d, recursive = TRUE)

FILES <- list(
  rmet_main = "RMET_Results_20260912.csv",
  rmet_june = "RMET_Results.csv",
  sdo_main  = "SDO_VSA_Results_20260830.csv",
  sdo_june  = "SDO_VSA_Results.csv"
)

# Analysis constants -----------------------------------------------------------
SEED      <- 20260922
B_BOOT    <- 2000     # bootstrap resamples
B_PERM    <- 5000     # permutation resamples (Monte Carlo tests)
CI_LEVEL  <- 0.95
ALPHA     <- 0.05
N_RUNS    <- 5        # repeats per model-item pair
MAX_API_FAIL <- 0.50  # a model is excluded if more than this share of calls failed

# Floating-point ties ----------------------------------------------------------
# A mean of whole-number answers is usually not exact in binary. Where R has no
# extra (long double) precision, as on Apple-silicon Macs, two mathematically
# equal means can differ in the last binary digit, and so can differences
# between them. That turns ties into spurious orderings, signs or band changes.
# tie_round() is therefore applied once, to the full-precision value, wherever a
# tie matters (ranks, signs, Wilcoxon and Kruskal-Wallis tests, Spearman and
# Kendall correlations, bands, thresholds, bootstrap counts). Values that feed
# further arithmetic are not rounded early. Ten digits is far below any
# reported precision, so nothing else changes.
TIE_DIGITS <- 10
tie_round  <- function(x) round(x, TIE_DIGITS)

# Human RMET benchmark: Baron-Cohen et al. (2001), J Child Psychol Psychiatry
# 42(2), Table 3 (verified against the PDF; decimal points print as "+/-").
HUMAN_RMET <- tibble::tribble(
  ~group,                        ~n,  ~mean, ~sd,
  "General population controls", 122, 26.2,  3.6,
  "Cambridge students",          103, 28.0,  3.5,
  "AS/HFA adults",                15, 21.9,  6.6,
  "IQ-matched controls",          14, 30.9,  3.0
)
RMET_ITEMS <- 36
RMET_CHANCE <- 0.25

# Smallest effect sizes of interest (SESOI) for the two equivalence claims.
# 0.5 of the human SD (1.8 of 36 items) is used for both: half the distance
# that separates the AS/HFA group from general-population controls.
SESOI_ITEMS <- 0.5 * 3.6                 # human-benchmark equivalence, in items
SESOI_PP    <- 100 * SESOI_ITEMS / RMET_ITEMS  # definitions null, in % points (5 pp)

# Baron-Cohen et al. (2001) item-retention rule: >= 50% choose the target and
# no single foil chosen by > 25%.
RETENTION_TARGET <- 0.50
RETENTION_FOIL   <- 0.25

set.seed(SEED)
options(width = 200, stringsAsFactors = FALSE, dplyr.summarise.inform = FALSE,
        scipen = 6)

# Packages ---------------------------------------------------------------------
PKGS <- c("dplyr", "tidyr", "readr", "purrr", "stringr", "tibble",
          "lme4", "lmerTest", "ordinal", "metafor", "psych", "GPArotation",
          "pwr", "afex", "cluster", "nnet")
AUTO_INSTALL <- TRUE
missing_pkgs <- PKGS[!vapply(PKGS, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs)) {
  if (AUTO_INSTALL) {
    message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))
    install.packages(missing_pkgs)
  } else {
    stop("Missing packages: ", paste(missing_pkgs, collapse = ", "), call. = FALSE)
  }
}
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(purrr)
  library(stringr); library(tibble); library(lme4)
})


# ------------------------------------------------------------------------------
# 0b. LOGGING AND REPORTING HELPERS
# ------------------------------------------------------------------------------

close_log <- function() {
  while (sink.number() > 0) sink()
  if (exists("LOG_CON", envir = .GlobalEnv)) {
    con <- get("LOG_CON", envir = .GlobalEnv)
    try(if (inherits(con, "connection") && isOpen(con)) close(con), silent = TRUE)
  }
  invisible(TRUE)
}
close_log()
LOG_FILE <- file.path(DIR_OUT, "stats_log.txt")
LOG_CON  <- file(LOG_FILE, open = "wt", encoding = "UTF-8")
sink(LOG_CON, split = TRUE)

RULE_MAJOR <- strrep("=", 90)
RULE_MINOR <- strrep("-", 90)

banner <- function(num, title) {
  cat("\n\n", RULE_MAJOR, "\n", sprintf("SECTION %s.  %s", num, title), "\n",
      RULE_MAJOR, "\n", sep = "")
}
subhead <- function(title) cat("\n", title, "\n", RULE_MINOR, "\n", sep = "")
note    <- function(...) {
  txt <- paste0(..., collapse = "")
  cat(paste0("  ", strwrap(paste("Note.", txt), width = 86), collapse = "\n"), "\n", sep = "")
}
say <- function(...) cat("  ", ..., "\n", sep = "")

fmt_p <- function(p) {
  ifelse(is.na(p), "NA",
         ifelse(p < .001, "< .001", sub("^0", "", sprintf("%.3f", p))))
}
fp <- function(p) ifelse(is.na(p), "p = NA", ifelse(p < .001, "p < .001", paste0("p = ", fmt_p(p))))
fmt_n  <- function(x, d = 2) ifelse(is.na(x), "NA", formatC(x, format = "f", digits = d))
fmt_ci <- function(lo, hi, d = 2) paste0("[", fmt_n(lo, d), ", ", fmt_n(hi, d), "]")
pct    <- function(x, d = 1) paste0(fmt_n(100 * x, d), "%")

TABLE_INDEX <- list()

#' Print a table to the log and write it to Stats_tables/.
report_table <- function(df, file = NULL, caption = NULL, digits = 3,
                         max_rows = 80) {
  if (!is.null(caption)) subhead(caption)
  out <- as.data.frame(df)
  num <- vapply(out, is.numeric, logical(1))
  out[num] <- lapply(out[num], function(x) round(x, digits))
  if (nrow(out) > max_rows) {
    print(utils::head(out, max_rows), row.names = FALSE, right = FALSE)
    cat("  ... ", nrow(out) - max_rows, " more rows in ", file, "\n", sep = "")
  } else {
    print(out, row.names = FALSE, right = FALSE)
  }
  if (!is.null(file)) {
    readr::write_csv(as.data.frame(df), file.path(DIR_TABLES, file), na = "")
    TABLE_INDEX[[file]] <<- if (is.null(caption)) file else caption
  }
  invisible(df)
}

# Registry of every hypothesis test --------------------------------------------
REGISTRY <- list()
register <- function(family, analysis, test, estimate = NA_real_,
                     ci_low = NA_real_, ci_high = NA_real_, statistic = NA_real_,
                     df = NA_character_, p = NA_real_, n = NA_character_,
                     effect = NA_character_, note = NA_character_) {
  REGISTRY[[length(REGISTRY) + 1]] <<- tibble::tibble(
    family = family, analysis = analysis, test = test,
    estimate = as.numeric(estimate), ci_low = as.numeric(ci_low),
    ci_high = as.numeric(ci_high), statistic = as.numeric(statistic),
    df = as.character(df), p = as.numeric(p), n = as.character(n),
    effect = as.character(effect), note = as.character(note))
  invisible(NULL)
}

# Objects passed to the figure script
FIG <- list()

#' Evaluate a model fit, capturing warnings instead of printing them.
capture_fit <- function(expr) {
  warns <- character(0)
  val <- withCallingHandlers(
    tryCatch(expr, error = function(e) structure(list(msg = conditionMessage(e)),
                                                 class = "fit_error")),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning")
    },
    message = function(m) invokeRestart("muffleMessage"))
  list(value = val, warnings = unique(warns),
       ok = !inherits(val, "fit_error"))
}

cat(RULE_MAJOR, "\n", sep = "")
cat("MS_AAI_ToM  |  Statistical analysis log (02_statistical_analysis.R)\n")
cat("Generated : ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n", sep = "")
cat("Root      : ", PROJECT_ROOT, "\n", sep = "")
cat("Seed ", SEED, " | bootstrap B = ", B_BOOT, " | permutation B = ", B_PERM,
    " | CI level = ", CI_LEVEL, "\n", sep = "")
cat(RULE_MAJOR, "\n", sep = "")


# ------------------------------------------------------------------------------
# 0c. STATISTICAL HELPER FUNCTIONS
# ------------------------------------------------------------------------------

#' ICC(1,1), ICC(A,1) [= ICC(2,1)] and ICC(C,1) [= ICC(3,1)] with exact or
#' McGraw & Wong (1996) confidence intervals. Rows = subjects, cols = occasions.
icc_suite <- function(M, conf = CI_LEVEL) {
  M <- as.matrix(M)
  M <- M[stats::complete.cases(M), , drop = FALSE]
  n <- nrow(M); k <- ncol(M)
  out_na <- tibble(n = n, k = k, icc1 = NA_real_, icc1_lo = NA_real_,
                   icc1_hi = NA_real_, iccA = NA_real_, iccA_lo = NA_real_,
                   iccA_hi = NA_real_, iccC = NA_real_, iccC_lo = NA_real_,
                   iccC_hi = NA_real_, sd_between = NA_real_,
                   sd_within = NA_real_, F1 = NA_real_, p1 = NA_real_)
  if (n < 3 || k < 2) return(out_na)
  gm <- mean(M); rm <- rowMeans(M); cm <- colMeans(M)
  SSR <- k * sum((rm - gm)^2); SSC <- n * sum((cm - gm)^2)
  SST <- sum((M - gm)^2);      SSW <- max(0, SST - SSR)
  SSE <- max(0, SST - SSR - SSC)
  MSR <- SSR / (n - 1); MSC <- SSC / (k - 1)
  MSW <- SSW / (n * (k - 1)); MSE <- SSE / ((n - 1) * (k - 1))
  a <- (1 - conf) / 2
  if (MSR == 0 && MSW == 0) return(out_na)

  # ICC(1,1)
  icc1 <- (MSR - MSW) / (MSR + (k - 1) * MSW)
  F1 <- MSR / MSW; d1 <- n - 1; d2 <- n * (k - 1)
  if (is.finite(F1)) {
    FL <- F1 / qf(1 - a, d1, d2); FU <- F1 * qf(1 - a, d2, d1)
    lo1 <- (FL - 1) / (FL + k - 1); hi1 <- (FU - 1) / (FU + k - 1)
    p1 <- pf(F1, d1, d2, lower.tail = FALSE)
  } else { lo1 <- hi1 <- 1; p1 <- 0 }

  # ICC(C,1)
  iccC <- (MSR - MSE) / (MSR + (k - 1) * MSE)
  if (MSE > 0) {
    F3 <- MSR / MSE; d3 <- (n - 1) * (k - 1)
    FL3 <- F3 / qf(1 - a, d1, d3); FU3 <- F3 * qf(1 - a, d3, d1)
    loC <- (FL3 - 1) / (FL3 + k - 1); hiC <- (FU3 - 1) / (FU3 + k - 1)
  } else { loC <- hiC <- 1 }

  # ICC(A,1), McGraw & Wong (1996) case 2A
  iccA <- (MSR - MSE) / (MSR + (k - 1) * MSE + k * (MSC - MSE) / n)
  if (MSE > 0 && iccA < 1) {
    aa <- k * iccA / (n * (1 - iccA))
    bb <- 1 + k * iccA * (n - 1) / (n * (1 - iccA))
    v  <- (aa * MSC + bb * MSE)^2 /
      ((aa * MSC)^2 / (k - 1) + (bb * MSE)^2 / ((n - 1) * (k - 1)))
    Fs <- qf(1 - a, n - 1, v); Fs2 <- qf(1 - a, v, n - 1)
    loA <- n * (MSR - Fs * MSE) / (Fs * (k * MSC + (k * n - k - n) * MSE) + n * MSR)
    hiA <- n * (Fs2 * MSR - MSE) / (k * MSC + (k * n - k - n) * MSE + n * Fs2 * MSR)
  } else { loA <- hiA <- iccA }

  tibble(n = n, k = k, icc1 = icc1, icc1_lo = lo1, icc1_hi = hi1,
         iccA = iccA, iccA_lo = loA, iccA_hi = hiA,
         iccC = iccC, iccC_lo = loC, iccC_hi = hiC,
         sd_between = sqrt(max(0, (MSR - MSW) / k)), sd_within = sqrt(MSW),
         F1 = F1, p1 = p1)
}

#' Krippendorff's alpha for a units x coders matrix (NA allowed).
kripp_alpha <- function(M, level = c("interval", "ordinal", "nominal")) {
  level <- match.arg(level)
  M <- as.matrix(M)
  if (is.character(M) || is.factor(M)) {
    vals <- sort(unique(stats::na.omit(as.vector(M))))
    level <- "nominal"
  } else {
    vals <- sort(unique(stats::na.omit(as.vector(M))))
  }
  K <- length(vals)
  if (K < 2) return(NA_real_)
  C <- vapply(vals, function(v) rowSums(M == v, na.rm = TRUE), numeric(nrow(M)))
  C <- matrix(C, nrow = nrow(M))
  m_u <- rowSums(C)
  keep <- m_u >= 2
  C <- C[keep, , drop = FALSE]; m_u <- m_u[keep]
  if (!nrow(C)) return(NA_real_)
  W <- C / (m_u - 1)
  O <- crossprod(W, C) - diag(colSums(W), K)
  n_c <- rowSums(O); n <- sum(n_c)
  D <- switch(level,
    nominal  = 1 - diag(K),
    interval = outer(as.numeric(vals), as.numeric(vals), "-")^2,
    ordinal  = {
      Dm <- matrix(0, K, K)
      for (c in seq_len(K)) for (k in seq_len(K)) {
        if (c == k) next
        lo <- min(c, k); hi <- max(c, k)
        Dm[c, k] <- (sum(n_c[lo:hi]) - (n_c[lo] + n_c[hi]) / 2)^2
      }
      Dm
    })
  De <- sum(outer(n_c, n_c) * D) / (n - 1)
  if (De == 0) return(NA_real_)
  1 - sum(O * D) / De
}

#' Wilson score interval.
wilson_ci <- function(k, n, conf = CI_LEVEL) {
  z <- qnorm(1 - (1 - conf) / 2); p <- k / n
  d <- 1 + z^2 / n
  ctr <- (p + z^2 / (2 * n)) / d
  hw  <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / d
  c(estimate = p, lower = max(0, ctr - hw), upper = min(1, ctr + hw))
}

#' Percentile bootstrap of the mean of a models x items matrix of proportions.
#' type = "pigeonhole" resamples models AND items independently (Owen, 2007),
#' the correct scheme for a crossed random design; "models" or "items"
#' resample one facet only.
crossed_boot <- function(P, B = B_BOOT, type = c("pigeonhole", "models", "items"),
                         conf = CI_LEVEL) {
  type <- match.arg(type)
  nr <- nrow(P); nc <- ncol(P)
  est <- mean(P, na.rm = TRUE)
  bs <- vapply(seq_len(B), function(b) {
    r <- if (type %in% c("pigeonhole", "models")) sample.int(nr, nr, TRUE) else seq_len(nr)
    c <- if (type %in% c("pigeonhole", "items"))  sample.int(nc, nc, TRUE) else seq_len(nc)
    mean(P[r, c, drop = FALSE], na.rm = TRUE)
  }, numeric(1))
  q <- stats::quantile(bs, c((1 - conf) / 2, 1 - (1 - conf) / 2), na.rm = TRUE)
  c(estimate = est, lower = unname(q[1]), upper = unname(q[2]), se = sd(bs))
}

#' Percentile bootstrap CI for the mean of a vector (resampling its elements).
boot_mean_ci <- function(x, B = B_BOOT, conf = CI_LEVEL) {
  x <- x[!is.na(x)]
  if (length(x) < 2) return(c(estimate = mean(x), lower = NA, upper = NA))
  bs <- replicate(B, mean(x[sample.int(length(x), replace = TRUE)]))
  q <- stats::quantile(bs, c((1 - conf) / 2, 1 - (1 - conf) / 2))
  c(estimate = mean(x), lower = unname(q[1]), upper = unname(q[2]))
}

#' Correlation with percentile bootstrap CI and permutation p-value.
cor_test_full <- function(x, y, method = "spearman", B = B_BOOT, B_p = B_PERM) {
  ok <- stats::complete.cases(x, y); x <- tie_round(x[ok]); y <- tie_round(y[ok]); n <- length(x)
  if (n < 4 || stats::sd(x) == 0 || stats::sd(y) == 0) {
    return(tibble(n = n, r = NA_real_, ci_low = NA_real_, ci_high = NA_real_,
                  p_perm = NA_real_))
  }
  r  <- suppressWarnings(stats::cor(x, y, method = method))
  bs <- replicate(B, {
    i <- sample.int(n, n, TRUE)
    suppressWarnings(stats::cor(x[i], y[i], method = method))
  })
  nul <- replicate(B_p, suppressWarnings(stats::cor(x, sample(y), method = method)))
  q <- stats::quantile(bs, c((1 - CI_LEVEL) / 2, 1 - (1 - CI_LEVEL) / 2), na.rm = TRUE)
  tibble(n = n, r = r, ci_low = unname(q[1]), ci_high = unname(q[2]),
         p_perm = (1 + sum(abs(nul) >= abs(r) - 1e-12, na.rm = TRUE)) / (1 + B_p))
}

#' Welch two-sample comparison from summary statistics, with Hedges' g and a
#' TOST equivalence test against +/- sesoi (raw units).
welch_tost_summary <- function(m1, s1, n1, m2, s2, n2, sesoi) {
  diff <- m1 - m2
  se   <- sqrt(s1^2 / n1 + s2^2 / n2)
  dfw  <- (s1^2 / n1 + s2^2 / n2)^2 /
    ((s1^2 / n1)^2 / (n1 - 1) + (s2^2 / n2)^2 / (n2 - 1))
  t    <- diff / se
  p    <- 2 * stats::pt(-abs(t), dfw)
  ci95 <- diff + c(-1, 1) * stats::qt(0.975, dfw) * se
  ci90 <- diff + c(-1, 1) * stats::qt(0.95,  dfw) * se
  p_lo <- stats::pt((diff + sesoi) / se, dfw, lower.tail = FALSE)
  p_hi <- stats::pt((diff - sesoi) / se, dfw, lower.tail = TRUE)
  sp   <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))
  J    <- 1 - 3 / (4 * (n1 + n2) - 9)
  tibble(diff = diff, se = se, df = dfw, t = t, p = p,
         ci95_lo = ci95[1], ci95_hi = ci95[2],
         ci90_lo = ci90[1], ci90_hi = ci90[2],
         p_tost = max(p_lo, p_hi), sesoi = sesoi,
         min_equiv_bound = max(abs(ci90)),
         hedges_g = J * diff / sp)
}

#' Paired analysis of a vector of within-model differences: t-test, Wilcoxon,
#' sign test, d_z, and TOST against +/- sesoi.
paired_suite <- function(d, sesoi = NA_real_) {
  d <- tie_round(d[!is.na(d)]); n <- length(d)
  tt <- stats::t.test(d)
  wx <- suppressWarnings(stats::wilcox.test(d, exact = NULL, conf.int = FALSE))
  bt <- stats::binom.test(sum(d > 0), sum(d != 0))
  se <- stats::sd(d) / sqrt(n)
  out <- tibble(n = n, mean_diff = mean(d), sd_diff = stats::sd(d),
                ci_low = tt$conf.int[1], ci_high = tt$conf.int[2],
                t = unname(tt$statistic), df = n - 1, p_t = tt$p.value,
                V = unname(wx$statistic), p_wilcoxon = wx$p.value,
                n_positive = sum(d > 0), n_negative = sum(d < 0),
                p_sign = bt$p.value, d_z = mean(d) / stats::sd(d))
  if (!is.na(sesoi)) {
    p_lo <- stats::pt((mean(d) + sesoi) / se, n - 1, lower.tail = FALSE)
    p_hi <- stats::pt((mean(d) - sesoi) / se, n - 1, lower.tail = TRUE)
    ci90 <- mean(d) + c(-1, 1) * stats::qt(0.95, n - 1) * se
    out <- mutate(out, sesoi = sesoi, p_tost = max(p_lo, p_hi),
                  ci90_low = ci90[1], ci90_high = ci90[2],
                  min_equiv_bound = max(abs(ci90)))
  }
  out
}

#' Random-effects meta-analysis (REML, Knapp-Hartung) with prediction interval.
meta_re <- function(yi, sei, mods = NULL, data = NULL) {
  ok <- is.finite(yi) & is.finite(sei) & sei > 0
  fit <- if (is.null(mods)) {
    metafor::rma(yi = yi[ok], sei = sei[ok], method = "REML", test = "knha")
  } else {
    metafor::rma(yi = yi[ok], sei = sei[ok], mods = mods, data = data[ok, , drop = FALSE],
                 method = "REML", test = "knha")
  }
  fit
}
meta_summary <- function(fit) {
  pr <- stats::predict(fit)
  tibble(k = fit$k, estimate = as.numeric(fit$b[1]), ci_low = fit$ci.lb[1],
         ci_high = fit$ci.ub[1], p = fit$pval[1],
         pi_low = pr$pi.lb[1], pi_high = pr$pi.ub[1],
         tau = sqrt(fit$tau2), I2 = fit$I2, Q = fit$QE, Q_df = fit$k - fit$p,
         Q_p = fit$QEp)
}

#' Adjusted Rand index between two partitions.
adj_rand <- function(a, b) {
  tab <- table(a, b); n <- sum(tab)
  s_ij <- sum(choose(tab, 2)); s_a <- sum(choose(rowSums(tab), 2))
  s_b <- sum(choose(colSums(tab), 2)); e <- s_a * s_b / choose(n, 2)
  (s_ij - e) / (0.5 * (s_a + s_b) - e)
}

#' Cramer's V for an r x c table.
cramers_v <- function(tab) {
  chi <- suppressWarnings(stats::chisq.test(tab, correct = FALSE)$statistic)
  as.numeric(sqrt(chi / (sum(tab) * (min(dim(tab)) - 1))))
}

#' Glmer with a sequence of fallback random-effect structures. Returns the
#' first non-singular, converged fit (or the last fit that ran).
fit_glmer_seq <- function(formulas, data, family = stats::binomial()) {
  last <- NULL
  for (f in formulas) {
    res <- capture_fit(lme4::glmer(
      f, data = data, family = family,
      control = lme4::glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5))))
    if (!res$ok) next
    conv_bad <- any(grepl("converge|Hessian|eigenvalue", res$warnings, ignore.case = TRUE))
    last <- list(fit = res$value, formula = f, warnings = res$warnings)
    if (!lme4::isSingular(res$value) && !conv_bad) return(last)
  }
  last
}

#' Likelihood-ratio test between two nested (g)lmer fits (ML), computed from
#' the log-likelihoods so it does not depend on how the data were passed.
lrt <- function(full, reduced) {
  l1 <- stats::logLik(full); l0 <- stats::logLik(reduced)
  chisq <- max(0, 2 * (as.numeric(l1) - as.numeric(l0)))
  df <- attr(l1, "df") - attr(l0, "df")
  tibble(chisq = chisq, df = df, p = stats::pchisq(chisq, df, lower.tail = FALSE))
}

#' Fit a glmer with the same control settings used throughout.
glmer_fit <- function(formula, data, family = stats::binomial()) {
  capture_fit(lme4::glmer(formula, data = data, family = family,
                          control = lme4::glmerControl(optimizer = "bobyqa",
                                                       optCtrl = list(maxfun = 2e5))))
}

#' Fixed-effect table (log-odds) with Wald CIs, as odds ratios.
glmm_or_table <- function(fit) {
  co <- summary(fit)$coefficients
  z  <- stats::qnorm(1 - (1 - CI_LEVEL) / 2)
  tibble(term = rownames(co), log_odds = co[, 1], se = co[, 2],
         OR = exp(co[, 1]), OR_low = exp(co[, 1] - z * co[, 2]),
         OR_high = exp(co[, 1] + z * co[, 2]), z = co[, 3], p = co[, 4])
}

#' Variance components of a merMod as a tidy table.
varcomp <- function(fit) {
  vc <- as.data.frame(lme4::VarCorr(fit))
  vc <- vc[is.na(vc$var2), c("grp", "var1", "vcov", "sdcor")]
  vc$grp <- sub("\\.[0-9]+$", "", vc$grp)
  names(vc) <- c("group", "term", "variance", "sd")
  vc$share <- vc$variance / sum(vc$variance)
  tibble::as_tibble(vc)
}

# ==============================================================================
# SECTION 1. DATA PREPARATION AND QUALITY AUDIT
# ==============================================================================

banner(1, "Data preparation and quality audit")

read_results <- function(f) {
  path <- file.path(DIR_RESULTS, f)
  if (!file.exists(path)) stop("Missing input file: ", path, call. = FALSE)
  x <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE,
                       guess_max = 10000)
  names(x) <- sub("^\ufeff", "", names(x))
  if ("repeat" %in% names(x)) x <- dplyr::rename(x, run = `repeat`)
  x
}

# Model metadata ----------------------------------------------------------------
# Display names and developer groupings. Edit here if endpoints change.
MODEL_LOOKUP <- tibble::tribble(
  ~short,                          ~display,             ~developer,  ~family,
  "gpt-4o",                        "GPT-4o",             "OpenAI",    "GPT-4",
  "gpt-4.1",                       "GPT-4.1",            "OpenAI",    "GPT-4",
  "gpt-5",                         "GPT-5",              "OpenAI",    "GPT-5",
  "gpt-5.1",                       "GPT-5.1",            "OpenAI",    "GPT-5",
  "gpt-5.2",                       "GPT-5.2",            "OpenAI",    "GPT-5",
  "gpt-5.4",                       "GPT-5.4",            "OpenAI",    "GPT-5",
  "gpt-5.5",                       "GPT-5.5",            "OpenAI",    "GPT-5",
  "gpt-5.6-sol",                   "GPT-5.6-sol",        "OpenAI",    "GPT-5",
  "gpt-oss-20b",                   "GPT-OSS 20B",        "OpenAI",    "GPT-OSS",
  "gpt-oss-120b",                  "GPT-OSS 120B",       "OpenAI",    "GPT-OSS",
  "claude-haiku-4-5",              "Claude Haiku 4.5",   "Anthropic", "Claude 4",
  "claude-sonnet-4",               "Claude Sonnet 4",    "Anthropic", "Claude 4",
  "claude-sonnet-4-5",             "Claude Sonnet 4.5",  "Anthropic", "Claude 4",
  "claude-sonnet-4-6",             "Claude Sonnet 4.6",  "Anthropic", "Claude 4",
  "claude-opus-4-1",               "Claude Opus 4.1",    "Anthropic", "Claude 4",
  "claude-opus-4-5",               "Claude Opus 4.5",    "Anthropic", "Claude 4",
  "claude-opus-4-6",               "Claude Opus 4.6",    "Anthropic", "Claude 4",
  "claude-opus-4-7",               "Claude Opus 4.7",    "Anthropic", "Claude 4",
  "claude-opus-4-8",               "Claude Opus 4.8",    "Anthropic", "Claude 4",
  "claude-sonnet-5",               "Claude Sonnet 5",    "Anthropic", "Claude 5",
  "claude-opus-5",                 "Claude Opus 5",      "Anthropic", "Claude 5",
  "claude-fable-5",                "Claude Fable 5",     "Anthropic", "Claude 5",
  "claude-fable-5-1",              "Claude Fable 5.1",   "Anthropic", "Claude 5",
  "gemma-3-12b",                   "Gemma 3 12B",        "Google",    "Gemma",
  "llama-4-maverick",              "Llama 4 Maverick",   "Meta",      "Llama",
  "meta-llama-3-1-8b-instruct",    "Llama 3.1 8B",       "Meta",      "Llama",
  "meta-llama-3-3-70b-instruct",   "Llama 3.3 70B",      "Meta",      "Llama",
  "qwen3-next-80b-a3b-instruct",   "Qwen3-Next 80B",     "Alibaba",   "Qwen",
  "qwen35-122b-a10b",              "Qwen3.5 122B",       "Alibaba",   "Qwen"
)
DEVELOPER_LEVELS <- c("OpenAI", "Anthropic", "Google", "Meta", "Alibaba", "Other")

make_model_meta <- function(labels) {
  tibble(model_label = sort(unique(labels))) %>%
    mutate(short = str_remove(model_label, "^(azure|databricks)-")) %>%
    left_join(MODEL_LOOKUP, by = "short") %>%
    mutate(
      display   = coalesce(display, short),
      developer = coalesce(developer, case_when(
        str_detect(short, "gpt")    ~ "OpenAI",
        str_detect(short, "claude") ~ "Anthropic",
        str_detect(short, "gemma|gemini") ~ "Google",
        str_detect(short, "llama")  ~ "Meta",
        str_detect(short, "qwen")   ~ "Alibaba",
        TRUE ~ "Other")),
      family    = coalesce(family, developer),
      developer = factor(developer, levels = DEVELOPER_LEVELS),
      # Three-level grouping used when the smaller developers must be pooled
      developer_group = factor(case_when(
        developer == "OpenAI" ~ "OpenAI",
        developer == "Anthropic" ~ "Anthropic",
        TRUE ~ "Other developers"),
        levels = c("OpenAI", "Anthropic", "Other developers")),
      origin      = if_else(developer == "Alibaba", "China", "US"),
      open_weight = str_detect(short, "gpt-oss|gemma|llama|qwen")
    )
}

classify_outcome <- function(scored, parse_error, raw) {
  refusal_rx <- paste0("(?i)(sorry|can[\u2019']?t|cannot|unable to|won[\u2019']?t|",
                       "not able to|decline|\u4e0d\u80fd|\u65e0\u6cd5|",
                       "\u62b1\u6b49|\u62d2\u7edd)")
  raw <- as.character(raw)
  case_when(
    scored                                   ~ "scored",
    !is.na(parse_error) & parse_error == "API_ERROR" ~ "api_error",
    is.na(raw) | str_trim(raw) == ""         ~ "empty_response",
    str_detect(raw, refusal_rx)              ~ "refusal",
    TRUE                                     ~ "unparseable"
  )
}
OUTCOME_LEVELS <- c("scored", "refusal", "empty_response", "unparseable", "api_error")

# RMET ---------------------------------------------------------------------------
prep_rmet <- function(x, source_label) {
  x %>%
    mutate(
      source      = source_label,
      item        = as.integer(str_extract(question_id, "\\d+")),
      definitions_shown = as.logical(definitions_shown),
      condition   = factor(if_else(definitions_shown, "Definitions", "No definitions"),
                           levels = c("No definitions", "Definitions")),
      choice_l    = str_to_lower(str_trim(as.character(choice))),
      target_l    = str_to_lower(str_trim(correct_answer)),
      opts        = str_split(str_to_lower(options), ",\\s*"),
      choice_idx  = map2_int(opts, choice_l, ~ match(.y, str_trim(.x))),
      target_idx  = map2_int(opts, target_l, ~ match(.y, str_trim(.x))),
      valid_choice = !is.na(choice_idx),
      outcome     = classify_outcome(valid_choice, parse_error, raw_response),
      correct     = if_else(outcome == "scored", as.integer(choice_l == target_l), NA_integer_)
    )
}

rmet_raw  <- prep_rmet(read_results(FILES$rmet_main), "Sep 2026 (5 repeats)")
rmet_june <- prep_rmet(read_results(FILES$rmet_june), "Jun 2026 (1 run)")

audit_rmet <- tibble(
  check = c("Scored choices not among the four options",
            "is_correct in file disagrees with recomputed correctness",
            "Items with more than one answer key"),
  n = c(sum(!is.na(rmet_raw$choice) & !rmet_raw$valid_choice),
        sum(rmet_raw$outcome == "scored" &
              as.logical(rmet_raw$is_correct) != as.logical(rmet_raw$correct), na.rm = TRUE),
        sum(tapply(rmet_raw$target_l, rmet_raw$item, function(v) length(unique(v))) > 1))
)

# SDO / VSA ----------------------------------------------------------------------
SCALE_BOUNDS <- tibble::tribble(
  ~instrument, ~scale_min, ~scale_max,
  "SDO7s",      1,          7,
  "SDO6",       1,          7,
  "VSA",       -4,          4
)
# Reverse-keyed (con-trait) items. SDO6 keys come from the file (items 9-16);
# SDO7s and VSA keys are restored from the published instruments because the
# source file does not carry them (Ho et al., 2015; Bizumic & Duckitt, 2018).
REVERSE_KEYS <- c("sdo-q3", "sdo-q4", "sdo-q7", "sdo-q8",
                  "vsa-q1", "vsa-q4", "vsa-q5")
# Subscales for SDO7s and VSA. VSA dimensions follow Duckitt's ACT model:
# items 1-2 conservatism (submission), 3-4 traditionalism, 5-6 authoritarianism
# (aggression). CHECK against Bizumic & Duckitt (2018) before citing subscales.
SUBSCALE_MAP <- c(
  "sdo-q1" = "dominance", "sdo-q2" = "dominance",
  "sdo-q3" = "dominance", "sdo-q4" = "dominance",
  "sdo-q5" = "anti_egalitarianism", "sdo-q6" = "anti_egalitarianism",
  "sdo-q7" = "anti_egalitarianism", "sdo-q8" = "anti_egalitarianism",
  "vsa-q1" = "conservatism",     "vsa-q2" = "conservatism",
  "vsa-q3" = "traditionalism",   "vsa-q4" = "traditionalism",
  "vsa-q5" = "authoritarianism", "vsa-q6" = "authoritarianism"
)
INSTR_LEVELS <- c("SDO7s", "SDO6-EN", "SDO6-ZH", "VSA")

prep_sdo <- function(x, source_label) {
  if (!"instrument" %in% names(x)) {
    x <- x %>% mutate(instrument = if_else(category == "vsa", "VSA", "SDO"),
                      language = "en", item_no = NA_real_, subscale = NA_character_,
                      reverse_scored = FALSE, value_recoded = NA_real_)
  }
  x %>%
    mutate(
      source     = source_label,
      instrument = if_else(instrument == "SDO", "SDO7s", instrument),
      language   = factor(language, levels = c("en", "zh")),
      item_no    = as.integer(coalesce(item_no,
                     suppressWarnings(as.numeric(str_extract(question_id, "\\d+$"))))),
      item_uid   = sprintf("%s-%02d", instrument, item_no),
      inst_lang  = factor(case_when(
        instrument == "SDO6" & language == "en" ~ "SDO6-EN",
        instrument == "SDO6" & language == "zh" ~ "SDO6-ZH",
        TRUE ~ instrument), levels = INSTR_LEVELS),
      subscale   = coalesce(subscale, unname(SUBSCALE_MAP[question_id])),
      reverse_key = question_id %in% REVERSE_KEYS |
        (instrument == "SDO6" & !is.na(reverse_scored) & reverse_scored),
      keying     = if_else(reverse_key, "con-trait", "pro-trait")
    ) %>%
    left_join(SCALE_BOUNDS, by = "instrument") %>%
    mutate(
      midpoint  = (scale_min + scale_max) / 2,
      span      = scale_max - scale_min,
      outcome   = classify_outcome(!is.na(value), parse_error, raw_response),
      value_key = case_when(
        outcome != "scored"               ~ NA_real_,
        reverse_key & instrument == "VSA" ~ -value,
        reverse_key                       ~ (scale_min + scale_max) - value,
        TRUE                              ~ value),
      value_raw = if_else(outcome == "scored", value, NA_real_),
      pos       = (value_key - scale_min) / span,    # 0 = construct floor
      pos_raw   = (value_raw - scale_min) / span     # 0 = strongest disagreement
    )
}

sdo_raw  <- prep_sdo(read_results(FILES$sdo_main), "Aug 2026 (5 repeats)")
ITEM_TEXT <- sdo_raw %>% filter(language == "en") %>%
  distinct(instrument, item_no, subscale, keying, question_text) %>% arrange(instrument, item_no)
SDO6_TEXT <- with(filter(ITEM_TEXT, instrument == "SDO6"), setNames(question_text, item_no))
sdo_june <- prep_sdo(read_results(FILES$sdo_june), "Jun 2026 (5 repeats)")

audit_sdo <- sdo_raw %>%
  filter(outcome == "scored") %>%
  group_by(instrument) %>%
  summarise(n_scored = n(),
            n_reverse_rows = sum(reverse_key),
            source_recode_agrees = sum(!is.na(value_recoded) & value_recoded == value_key),
            source_recode_missing_or_wrong = sum(is.na(value_recoded) | value_recoded != value_key),
            .groups = "drop")

# Model exclusions ----------------------------------------------------------------
api_share <- bind_rows(
  rmet_raw  %>% transmute(dataset = "RMET Sep",  model_label, outcome),
  rmet_june %>% transmute(dataset = "RMET Jun",  model_label, outcome),
  sdo_raw   %>% transmute(dataset = "SDO/VSA Aug", model_label, outcome),
  sdo_june  %>% transmute(dataset = "SDO/VSA Jun", model_label, outcome)
) %>%
  group_by(dataset, model_label) %>%
  summarise(n_calls = n(), api_fail = mean(outcome == "api_error"), .groups = "drop")

EXCLUDED <- api_share %>% filter(api_fail > MAX_API_FAIL)
excluded_in <- function(ds) EXCLUDED$model_label[EXCLUDED$dataset == ds]

rmet  <- rmet_raw  %>% filter(!model_label %in% excluded_in("RMET Sep"))
rmetJ <- rmet_june %>% filter(!model_label %in% excluded_in("RMET Jun"))
sdo   <- sdo_raw   %>% filter(!model_label %in% excluded_in("SDO/VSA Aug"))
sdoJ  <- sdo_june  %>% filter(!model_label %in% excluded_in("SDO/VSA Jun"))

MODELS <- make_model_meta(c(rmet_raw$model_label, rmet_june$model_label,
                            sdo_raw$model_label, sdo_june$model_label)) %>%
  mutate(in_rmet = model_label %in% rmet$model_label,
         in_sdo  = model_label %in% sdo$model_label,
         in_rmet_june = model_label %in% rmetJ$model_label,
         in_sdo_june  = model_label %in% sdoJ$model_label,
         excluded_note = map_chr(model_label, function(m) {
           e <- EXCLUDED$dataset[EXCLUDED$model_label == m]
           if (length(e)) paste("API failure in:", paste(e, collapse = "; ")) else ""
         }))

add_meta <- function(df) {
  df %>% left_join(MODELS %>% select(model_label, display, developer,
                                     developer_group, family, origin, open_weight),
                   by = "model_label")
}

# Report -------------------------------------------------------------------------
design_tab <- bind_rows(
  rmet %>% group_by(dataset = "RMET (Sep 2026)", unit = as.character(condition)) %>%
    summarise(models = n_distinct(model_label), items = n_distinct(item),
              runs = n_distinct(run), calls = n(), .groups = "drop"),
  rmetJ %>% group_by(dataset = "RMET (Jun 2026)", unit = as.character(condition)) %>%
    summarise(models = n_distinct(model_label), items = n_distinct(item),
              runs = n_distinct(run), calls = n(), .groups = "drop"),
  sdo %>% group_by(dataset = "SDO/VSA (Aug 2026)", unit = as.character(inst_lang)) %>%
    summarise(models = n_distinct(model_label), items = n_distinct(item_no),
              runs = n_distinct(run), calls = n(), .groups = "drop"),
  sdoJ %>% group_by(dataset = "SDO/VSA (Jun 2026)", unit = as.character(inst_lang)) %>%
    summarise(models = n_distinct(model_label), items = n_distinct(item_no),
              runs = n_distinct(run), calls = n(), .groups = "drop")
)
report_table(design_tab, "S01_design.csv",
             "Table S1.1. Analysis design after exclusions")

report_table(EXCLUDED %>% left_join(MODELS %>% select(model_label, display), by = "model_label"),
             "S01_excluded_models.csv",
             paste0("Table S1.2. Models excluded (API failure > ", 100 * MAX_API_FAIL, "% of calls)"))

report_table(audit_rmet, "S01_audit_rmet.csv", "Table S1.3. RMET data-integrity checks")
report_table(audit_sdo, "S01_audit_sdo_recoding.csv",
             "Table S1.4. Reverse-key recoding audit (source value_recoded vs re-derived key)")
note("The source value_recoded column is correct for SDO6 only. SDO7s and VSA ",
     "reverse-keyed items are re-derived here from the published keys; every ",
     "analysis below uses the re-derived metric (higher = more of the construct).")

outcome_tab <- bind_rows(
  rmet_raw  %>% count(dataset = "RMET Sep", model_label, outcome),
  rmet_june %>% count(dataset = "RMET Jun", model_label, outcome),
  sdo_raw   %>% count(dataset = "SDO/VSA Aug", model_label, outcome),
  sdo_june  %>% count(dataset = "SDO/VSA Jun", model_label, outcome)
) %>%
  mutate(outcome = factor(outcome, levels = OUTCOME_LEVELS)) %>%
  pivot_wider(names_from = outcome, values_from = n, values_fill = 0,
              names_expand = TRUE) %>%
  mutate(n_calls = scored + refusal + empty_response + unparseable + api_error) %>%
  filter(n_calls != scored) %>%
  arrange(dataset, desc(n_calls - scored))
report_table(outcome_tab, "S01_nonscored_by_model.csv",
             "Table S1.5. Non-scored responses by model (models with any)")
note("Outcomes are disaggregated because they are different phenomena: API errors ",
     "are infrastructure failures, empty responses carry no content, refusals are ",
     "model behaviour and are content dependent, and unparseable responses (for ",
     "example truncated JSON) are harness failures.")

# Are refusals content- and language-dependent? SDO6 is the only instrument
# administered in both languages. Stratifying by model respects the fact that
# refusals cluster in a few models (a pooled Fisher test would not).
sdo6_miss <- sdo %>% filter(instrument == "SDO6") %>%
  mutate(refused = outcome == "refusal")
ref_by_item <- sdo %>%
  filter(outcome %in% c("refusal", "empty_response", "unparseable")) %>%
  count(inst_lang, item_no, subscale, keying, outcome) %>%
  arrange(desc(n))
report_table(ref_by_item, "S01_nonscored_by_item.csv",
             "Table S1.6. SDO/VSA refusals, empty and unparseable responses by item")

if (sum(sdo6_miss$refused) > 0) {
  refusers <- unique(sdo6_miss$model_label[sdo6_miss$refused])
  arr_lang <- xtabs(~ language + refused + model_label,
                    data = sdo6_miss %>% filter(model_label %in% refusers) %>%
                      mutate(refused = factor(refused, c(FALSE, TRUE))))
  cmh_lang <- if (length(refusers) > 1) {
    stats::mantelhaen.test(arr_lang, exact = TRUE)
  } else {
    stats::fisher.test(arr_lang[, , 1])
  }
  arr_sub <- xtabs(~ subscale + refused + model_label,
                   data = sdo6_miss %>% filter(model_label %in% refusers) %>%
                     mutate(refused = factor(refused, c(FALSE, TRUE))))
  cmh_sub <- if (length(refusers) > 1) {
    stats::mantelhaen.test(arr_sub, exact = TRUE)
  } else {
    stats::fisher.test(arr_sub[, , 1])
  }
  subhead("Refusal x language and refusal x subscale (SDO6), stratified by model")
  say("Models with any SDO6 refusal: ", paste(MODELS$display[MODELS$model_label %in% refusers],
                                              collapse = ", "))
  say("Refusals: English ", sum(sdo6_miss$refused & sdo6_miss$language == "en"),
      ", Chinese ", sum(sdo6_miss$refused & sdo6_miss$language == "zh"))
  say("Exact conditional test, language: OR = ", fmt_n(cmh_lang$estimate),
      " (Chinese vs English), 95% CI ", fmt_ci(cmh_lang$conf.int[1], cmh_lang$conf.int[2]),
      ", ", fp(cmh_lang$p.value))
  say("Exact conditional test, subscale: OR = ", fmt_n(cmh_sub$estimate),
      " (dominance vs anti-egalitarianism), ", fp(cmh_sub$p.value))
  register("S01 Missingness", "SDO6 refusals", "Exact conditional (CMH) test: refusal x language, stratified by model",
           estimate = cmh_lang$estimate, ci_low = cmh_lang$conf.int[1],
           ci_high = cmh_lang$conf.int[2], p = cmh_lang$p.value,
           n = paste(length(refusers), "refusing models"), effect = "odds ratio (ZH vs EN)")
  register("S01 Missingness", "SDO6 refusals", "Exact conditional (CMH) test: refusal x subscale, stratified by model",
           estimate = cmh_sub$estimate, p = cmh_sub$p.value,
           n = paste(length(refusers), "refusing models"), effect = "odds ratio")
  note("Refusals are behaviour, not noise. They are analysed as missing in the ",
       "sections below, and Section 13 checks that the language effect survives ",
       "restricting to pairs that were scored in both languages.")
}

FIG$models  <- MODELS
FIG$item_text <- ITEM_TEXT
FIG$outcome <- outcome_tab

# Analysis-ready subsets --------------------------------------------------------
rmet_ok  <- rmet  %>% filter(outcome == "scored") %>% add_meta()
rmetJ_ok <- rmetJ %>% filter(outcome == "scored") %>% add_meta()
sdo_ok   <- sdo   %>% filter(outcome == "scored") %>% add_meta()
sdoJ_ok  <- sdoJ  %>% filter(outcome == "scored") %>% add_meta()

say("RMET (Sep): ", n_distinct(rmet_ok$model_label), " models, ",
    nrow(rmet_ok), " scored responses (", pct(nrow(rmet_ok) / nrow(rmet)), " of calls)")
say("SDO/VSA (Aug): ", n_distinct(sdo_ok$model_label), " models, ",
    nrow(sdo_ok), " scored responses (", pct(nrow(sdo_ok) / nrow(sdo)), " of calls)")

# ==============================================================================
# SECTION 2. RMET ACCURACY AND THE HUMAN BENCHMARK
# ==============================================================================

banner(2, "RMET accuracy and the human benchmark")
note("Unit of generalisation. Accuracy is averaged over repeats within each ",
     "model-item pair, then over items (equal item weights). Uncertainty comes ",
     "from BOTH the sample of models and the sample of items, so the headline ",
     "interval uses a two-way (pigeonhole) bootstrap that resamples models and ",
     "items independently (Owen, 2007). Naive trial-level intervals treat 7,500 ",
     "correlated responses as independent and are shown only for contrast.")

rmet_pairs <- rmet_ok %>%
  group_by(model_label, condition, item) %>%
  summarise(n = n(), k = sum(correct), p = k / n,
            n_choices = n_distinct(choice_idx),
            modal_share = max(tabulate(choice_idx, 4)) / n, .groups = "drop") %>%
  mutate(changed = n_choices > 1)

to_matrix <- function(df, value, row = "model_label", col = "item") {
  w <- df %>% select(all_of(c(row, col, value))) %>%
    pivot_wider(names_from = all_of(col), values_from = all_of(value))
  M <- as.matrix(w[, -1]); rownames(M) <- w[[row]]; M
}
P_nodef <- to_matrix(filter(rmet_pairs, condition == "No definitions"), "p")
P_def   <- to_matrix(filter(rmet_pairs, condition == "Definitions"), "p")
P_def   <- P_def[rownames(P_nodef), colnames(P_nodef)]
P_pool  <- (P_nodef + P_def) / 2

# Overall accuracy with four CI schemes --------------------------------------------
overall_rows <- list(
  list(label = "No definitions", P = P_nodef, cond = "No definitions"),
  list(label = "Definitions",    P = P_def,   cond = "Definitions"),
  list(label = "Pooled",         P = P_pool,  cond = c("No definitions", "Definitions")))
rmet_overall <- map_dfr(overall_rows, function(r) {
  trials <- rmet_ok %>% filter(condition %in% r$cond)
  w  <- wilson_ci(sum(trials$correct), nrow(trials))
  pg <- crossed_boot(r$P, type = "pigeonhole")
  md <- crossed_boot(r$P, type = "models")
  it <- crossed_boot(r$P, type = "items")
  tibble(condition = r$label, accuracy = pg["estimate"],
         score36 = RMET_ITEMS * pg["estimate"],
         naive_wilson_lo = w["lower"], naive_wilson_hi = w["upper"],
         items_boot_lo = it["lower"], items_boot_hi = it["upper"],
         models_boot_lo = md["lower"], models_boot_hi = md["upper"],
         pigeonhole_lo = pg["lower"], pigeonhole_hi = pg["upper"])
})
report_table(rmet_overall, "S02_rmet_accuracy_overall.csv",
             "Table S2.1. RMET accuracy with four interval schemes (95%)", digits = 4)
note("The pigeonhole interval is the defensible one. Its width relative to the ",
     "naive Wilson interval is itself an aggregation result: pooled trial counts ",
     "overstate precision because responses are nested in models and items.")

# Per-model accuracy (item bootstrap) -----------------------------------------------
model_acc_tab <- function(P, label) {
  map_dfr(rownames(P), function(m) {
    ci <- boot_mean_ci(P[m, ])
    tibble(model_label = m, condition = label, accuracy = ci["estimate"],
           ci_low = ci["lower"], ci_high = ci["upper"])
  })
}
rmet_model <- bind_rows(model_acc_tab(P_nodef, "No definitions"),
                        model_acc_tab(P_def, "Definitions"),
                        model_acc_tab(P_pool, "Pooled")) %>%
  add_meta() %>%
  mutate(score36 = RMET_ITEMS * accuracy,
         human_z = (score36 - HUMAN_RMET$mean[1]) / HUMAN_RMET$sd[1],
         human_percentile = 100 * stats::pnorm(human_z))

rmet_model_wide <- rmet_model %>%
  select(display, developer, condition, accuracy, ci_low, ci_high) %>%
  mutate(txt = paste0(fmt_n(100 * accuracy, 1), " ", fmt_ci(100 * ci_low, 100 * ci_high, 1))) %>%
  select(display, developer, condition, txt) %>%
  pivot_wider(names_from = condition, values_from = txt)
pooled_rank <- rmet_model %>% filter(condition == "Pooled") %>%
  arrange(desc(accuracy)) %>%
  transmute(display, score36, human_percentile,
            vs_human_mean = case_when(
              tie_round(RMET_ITEMS * ci_high) < HUMAN_RMET$mean[1] ~ "below",
              tie_round(RMET_ITEMS * ci_low)  > HUMAN_RMET$mean[1] ~ "above",
              TRUE ~ "CI includes human mean"))
report_table(rmet_model_wide %>% left_join(pooled_rank, by = "display") %>%
               arrange(desc(score36)),
             "S02_rmet_accuracy_by_model.csv",
             "Table S2.2. RMET accuracy by model, % [95% item-bootstrap CI]; score out of 36")

# Comparison with the human benchmark ---------------------------------------------
llm_scores <- rmet_model %>% filter(condition == "Pooled") %>% pull(score36)
bench <- map_dfr(1:2, function(i) {
  h <- HUMAN_RMET[i, ]
  welch_tost_summary(mean(llm_scores), sd(llm_scores), length(llm_scores),
                     h$mean, h$sd, h$n, SESOI_ITEMS) %>%
    mutate(benchmark = h$group, human_mean = h$mean, llm_mean = mean(llm_scores),
           llm_sd = sd(llm_scores), n_models = length(llm_scores), .before = 1)
})
report_table(bench, "S02_rmet_vs_human.csv",
             paste0("Table S2.3. LLM models vs human RMET benchmarks (Welch t, Hedges g, ",
                    "TOST with SESOI = +/-", SESOI_ITEMS, " items)"))
for (i in seq_len(nrow(bench))) {
  b <- bench[i, ]
  say(b$benchmark, ": LLM mean ", fmt_n(b$llm_mean), " vs ", fmt_n(b$human_mean),
      " items; difference ", fmt_n(b$diff), " ", fmt_ci(b$ci95_lo, b$ci95_hi),
      ", Welch t(", fmt_n(b$df, 1), ") = ", fmt_n(b$t), ", ", fp(b$p),
      ", g = ", fmt_n(b$hedges_g), "; TOST ", fp(b$p_tost),
      "; equivalence holds for any bound >= ", fmt_n(b$min_equiv_bound), " items")
  register("S02 RMET benchmark", paste("LLM vs", b$benchmark), "Welch two-sample t (summary statistics)",
           estimate = b$diff, ci_low = b$ci95_lo, ci_high = b$ci95_hi,
           statistic = b$t, df = round(b$df, 1), p = b$p,
           n = paste0(b$n_models, " models vs ", HUMAN_RMET$n[i], " humans"),
           effect = paste0("difference in items; g = ", round(b$hedges_g, 2)))
  register("S02 RMET benchmark", paste("LLM vs", b$benchmark),
           paste0("TOST equivalence (+/-", SESOI_ITEMS, " items)"),
           estimate = b$diff, ci_low = b$ci90_lo, ci_high = b$ci90_hi, p = b$p_tost,
           n = paste0(b$n_models, " models"), effect = "90% CI of difference",
           note = paste0("minimal equivalence bound = ", round(b$min_equiv_bound, 2), " items"))
}
note("'Matches the human benchmark' is an equivalence claim, so it needs TOST, ",
     "not a non-significant t-test. The minimal equivalence bound is reported so ",
     "a reader can apply a different SESOI. Contamination caveat: the RMET items and ",
     "answer key are public, so accuracy is a performance floor, not evidence of ",
     "capacity.")

FIG$rmet_model   <- rmet_model
FIG$rmet_overall <- rmet_overall
FIG$human_rmet   <- HUMAN_RMET
FIG$rmet_bench   <- bench


# ==============================================================================
# SECTION 3. RMET: THE DEFINITIONS MANIPULATION
# ==============================================================================

banner(3, "RMET: does showing word definitions change accuracy?")

rmet_glmm_dat <- rmet_ok %>%
  mutate(defs_c = if_else(condition == "Definitions", 0.5, -0.5),
         item_f = factor(item), pair = interaction(model_label, item_f, drop = TRUE))

f_defs <- list(
  correct ~ defs_c + (1 + defs_c | model_label) + (1 + defs_c | item_f) + (1 | pair),
  correct ~ defs_c + (1 + defs_c || model_label) + (1 + defs_c || item_f) + (1 | pair),
  correct ~ defs_c + (1 | model_label) + (1 | item_f) + (1 | pair),
  correct ~ defs_c + (1 | model_label) + (1 | item_f))
g_defs <- fit_glmer_seq(f_defs, rmet_glmm_dat)
subhead("Mixed-effects logistic regression (crossed random effects)")
say("Selected structure: ", deparse(g_defs$formula, width.cutoff = 200))
if (length(g_defs$warnings)) say("Fit warnings: ", paste(g_defs$warnings, collapse = " | "))
or_defs <- glmm_or_table(g_defs$fit)
report_table(or_defs, "S03_rmet_glmm_fixed.csv", "Table S3.1. GLMM fixed effects (odds ratios)")
report_table(varcomp(g_defs$fit), "S03_rmet_glmm_varcomp.csv",
             "Table S3.2. GLMM variance components (logit scale)")
note("The model-item pair variance dominates: most pairs are answered the same ",
     "way on every repeat, and models differ in WHICH items they get right. The ",
     "large conditional intercept is a consequence of that near-determinism, not ",
     "a fitting problem; the definitions odds ratio is the quantity of interest.")
f0 <- update(g_defs$formula, . ~ . - defs_c)
g0 <- glmer_fit(f0, rmet_glmm_dat)
lrt_defs <- lrt(g_defs$fit, g0$value)
r_defs <- or_defs %>% filter(term == "defs_c")
say("Definitions: OR = ", fmt_n(r_defs$OR), " ", fmt_ci(r_defs$OR_low, r_defs$OR_high),
    "; likelihood-ratio chi2(1) = ", fmt_n(lrt_defs$chisq), ", ", fp(lrt_defs$p))
register("S03 RMET definitions", "Definitions effect", "GLMM likelihood-ratio test",
         estimate = r_defs$OR, ci_low = r_defs$OR_low, ci_high = r_defs$OR_high,
         statistic = lrt_defs$chisq, df = lrt_defs$df, p = lrt_defs$p,
         n = paste(nrow(rmet_glmm_dat), "responses"), effect = "odds ratio")

# Model-level paired analysis (the unit at which models are compared) -------------
defs_model <- rmet_model %>% filter(condition != "Pooled") %>%
  select(model_label, display, developer_group, condition, accuracy) %>%
  pivot_wider(names_from = condition, values_from = accuracy) %>%
  mutate(diff_pp = 100 * (Definitions - `No definitions`))
ps_defs <- paired_suite(defs_model$diff_pp, sesoi = SESOI_PP)
report_table(ps_defs, "S03_rmet_definitions_paired.csv",
             paste0("Table S3.3. Model-level paired comparison, definitions minus none (% points); ",
                    "TOST SESOI = +/-", fmt_n(SESOI_PP, 1), " pp"))
say("Mean difference ", fmt_n(ps_defs$mean_diff), " pp ", fmt_ci(ps_defs$ci_low, ps_defs$ci_high),
    "; paired t(", ps_defs$df, ") = ", fmt_n(ps_defs$t), ", ", fp(ps_defs$p_t),
    "; Wilcoxon V = ", ps_defs$V, ", ", fp(ps_defs$p_wilcoxon),
    "; TOST ", fp(ps_defs$p_tost), " (equivalent within +/-", fmt_n(ps_defs$min_equiv_bound), " pp)")
register("S03 RMET definitions", "Definitions effect (model level)", "Paired t-test",
         estimate = ps_defs$mean_diff, ci_low = ps_defs$ci_low, ci_high = ps_defs$ci_high,
         statistic = ps_defs$t, df = ps_defs$df, p = ps_defs$p_t, n = paste(ps_defs$n, "models"),
         effect = paste0("% points; d_z = ", round(ps_defs$d_z, 2)))
register("S03 RMET definitions", "Definitions effect (model level)", "Wilcoxon signed-rank",
         estimate = ps_defs$mean_diff, statistic = ps_defs$V, p = ps_defs$p_wilcoxon,
         n = paste(ps_defs$n, "models"), effect = "% points")
register("S03 RMET definitions", "Definitions effect (model level)",
         paste0("TOST equivalence (+/-", round(SESOI_PP, 1), " pp)"),
         estimate = ps_defs$mean_diff, ci_low = ps_defs$ci90_low, ci_high = ps_defs$ci90_high,
         p = ps_defs$p_tost, n = paste(ps_defs$n, "models"), effect = "90% CI, % points")

# Repeated-measures ANOVA: condition x repeat, models as subjects -----------------
sitting <- rmet_ok %>%
  group_by(model_label, condition, run) %>%
  summarise(acc = mean(correct), n_items = n(), .groups = "drop") %>%
  mutate(run = factor(run))
rm_fit <- capture_fit(afex::aov_ez(id = "model_label", dv = "acc", data = sitting,
                                   within = c("condition", "run"),
                                   anova_table = list(correction = "GG", es = "ges")))
if (rm_fit$ok) {
  at <- as.data.frame(rm_fit$value$anova_table)
  at$effect <- rownames(at)
  rm_tab <- tibble(effect = at$effect, df_num = at$`num Df`, df_den = at$`den Df`,
                   MSE = at$MSE, F = at$F, ges = at$ges, p_GG = at$`Pr(>F)`)
  report_table(rm_tab, "S03_rmet_rm_anova.csv",
               "Table S3.4. Repeated-measures ANOVA on sitting accuracy (Greenhouse-Geisser)")
  for (i in seq_len(nrow(rm_tab))) {
    register("S03 RMET definitions", "RM-ANOVA (models as subjects)",
             paste("F-test:", rm_tab$effect[i]), statistic = rm_tab$F[i],
             df = paste0(round(rm_tab$df_num[i], 2), ", ", round(rm_tab$df_den[i], 2)),
             p = rm_tab$p_GG[i], n = paste(n_distinct(sitting$model_label), "models"),
             effect = paste0("generalised eta^2 = ", signif(rm_tab$ges[i], 3)))
  }
  note("A sitting is one complete pass through the 36 items. The run factor tests ",
       "exchangeability of repeats: a significant run effect would mean the five ",
       "administrations are not interchangeable replicates.")
}

# Random-effects meta-analysis of per-model effects --------------------------------
defs_pairs <- rmet_pairs %>%
  select(model_label, item, condition, p) %>%
  pivot_wider(names_from = condition, values_from = p) %>%
  mutate(d = Definitions - `No definitions`)
defs_studies <- defs_pairs %>%
  group_by(model_label) %>%
  summarise(k_items = sum(!is.na(d)), yi = mean(d, na.rm = TRUE),
            sei = sd(d, na.rm = TRUE) / sqrt(k_items),
            p1 = mean(`No definitions`, na.rm = TRUE), p2 = mean(Definitions, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(sei = if_else(sei > 0, sei,
                       sqrt(p1 * (1 - p1) / (k_items * N_RUNS) + p2 * (1 - p2) / (k_items * N_RUNS))),
         t_item = yi / sei,
         p_item = 2 * stats::pt(-abs(t_item), k_items - 1),
         p_item_bh = p.adjust(p_item, "BH")) %>%
  add_meta()
ma_defs <- meta_re(defs_studies$yi * 100, defs_studies$sei * 100)
ms_defs <- meta_summary(ma_defs)
report_table(ms_defs, "S03_rmet_definitions_meta.csv",
             "Table S3.5. Random-effects meta-analysis of per-model definition effects (% points)")
mr_defs <- meta_re(defs_studies$yi * 100, defs_studies$sei * 100,
                   mods = ~ developer_group, data = defs_studies)
say("Pooled effect ", fmt_n(ms_defs$estimate), " pp ", fmt_ci(ms_defs$ci_low, ms_defs$ci_high),
    "; 95% prediction interval ", fmt_ci(ms_defs$pi_low, ms_defs$pi_high),
    "; tau = ", fmt_n(ms_defs$tau), " pp, I2 = ", fmt_n(ms_defs$I2, 1), "%, Q(",
    ms_defs$Q_df, ") = ", fmt_n(ms_defs$Q), ", ", fp(ms_defs$Q_p))
say("Moderator (developer group): F(", mr_defs$QMdf[1], ", ", mr_defs$QMdf[2], ") = ",
    fmt_n(mr_defs$QM), ", ", fp(mr_defs$QMp))
register("S03 RMET definitions", "Meta-analysis of per-model effects", "Random-effects pooled estimate (REML, Knapp-Hartung)",
         estimate = ms_defs$estimate, ci_low = ms_defs$ci_low, ci_high = ms_defs$ci_high,
         p = ms_defs$p, n = paste(ms_defs$k, "models"), effect = "% points",
         note = paste0("95% PI [", round(ms_defs$pi_low, 2), ", ", round(ms_defs$pi_high, 2), "]"))
register("S03 RMET definitions", "Meta-analysis of per-model effects", "Cochran Q heterogeneity",
         estimate = ms_defs$I2, statistic = ms_defs$Q, df = ms_defs$Q_df, p = ms_defs$Q_p,
         n = paste(ms_defs$k, "models"), effect = "I^2 (%)")
register("S03 RMET definitions", "Meta-analysis of per-model effects", "Meta-regression: developer group",
         statistic = mr_defs$QM, df = paste(mr_defs$QMdf, collapse = ", "), p = mr_defs$QMp,
         n = paste(ms_defs$k, "models"), effect = "omnibus F")
report_table(defs_studies %>% transmute(display, developer, effect_pp = 100 * yi, se_pp = 100 * sei,
                                        p_item, p_item_bh) %>% arrange(effect_pp),
             "S03_rmet_definitions_by_model.csv",
             "Table S3.6. Per-model definition effect (paired over items), BH-adjusted")
note("The meta-analysis answers a different question from the GLMM: not only ",
     "whether the average effect is zero, but how much it varies between models. ",
     "The prediction interval is the range expected for a new model.")

FIG$defs_studies <- defs_studies
FIG$defs_meta    <- ms_defs
FIG$defs_paired  <- ps_defs

# ==============================================================================
# SECTION 4. RMET RELIABILITY ACROSS FIVE REPEATS
# ==============================================================================

banner(4, "RMET reliability across five identical repeats")
note("A model-item pair 'changed' if the five answers to that item (within one ",
     "condition) were not all the same word. Chance-corrected agreement uses ",
     "Krippendorff's alpha on the chosen option (nominal), with items as units ",
     "and repeats as coders.")

C_nodef <- to_matrix(filter(rmet_pairs, condition == "No definitions") %>%
                       mutate(ch = as.numeric(changed)), "ch")
C_def   <- to_matrix(filter(rmet_pairs, condition == "Definitions") %>%
                       mutate(ch = as.numeric(changed)), "ch")[rownames(C_nodef), colnames(C_nodef)]
C_pool  <- (C_nodef + C_def) / 2

instab_overall <- map_dfr(list(list("No definitions", C_nodef), list("Definitions", C_def),
                               list("Pooled", C_pool)), function(r) {
  pg <- crossed_boot(r[[2]], type = "pigeonhole")
  tibble(condition = r[[1]], pct_pairs_changed = 100 * pg["estimate"],
         ci_low = 100 * pg["lower"], ci_high = 100 * pg["upper"])
})
report_table(instab_overall, "S04_rmet_instability_overall.csv",
             "Table S4.1. RMET model-item pairs that changed across 5 repeats (%, pigeonhole 95% CI)")

# Per model: instability, alpha, mean pairwise agreement, sitting SD ----------------
pair_agree <- function(v) {                   # P(two random repeats agree)
  v <- v[!is.na(v)]; n <- length(v)
  if (n < 2) return(NA_real_)
  tb <- table(v); sum(tb * (tb - 1)) / (n * (n - 1))
}
rmet_runs_wide <- rmet_ok %>%
  select(model_label, condition, item, run, choice_idx) %>%
  pivot_wider(names_from = run, values_from = choice_idx, names_prefix = "r")
run_cols_r <- grep("^r[0-9]", names(rmet_runs_wide), value = TRUE)

rmet_rel_model <- rmet_runs_wide %>%
  group_by(model_label, condition) %>%
  group_modify(function(d, key) {
    M <- as.matrix(d[, run_cols_r])
    tibble(n_items = nrow(M),
           pct_changed = 100 * mean(apply(M, 1, function(r) length(unique(na.omit(r))) > 1)),
           mean_pair_agreement = mean(apply(M, 1, pair_agree), na.rm = TRUE),
           alpha_nominal = kripp_alpha(M, "nominal"))
  }) %>% ungroup()

sit_sd <- sitting %>% group_by(model_label, condition) %>%
  summarise(sitting_sd_items = RMET_ITEMS * sd(acc),
            sitting_range_items = RMET_ITEMS * diff(range(acc)), .groups = "drop")
rmet_rel_model <- rmet_rel_model %>% left_join(sit_sd, by = c("model_label", "condition"))

pooled_instab <- map_dfr(rownames(C_pool), function(m) {
  ci <- boot_mean_ci(C_pool[m, ])
  tibble(model_label = m, condition = "Pooled", pct_changed = tie_round(100 * ci["estimate"]),
         ci_low = 100 * ci["lower"], ci_high = 100 * ci["upper"])
})
rmet_instab_model <- bind_rows(
  rmet_rel_model %>% select(model_label, condition, pct_changed) %>%
    mutate(condition = as.character(condition)) %>%
    left_join(map_dfr(c("No definitions", "Definitions"), function(cn) {
      Cm <- if (cn == "Definitions") C_def else C_nodef
      map_dfr(rownames(Cm), function(m) {
        ci <- boot_mean_ci(Cm[m, ])
        tibble(model_label = m, condition = cn, ci_low = 100 * ci["lower"],
               ci_high = 100 * ci["upper"])
      })
    }), by = c("model_label", "condition")),
  pooled_instab) %>% add_meta()

report_table(rmet_rel_model %>% add_meta() %>%
               select(display, developer, condition, pct_changed, mean_pair_agreement,
                      alpha_nominal, sitting_sd_items, sitting_range_items) %>%
               arrange(condition, desc(pct_changed)),
             "S04_rmet_reliability_by_model.csv",
             "Table S4.2. RMET reliability by model and condition")
note("sitting_sd_items is the standard deviation of a model's total score (out of ",
     "36) across its five sittings, i.e. the test-retest standard error of ",
     "measurement for that model.")

# Aggregate (sitting-score) reliability ------------------------------------------
sit_icc <- map_dfr(levels(sitting$condition), function(cn) {
  W <- sitting %>% filter(condition == cn) %>% select(model_label, run, acc) %>%
    pivot_wider(names_from = run, values_from = acc)
  ic <- icc_suite(as.matrix(W[, -1]))
  mutate(ic, condition = cn, SEM_items = RMET_ITEMS * sd_within, .before = 1)
})
rmet_alpha_pooled <- rmet_runs_wide %>% group_by(condition) %>%
  summarise(alpha_all_pairs = kripp_alpha(as.matrix(pick(all_of(run_cols_r))), "nominal"),
            .groups = "drop")
report_table(sit_icc %>% left_join(rmet_alpha_pooled %>% mutate(condition = as.character(condition)),
                                   by = "condition"),
             "S04_rmet_aggregate_reliability.csv",
             "Table S4.3. Aggregate reliability: sitting-score ICCs vs pair-level alpha")
note("This is the aggregation contrast for the RMET: total scores are highly ",
     "reproducible across sittings even though a non-trivial share of individual ",
     "item answers change.")

# Does showing definitions change stability? ----------------------------------------
instab_pairs <- rmet_pairs %>% mutate(defs_c = if_else(condition == "Definitions", 0.5, -0.5),
                                      item_f = factor(item), changed = as.integer(changed)) %>%
  add_meta()
g_inst <- fit_glmer_seq(list(changed ~ defs_c + (1 | model_label) + (1 | item_f)), instab_pairs)
g_inst0 <- glmer_fit(changed ~ 1 + (1 | model_label) + (1 | item_f), instab_pairs)
lrt_inst <- lrt(g_inst$fit, g_inst0$value)
or_inst <- glmm_or_table(g_inst$fit) %>% filter(term == "defs_c")
inst_paired <- rmet_rel_model %>% select(model_label, condition, pct_changed) %>%
  pivot_wider(names_from = condition, values_from = pct_changed) %>%
  mutate(d = Definitions - `No definitions`)
ps_inst <- paired_suite(inst_paired$d)
subhead("Effect of definitions on instability")
say("GLMM (pairs): OR = ", fmt_n(or_inst$OR), " ", fmt_ci(or_inst$OR_low, or_inst$OR_high),
    ", LR chi2(1) = ", fmt_n(lrt_inst$chisq), ", ", fp(lrt_inst$p))
say("Model level: mean change ", fmt_n(ps_inst$mean_diff), " pp ", fmt_ci(ps_inst$ci_low, ps_inst$ci_high),
    ", Wilcoxon V = ", ps_inst$V, ", ", fp(ps_inst$p_wilcoxon),
    "; ", ps_inst$n_positive, " models less stable with definitions, ",
    ps_inst$n_negative, " more stable")
register("S04 RMET reliability", "Definitions and instability", "GLMM likelihood-ratio test",
         estimate = or_inst$OR, ci_low = or_inst$OR_low, ci_high = or_inst$OR_high,
         statistic = lrt_inst$chisq, df = 1, p = lrt_inst$p,
         n = paste(nrow(instab_pairs), "pairs"), effect = "odds ratio (changed)")
register("S04 RMET reliability", "Definitions and instability", "Wilcoxon signed-rank (model level)",
         estimate = ps_inst$mean_diff, statistic = ps_inst$V, p = ps_inst$p_wilcoxon,
         n = paste(ps_inst$n, "models"), effect = "% points of pairs changed")

# Developer differences ------------------------------------------------------------
dev_inst <- rmet_instab_model %>% filter(condition == "Pooled")
kw_dev <- stats::kruskal.test(pct_changed ~ developer_group, data = dev_inst)
oa <- dev_inst %>% filter(developer_group %in% c("OpenAI", "Anthropic"))
wt_oa <- stats::t.test(pct_changed ~ droplevels(developer_group), data = oa)
perm_diff <- function(y, g, B = B_PERM) {
  g <- as.character(g); lv <- unique(g)
  obs <- mean(y[g == lv[1]]) - mean(y[g == lv[2]])
  nul <- replicate(B, { gg <- sample(g); mean(y[gg == lv[1]]) - mean(y[gg == lv[2]]) })
  list(obs = obs, p = (1 + sum(abs(nul) >= abs(obs) - 1e-12)) / (1 + B), levels = lv)
}
pm_oa <- perm_diff(oa$pct_changed, oa$developer_group)
g_dev <- glmer_fit(changed ~ developer_group + defs_c + (1 | model_label) + (1 | item_f), instab_pairs)
g_dev0 <- glmer_fit(changed ~ defs_c + (1 | model_label) + (1 | item_f), instab_pairs)
lrt_dev <- lrt(g_dev$value, g_dev0$value)

report_table(dev_inst %>% group_by(developer_group) %>%
               summarise(n_models = n(), mean_pct_changed = mean(pct_changed),
                         median = median(pct_changed), min = min(pct_changed),
                         max = max(pct_changed), .groups = "drop"),
             "S04_rmet_instability_by_developer.csv",
             "Table S4.4. RMET instability by developer group (pooled over conditions)")
say("Kruskal-Wallis: chi2(", kw_dev$parameter, ") = ", fmt_n(kw_dev$statistic), ", ", fp(kw_dev$p.value))
say("OpenAI vs Anthropic: Welch t(", fmt_n(wt_oa$parameter, 1), ") = ", fmt_n(wt_oa$statistic),
    ", ", fp(wt_oa$p.value), "; permutation ", fp(pm_oa$p), " (difference ",
    fmt_n(pm_oa$obs), " pp, ", pm_oa$levels[1], " minus ", pm_oa$levels[2], ")")
say("GLMM: developer group LR chi2(", lrt_dev$df, ") = ", fmt_n(lrt_dev$chisq), ", ", fp(lrt_dev$p))
register("S04 RMET reliability", "Instability by developer", "Kruskal-Wallis (3 groups, model level)",
         statistic = kw_dev$statistic, df = kw_dev$parameter, p = kw_dev$p.value,
         n = paste(nrow(dev_inst), "models"))
register("S04 RMET reliability", "Instability by developer", "Welch t-test: OpenAI vs Anthropic",
         estimate = unname(diff(rev(wt_oa$estimate))), ci_low = wt_oa$conf.int[1],
         ci_high = wt_oa$conf.int[2], statistic = wt_oa$statistic,
         df = round(wt_oa$parameter, 1), p = wt_oa$p.value, n = paste(nrow(oa), "models"),
         effect = "% points")
register("S04 RMET reliability", "Instability by developer", "Permutation test: OpenAI vs Anthropic",
         estimate = pm_oa$obs, p = pm_oa$p, n = paste(nrow(oa), "models"), effect = "% points")
register("S04 RMET reliability", "Instability by developer", "GLMM likelihood-ratio test (pairs)",
         statistic = lrt_dev$chisq, df = lrt_dev$df, p = lrt_dev$p, n = paste(nrow(instab_pairs), "pairs"))

# Are less accurate models less stable? -------------------------------------------
acc_inst <- rmet_model %>% filter(condition == "Pooled") %>% select(model_label, accuracy) %>%
  left_join(dev_inst %>% select(model_label, pct_changed), by = "model_label")
ct_ai <- cor_test_full(acc_inst$accuracy, acc_inst$pct_changed)
say("Accuracy vs instability across models: Spearman rho = ", fmt_n(ct_ai$r), " ",
    fmt_ci(ct_ai$ci_low, ct_ai$ci_high), ", permutation ", fp(ct_ai$p_perm))
register("S04 RMET reliability", "Accuracy vs instability", "Spearman correlation (model level)",
         estimate = ct_ai$r, ci_low = ct_ai$ci_low, ci_high = ct_ai$ci_high, p = ct_ai$p_perm,
         n = paste(ct_ai$n, "models"), effect = "rho")

FIG$rmet_instab_model <- rmet_instab_model
FIG$rmet_rel_model    <- rmet_rel_model %>% add_meta()
FIG$rmet_sitting      <- sitting %>% add_meta()
FIG$rmet_sit_icc      <- sit_icc


# ==============================================================================
# SECTION 5. RMET ITEMS: DIFFICULTY, ERROR TYPE AND FOIL CONCENTRATION
# ==============================================================================

banner(5, "RMET items: difficulty, stochastic vs systematic error, foils")

item_words <- rmet_ok %>% distinct(item, target_l, opts) %>%
  group_by(item) %>% slice(1) %>% ungroup()

# Variance decomposition per item (law of total variance over model x condition)
item_tab <- rmet_pairs %>%
  group_by(item) %>%
  summarise(accuracy = mean(p),
            pct_pairs_changed = 100 * mean(changed),
            within_var = mean(p * (1 - p)),
            between_var = mean((p - mean(p))^2),
            .groups = "drop") %>%
  mutate(total_var = accuracy * (1 - accuracy),
         stochastic_share = if_else(total_var > 0, within_var / total_var, NA_real_),
         systematic_share = 1 - stochastic_share)

# Choice shares per item, weighting every model x condition equally
choice_share <- rmet_ok %>%
  count(model_label, condition, item, choice_idx) %>%
  group_by(model_label, condition, item) %>% mutate(share = n / sum(n)) %>% ungroup() %>%
  complete(nesting(model_label, condition, item), choice_idx = 1:4, fill = list(n = 0, share = 0)) %>%
  group_by(item, choice_idx) %>% summarise(share = tie_round(mean(share)), .groups = "drop") %>%
  left_join(item_words %>% select(item, opts, target_l), by = "item") %>%
  mutate(option = map2_chr(opts, choice_idx, ~ str_trim(.x[.y])),
         is_target = option == target_l)
retention <- choice_share %>% group_by(item) %>%
  summarise(target = option[is_target], target_share = share[is_target],
            top_foil = option[!is_target][which.max(share[!is_target])],
            top_foil_share = max(share[!is_target]), .groups = "drop") %>%
  mutate(passes_target_rule = target_share >= RETENTION_TARGET,
         passes_foil_rule = top_foil_share <= RETENTION_FOIL,
         retained = passes_target_rule & passes_foil_rule)

# Foil concentration: does each erring model's modal wrong answer pile onto one
# foil? Null: modal wrong answers are uniform over the three foils.
modal_foil <- rmet_ok %>% filter(correct == 0) %>%
  count(model_label, item, choice_idx) %>%
  group_by(model_label, item) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>% ungroup()
foil_test <- modal_foil %>% group_by(item) %>%
  summarise(n_models_erring = n(),
            top_foil_models = max(tabulate(choice_idx, 4)),
            chisq = {
              cnt <- tabulate(choice_idx, 4)
              tgt <- item_words$opts[[match(item[1], item_words$item)]]
              tidx <- match(item_words$target_l[match(item[1], item_words$item)], str_trim(tgt))
              o <- cnt[-tidx]; e <- rep(sum(o) / 3, 3); sum((o - e)^2 / e)
            }, .groups = "drop")
set.seed(SEED + 5)
foil_test <- foil_test %>% mutate(p_mc = map2_dbl(n_models_erring, chisq, function(n, x) {
  if (n < 3) return(NA_real_)
  sims <- stats::rmultinom(B_PERM, n, rep(1 / 3, 3))
  e <- n / 3; nul <- colSums((sims - e)^2 / e)
  (1 + sum(nul >= x - 1e-12)) / (1 + B_PERM)
})) %>% mutate(p_bh = p.adjust(p_mc, "BH"))
# Omnibus: sum of chi-square statistics across items vs simulated sum
omni_items <- foil_test %>% filter(!is.na(p_mc))
nul_sum <- rowSums(sapply(omni_items$n_models_erring, function(n) {
  sims <- stats::rmultinom(B_PERM, n, rep(1 / 3, 3)); e <- n / 3
  colSums((sims - e)^2 / e)
}))
p_omni_foil <- (1 + sum(nul_sum >= sum(omni_items$chisq) - 1e-9)) / (1 + B_PERM)

item_full <- item_tab %>% left_join(retention, by = "item") %>%
  left_join(foil_test, by = "item") %>% arrange(accuracy)
report_table(item_full %>% select(item, target, accuracy, pct_pairs_changed, stochastic_share,
                                  target_share, top_foil, top_foil_share, retained,
                                  n_models_erring, top_foil_models, p_mc, p_bh),
             "S05_rmet_items.csv",
             "Table S5.1. RMET item analysis (sorted by accuracy)")
say("Items failing the Baron-Cohen retention rule in LLM responses: ",
    sum(!item_full$retained), " of ", nrow(item_full), " (target < 50%: ",
    sum(!item_full$passes_target_rule), "; a foil > 25%: ", sum(!item_full$passes_foil_rule), ")")
say("Mean stochastic share of item error variance: ",
    pct(mean(item_full$stochastic_share, na.rm = TRUE)),
    " (the remainder is systematic: models disagree with each other but each is consistent)")
say("Foil concentration: ", sum(foil_test$p_bh < ALPHA, na.rm = TRUE), " of ",
    sum(!is.na(foil_test$p_mc)), " testable items show a dominant wrong answer (BH < .05); ",
    "omnibus Monte Carlo ", fp(p_omni_foil))
register("S05 RMET items", "Foil concentration", "Omnibus Monte Carlo test (sum of item chi-squares)",
         statistic = sum(omni_items$chisq), p = p_omni_foil,
         n = paste(nrow(omni_items), "items"), note = "null: modal wrong answers uniform over 3 foils")
for (i in which(!is.na(foil_test$p_mc))) {
  register("S05 RMET items (per item)", paste("Item", foil_test$item[i]),
           "Monte Carlo chi-square: modal wrong answer uniform over foils",
           statistic = foil_test$chisq[i], p = foil_test$p_mc[i],
           n = paste(foil_test$n_models_erring[i], "models erring"))
}
note("Criterion from Baron-Cohen et al. (2001): an item was retained only if at ",
     "least 50% of controls chose the target and no foil drew more than 25%. ",
     "Items that fail it here are items on which LLMs do not reproduce the human ",
     "consensus. A dominant shared foil indicates a systematic misreading rather ",
     "than guessing.")

FIG$rmet_items <- item_full
FIG$rmet_choice_share <- choice_share %>% select(item, choice_idx, option, is_target, share)


# ==============================================================================
# SECTION 6. RMET: DO MODELS MAKE THE SAME MISTAKES?
# ==============================================================================

banner(6, "RMET: between-model error consistency and clustering")
note("Error consistency (Geirhos et al., 2020) is Cohen's kappa for trial-by-trial ",
     "agreement in correctness, with chance agreement set by the two models' ",
     "accuracies. It asks whether two models fail on the SAME items beyond what ",
     "their accuracy alone predicts. Units are the 72 item-by-condition combinations; ",
     "each model's repeat distribution is used, not a single answer.")

P_units <- cbind(P_nodef, P_def)          # models x 72 units (proportion correct)
mods_r  <- rownames(P_units)
acc_m   <- rowMeans(P_units, na.rm = TRUE)
nM <- length(mods_r)
EC <- matrix(NA_real_, nM, nM, dimnames = list(mods_r, mods_r))
for (a in seq_len(nM)) for (b in seq_len(nM)) {
  pa <- P_units[a, ]; pb <- P_units[b, ]
  c_obs <- mean(pa * pb + (1 - pa) * (1 - pb), na.rm = TRUE)
  c_exp <- acc_m[a] * acc_m[b] + (1 - acc_m[a]) * (1 - acc_m[b])
  EC[a, b] <- (c_obs - c_exp) / (1 - c_exp)
}

# Wrong-answer agreement: when both models err on a unit, do they pick the same foil?
foil_dist <- rmet_ok %>% filter(correct == 0) %>%
  count(model_label, condition, item, choice_idx) %>%
  group_by(model_label, condition, item) %>% mutate(q = n / sum(n)) %>% ungroup()
wrong_agree <- foil_dist %>%
  inner_join(foil_dist, by = c("condition", "item", "choice_idx"),
             suffix = c("_a", "_b"), relationship = "many-to-many") %>%
  filter(model_label_a < model_label_b) %>%
  group_by(model_label_a, model_label_b, condition, item) %>%
  summarise(same = sum(q_a * q_b), .groups = "drop")
both_err <- foil_dist %>% distinct(model_label, condition, item)
both_err <- both_err %>% inner_join(both_err, by = c("condition", "item"),
                                    suffix = c("_a", "_b"), relationship = "many-to-many") %>%
  filter(model_label_a < model_label_b)
wrong_agree <- both_err %>% left_join(wrong_agree, by = c("model_label_a", "model_label_b",
                                                          "condition", "item")) %>%
  mutate(same = coalesce(same, 0))

dev_of <- setNames(as.character(MODELS$developer), MODELS$model_label)
wa_summary <- wrong_agree %>%
  mutate(same_dev = dev_of[model_label_a] == dev_of[model_label_b]) %>%
  group_by(same_dev) %>%
  summarise(n_unit_pairs = n(), p_same_foil = mean(same), .groups = "drop")

# Within- vs between-developer error consistency, permutation of developer labels
dev_vec <- dev_of[mods_r]
ec_contrast <- function(lab) {
  same <- outer(lab, lab, "==")
  off  <- upper.tri(EC)
  mean(EC[off & same]) - mean(EC[off & !same])
}
obs_ec <- ec_contrast(dev_vec)
set.seed(SEED + 6)
nul_ec <- replicate(B_PERM, ec_contrast(sample(dev_vec)))
p_ec <- (1 + sum(nul_ec >= obs_ec - 1e-12)) / (1 + B_PERM)

subhead("Error consistency")
off <- upper.tri(EC)
say("Mean between-model error consistency (kappa): ", fmt_n(mean(EC[off]), 3),
    " (range ", fmt_n(min(EC[off]), 2), " to ", fmt_n(max(EC[off]), 2), ")")
say("Same developer minus different developer: ", fmt_n(obs_ec, 3),
    ", one-sided permutation ", fp(p_ec))
say("When two models both err on an item, they choose the same wrong word with ",
    "probability ", fmt_n(mean(wrong_agree$same), 3), " (uniform chance = 0.333; see permutation test below)")
report_table(wa_summary, "S06_rmet_wrong_answer_agreement.csv",
             "Table S6.1. Probability that two erring models choose the same foil")
# Permutation null for shared wrong answers: within each model and item x condition,
# the three foil labels are shuffled. This keeps each model's own consistency (a
# model that always picks one foil still does) and breaks only cross-model
# convergence on the SAME foil, which is the hypothesis of interest.
unit_key <- rmet_ok %>% distinct(condition, item) %>% arrange(condition, item) %>% mutate(u = row_number())
foil_slots <- item_words %>% mutate(foils = map2(opts, target_l, ~ which(str_trim(.x) != .y))) %>%
  select(item, foils)
nU <- nrow(unit_key)
Q <- array(0, dim = c(nM, nU, 3), dimnames = list(mods_r, NULL, NULL))
fd <- foil_dist %>% left_join(unit_key, by = c("condition", "item")) %>%
  left_join(foil_slots, by = "item") %>%
  mutate(slot = map2_int(foils, choice_idx, ~ match(.y, .x)), m = match(model_label, mods_r))
Q[cbind(fd$m, fd$u, fd$slot)] <- fd$q
erred <- apply(Q, c(1, 2), sum) > 0
same_dev_mat <- outer(dev_vec, dev_vec, "==")
foil_stat <- function(Qa) {
  num <- c(all = 0, same = 0, diff = 0); den <- num
  for (u in seq_len(nU)) {
    G <- Qa[, u, ] %*% t(Qa[, u, ])
    both <- outer(erred[, u], erred[, u]) & upper.tri(G)
    num["all"] <- num["all"] + sum(G[both]); den["all"] <- den["all"] + sum(both)
    num["same"] <- num["same"] + sum(G[both & same_dev_mat]); den["same"] <- den["same"] + sum(both & same_dev_mat)
    num["diff"] <- num["diff"] + sum(G[both & !same_dev_mat]); den["diff"] <- den["diff"] + sum(both & !same_dev_mat)
  }
  num / den
}
perms3 <- as.matrix(expand.grid(1:3, 1:3, 1:3)); perms3 <- perms3[apply(perms3, 1, function(r) length(unique(r)) == 3), ]
Qm <- matrix(Q, nM * nU, 3)
obs_foil <- foil_stat(Q)
set.seed(SEED + 61)
nul_foil <- t(replicate(2000, {
  P <- perms3[sample.int(6, nrow(Qm), replace = TRUE), ]
  Qp <- matrix(Qm[cbind(rep(seq_len(nrow(Qm)), 3), as.vector(P))], nrow(Qm), 3)
  foil_stat(array(Qp, c(nM, nU, 3)))
}))
p_foil_all  <- (1 + sum(nul_foil[, "all"] >= obs_foil["all"] - 1e-12)) / (1 + nrow(nul_foil))
p_foil_same <- (1 + sum((nul_foil[, "same"] - nul_foil[, "diff"]) >=
                          (obs_foil["same"] - obs_foil["diff"]) - 1e-12)) / (1 + nrow(nul_foil))
say("Shared wrong answers: observed P(same foil | both wrong) = ", fmt_n(obs_foil["all"], 3),
    " vs label-permutation null mean ", fmt_n(mean(nul_foil[, "all"]), 3), ", one-sided ", fp(p_foil_all))
say("Same developer ", fmt_n(obs_foil["same"], 3), " vs different developer ", fmt_n(obs_foil["diff"], 3),
    "; excess over the null difference ", fp(p_foil_same))
register("S06 RMET error consistency", "Shared wrong answers", "Foil-label permutation test (within model and item)",
         estimate = obs_foil["all"], p = p_foil_all, n = paste(nM, "models x", nU, "units"),
         effect = "P(same foil | both wrong)", note = paste0("null mean ", round(mean(nul_foil[, "all"]), 3)))
register("S06 RMET error consistency", "Shared wrong answers, same vs different developer",
         "Foil-label permutation test of the difference",
         estimate = obs_foil["same"] - obs_foil["diff"], p = p_foil_same,
         n = paste(nM, "models"), effect = "difference in P(same foil)")

# Hierarchical clustering on 1 - kappa (average linkage) ------------------------------
D_ec <- stats::as.dist(1 - EC)
hc_ec <- stats::hclust(D_ec, method = "average")
sil_ec <- map_dfr(2:min(6, nM - 1), function(k) {
  cl <- stats::cutree(hc_ec, k)
  tibble(k = k, avg_silhouette = mean(cluster::silhouette(cl, D_ec)[, "sil_width"]))
})
k_ec <- sil_ec$k[which.max(sil_ec$avg_silhouette)]
cl_ec <- stats::cutree(hc_ec, k_ec)
coph <- stats::cor(stats::cophenetic(hc_ec), D_ec)
report_table(sil_ec, "S06_rmet_cluster_silhouette.csv",
             "Table S6.2. Average silhouette width by number of clusters (error consistency)")
ec_clusters <- tibble(model_label = names(cl_ec), cluster = cl_ec) %>% add_meta()
tab_ec <- table(ec_clusters$cluster, ec_clusters$developer_group)
ft_ec <- stats::fisher.test(tab_ec, simulate.p.value = TRUE, B = B_PERM)
say("Chosen k = ", k_ec, " (max silhouette ", fmt_n(max(sil_ec$avg_silhouette), 3),
    "); cophenetic correlation = ", fmt_n(coph, 3))
say("Cluster x developer group: Fisher exact (Monte Carlo) ", fp(ft_ec$p.value),
    ", Cramer's V = ", fmt_n(cramers_v(tab_ec)))
report_table(ec_clusters %>% select(display, developer, cluster) %>% arrange(cluster, developer),
             "S06_rmet_error_clusters.csv", "Table S6.3. Error-consistency clusters")
register("S06 RMET error consistency", "Clusters vs developer", "Fisher exact test (Monte Carlo)",
         estimate = cramers_v(tab_ec), p = ft_ec$p.value, n = paste(nM, "models"),
         effect = "Cramer's V")

readr::write_csv(as.data.frame(EC) %>% tibble::rownames_to_column("model_label"),
                 file.path(DIR_TABLES, "S06_rmet_error_consistency_matrix.csv"))
FIG$rmet_EC        <- EC
FIG$rmet_EC_order  <- hc_ec$labels[hc_ec$order]
FIG$rmet_EC_hclust <- hc_ec
FIG$rmet_EC_clusters <- ec_clusters

# ==============================================================================
# SECTION 7. SDO/VSA RELIABILITY AT THREE LEVELS OF AGGREGATION
# ==============================================================================

banner(7, "SDO/VSA reliability at three levels of aggregation")
note("The aggregation paradox is quantified here. The same five administrations ",
     "are summarised at three levels: the single item response (model-item pair), ",
     "the subscale score and the full-scale score. Reliability coefficients rise ",
     "with aggregation while the share of item responses that change stays high.")

sdo_complete <- sdo_ok %>%
  group_by(model_label, inst_lang, item_no) %>% filter(n() == N_RUNS) %>% ungroup()
sdo_pair <- sdo_complete %>%
  arrange(run) %>%
  group_by(model_label, inst_lang, instrument, language, item_no, item_uid, subscale,
           keying, scale_min, scale_max, span, midpoint) %>%
  summarise(mean_key = mean(value_key), sd_key = sd(value_key),
            range = max(value_key) - min(value_key),
            n_values = n_distinct(value_key),
            pos1 = first(pos), mean_pos = tie_round(mean(pos)), mean_raw = mean(value_raw),
            at_floor = all(value_key == scale_min[1]),
            at_ceiling = all(value_key == scale_max[1]),
            touches_end = any(value_key %in% c(scale_min[1], scale_max[1])),
            .groups = "drop") %>%
  mutate(changed = n_values > 1, change_2pt = range >= 2,
         range_pct = 100 * range / span, pinned = at_floor | at_ceiling) %>%
  add_meta()
say("Complete model-item pairs (5 scored runs): ", nrow(sdo_pair), " of ",
    nrow(distinct(sdo, model_label, inst_lang, item_no)))

# Level 1: item responses -----------------------------------------------------------
pair_matrix <- function(df, value = "changed") {
  df %>% mutate(v = as.numeric(.data[[value]])) %>%
    select(model_label, item_no, v) %>%
    pivot_wider(names_from = item_no, values_from = v) %>%
    { M <- as.matrix(.[, -1]); rownames(M) <- .$model_label; M }
}
instab_instr <- map_dfr(c(levels(sdo_pair$inst_lang), "All SDO/VSA"), function(il) {
  d <- if (il == "All SDO/VSA") sdo_pair else filter(sdo_pair, inst_lang == il)
  if (il == "All SDO/VSA") {
    ch <- c(estimate = mean(d$changed), lower = NA, upper = NA)
    c2 <- c(estimate = mean(d$change_2pt), lower = NA, upper = NA)
    # pigeonhole over models x instrument-items
    Mc <- d %>% mutate(v = as.numeric(changed), it = paste(inst_lang, item_no)) %>%
      select(model_label, it, v) %>% pivot_wider(names_from = it, values_from = v)
    Mc <- as.matrix(Mc[, -1]); ch <- crossed_boot(Mc)
    M2 <- d %>% mutate(v = as.numeric(change_2pt), it = paste(inst_lang, item_no)) %>%
      select(model_label, it, v) %>% pivot_wider(names_from = it, values_from = v)
    M2 <- as.matrix(M2[, -1]); c2 <- crossed_boot(M2)
  } else {
    ch <- crossed_boot(pair_matrix(d, "changed"))
    c2 <- crossed_boot(pair_matrix(d, "change_2pt"))
  }
  tibble(instrument = il, n_pairs = nrow(d),
         pct_changed = 100 * ch["estimate"], ch_lo = 100 * ch["lower"], ch_hi = 100 * ch["upper"],
         pct_change_2pt = 100 * c2["estimate"], c2_lo = 100 * c2["lower"], c2_hi = 100 * c2["upper"],
         pct_pinned = 100 * mean(d$pinned))
})
report_table(instab_instr, "S07_instability_by_instrument.csv",
             "Table S7.1. Model-item pairs that changed across 5 runs (%, pigeonhole 95% CI)")

agg_levels <- map_dfr(levels(sdo_pair$inst_lang), function(il) {
  dl <- sdo_complete %>% filter(inst_lang == il)
  span <- unique(dl$span)
  # level 1: pairs
  W1 <- dl %>% select(model_label, item_no, run, value_key) %>%
    pivot_wider(names_from = run, values_from = value_key)
  M1 <- as.matrix(W1[, -(1:2)])
  # level 2: subscales
  W2 <- dl %>% group_by(model_label, subscale, run) %>%
    summarise(v = mean(value_key), .groups = "drop") %>%
    pivot_wider(names_from = run, values_from = v)
  M2 <- as.matrix(W2[, -(1:2)])
  # level 3: full scale (items complete in all runs for that model)
  W3 <- dl %>% group_by(model_label, run) %>%
    summarise(v = mean(value_key), .groups = "drop") %>%
    pivot_wider(names_from = run, values_from = v)
  M3 <- as.matrix(W3[, -1])
  map_dfr(list(list("1 item response", M1), list("2 subscale score", M2),
               list("3 full-scale score", M3)), function(L) {
    M <- L[[2]]; ic <- icc_suite(M)
    tibble(instrument = il, level = L[[1]], n_units = nrow(M),
           icc1 = ic$icc1, icc1_lo = ic$icc1_lo, icc1_hi = ic$icc1_hi,
           iccA = ic$iccA, iccA_lo = ic$iccA_lo, iccA_hi = ic$iccA_hi,
           kripp_interval = kripp_alpha(M, "interval"),
           pct_units_changed = 100 * mean(apply(M, 1, function(r) diff(range(r)) > 1e-9)),
           within_sd_pct_span = 100 * mean(apply(M, 1, sd)) / span)
  })
})
report_table(agg_levels, "S07_reliability_by_aggregation_level.csv",
             "Table S7.2. Reliability at three levels of aggregation (ICC(1,1), ICC(A,1), 95% CI)")

# Rank-order stability of model scale scores between runs ---------------------------
rank_stab <- map_dfr(levels(sdo_pair$inst_lang), function(il) {
  S <- sdo_complete %>% filter(inst_lang == il) %>% group_by(model_label, run) %>%
    summarise(v = tie_round(mean(value_key)), .groups = "drop") %>%
    pivot_wider(names_from = run, values_from = v)
  S <- as.matrix(S[, -1]); nm <- nrow(S)
  rp <- utils::combn(ncol(S), 2)
  res <- apply(rp, 2, function(cc) {
    a <- S[, cc[1]]; b <- S[, cc[2]]
    sa <- sign(outer(a, a, "-")); sb <- sign(outer(b, b, "-"))
    up <- upper.tri(sa)
    c(disc = mean((sa * sb)[up] < 0),
      tau = suppressWarnings(stats::cor(a, b, method = "kendall")))
  })
  tibble(instrument = il, n_models = nm,
         mean_kendall_tau_between_runs = mean(res["tau", ]),
         pct_model_pairs_reversed = 100 * mean(res["disc", ]),
         max_pct_reversed = 100 * max(res["disc", ]))
})
report_table(rank_stab, "S07_rank_stability.csv",
             "Table S7.3. Rank-order stability of model scale scores between any two runs")
note("pct_model_pairs_reversed is the share of model pairs whose order on the ",
     "scale score flips between two identical administrations. A leaderboard ",
     "built from one administration inherits this.")

# Per-model instability and item-profile reliability ------------------------------
model_instr <- sdo_pair %>%
  group_by(model_label, display, developer, developer_group, inst_lang) %>%
  summarise(n_pairs = n(), n_changed = sum(changed),
            pct_changed = 100 * mean(changed), pct_change_2pt = 100 * mean(change_2pt),
            pct_pinned = 100 * mean(pinned), pct_at_floor = 100 * mean(at_floor),
            mean_score = mean(mean_key), .groups = "drop") %>%
  mutate(ci_low = 100 * map2_dbl(n_changed, n_pairs, ~ wilson_ci(.x, .y)["lower"]),
         ci_high = 100 * map2_dbl(n_changed, n_pairs, ~ wilson_ci(.x, .y)["upper"]))
profile_rel <- sdo_complete %>%
  group_by(model_label, inst_lang) %>%
  group_modify(function(d, key) {
    M <- d %>% select(item_no, run, value_key) %>%
      pivot_wider(names_from = run, values_from = value_key)
    M <- as.matrix(M[, -1])
    ic <- icc_suite(M)
    tibble(profile_icc = ic$icc1, all_identical = length(unique(as.vector(M))) == 1)
  }) %>% ungroup() %>%
  mutate(repeats_for_090 = case_when(
    all_identical ~ NA_real_,
    profile_icc >= 0.90 ~ 1,
    profile_icc <= 0 ~ Inf,
    TRUE ~ ceiling(0.90 * (1 - profile_icc) / (profile_icc * (1 - 0.90)))))
model_instr <- model_instr %>% left_join(profile_rel, by = c("model_label", "inst_lang"))
report_table(model_instr %>% select(display, inst_lang, n_pairs, pct_changed, ci_low, ci_high,
                                    pct_change_2pt, pct_at_floor, profile_icc, repeats_for_090) %>%
               arrange(inst_lang, desc(pct_changed)),
             "S07_instability_by_model.csv",
             "Table S7.4. Instability and item-profile reliability by model and instrument")
note("profile_icc treats a model's items as subjects and its runs as raters: how ",
     "reproducible is that model's pattern of answers across items. repeats_for_090 ",
     "is the Spearman-Brown number of averaged runs needed for 0.90; NA means every ",
     "response was identical (reliability undefined, not perfect).")

# Reliability generalisation: meta-analysis of instability rates -------------------
rg <- bind_rows(
  model_instr %>% transmute(model_label, instrument = as.character(inst_lang), x = n_changed, n = n_pairs),
  rmet_pairs %>% group_by(model_label) %>%
    summarise(x = sum(changed), n = n(), .groups = "drop") %>% mutate(instrument = "RMET")
)
rg_tab <- rg %>% group_by(instrument) %>% group_modify(function(d, key) {
  es  <- metafor::escalc(measure = "PLO", xi = d$x, ni = d$n)
  fit <- metafor::rma(yi = es$yi, vi = es$vi, method = "REML", test = "knha")
  pr  <- stats::predict(fit, transf = metafor::transf.ilogit)
  tibble(k_models = fit$k, pooled_pct = 100 * pr$pred, ci_low = 100 * pr$ci.lb,
         ci_high = 100 * pr$ci.ub, pi_low = 100 * pr$pi.lb, pi_high = 100 * pr$pi.ub,
         tau_logit = sqrt(fit$tau2), I2 = fit$I2, Q = fit$QE, Q_p = fit$QEp)
}) %>% ungroup() %>%
  mutate(instrument = factor(instrument, levels = c(INSTR_LEVELS, "RMET"))) %>% arrange(instrument)
report_table(rg_tab, "S07_reliability_generalisation.csv",
             "Table S7.5. Random-effects meta-analysis of instability rates across models (logit, back-transformed)")
note("The 95% prediction interval is the range of instability expected for a NEW ",
     "model drawn from the same population: the practical answer to 'how unstable ",
     "will the next model be?'. I2 near 100% means instability is a property of ",
     "the model-instrument combination, not of the instrument.")
for (i in seq_len(nrow(rg_tab))) {
  register("S07 SDO/VSA reliability", paste("Instability heterogeneity:", rg_tab$instrument[i]),
           "Cochran Q (random-effects meta-analysis of logit instability)",
           estimate = rg_tab$I2[i], statistic = rg_tab$Q[i], df = rg_tab$k_models[i] - 1,
           p = rg_tab$Q_p[i], n = paste(rg_tab$k_models[i], "models"), effect = "I^2 (%)",
           note = paste0("pooled ", round(rg_tab$pooled_pct[i], 1), "%, PI [",
                         round(rg_tab$pi_low[i], 1), ", ", round(rg_tab$pi_high[i], 1), "]"))
}

FIG$sdo_pair       <- sdo_pair
FIG$instab_instr   <- instab_instr
FIG$agg_levels     <- agg_levels
FIG$rank_stab      <- rank_stab
FIG$model_instr    <- model_instr
FIG$rel_general    <- rg_tab


# ==============================================================================
# SECTION 8. GENERALIZABILITY THEORY AND D-STUDY
# ==============================================================================

banner(8, "Generalizability theory: variance sources and how many repeats are needed")
note("A fully crossed models x items x runs G-study per instrument. Responses are ",
     "on the 0-1 normalised construct metric (RMET: correct = 1) so components ",
     "are comparable across instruments. Two decisions are distinguished: a ",
     "claim about a model's SCALE score (items fixed, as in published use) and a ",
     "claim about a model's answer to a single ITEM. The D-study projects each ",
     "coefficient for 1 to 20 averaged runs; this is the sample-size calculation ",
     "for the number of repeats.")

g_study <- function(d) {
  f <- y ~ 1 + (1 | model) + (1 | item) + (1 | run) + (1 | model:item) +
    (1 | model:run) + (1 | item:run)
  fit <- capture_fit(lme4::lmer(f, data = d, REML = TRUE,
                                control = lme4::lmerControl(calc.derivs = FALSE)))
  vc <- as.data.frame(lme4::VarCorr(fit$value))
  v <- setNames(vc$vcov, vc$grp)
  comp <- c(p = v[["model"]], i = v[["item"]], r = v[["run"]], pi = v[["model:item"]],
            pr = v[["model:run"]], ir = v[["item:run"]], res = v[["Residual"]])
  list(comp = comp, n_i = n_distinct(d$item), n_r = n_distinct(d$run),
       warnings = fit$warnings)
}
g_coefs <- function(comp, n_i, n_r) {
  with(as.list(comp), {
    rel_fixed  <- (p + pi / n_i) / (p + pi / n_i + pr / n_r + res / (n_i * n_r))
    phi_fixed  <- (p + pi / n_i) / (p + pi / n_i + pr / n_r + res / (n_i * n_r) +
                                      r / n_r + ir / (n_i * n_r))
    rel_random <- p / (p + pi / n_i + pr / n_r + res / (n_i * n_r))
    phi_pair   <- (p + i + pi) / (p + i + pi + (r + pr + ir + res) / n_r)
    item_rel   <- (p + pi) / (p + pi + (pr + res) / n_r)
    c(scale_rel_items_fixed = rel_fixed, scale_phi_items_fixed = phi_fixed,
      scale_rel_items_random = rel_random, item_rel_models = item_rel, pair_phi = phi_pair)
  })
}

g_inputs <- c(
  setNames(lapply(levels(sdo_pair$inst_lang), function(il) {
    sdo_complete %>% filter(inst_lang == il) %>%
      transmute(y = pos, model = model_label, item = factor(item_no), run = factor(run))
  }), levels(sdo_pair$inst_lang)),
  list(`RMET (no definitions)` = rmet_ok %>% filter(condition == "No definitions") %>%
         transmute(y = correct, model = model_label, item = factor(item), run = factor(run)),
       `RMET (definitions)` = rmet_ok %>% filter(condition == "Definitions") %>%
         transmute(y = correct, model = model_label, item = factor(item), run = factor(run))))
g_res <- lapply(g_inputs, g_study)

g_comp_tab <- imap_dfr(g_res, function(g, nm) {
  tibble(instrument = nm, component = names(g$comp), variance = g$comp,
         share = g$comp / sum(g$comp))
}) %>% mutate(component = recode(component, p = "model", i = "item", r = "run",
                                 pi = "model x item", pr = "model x run",
                                 ir = "item x run", res = "residual (model x item x run)"))
report_table(g_comp_tab %>% select(instrument, component, share) %>%
               pivot_wider(names_from = instrument, values_from = share),
             "S08_g_study_variance_shares.csv",
             "Table S8.1. G-study: share of response variance by source")

dstudy <- imap_dfr(g_res, function(g, nm) {
  map_dfr(1:20, function(nr) {
    cf <- g_coefs(g$comp, g$n_i, nr)
    tibble(instrument = nm, n_runs = nr, coefficient = names(cf), value = cf)
  })
})
d_obs <- dstudy %>% filter(n_runs == N_RUNS) %>%
  pivot_wider(names_from = coefficient, values_from = value)
d_need <- dstudy %>% group_by(instrument, coefficient) %>%
  summarise(runs_for_080 = suppressWarnings(min(n_runs[value >= 0.80])),
            runs_for_090 = suppressWarnings(min(n_runs[value >= 0.90])),
            runs_for_095 = suppressWarnings(min(n_runs[value >= 0.95])),
            single_run = value[n_runs == 1], .groups = "drop") %>%
  mutate(across(starts_with("runs_for"), ~ if_else(is.infinite(.x), NA_real_, .x)))
report_table(d_obs, "S08_g_coefficients_observed.csv",
             "Table S8.2. G coefficients for the observed design (5 runs)")
report_table(d_need, "S08_d_study_runs_needed.csv",
             "Table S8.3. D-study: runs needed to reach 0.80 / 0.90 / 0.95 (NA = not reached by 20)")
note("scale_rel_items_fixed: reliability of model SCALE scores (items fixed, the ",
     "published use). item_rel_models: reliability of comparing models on ONE item ",
     "(the item-level analogue). pair_phi: dependability of one model's answer to ",
     "one item relative to all model-item pairs (inflated by item differences, so ",
     "it is not the right comparison for the aggregation argument). The gap between ",
     "scale_rel_items_fixed and item_rel_models is the aggregation paradox expressed ",
     "as a number of repeats.")
FIG$g_components <- g_comp_tab
FIG$dstudy <- dstudy
FIG$d_need <- d_need


# ==============================================================================
# SECTION 9. ARE THE FIVE REPEATS EXCHANGEABLE? (FLAT-VARIANCE TESTS)
# ==============================================================================

banner(9, "Exchangeability of repeats (flat-variance tests)")
note("Calls are stateless, so repeats should be exchangeable draws. Drift, ",
     "warm-up or stickiness would each leave a signature in run order.")

all_perms <- function(v) {
  if (length(v) == 1L) return(matrix(v, 1L))
  do.call(rbind, lapply(seq_along(v), function(i) cbind(v[i], all_perms(v[-i]))))
}
order_permutation_test <- function(M, n_perm = B_PERM) {
  M <- as.matrix(M); n <- nrow(M); k <- ncol(M)
  x <- seq_len(k) - mean(seq_len(k))
  s_lag  <- function(A) { D <- A - rowMeans(A); sum(D[, -k] * D[, -1]) / sum(D^2) }
  s_trnd <- function(A) { D <- A - rowMeans(A); b <- as.vector(D %*% x) / sum(x^2)
                          sum(b^2) * sum(x^2) / sum(D^2) }
  s_first <- function(A) mean(A[, 1] - rowMeans(A[, -1, drop = FALSE]))
  obs <- c(s_lag(M), s_trnd(M), s_first(M))
  perms <- all_perms(seq_len(k)); rows <- rep(seq_len(n), k)
  nul <- t(vapply(seq_len(n_perm), function(i) {
    idx <- perms[sample.int(nrow(perms), n, replace = TRUE), , drop = FALSE]
    Mp <- matrix(M[cbind(rows, as.vector(idx))], n, k)
    c(s_lag(Mp), s_trnd(Mp), s_first(Mp))
  }, numeric(3)))
  tibble(statistic = c("Lag-1 autocorrelation (stickiness)",
                       "Share of within-pair variance on a linear trend (drift)",
                       "First run minus mean of later runs (warm-up)"),
         observed = obs, null_mean = colMeans(nul),
         p_value = c((1 + sum(abs(nul[, 1]) >= abs(obs[1]) - 1e-12)) / (1 + n_perm),
                     (1 + sum(nul[, 2] >= obs[2] - 1e-12)) / (1 + n_perm),
                     (1 + sum(abs(nul[, 3]) >= abs(obs[3]) - 1e-12)) / (1 + n_perm)))
}

M_sdo <- sdo_complete %>% select(model_label, inst_lang, item_no, run, pos) %>%
  pivot_wider(names_from = run, values_from = pos) %>%
  { as.matrix(.[, -(1:3)]) }
M_rmet <- rmet_ok %>% group_by(model_label, condition, item) %>% filter(n() == N_RUNS) %>%
  ungroup() %>% select(model_label, condition, item, run, correct) %>%
  pivot_wider(names_from = run, values_from = correct) %>%
  { as.matrix(.[, -(1:3)]) }
set.seed(SEED + 9)
perm_sdo  <- order_permutation_test(M_sdo)
perm_rmet <- order_permutation_test(M_rmet)
report_table(bind_rows(mutate(perm_sdo, data = "SDO/VSA (position 0-1)", .before = 1),
                       mutate(perm_rmet, data = "RMET (correct 0/1)", .before = 1)),
             "S09_order_permutation_tests.csv",
             "Table S9.1. Within-pair permutation tests of run order", digits = 4)
for (i in 1:3) {
  register("S09 Exchangeability", "SDO/VSA run order", paste("Within-pair permutation:", perm_sdo$statistic[i]),
           estimate = perm_sdo$observed[i], p = perm_sdo$p_value[i], n = paste(nrow(M_sdo), "pairs"))
  register("S09 Exchangeability", "RMET run order", paste("Within-pair permutation:", perm_rmet$statistic[i]),
           estimate = perm_rmet$observed[i], p = perm_rmet$p_value[i], n = paste(nrow(M_rmet), "pairs"))
}

# Mean deviation from each pair's own mean, by run (for the figure)
run_dev <- bind_rows(
  as_tibble(M_sdo, .name_repair = ~ paste0("r", seq_along(.x))) %>% mutate(data = "SDO/VSA"),
  as_tibble(M_rmet, .name_repair = ~ paste0("r", seq_along(.x))) %>% mutate(data = "RMET")) %>%
  mutate(id = row_number()) %>%
  pivot_longer(starts_with("r"), names_to = "run", values_to = "v") %>%
  group_by(id) %>% mutate(dev = v - mean(v)) %>% ungroup() %>%
  group_by(data, run) %>%
  summarise(mean_dev = mean(dev), se = sd(dev) / sqrt(n()), n = n(), .groups = "drop") %>%
  mutate(run = as.integer(str_remove(run, "r")))
report_table(run_dev, "S09_mean_deviation_by_run.csv",
             "Table S9.2. Mean within-pair deviation by run", digits = 5)

# Friedman tests (pairs as blocks) and RM-ANOVA on SDO/VSA scale scores -------------
fr_sdo  <- stats::friedman.test(M_sdo)
fr_rmet <- stats::friedman.test(M_rmet)
say("Friedman, SDO/VSA pairs: chi2(", fr_sdo$parameter, ") = ", fmt_n(fr_sdo$statistic),
    ", ", fp(fr_sdo$p.value), "; RMET pairs: chi2(", fr_rmet$parameter, ") = ",
    fmt_n(fr_rmet$statistic), ", ", fp(fr_rmet$p.value))
register("S09 Exchangeability", "SDO/VSA run order", "Friedman test (pairs as blocks)",
         statistic = fr_sdo$statistic, df = fr_sdo$parameter, p = fr_sdo$p.value,
         n = paste(nrow(M_sdo), "pairs"))
register("S09 Exchangeability", "RMET run order", "Friedman test (pairs as blocks)",
         statistic = fr_rmet$statistic, df = fr_rmet$parameter, p = fr_rmet$p.value,
         n = paste(nrow(M_rmet), "pairs"))

scale_run <- sdo_complete %>% group_by(model_label, inst_lang, run) %>%
  summarise(score = mean(pos), .groups = "drop") %>%
  group_by(model_label) %>% filter(n_distinct(inst_lang) == 4) %>% ungroup() %>%
  mutate(run = factor(run))
rm_sdo <- capture_fit(afex::aov_ez(id = "model_label", dv = "score", data = scale_run,
                                   within = c("inst_lang", "run"),
                                   anova_table = list(correction = "GG", es = "ges")))
if (rm_sdo$ok) {
  at <- as.data.frame(rm_sdo$value$anova_table); at$effect <- rownames(at)
  rm_sdo_tab <- tibble(effect = at$effect, df_num = at$`num Df`, df_den = at$`den Df`,
                       F = at$F, ges = at$ges, p_GG = at$`Pr(>F)`)
  report_table(rm_sdo_tab, "S09_sdo_rm_anova.csv",
               "Table S9.3. RM-ANOVA on SDO/VSA scale scores (0-1): instrument x run, models as subjects")
  for (i in seq_len(nrow(rm_sdo_tab))) {
    register("S09 Exchangeability", "SDO/VSA RM-ANOVA", paste("F-test:", rm_sdo_tab$effect[i]),
             statistic = rm_sdo_tab$F[i],
             df = paste0(round(rm_sdo_tab$df_num[i], 2), ", ", round(rm_sdo_tab$df_den[i], 2)),
             p = rm_sdo_tab$p_GG[i], n = paste(n_distinct(scale_run$model_label), "models"),
             effect = paste0("generalised eta^2 = ", signif(rm_sdo_tab$ges[i], 3)))
  }
}

# RMET first-sitting effect by model ------------------------------------------------
first_sit <- sitting %>% group_by(model_label, run) %>% summarise(acc = mean(acc), .groups = "drop") %>%
  mutate(first = run == "0") %>% group_by(model_label) %>%
  summarise(first_minus_later_pp = 100 * (acc[first] - mean(acc[!first])), .groups = "drop") %>%
  add_meta()
ps_first <- paired_suite(first_sit$first_minus_later_pp)
say("RMET first sitting minus later sittings: ", fmt_n(ps_first$mean_diff), " pp ",
    fmt_ci(ps_first$ci_low, ps_first$ci_high), ", t(", ps_first$df, ") = ", fmt_n(ps_first$t),
    ", ", fp(ps_first$p_t), "; Wilcoxon ", fp(ps_first$p_wilcoxon), "; ",
    ps_first$n_negative, " of ", ps_first$n, " models lower on the first sitting")
register("S09 Exchangeability", "RMET first-sitting effect", "Paired t-test (model level)",
         estimate = ps_first$mean_diff, ci_low = ps_first$ci_low, ci_high = ps_first$ci_high,
         statistic = ps_first$t, df = ps_first$df, p = ps_first$p_t,
         n = paste(ps_first$n, "models"), effect = "% points")
register("S09 Exchangeability", "RMET first-sitting effect", "Wilcoxon signed-rank (model level)",
         estimate = ps_first$mean_diff, statistic = ps_first$V, p = ps_first$p_wilcoxon,
         n = paste(ps_first$n, "models"), effect = "% points")
report_table(first_sit %>% select(display, developer, first_minus_later_pp) %>%
               arrange(first_minus_later_pp),
             "S09_rmet_first_sitting_by_model.csv",
             "Table S9.4. RMET first sitting minus mean of sittings 2-5, by model (% points)")
note("Repeats were issued seconds apart (the repeat loop is innermost), so a run ",
     "effect cannot be slow API drift. If the first-sitting deficit is confined to ",
     "a few models it is best reported as a model-specific first-call effect.")
FIG$run_dev   <- run_dev
FIG$first_sit <- first_sit


# ==============================================================================
# SECTION 10. SCALE POSITION AND APPARENT STABILITY
# ==============================================================================

banner(10, "Scale position and apparent stability")
note("A response at an endpoint can move only one way; a midpoint response can ",
     "move either way. Instrument comparisons of stability must condition on ",
     "position. To avoid circularity, position is taken from the FIRST run and ",
     "the outcome is whether any later run differs from it (which is identical to ",
     "'the pair changed').")

pos_dist <- sdo_ok %>% group_by(inst_lang) %>%
  summarise(n = n(), pct_at_floor = 100 * mean(value_key == scale_min),
            pct_at_ceiling = 100 * mean(value_key == scale_max),
            pct_at_either_end = 100 * mean(value_key %in% c(scale_min[1], scale_max[1])),
            pct_at_midpoint = 100 * mean(value_key == midpoint), .groups = "drop")
report_table(pos_dist, "S10_response_position.csv",
             "Table S10.1. Response position by instrument (recoded metric)")

band_tab <- sdo_pair %>%
  mutate(band = cut(mean_pos, breaks = c(-0.01, 0.15, 0.40, 0.60, 0.85, 1.01),
                    labels = c("at/near floor", "lower", "middle", "upper", "at/near ceiling"))) %>%
  group_by(inst_lang, band) %>%
  summarise(n_pairs = n(), pct_changed = 100 * mean(changed),
            mean_range_pct = mean(range_pct), .groups = "drop")
report_table(band_tab, "S10_instability_by_position_band.csv",
             "Table S10.2. Instability by mean scale position (descriptive)")

pos_dat <- sdo_pair %>%
  mutate(edge = pmin(pos1, 1 - pos1), item_f = factor(paste(inst_lang, item_no)),
         inst = relevel(factor(inst_lang, levels = INSTR_LEVELS), ref = "SDO6-EN"),
         changed = as.integer(changed))
gA <- glmer_fit(changed ~ inst + (1 | model_label) + (1 | item_f), pos_dat)
gB <- glmer_fit(changed ~ inst + edge + I(edge^2) + (1 | model_label) + (1 | item_f), pos_dat)
gB0 <- glmer_fit(changed ~ edge + I(edge^2) + (1 | model_label) + (1 | item_f), pos_dat)
gA0 <- glmer_fit(changed ~ 1 + (1 | model_label) + (1 | item_f), pos_dat)
orA <- glmm_or_table(gA$value) %>% filter(str_detect(term, "^inst")) %>% mutate(model = "unadjusted")
orB <- glmm_or_table(gB$value) %>% filter(str_detect(term, "^inst")) %>% mutate(model = "adjusted for first-run position")
lrA <- lrt(gA$value, gA0$value); lrB <- lrt(gB$value, gB0$value)
lr_pos <- lrt(gB$value, gA$value)
report_table(bind_rows(orA, orB) %>% mutate(term = str_remove(term, "^inst")) %>%
               select(model, term, OR, OR_low, OR_high, p),
             "S10_instrument_or_by_position.csv",
             "Table S10.3. Odds of a pair changing, relative to SDO6-EN, before and after conditioning on position")
say("Instrument differences, unadjusted: LR chi2(", lrA$df, ") = ", fmt_n(lrA$chisq), ", ", fp(lrA$p))
say("Instrument differences, adjusted for position: LR chi2(", lrB$df, ") = ", fmt_n(lrB$chisq), ", ", fp(lrB$p))
say("Position (distance from nearest endpoint) given instrument: LR chi2(", lr_pos$df, ") = ",
    fmt_n(lr_pos$chisq), ", ", fp(lr_pos$p))
register("S10 Scale position", "Instrument differences in instability", "GLMM LRT, unadjusted",
         statistic = lrA$chisq, df = lrA$df, p = lrA$p, n = paste(nrow(pos_dat), "pairs"))
register("S10 Scale position", "Instrument differences in instability", "GLMM LRT, adjusted for first-run position",
         statistic = lrB$chisq, df = lrB$df, p = lrB$p, n = paste(nrow(pos_dat), "pairs"))
register("S10 Scale position", "Position effect", "GLMM LRT, quadratic in distance from endpoint",
         statistic = lr_pos$chisq, df = lr_pos$df, p = lr_pos$p, n = paste(nrow(pos_dat), "pairs"))

# Like-for-like: pairs that never touched an endpoint
fair <- sdo_pair %>% filter(!touches_end)
fair_tab <- fair %>% group_by(inst_lang) %>%
  summarise(n_pairs = n(), pct_changed = 100 * mean(changed),
            mean_sd_pct = 100 * mean(sd_key / span), .groups = "drop")
report_table(fair_tab, "S10_unpinned_comparison.csv",
             "Table S10.4. Instability among pairs that never touched an endpoint")
if (all(c("SDO6-EN", "VSA") %in% fair_tab$inst_lang)) {
  sub <- fair %>% filter(inst_lang %in% c("SDO6-EN", "VSA")) %>% mutate(inst_lang = droplevels(inst_lang))
  ft_fair <- stats::fisher.test(table(sub$inst_lang, sub$changed))
  wx_fair <- stats::wilcox.test(tie_round(sd_key / span) ~ inst_lang, data = sub, exact = FALSE)
  say("SDO6-EN vs VSA, unpinned pairs: Fisher ", fp(ft_fair$p.value),
      "; within-pair SD (% span) Wilcoxon W = ", fmt_n(wx_fair$statistic, 0), ", ", fp(wx_fair$p.value))
  register("S10 Scale position", "SDO6-EN vs VSA, unpinned pairs", "Fisher exact test (changed)",
           estimate = ft_fair$estimate, p = ft_fair$p.value, n = paste(nrow(sub), "pairs"),
           effect = "odds ratio", note = "pairs clustered in models; descriptive")
}

# Predicted curves for the figure (per instrument, population level)
pos_curves <- map_dfr(INSTR_LEVELS, function(il) {
  d <- pos_dat %>% filter(inst_lang == il)
  deg <- min(2, n_distinct(d$pos1) - 1)
  fit <- glmer_fit(changed ~ poly(pos1, degree = deg, raw = TRUE) + (1 | model_label) + (1 | item_f), d)
  if (!fit$ok) return(tibble())
  grid <- tibble(pos1 = sort(unique(d$pos1)))
  X <- stats::model.matrix(~ poly(pos1, degree = deg, raw = TRUE), data = grid)
  b <- lme4::fixef(fit$value); V <- as.matrix(stats::vcov(fit$value))
  eta <- as.vector(X %*% b); se <- sqrt(rowSums((X %*% V) * X))
  obs <- d %>% group_by(pos1) %>% summarise(n = n(), k = sum(changed), .groups = "drop")
  grid %>% mutate(inst_lang = il, fit = stats::plogis(eta),
                  lo = stats::plogis(eta - 1.96 * se), hi = stats::plogis(eta + 1.96 * se)) %>%
    left_join(obs, by = "pos1") %>%
    mutate(obs = k / n,
           obs_lo = map2_dbl(k, n, ~ wilson_ci(.x, .y)["lower"]),
           obs_hi = map2_dbl(k, n, ~ wilson_ci(.x, .y)["upper"]))
})
report_table(pos_curves, "S10_position_curves.csv",
             "Table S10.5. Probability a pair changes, by first-run position (observed and GLMM)")
FIG$pos_curves <- pos_curves
FIG$pos_dist   <- pos_dist
FIG$pos_or     <- bind_rows(orA, orB)

# Raw vs recoded ICC (continuity with the earlier script and the manuscript) ------
icc_raw_rec <- map_dfr(levels(sdo_pair$inst_lang), function(il) {
  dl <- sdo_complete %>% filter(inst_lang == il)
  Mr <- dl %>% select(model_label, item_no, run, value_raw) %>%
    pivot_wider(names_from = run, values_from = value_raw) %>% { as.matrix(.[, -(1:2)]) }
  Mk <- dl %>% select(model_label, item_no, run, value_key) %>%
    pivot_wider(names_from = run, values_from = value_key) %>% { as.matrix(.[, -(1:2)]) }
  a <- icc_suite(Mr); b <- icc_suite(Mk)
  n_i <- n_distinct(dl$item_no)
  tibble(instrument = il,
         icc1_single_raw = a$icc1, icc1_single_recoded = b$icc1,
         icc1k_mean5_raw = (N_RUNS * a$icc1) / (1 + (N_RUNS - 1) * a$icc1),
         icc1k_mean5_recoded = (N_RUNS * b$icc1) / (1 + (N_RUNS - 1) * b$icc1))
})
report_table(icc_raw_rec, "S07_icc_raw_vs_recoded.csv",
             "Table S7.6. Item-level ICC on raw vs reverse-key-corrected responses; single run vs mean of 5")
note("Recoding cannot change within-pair variation; it removes the artificial ",
     "between-item spread created by keying direction, so the recoded ICC is the ",
     "defensible one. Whichever ICC the manuscript quotes, state (a) raw or recoded ",
     "and (b) single administration or mean of five: these differ materially.")


# ==============================================================================
# SECTION 11. SUBDIMENSIONS, RANK REVERSALS AND RESPONSE STYLES
# ==============================================================================

banner(11, "Subdimensions, rank reversals and response styles")
note("A total SDO score averages dominance and anti-egalitarianism. If models ",
     "order differently on the two, the total hides it. Within each model, the ",
     "subscale difference is tested with an exact permutation test that ",
     "enumerates every split of the items into two subscales of the observed ",
     "sizes (70 splits for SDO7s, 12,870 for SDO6).")

item_means <- sdo_ok %>%
  group_by(model_label, inst_lang, instrument, item_no, subscale, keying, midpoint, span,
           scale_min, scale_max) %>%
  summarise(key = mean(value_key), raw = mean(value_raw), pos = mean(pos), .groups = "drop")

exact_split_test <- function(x, grp) {
  a_lab <- sort(unique(grp))[1]
  n <- length(x); na <- sum(grp == a_lab)
  obs <- tie_round(mean(x[grp != a_lab]) - mean(x[grp == a_lab]))
  if (stats::sd(x) == 0) return(c(obs = obs, p = 1))
  cmb <- utils::combn(n, na)
  s_all <- sum(x)
  sa <- colSums(matrix(x[cmb], nrow = na))
  nul <- (s_all - sa) / (n - na) - sa / na
  c(obs = obs, p = mean(abs(nul) >= abs(obs) - 1e-12))
}

sub_models <- item_means %>% filter(instrument %in% c("SDO7s", "SDO6")) %>%
  group_by(model_label, inst_lang) %>%
  group_modify(function(d, key) {
    et <- exact_split_test(d$key, d$subscale)
    tibble(dominance = tie_round(mean(d$key[d$subscale == "dominance"])),
           anti_egalitarianism = tie_round(mean(d$key[d$subscale == "anti_egalitarianism"])),
           total = tie_round(mean(d$key)), diff_AE_minus_D = et["obs"], p_exact = et["p"])
  }) %>% ungroup() %>%
  group_by(inst_lang) %>% mutate(p_bh = p.adjust(p_exact, "BH")) %>% ungroup() %>% add_meta()

sub_summary <- sub_models %>% group_by(inst_lang) %>%
  summarise(n_models = n(), mean_D = mean(dominance), mean_AE = mean(anti_egalitarianism),
            n_AE_higher_sig = sum(p_bh < ALPHA & diff_AE_minus_D > 0),
            n_D_higher_sig = sum(p_bh < ALPHA & diff_AE_minus_D < 0), .groups = "drop")
report_table(sub_summary, "S11_subscale_summary.csv",
             "Table S11.1. Dominance vs anti-egalitarianism within models (exact permutation, BH)")
report_table(sub_models %>% select(display, inst_lang, dominance, anti_egalitarianism,
                                   diff_AE_minus_D, p_exact, p_bh) %>% arrange(inst_lang, desc(diff_AE_minus_D)),
             "S11_subscales_by_model.csv", "Table S11.2. Subscale scores by model")
ps_sub <- sub_models %>% group_by(inst_lang) %>%
  group_modify(~ paired_suite(.x$diff_AE_minus_D)) %>% ungroup()
for (i in seq_len(nrow(ps_sub))) {
  register("S11 Subdimensions", paste("AE minus D across models:", ps_sub$inst_lang[i]),
           "Wilcoxon signed-rank (model level)", estimate = ps_sub$mean_diff[i],
           statistic = ps_sub$V[i], p = ps_sub$p_wilcoxon[i], n = paste(ps_sub$n[i], "models"),
           effect = "scale points")
}
for (i in seq_len(nrow(sub_models))) {
  register("S11 Subdimensions (per model)", paste(sub_models$display[i], sub_models$inst_lang[i]),
           "Exact permutation test: anti-egalitarianism vs dominance items",
           estimate = sub_models$diff_AE_minus_D[i], p = sub_models$p_exact[i], n = "item means")
}

# Rank reversals hidden by the total --------------------------------------------------
reversal_tab <- sub_models %>% group_by(inst_lang) %>% group_modify(function(d, key) {
  n <- nrow(d); pr <- utils::combn(n, 2)
  sgn <- function(v) sign(v[pr[1, ]] - v[pr[2, ]])
  sT <- sgn(d$total); sD <- sgn(d$dominance); sA <- sgn(d$anti_egalitarianism)
  informative <- sT != 0
  tau <- suppressWarnings(stats::cor(d$dominance, d$anti_egalitarianism, method = "kendall"))
  bs <- replicate(B_BOOT, { i <- sample.int(n, n, TRUE)
    suppressWarnings(stats::cor(d$dominance[i], d$anti_egalitarianism[i], method = "kendall")) })
  tibble(n_models = n, kendall_tau_D_AE = tau,
         tau_ci_low = stats::quantile(bs, .025, na.rm = TRUE),
         tau_ci_high = stats::quantile(bs, .975, na.rm = TRUE),
         pct_pairs_D_AE_discordant = 100 * mean((sD * sA)[sD != 0 & sA != 0] < 0),
         pct_total_order_reversed_by_a_subscale =
           100 * mean(((sD * sT) < 0 | (sA * sT) < 0)[informative]))
}) %>% ungroup()
report_table(reversal_tab, "S11_rank_reversals.csv",
             "Table S11.3. How often subscales order models differently from each other and from the total")

# Response styles on the RAW metric --------------------------------------------------
styles <- sdo_ok %>%
  group_by(model_label, inst_lang) %>%
  summarise(ARS = mean((value_raw - midpoint) / (span / 2)),
            ERS = mean(value_raw %in% c(scale_min[1], scale_max[1])),
            MRS = mean(value_raw == midpoint),
            .groups = "drop") %>% add_meta()
ars_tests <- styles %>% group_by(inst_lang) %>%
  summarise(n = n(), mean_ARS = mean(ARS),
            p_wilcoxon = suppressWarnings(stats::wilcox.test(tie_round(ARS), mu = 0)$p.value),
            mean_ERS = mean(ERS), mean_MRS = mean(MRS), .groups = "drop")
report_table(ars_tests, "S11_response_styles.csv",
             "Table S11.4. Response styles (raw metric): acquiescence (ARS, -1 to 1), extreme (ERS) and midpoint (MRS) responding")
note("All three instruments are balanced for keying, so a model with no ",
     "acquiescence has ARS = 0 whatever its attitude. ARS > 0 means agreeing with ",
     "statements regardless of direction.")
for (i in seq_len(nrow(ars_tests))) {
  register("S11 Response styles", paste("Acquiescence:", ars_tests$inst_lang[i]),
           "One-sample Wilcoxon, ARS = 0", estimate = ars_tests$mean_ARS[i],
           p = ars_tests$p_wilcoxon[i], n = paste(ars_tests$n[i], "models"), effect = "ARS (-1 to 1)")
}
sty_lang <- styles %>% filter(inst_lang %in% c("SDO6-EN", "SDO6-ZH")) %>%
  select(model_label, inst_lang, ARS, ERS, MRS) %>%
  pivot_longer(c(ARS, ERS, MRS), names_to = "style") %>%
  pivot_wider(names_from = inst_lang, values_from = value) %>%
  group_by(style) %>% group_modify(~ paired_suite(.x$`SDO6-ZH` - .x$`SDO6-EN`)) %>% ungroup()
report_table(sty_lang, "S11_response_styles_language.csv",
             "Table S11.5. Response styles, Chinese minus English SDO6 (paired over models)")
for (i in seq_len(nrow(sty_lang))) {
  register("S13 Language", paste("Response style shift:", sty_lang$style[i]),
           "Wilcoxon signed-rank (ZH - EN, model level)", estimate = sty_lang$mean_diff[i],
           statistic = sty_lang$V[i], p = sty_lang$p_wilcoxon[i], n = paste(sty_lang$n[i], "models"))
}
ers_inst <- styles %>% left_join(model_instr %>% select(model_label, inst_lang, pct_changed),
                                 by = c("model_label", "inst_lang")) %>%
  group_by(inst_lang) %>% group_modify(~ cor_test_full(.x$ERS, .x$pct_changed)) %>% ungroup()
report_table(ers_inst, "S11_ers_vs_instability.csv",
             "Table S11.6. Extreme responding vs instability across models (Spearman, bootstrap CI)")
for (i in seq_len(nrow(ers_inst))) {
  register("S11 Response styles", paste("ERS vs instability:", ers_inst$inst_lang[i]),
           "Spearman correlation (model level)", estimate = ers_inst$r[i],
           ci_low = ers_inst$ci_low[i], ci_high = ers_inst$ci_high[i], p = ers_inst$p_perm[i],
           n = paste(ers_inst$n[i], "models"), effect = "rho")
}
FIG$sub_models <- sub_models
FIG$reversals  <- reversal_tab
FIG$styles     <- styles


# ==============================================================================
# SECTION 12. STRUCTURAL VALIDITY
# ==============================================================================

banner(12, "Structural validity: do the items hang together across models?")
note("Models are treated as respondents (n = 27) and each model's item means as ",
     "its responses. Factor analysis on 27 respondents is fragile, so the section ",
     "leads with tests that are valid at this n: internal consistency with a ",
     "bootstrap CI, and exact permutation tests of whether items correlate more ",
     "within their subscale (or keying direction) than across. EFA runs only if ",
     "the correlation matrix is factorable (KMO >= .60) and is labelled exploratory.")

item_wide <- function(il) {
  W <- item_means %>% filter(inst_lang == il) %>% select(model_label, item_no, key) %>%
    pivot_wider(names_from = item_no, values_from = key)
  M <- as.matrix(W[, -1]); rownames(M) <- W$model_label
  M <- M[stats::complete.cases(M), , drop = FALSE]
  M[, apply(M, 2, stats::sd) > 0, drop = FALSE]
}
alpha_boot <- function(M, B = B_BOOT) {
  a_fun <- function(X) { k <- ncol(X); v <- apply(X, 2, stats::var)
    (k / (k - 1)) * (1 - sum(v) / stats::var(rowSums(X))) }
  est <- a_fun(M)
  bs <- replicate(B, { i <- sample.int(nrow(M), replace = TRUE)
    X <- M[i, , drop = FALSE]; if (any(apply(X, 2, stats::sd) == 0)) NA else a_fun(X) })
  c(alpha = est, lo = unname(stats::quantile(bs, .025, na.rm = TRUE)),
    hi = unname(stats::quantile(bs, .975, na.rm = TRUE)))
}
coherence_test <- function(M, labels) {
  R <- stats::cor(M); k <- ncol(M); up <- upper.tri(R)
  stat <- function(lab) { same <- outer(lab, lab, "=="); mean(R[up & same]) - mean(R[up & !same]) }
  obs <- stat(labels)
  a_lab <- sort(unique(labels))[1]; na <- sum(labels == a_lab)
  cmb <- utils::combn(k, na)
  nul <- apply(cmb, 2, function(ix) { lab <- rep("b", k); lab[ix] <- "a"; stat(lab) })
  c(obs = obs, p = mean(nul >= obs - 1e-12), n_splits = ncol(cmb))
}

struct_tab <- map_dfr(INSTR_LEVELS, function(il) {
  M <- item_wide(il)
  meta <- item_means %>% filter(inst_lang == il) %>% distinct(item_no, subscale, keying)
  meta <- meta[match(as.integer(colnames(M)), meta$item_no), ]
  ab <- alpha_boot(M)
  R <- stats::cor(M)
  out <- tibble(instrument = il, n_models = nrow(M), n_items_used = ncol(M),
                alpha = ab["alpha"], alpha_lo = ab["lo"], alpha_hi = ab["hi"],
                mean_inter_item_r = mean(R[upper.tri(R)]))
  if (n_distinct(meta$subscale) == 2) {
    ct <- coherence_test(M, meta$subscale)
    out <- mutate(out, subscale_coherence = ct["obs"], subscale_p_exact = ct["p"])
  }
  if (n_distinct(meta$keying) == 2 && !identical(meta$keying == meta$keying[1],
                                                  meta$subscale == meta$subscale[1])) {
    ck <- coherence_test(M, meta$keying)
    out <- mutate(out, keying_coherence = ck["obs"], keying_p_exact = ck["p"])
  }
  out
})
report_table(struct_tab, "S12_internal_structure.csv",
             "Table S12.1. Internal consistency (bootstrap CI) and exact coherence tests")
note("subscale_coherence = mean within-subscale r minus mean between-subscale r. ",
     "keying_coherence does the same for pro-trait vs con-trait wording, which is ",
     "separable from content only in the SDO7s (SDO6 confounds the two). If ",
     "keying coherence exceeds content coherence, responses are organised by ",
     "item wording direction rather than by construct.")
for (i in seq_len(nrow(struct_tab))) {
  if (!is.null(struct_tab$subscale_p_exact) && !is.na(struct_tab$subscale_p_exact[i]))
    register("S12 Structural validity", paste("Subscale coherence:", struct_tab$instrument[i]),
             "Exact permutation over item partitions", estimate = struct_tab$subscale_coherence[i],
             p = struct_tab$subscale_p_exact[i], n = paste(struct_tab$n_models[i], "models"),
             effect = "within minus between mean r", note = "one-sided")
  if (!is.null(struct_tab$keying_p_exact) && !is.na(struct_tab$keying_p_exact[i]))
    register("S12 Structural validity", paste("Keying coherence:", struct_tab$instrument[i]),
             "Exact permutation over item partitions", estimate = struct_tab$keying_coherence[i],
             p = struct_tab$keying_p_exact[i], n = paste(struct_tab$n_models[i], "models"),
             effect = "within minus between mean r", note = "one-sided")
}

item_rest <- map_dfr(INSTR_LEVELS, function(il) {
  M <- item_wide(il)
  tibble(instrument = il, item_no = as.integer(colnames(M)),
         item_rest_r = vapply(seq_len(ncol(M)), function(j)
           stats::cor(M[, j], rowMeans(M[, -j, drop = FALSE])), numeric(1)),
         sd_across_models = apply(M, 2, stats::sd))
})
report_table(item_rest %>% arrange(instrument, item_rest_r), "S12_item_rest_correlations.csv",
             "Table S12.2. Item-rest correlations across models (low values = item does not cohere)")

# Factorability and exploratory factor analysis -----------------------------------------
efa_out <- list()
fact_tab <- map_dfr(c("SDO6-EN", "SDO6-ZH", "SDO7s"), function(il) {
  M <- item_wide(il)
  R <- stats::cor(M)
  singular <- inherits(try(solve(R), silent = TRUE), "try-error")
  km <- bt <- list(ok = FALSE)
  if (!singular) invisible(utils::capture.output({
    km <- capture_fit(psych::KMO(R))
    bt <- capture_fit(psych::cortest.bartlett(R, n = nrow(M)))
  }))
  kmo <- if (km$ok) km$value$MSA else NA_real_
  row <- tibble(instrument = il, n_models = nrow(M), n_items = ncol(M),
                cor_matrix_singular = singular, KMO = kmo,
                bartlett_chisq = if (bt$ok) bt$value$chisq else NA_real_,
                bartlett_df = if (bt$ok) bt$value$df else NA_real_,
                bartlett_p = if (bt$ok) bt$value$p.value else NA_real_,
                efa_run = !is.na(kmo) && kmo >= 0.60)
  if (isTRUE(row$efa_run)) {
    invisible(utils::capture.output({
      pa <- capture_fit(psych::fa.parallel(M, fm = "minres", fa = "fa", n.iter = 100,
                                           plot = FALSE))
      f1 <- capture_fit(psych::fa(M, nfactors = 1, fm = "minres"))
      f2 <- capture_fit(psych::fa(M, nfactors = 2, fm = "minres", rotate = "oblimin"))
    }))
    row <- mutate(row, parallel_nfactors = if (pa$ok) pa$value$nfact else NA_integer_,
                  var_explained_1f = if (f1$ok) f1$value$Vaccounted[2, 1] else NA_real_)
    efa_out[[il]] <<- list(f1 = if (f1$ok) f1$value else NULL, f2 = if (f2$ok) f2$value else NULL)
  }
  row
})
report_table(fact_tab, "S12_factorability.csv",
             "Table S12.3. Factorability (KMO, Bartlett) and parallel analysis")
if (any(fact_tab$cor_matrix_singular)) {
  note("The inter-item correlation matrix (models as cases) cannot be inverted for ",
       paste(fact_tab$instrument[fact_tab$cor_matrix_singular], collapse = ", "),
       ": some items are perfectly collinear across models. KMO and ",
       "Bartlett's test are undefined there and are reported as NA; no EFA is run.")
}
if (length(efa_out)) {
  load_tab <- imap_dfr(efa_out, function(e, il) {
    if (is.null(e$f1)) return(tibble())
    L1 <- unclass(e$f1$loadings)[, 1]
    out <- tibble(instrument = il, item_no = as.integer(names(L1)), loading_1f = L1)
    if (!is.null(e$f2)) {
      L2 <- unclass(e$f2$loadings)
      out <- mutate(out, f2_MR1 = L2[, 1], f2_MR2 = L2[, 2])
    }
    out
  })
  report_table(load_tab, "S12_efa_loadings.csv",
               "Table S12.4. Exploratory factor loadings (minres; 2-factor oblimin) - exploratory, n = models")
  if (all(c("SDO6-EN", "SDO6-ZH") %in% names(efa_out)) &&
      !is.null(efa_out$`SDO6-EN`$f1) && !is.null(efa_out$`SDO6-ZH`$f1)) {
    Le <- unclass(efa_out$`SDO6-EN`$f1$loadings); Lz <- unclass(efa_out$`SDO6-ZH`$f1$loadings)
    common <- intersect(rownames(Le), rownames(Lz))
    phi <- sum(Le[common, 1] * Lz[common, 1]) /
      sqrt(sum(Le[common, 1]^2) * sum(Lz[common, 1]^2))
    say("Tucker congruence of 1-factor loadings, English vs Chinese SDO6: phi = ", fmt_n(phi, 3),
        " (>= .95 conventionally indicates factor equivalence; .85-.94 fair similarity)")
    FIG$tucker_phi <- phi
  }
}

# Configural similarity across languages: Tucker congruence of first-component
# loadings (PCA needs no matrix inversion, so it is computable when EFA is not).
pc1 <- function(M) { e <- eigen(stats::cor(M), symmetric = TRUE)
  v <- e$vectors[, 1] * sqrt(e$values[1]); names(v) <- colnames(M); if (sum(v) < 0) -v else v }
tucker <- function(a, b) sum(a * b) / sqrt(sum(a^2) * sum(b^2))
Me <- item_wide("SDO6-EN"); Mz <- item_wide("SDO6-ZH")
common_m <- intersect(rownames(Me), rownames(Mz)); common_i <- intersect(colnames(Me), colnames(Mz))
Me <- Me[common_m, common_i]; Mz <- Mz[common_m, common_i]
phi_obs <- tucker(pc1(Me), pc1(Mz))
set.seed(SEED + 12)
phi_bs <- replicate(B_BOOT, {
  i <- sample.int(nrow(Me), replace = TRUE)
  A <- Me[i, , drop = FALSE]; Z <- Mz[i, , drop = FALSE]
  if (any(apply(A, 2, stats::sd) == 0) || any(apply(Z, 2, stats::sd) == 0)) NA else tucker(pc1(A), pc1(Z))
})
say("Tucker congruence of first-component loadings, SDO6 English vs Chinese (", length(common_i),
    " items): phi = ", fmt_n(phi_obs, 3), ", bootstrap 95% CI ",
    fmt_ci(stats::quantile(phi_bs, .025, na.rm = TRUE), stats::quantile(phi_bs, .975, na.rm = TRUE), 3),
    " (>= .95 = equivalent; .85-.94 = fair similarity)")
register("S12 Structural validity", "Configural similarity EN vs ZH", "Tucker congruence of PC1 loadings (bootstrap CI)",
         estimate = phi_obs, ci_low = stats::quantile(phi_bs, .025, na.rm = TRUE),
         ci_high = stats::quantile(phi_bs, .975, na.rm = TRUE), n = paste(length(common_m), "models"),
         effect = "Tucker phi", note = "descriptive; no p-value")
FIG$pc1_loadings <- tibble(item_no = as.integer(common_i), EN = pc1(Me), ZH = pc1(Mz))

# Score-level equivalence across languages -----------------------------------------------
lang_scores <- sdo_ok %>% filter(instrument == "SDO6") %>%
  group_by(model_label, language) %>% summarise(score = mean(value_key), .groups = "drop") %>%
  pivot_wider(names_from = language, values_from = score) %>% filter(!is.na(en), !is.na(zh))
ic_lang <- icc_suite(as.matrix(lang_scores[, c("en", "zh")]))
sp_lang <- cor_test_full(lang_scores$en, lang_scores$zh)
ba <- with(lang_scores, { d <- zh - en; c(bias = mean(d), loa_lo = mean(d) - 1.96 * sd(d),
                                          loa_hi = mean(d) + 1.96 * sd(d)) })
equiv_tab <- tibble(n_models = nrow(lang_scores), spearman_rho = sp_lang$r,
                    rho_ci_low = sp_lang$ci_low, rho_ci_high = sp_lang$ci_high,
                    icc_absolute = ic_lang$iccA, iccA_lo = ic_lang$iccA_lo, iccA_hi = ic_lang$iccA_hi,
                    icc_consistency = ic_lang$iccC,
                    bland_altman_bias = ba["bias"], loa_low = ba["loa_lo"], loa_high = ba["loa_hi"])
report_table(equiv_tab, "S12_language_score_equivalence.csv",
             "Table S12.5. Are English and Chinese SDO6 scores interchangeable? (models as subjects)")
note("Rank order can be preserved (high Spearman, high consistency ICC) while ",
     "absolute agreement fails (lower ICC(A,1), non-zero Bland-Altman bias). ",
     "That pattern is scalar non-invariance at the score level.")
register("S12 Structural validity", "EN vs ZH score rank order", "Spearman correlation (model level)",
         estimate = sp_lang$r, ci_low = sp_lang$ci_low, ci_high = sp_lang$ci_high, p = sp_lang$p_perm,
         n = paste(sp_lang$n, "models"), effect = "rho")
FIG$struct   <- struct_tab
FIG$item_rest <- item_rest
FIG$lang_equiv <- equiv_tab


# ==============================================================================
# SECTION 13. ENGLISH VS CHINESE ADMINISTRATION OF SDO6
# ==============================================================================

banner(13, "English vs Chinese administration of SDO6")

sdo6 <- sdo_ok %>% filter(instrument == "SDO6") %>%
  mutate(lang_c = if_else(language == "zh", 0.5, -0.5),
         model = factor(model_label), item = factor(item_no),
         pair = interaction(model, item, drop = TRUE))

report_table(sdo6 %>% group_by(language) %>%
               summarise(n = n(), mean = mean(value_key), sd = sd(value_key),
                         median = median(value_key), pct_at_floor = 100 * mean(value_key == 1),
                         .groups = "drop"),
             "S13_language_descriptives.csv", "Table S13.1. SDO6 responses by language (recoded, 1-7)")

# Model-level paired comparison ------------------------------------------------------------
lang_model <- lang_scores %>% mutate(diff = zh - en) %>% add_meta()
ps_lang <- paired_suite(lang_model$diff)
bci_lang <- boot_mean_ci(lang_model$diff)
say("Models: ", ps_lang$n, "; mean ZH - EN = ", fmt_n(ps_lang$mean_diff, 3), " scale points ",
    fmt_ci(ps_lang$ci_low, ps_lang$ci_high, 3), " (bootstrap ", fmt_ci(bci_lang["lower"], bci_lang["upper"], 3), ")")
say("Paired t(", ps_lang$df, ") = ", fmt_n(ps_lang$t), ", ", fp(ps_lang$p_t), ", d_z = ", fmt_n(ps_lang$d_z),
    "; Wilcoxon ", fp(ps_lang$p_wilcoxon), "; ", ps_lang$n_positive, " of ", ps_lang$n,
    " models higher in Chinese (sign test ", fp(ps_lang$p_sign), ")")
register("S13 Language", "ZH - EN SDO6 score (model level)", "Paired t-test",
         estimate = ps_lang$mean_diff, ci_low = ps_lang$ci_low, ci_high = ps_lang$ci_high,
         statistic = ps_lang$t, df = ps_lang$df, p = ps_lang$p_t, n = paste(ps_lang$n, "models"),
         effect = paste0("scale points; d_z = ", round(ps_lang$d_z, 2)))
register("S13 Language", "ZH - EN SDO6 score (model level)", "Wilcoxon signed-rank",
         estimate = ps_lang$mean_diff, statistic = ps_lang$V, p = ps_lang$p_wilcoxon,
         n = paste(ps_lang$n, "models"))
register("S13 Language", "ZH - EN SDO6 score (model level)", "Sign test",
         estimate = ps_lang$n_positive, p = ps_lang$p_sign, n = paste(ps_lang$n, "models"),
         effect = "models higher in Chinese")

# Linear mixed model (continuity with the earlier analysis) -----------------------------
lmm_forms <- list(
  value_key ~ lang_c + (1 + lang_c | model) + (1 | item) + (1 | pair),
  value_key ~ lang_c + (1 + lang_c || model) + (1 | item) + (1 | pair),
  value_key ~ lang_c + (1 | model) + (1 | item) + (1 | pair))
lmm_fit <- NULL
for (f in lmm_forms) {
  r <- capture_fit(lmerTest::lmer(f, data = sdo6, REML = TRUE,
                                  control = lme4::lmerControl(optimizer = "bobyqa")))
  if (r$ok && !lme4::isSingular(r$value)) { lmm_fit <- list(fit = r$value, formula = f); break }
  if (r$ok) lmm_fit <- list(fit = r$value, formula = f)
}
co_l <- summary(lmm_fit$fit)$coefficients
ci_l <- co_l["lang_c", "Estimate"] + c(-1, 1) * stats::qt(0.975, co_l["lang_c", "df"]) * co_l["lang_c", "Std. Error"]
subhead("Linear mixed model (Satterthwaite df)")
say("Structure: ", deparse(lmm_fit$formula, width.cutoff = 200))
say("Language (ZH - EN): b = ", fmt_n(co_l["lang_c", "Estimate"], 3), " ", fmt_ci(ci_l[1], ci_l[2], 3),
    ", t(", fmt_n(co_l["lang_c", "df"], 1), ") = ", fmt_n(co_l["lang_c", "t value"]), ", ",
    fp(co_l["lang_c", "Pr(>|t|)"]))
r2_lmm <- capture_fit(performance::r2_nakagawa(lmm_fit$fit))
if (r2_lmm$ok) say("Nakagawa R2: marginal = ", fmt_n(r2_lmm$value$R2_marginal, 3),
                   ", conditional = ", fmt_n(r2_lmm$value$R2_conditional, 3))
report_table(varcomp(lmm_fit$fit), "S13_lmm_varcomp.csv", "Table S13.2. LMM variance components")
register("S13 Language", "ZH - EN (response level)", "Linear mixed model, Satterthwaite t",
         estimate = co_l["lang_c", "Estimate"], ci_low = ci_l[1], ci_high = ci_l[2],
         statistic = co_l["lang_c", "t value"], df = round(co_l["lang_c", "df"], 1),
         p = co_l["lang_c", "Pr(>|t|)"], n = paste(nrow(sdo6), "responses"), effect = "scale points")

# Cumulative link mixed model (the appropriate model for 7-point ordinal data) -------------
sdo6 <- sdo6 %>% mutate(y_ord = factor(value_key, levels = 1:7, ordered = TRUE))
clmm_forms <- list(
  y_ord ~ lang_c + (1 + lang_c | model) + (1 | item) + (1 | pair),
  y_ord ~ lang_c + (1 | model) + (1 | item) + (1 | pair))
clmm_fit <- NULL
for (f in clmm_forms) {
  r <- capture_fit(ordinal::clmm(f, data = sdo6, link = "logit", threshold = "flexible"))
  if (r$ok) { clmm_fit <- list(fit = r$value, formula = f, warnings = r$warnings); break }
}
if (!is.null(clmm_fit)) {
  cc <- summary(clmm_fit$fit)$coefficients
  b <- cc["lang_c", "Estimate"]; se <- cc["lang_c", "Std. Error"]
  f0 <- update(clmm_fit$formula, . ~ . - lang_c)
  c0 <- capture_fit(ordinal::clmm(f0, data = sdo6, link = "logit", threshold = "flexible"))
  lr_c <- if (c0$ok) 2 * (as.numeric(stats::logLik(clmm_fit$fit)) - as.numeric(stats::logLik(c0$value))) else NA
  subhead("Cumulative link mixed model (ordinal logit)")
  say("Structure: ", deparse(clmm_fit$formula, width.cutoff = 200))
  say("Language (ZH vs EN): cumulative OR = ", fmt_n(exp(b)), " ", fmt_ci(exp(b - 1.96 * se), exp(b + 1.96 * se)),
      ", Wald z = ", fmt_n(cc["lang_c", "z value"]), ", ", fp(cc["lang_c", "Pr(>|z|)"]),
      "; LR chi2(1) = ", fmt_n(lr_c), ", ", fp(stats::pchisq(lr_c, 1, lower.tail = FALSE)))
  register("S13 Language", "ZH vs EN (response level)", "Cumulative link mixed model, LR test",
           estimate = exp(b), ci_low = exp(b - 1.96 * se), ci_high = exp(b + 1.96 * se),
           statistic = lr_c, df = 1, p = stats::pchisq(lr_c, 1, lower.tail = FALSE),
           n = paste(nrow(sdo6), "responses"), effect = "cumulative odds ratio")
  FIG$clmm_lang <- tibble(OR = exp(b), lo = exp(b - 1.96 * se), hi = exp(b + 1.96 * se))
}

# Meta-analysis of per-model language shifts; developer origin as moderator ---------------
lang_pairs <- item_means %>% filter(instrument == "SDO6") %>%
  select(model_label, item_no, subscale, inst_lang, key) %>%
  pivot_wider(names_from = inst_lang, values_from = key) %>%
  mutate(d = tie_round(`SDO6-ZH` - `SDO6-EN`))
lang_studies <- lang_pairs %>% group_by(model_label) %>%
  summarise(k_items = sum(!is.na(d)), yi = mean(d, na.rm = TRUE),
            sei = sd(d, na.rm = TRUE) / sqrt(k_items), .groups = "drop") %>%
  mutate(sei = if_else(sei > 0, sei, 0.5 / k_items)) %>% add_meta()
ma_lang <- meta_re(lang_studies$yi, lang_studies$sei)
ms_lang <- meta_summary(ma_lang)
mr_origin <- meta_re(lang_studies$yi, lang_studies$sei, mods = ~ origin, data = lang_studies)
mr_dev    <- meta_re(lang_studies$yi, lang_studies$sei, mods = ~ developer_group, data = lang_studies)
report_table(ms_lang, "S13_language_meta.csv",
             "Table S13.3. Random-effects meta-analysis of per-model language shifts (scale points)")
say("Pooled shift ", fmt_n(ms_lang$estimate, 3), " ", fmt_ci(ms_lang$ci_low, ms_lang$ci_high, 3),
    "; 95% PI ", fmt_ci(ms_lang$pi_low, ms_lang$pi_high, 3), "; I2 = ", fmt_n(ms_lang$I2, 1), "%")
say("Moderator origin (China vs US developer): F(", mr_origin$QMdf[1], ", ", mr_origin$QMdf[2], ") = ",
    fmt_n(mr_origin$QM), ", ", fp(mr_origin$QMp), "; developer group: F = ", fmt_n(mr_dev$QM), ", ", fp(mr_dev$QMp))
note("Only two models were developed in China, so the origin contrast has almost ",
     "no power (Section 18 gives the sample needed).")
register("S13 Language", "Meta-analysis of per-model shifts", "Random-effects pooled estimate (REML, Knapp-Hartung)",
         estimate = ms_lang$estimate, ci_low = ms_lang$ci_low, ci_high = ms_lang$ci_high, p = ms_lang$p,
         n = paste(ms_lang$k, "models"), effect = "scale points",
         note = paste0("95% PI [", round(ms_lang$pi_low, 3), ", ", round(ms_lang$pi_high, 3), "]"))
register("S13 Language", "Meta-analysis of per-model shifts", "Cochran Q heterogeneity",
         estimate = ms_lang$I2, statistic = ms_lang$Q, df = ms_lang$Q_df, p = ms_lang$Q_p,
         n = paste(ms_lang$k, "models"), effect = "I^2 (%)")
register("S13 Language", "Meta-analysis of per-model shifts", "Meta-regression: developer origin (China vs US)",
         statistic = mr_origin$QM, df = paste(mr_origin$QMdf, collapse = ", "), p = mr_origin$QMp,
         n = paste(ms_lang$k, "models (2 China)"))
FIG$lang_model   <- lang_model
FIG$lang_studies <- lang_studies
FIG$lang_meta    <- ms_lang

# Item-level shifts, BH-adjusted ---------------------------------------------------------
lang_item <- lang_pairs %>% filter(!is.na(d)) %>%
  group_by(item_no, subscale) %>%
  summarise(n_models = n(), mean_en = mean(`SDO6-EN`), mean_zh = mean(`SDO6-ZH`),
            diff = mean(d), se = sd(d) / sqrt(n()),
            p_t = if (sd(d) > 0) stats::t.test(d)$p.value else 1,
            p_wilcoxon = if (any(d != 0)) suppressWarnings(stats::wilcox.test(d)$p.value) else 1,
            n_models_up = sum(d > 0), n_models_down = sum(d < 0), .groups = "drop") %>%
  mutate(ci_low = diff - stats::qt(0.975, n_models - 1) * se,
         ci_high = diff + stats::qt(0.975, n_models - 1) * se,
         p_t_bh = p.adjust(p_t, "BH"), p_wilcoxon_bh = p.adjust(p_wilcoxon, "BH"),
         item_text = SDO6_TEXT[item_no])
report_table(lang_item %>% arrange(desc(diff)) %>% select(-item_text), "S13_language_by_item.csv",
             "Table S13.4. Language shift by SDO6 item (paired over models, BH-adjusted)")
FIG$lang_item <- lang_item
for (i in seq_len(nrow(lang_item))) {
  register("S13 Language (per item)", paste("SDO6 item", lang_item$item_no[i]),
           "Wilcoxon signed-rank (ZH - EN over models)", estimate = lang_item$diff[i],
           ci_low = lang_item$ci_low[i], ci_high = lang_item$ci_high[i], p = lang_item$p_wilcoxon[i],
           n = paste(lang_item$n_models[i], "models"), effect = "scale points")
}

# Measurement invariance at the item level --------------------------------------------------
subhead("Item-level invariance: is the language shift the same for every item?")
note("Models answer both versions, so invariance is tested within models. Under ",
     "scalar invariance with a true change in the construct, every item shifts by ",
     "the same amount (impact); items that shift more or less than the rest show ",
     "differential item functioning (DIF). (1) The item x language interaction in ",
     "a linear mixed model tests whether shifts are equal across items. (2) Robust ",
     "linking locates the DIF items: each item's shift minus the MEDIAN item shift ",
     "(valid if fewer than half the items have DIF), bootstrapped over models. ",
     "Ordinal-regression DIF with rest-score matching was not used: with 27 models ",
     "and most responses at the floor it produces quasi-separation and unstable ",
     "odds ratios.")
int_fit <- capture_fit(lmerTest::lmer(value_key ~ lang_c * item + (1 + lang_c | model) + (1 | pair),
                                      data = sdo6, REML = TRUE,
                                      control = lme4::lmerControl(optimizer = "bobyqa")))
if (int_fit$ok) {
  an_int <- as.data.frame(stats::anova(int_fit$value, type = 3, ddf = "Satterthwaite"))
  ri <- an_int["lang_c:item", ]
  say("Item x language interaction: F(", ri$NumDF, ", ", fmt_n(ri$DenDF, 1), ") = ",
      fmt_n(ri$`F value`), ", ", fp(ri$`Pr(>F)`))
  register("S13 Language invariance", "Equal shift across items", "LMM item x language interaction (Satterthwaite F)",
           statistic = ri$`F value`, df = paste0(ri$NumDF, ", ", round(ri$DenDF, 1)),
           p = ri$`Pr(>F)`, n = paste(nrow(sdo6), "responses"),
           note = "H0: every item shifts by the same amount (scalar invariance with impact)")
}
rl_dat <- lang_pairs %>% filter(!is.na(d)) %>% select(model_label, item_no, d)
rl_wide <- rl_dat %>% pivot_wider(names_from = item_no, values_from = d)
D_mat <- as.matrix(rl_wide[, -1])
rl_stat <- function(D) { sh <- colMeans(D, na.rm = TRUE); sh - stats::median(sh) }
dif_obs <- rl_stat(D_mat)
set.seed(SEED + 13)
dif_bs <- t(replicate(B_BOOT, rl_stat(D_mat[sample.int(nrow(D_mat), replace = TRUE), , drop = FALSE])))
dif <- tibble(item_no = as.integer(colnames(D_mat)),
              shift = colMeans(D_mat, na.rm = TRUE),
              common_shift_median = stats::median(colMeans(D_mat, na.rm = TRUE)),
              dif = dif_obs,
              ci_low = apply(dif_bs, 2, stats::quantile, .025, na.rm = TRUE),
              ci_high = apply(dif_bs, 2, stats::quantile, .975, na.rm = TRUE),
              p_boot = apply(dif_bs, 2, function(b) min(1, 2 * min((1 + sum(b <= 1e-9)) / (1 + length(b)),
                                                                   (1 + sum(b >= -1e-9)) / (1 + length(b)))))) %>%
  mutate(p_bh = p.adjust(p_boot, "BH"), flagged = p_bh < ALPHA,
         direction = case_when(!flagged ~ "invariant", dif > 0 ~ "shifts more than scale",
                               TRUE ~ "shifts less than scale")) %>%
  left_join(lang_item %>% select(item_no, subscale), by = "item_no") %>%
  relocate(subscale, .after = item_no)
report_table(dif, "S13_dif_robust_linking.csv",
             "Table S13.5. Item-level DIF by robust (median) linking, bootstrap over models, BH-adjusted")
say("Common (median) item shift: ", fmt_n(dif$common_shift_median[1], 3), " scale points; items with DIF: ",
    sum(dif$flagged), " of ", nrow(dif), " (", paste(dif$item_no[dif$flagged], collapse = ", "), ")")
for (i in seq_len(nrow(dif))) {
  register("S13 Language DIF (per item)", paste("SDO6 item", dif$item_no[i]),
           "Robust-linking DIF (item shift minus median shift), bootstrap test",
           estimate = dif$dif[i], ci_low = dif$ci_low[i], ci_high = dif$ci_high[i],
           p = dif$p_boot[i], n = paste(nrow(D_mat), "models"), effect = "scale points")
}
note("A language effect carried by a few DIF items is a translation or item-meaning ",
     "problem rather than a change in the construct: report it as partial ",
     "non-invariance and show the scale difference with the DIF items removed. ",
     "Caveat: shifts are on the response metric, so items already at the floor ",
     "in both languages cannot shift much; 'shifts less than scale' for floor ",
     "items partly reflects that bound.")
inv_items <- dif$item_no[!dif$flagged]
ml_inv <- sdo6 %>% filter(item_no %in% inv_items) %>% group_by(model_label, language) %>%
  summarise(s = mean(value_key), .groups = "drop") %>% pivot_wider(names_from = language, values_from = s)
ps_inv <- paired_suite(ml_inv$zh - ml_inv$en)
say("Shift on invariant items only (", length(inv_items), " items): ", fmt_n(ps_inv$mean_diff, 3), " ",
    fmt_ci(ps_inv$ci_low, ps_inv$ci_high, 3), ", t(", ps_inv$df, ") = ", fmt_n(ps_inv$t), ", ", fp(ps_inv$p_t),
    ", d_z = ", fmt_n(ps_inv$d_z))
register("S13 Language invariance", "ZH - EN on invariant items only", "Paired t-test (model level)",
         estimate = ps_inv$mean_diff, ci_low = ps_inv$ci_low, ci_high = ps_inv$ci_high,
         statistic = ps_inv$t, df = ps_inv$df, p = ps_inv$p_t, n = paste(ps_inv$n, "models"),
         effect = paste0("scale points; d_z = ", round(ps_inv$d_z, 2)))
FIG$dif <- dif

# Extremity softening vs direction change -------------------------------------------------
xd <- item_means %>% filter(instrument == "SDO6") %>%
  select(model_label, item_no, subscale, keying, inst_lang, raw, key) %>%
  pivot_wider(names_from = inst_lang, values_from = c(raw, key)) %>%
  filter(!is.na(`raw_SDO6-EN`), !is.na(`raw_SDO6-ZH`)) %>%
  mutate(side_en = sign(tie_round(`raw_SDO6-EN` - 4)), side_zh = sign(tie_round(`raw_SDO6-ZH` - 4)),
         ext_en = abs(`raw_SDO6-EN` - 4), ext_zh = abs(`raw_SDO6-ZH` - 4),
         dkey = `key_SDO6-ZH` - `key_SDO6-EN`,
         pattern = case_when(
           abs(dkey) < 1e-9 ~ "unchanged",
           side_en == 0 | side_zh == 0 ~ "at or through midpoint",
           side_en != side_zh ~ "direction reversed",
           ext_zh < ext_en ~ "same side, less extreme in Chinese",
           TRUE ~ "same side, more extreme in Chinese"))
decomp <- xd %>% group_by(subscale, pattern) %>%
  summarise(n_pairs = n(), sum_shift = sum(dkey), .groups = "drop") %>%
  group_by(subscale) %>%
  mutate(pct_pairs = 100 * n_pairs / sum(n_pairs),
         share_of_net_shift = sum_shift / sum(sum_shift)) %>% ungroup()
report_table(decomp, "S13_extremity_vs_direction.csv",
             "Table S13.6. Decomposition of the language shift: extremity change vs direction reversal")
ext_model <- xd %>% group_by(model_label) %>%
  summarise(ext_en = mean(ext_en), ext_zh = mean(ext_zh), .groups = "drop")
ps_ext <- paired_suite(ext_model$ext_zh - ext_model$ext_en)
say("Mean extremity |x - 4|: EN ", fmt_n(mean(ext_model$ext_en), 3), ", ZH ", fmt_n(mean(ext_model$ext_zh), 3),
    "; paired Wilcoxon ", fp(ps_ext$p_wilcoxon), "; ", ps_ext$n_negative, " of ", ps_ext$n,
    " models less extreme in Chinese")
register("S13 Language", "Extremity softening", "Wilcoxon signed-rank on mean |x - midpoint| (model level)",
         estimate = ps_ext$mean_diff, statistic = ps_ext$V, p = ps_ext$p_wilcoxon,
         n = paste(ps_ext$n, "models"), effect = "scale points (ZH - EN)")
g_ers <- glmer_fit(I(value_raw %in% c(1, 7)) ~ lang_c + (1 | model) + (1 | item), sdo6)
g_ers0 <- glmer_fit(I(value_raw %in% c(1, 7)) ~ 1 + (1 | model) + (1 | item), sdo6)
if (g_ers$ok && g_ers0$ok) {
  or_ers <- glmm_or_table(g_ers$value) %>% filter(term == "lang_c")
  lr_ers <- lrt(g_ers$value, g_ers0$value)
  say("Endpoint responding, GLMM: OR (ZH vs EN) = ", fmt_n(or_ers$OR), " ",
      fmt_ci(or_ers$OR_low, or_ers$OR_high), ", LR ", fp(lr_ers$p))
  register("S13 Language", "Endpoint (extreme) responding", "GLMM likelihood-ratio test",
           estimate = or_ers$OR, ci_low = or_ers$OR_low, ci_high = or_ers$OR_high,
           statistic = lr_ers$chisq, df = 1, p = lr_ers$p, n = paste(nrow(sdo6), "responses"),
           effect = "odds ratio")
}
FIG$lang_decomp <- decomp
FIG$resp_counts <- sdo_ok %>% count(instrument, inst_lang, language, subscale, keying, value_key)
FIG$lang_pairs_x <- xd

# Is Chinese administration less stable? ---------------------------------------------------
st_lang <- sdo_pair %>% filter(instrument == "SDO6") %>%
  mutate(lang_c = if_else(language == "zh", 0.5, -0.5), item = factor(item_no),
         changed = as.integer(changed))
g_st <- glmer_fit(changed ~ lang_c + (1 | model_label) + (1 | item), st_lang)
g_st0 <- glmer_fit(changed ~ 1 + (1 | model_label) + (1 | item), st_lang)
or_st <- glmm_or_table(g_st$value) %>% filter(term == "lang_c")
lr_st <- lrt(g_st$value, g_st0$value)
st_model <- model_instr %>% filter(inst_lang %in% c("SDO6-EN", "SDO6-ZH")) %>%
  select(model_label, inst_lang, pct_changed) %>%
  pivot_wider(names_from = inst_lang, values_from = pct_changed)
ps_st <- paired_suite(st_model$`SDO6-ZH` - st_model$`SDO6-EN`)
say("Instability ZH vs EN: GLMM OR = ", fmt_n(or_st$OR), " ", fmt_ci(or_st$OR_low, or_st$OR_high),
    ", LR ", fp(lr_st$p), "; model level mean ", fmt_n(ps_st$mean_diff), " pp, Wilcoxon ", fp(ps_st$p_wilcoxon))
register("S13 Language", "Instability by language", "GLMM likelihood-ratio test",
         estimate = or_st$OR, ci_low = or_st$OR_low, ci_high = or_st$OR_high,
         statistic = lr_st$chisq, df = 1, p = lr_st$p, n = paste(nrow(st_lang), "pairs"), effect = "odds ratio")

# Refusal sensitivity: pairs scored on all 10 calls (5 per language) -----------------------
clean_pairs <- sdo %>% filter(instrument == "SDO6") %>%
  group_by(model_label, item_no) %>% summarise(ok = all(outcome == "scored"), .groups = "drop") %>%
  filter(ok)
ml_clean <- sdo6 %>% semi_join(clean_pairs, by = c("model_label", "item_no")) %>%
  group_by(model_label, language) %>% summarise(s = mean(value_key), .groups = "drop") %>%
  pivot_wider(names_from = language, values_from = s)
ps_clean <- paired_suite(ml_clean$zh - ml_clean$en)
say("Sensitivity (pairs scored on every call in both languages): mean shift ",
    fmt_n(ps_clean$mean_diff, 3), " ", fmt_ci(ps_clean$ci_low, ps_clean$ci_high, 3),
    ", t(", ps_clean$df, ") = ", fmt_n(ps_clean$t), ", ", fp(ps_clean$p_t))
register("S13 Language", "ZH - EN, complete-case sensitivity", "Paired t-test (model level)",
         estimate = ps_clean$mean_diff, ci_low = ps_clean$ci_low, ci_high = ps_clean$ci_high,
         statistic = ps_clean$t, df = ps_clean$df, p = ps_clean$p_t, n = paste(ps_clean$n, "models"))

# ==============================================================================
# SECTION 14. LONGITUDINAL DRIFT (JUNE VS AUGUST / SEPTEMBER 2026)
# ==============================================================================

banner(14, "Longitudinal drift: are the same endpoints stable across months?")
note("The same model endpoints were administered in June 2026 and again in ",
     "August (SDO7s, VSA) or September (RMET). If an endpoint is unchanged, a ",
     "June answer is just another draw from the same response distribution, so ",
     "it should agree with a later answer exactly as often as two later answers ",
     "agree with each other. Excess disagreement indicates drift, for example a ",
     "silent model update behind the same endpoint name.")

# RMET: June single run vs September five runs ------------------------------------------
common_r <- intersect(unique(rmetJ_ok$model_label), unique(rmet_ok$model_label))
drift_units <- rmet_ok %>% filter(model_label %in% common_r) %>%
  group_by(model_label, condition, item) %>%
  summarise(sep = list(choice_idx), sep_acc = mean(correct), .groups = "drop") %>%
  inner_join(rmetJ_ok %>% select(model_label, condition, item, jun = choice_idx, jun_correct = correct),
             by = c("model_label", "condition", "item")) %>%
  mutate(within = map_dbl(sep, pair_agree),
         between = map2_dbl(sep, jun, ~ mean(.x == .y)))
drift_rmet <- drift_units %>% group_by(model_label) %>%
  group_modify(function(d, key) {
    obs <- tie_round(mean(d$between) - mean(d$within, na.rm = TRUE))
    bs <- replicate(B_BOOT, { i <- sample.int(nrow(d), replace = TRUE)
      tie_round(mean(d$between[i]) - mean(d$within[i], na.rm = TRUE)) })
    tibble(n_units = nrow(d), within_sep = mean(d$within, na.rm = TRUE),
           between_jun_sep = mean(d$between), excess_disagreement = -obs,
           ci_low = -stats::quantile(bs, .975), ci_high = -stats::quantile(bs, .025),
           p_boot = min(1, 2 * min(mean(bs >= 0), mean(bs <= 0)) + 1 / B_BOOT),
           acc_jun = mean(d$jun_correct), acc_sep = mean(d$sep_acc))
  }) %>% ungroup() %>% mutate(p_bh = p.adjust(p_boot, "BH")) %>% add_meta()
ps_drift <- paired_suite(drift_rmet$between_jun_sep - drift_rmet$within_sep)
ps_acc_drift <- paired_suite(100 * (drift_rmet$acc_sep - drift_rmet$acc_jun))
report_table(drift_rmet %>% select(display, within_sep, between_jun_sep, excess_disagreement,
                                   ci_low, ci_high, p_bh, acc_jun, acc_sep) %>%
               arrange(desc(excess_disagreement)),
             "S14_rmet_drift_by_model.csv",
             "Table S14.1. RMET: agreement within September vs between June and September, by model")
say("Across ", ps_drift$n, " models: between-session minus within-session agreement = ",
    fmt_n(ps_drift$mean_diff, 3), " ", fmt_ci(ps_drift$ci_low, ps_drift$ci_high, 3),
    ", Wilcoxon ", fp(ps_drift$p_wilcoxon), "; models with significant drift (BH): ",
    sum(drift_rmet$p_bh < ALPHA))
say("Accuracy change (September minus June): ", fmt_n(ps_acc_drift$mean_diff), " pp ",
    fmt_ci(ps_acc_drift$ci_low, ps_acc_drift$ci_high), ", ", fp(ps_acc_drift$p_t))
register("S14 Drift", "RMET June vs September", "Wilcoxon signed-rank: between- vs within-session agreement",
         estimate = ps_drift$mean_diff, ci_low = ps_drift$ci_low, ci_high = ps_drift$ci_high,
         statistic = ps_drift$V, p = ps_drift$p_wilcoxon, n = paste(ps_drift$n, "models"),
         effect = "agreement difference (negative = drift)")
register("S14 Drift", "RMET June vs September", "Paired t-test: accuracy change",
         estimate = ps_acc_drift$mean_diff, ci_low = ps_acc_drift$ci_low, ci_high = ps_acc_drift$ci_high,
         statistic = ps_acc_drift$t, df = ps_acc_drift$df, p = ps_acc_drift$p_t,
         n = paste(ps_acc_drift$n, "models"), effect = "% points")
for (i in seq_len(nrow(drift_rmet))) {
  register("S14 Drift (per model)", paste("RMET", drift_rmet$display[i]),
           "Bootstrap test: excess June-September disagreement",
           estimate = drift_rmet$excess_disagreement[i], ci_low = drift_rmet$ci_low[i],
           ci_high = drift_rmet$ci_high[i], p = drift_rmet$p_boot[i], n = "72 item x condition units")
}

# SDO7s and VSA: June five runs vs August five runs -------------------------------------
common_s <- intersect(unique(sdoJ_ok$model_label), unique(sdo_ok$model_label))
sess <- bind_rows(sdoJ_ok %>% mutate(session = "Jun"),
                  sdo_ok %>% filter(inst_lang %in% c("SDO7s", "VSA")) %>% mutate(session = "Aug")) %>%
  filter(model_label %in% common_s) %>%
  group_by(model_label, inst_lang, item_no) %>%
  filter(sum(session == "Jun") == N_RUNS, sum(session == "Aug") == N_RUNS) %>% ungroup()
session_perm <- function(d, B = B_PERM) {
  V <- d %>% arrange(model_label, item_no, session, run) %>%
    group_by(model_label, item_no) %>%
    summarise(v = list(c(value_key[session == "Jun"], value_key[session == "Aug"])), .groups = "drop")
  V <- do.call(rbind, V$v)
  stat <- function(A) sum((rowMeans(A[, 1:5]) - rowMeans(A[, 6:10]))^2)
  obs <- stat(V)
  nul <- vapply(seq_len(B), function(b) {
    idx <- t(apply(matrix(stats::runif(length(V)), nrow(V)), 1, order))
    stat(matrix(V[cbind(rep(seq_len(nrow(V)), 10), as.vector(idx))], nrow(V)))
  }, numeric(1))
  c(obs = obs, null_mean = mean(nul), ratio = obs / mean(nul),
    p = (1 + sum(nul >= obs - 1e-12)) / (1 + B), n_pairs = nrow(V))
}
set.seed(SEED + 14)
drift_sdo <- map_dfr(c("SDO7s", "VSA"), function(il) {
  d <- sess %>% filter(inst_lang == il)
  sp <- session_perm(d, B = 2000)
  vc_fit <- capture_fit(lme4::lmer(value_key ~ 1 + (1 | pairid) + (1 | pairid:session),
                                   data = d %>% mutate(pairid = paste(model_label, item_no)), REML = TRUE))
  vc <- as.data.frame(lme4::VarCorr(vc_fit$value))
  v_sess <- vc$vcov[vc$grp == "pairid:session"]; v_run <- vc$vcov[vc$grp == "Residual"]
  sc <- d %>% group_by(model_label, session) %>% summarise(s = mean(value_key), .groups = "drop") %>%
    pivot_wider(names_from = session, values_from = s)
  ic <- icc_suite(as.matrix(sc[, c("Jun", "Aug")]))
  ps <- paired_suite(sc$Aug - sc$Jun)
  tibble(instrument = il, n_models = nrow(sc), n_pairs = sp["n_pairs"],
         perm_stat_ratio = sp["ratio"], p_perm = sp["p"],
         var_session_within_pair = v_sess, var_run_within_session = v_run,
         session_share_of_within_pair_var = v_sess / (v_sess + v_run),
         scale_icc_A = ic$iccA, scale_icc_lo = ic$iccA_lo, scale_icc_hi = ic$iccA_hi,
         scale_mean_change = ps$mean_diff, scale_change_p = ps$p_wilcoxon)
})
report_table(drift_sdo, "S14_sdo_vsa_drift.csv",
             "Table S14.2. SDO7s and VSA, June vs August (5 runs each)")
note("perm_stat_ratio compares the observed between-session squared differences ",
     "with the value expected if session labels were arbitrary (1 = no drift). ",
     "session_share is the share of within-pair variance attributable to the ",
     "session rather than to run-to-run noise.")
for (i in seq_len(nrow(drift_sdo))) {
  register("S14 Drift", paste(drift_sdo$instrument[i], "June vs August"),
           "Within-pair permutation of session labels", estimate = drift_sdo$perm_stat_ratio[i],
           p = drift_sdo$p_perm[i], n = paste(drift_sdo$n_pairs[i], "pairs"),
           effect = "observed / null squared difference")
  register("S14 Drift", paste(drift_sdo$instrument[i], "June vs August"),
           "Wilcoxon signed-rank: scale-score change", estimate = drift_sdo$scale_mean_change[i],
           p = drift_sdo$scale_change_p[i], n = paste(drift_sdo$n_models[i], "models"))
}
FIG$drift_rmet <- drift_rmet
FIG$drift_sdo_scores <- sess %>% group_by(model_label, inst_lang, session) %>%
  summarise(score = mean(pos), .groups = "drop") %>% add_meta()
FIG$drift_sdo <- drift_sdo


# ==============================================================================
# SECTION 15. ALIGNMENT PROXY: TESTING IT PROPERLY
# ==============================================================================

banner(15, "Alignment proxy: floor pinning and instability")
note("Hypothesis: a model pinned at the construct floor is expressing refusal ",
     "tuning, and such models are also the stable ones. The raw correlation is ",
     "partly MECHANICAL, because a pair pinned at the floor on all five runs ",
     "cannot have changed. Four further tests remove that overlap: instability ",
     "among non-pinned pairs only; split-item correlations (pinning measured on ",
     "one random half of the items, instability on the other half); prediction ",
     "of VSA instability; and a discriminant test against RMET instability, ",
     "which should NOT follow if the effect is about attitude-refusal tuning ",
     "rather than general response determinism.")

en_pairs <- sdo_pair %>% filter(inst_lang %in% c("SDO7s", "SDO6-EN"))
align <- en_pairs %>% group_by(model_label) %>%
  summarise(n_pairs = n(), pct_at_floor = 100 * mean(at_floor), pct_changed = 100 * mean(changed),
            pct_changed_nonpinned = if (any(!at_floor)) 100 * mean(changed[!at_floor]) else NA_real_,
            n_nonpinned = sum(!at_floor), mean_pos = mean(mean_pos), .groups = "drop") %>%
  left_join(model_instr %>% filter(inst_lang == "VSA") %>% select(model_label, vsa_changed = pct_changed),
            by = "model_label") %>%
  left_join(rmet_instab_model %>% filter(condition == "Pooled") %>% select(model_label, rmet_changed = pct_changed),
            by = "model_label") %>% add_meta()
report_table(align %>% select(display, developer, n_pairs, pct_at_floor, pct_changed,
                              n_nonpinned, pct_changed_nonpinned, vsa_changed, rmet_changed) %>%
               arrange(desc(pct_at_floor)),
             "S15_alignment_by_model.csv",
             "Table S15.1. Floor pinning (SDO7s + SDO6-EN) and instability by model")

split_item_rho <- function(pairs, n_split = 200) {
  items <- unique(paste(pairs$inst_lang, pairs$item_no))
  pairs <- pairs %>% mutate(it = paste(inst_lang, item_no))
  vapply(seq_len(n_split), function(s) {
    A <- sample(items, floor(length(items) / 2))
    a <- pairs %>% filter(it %in% A) %>% group_by(model_label) %>% summarise(f = mean(at_floor), .groups = "drop")
    b <- pairs %>% filter(!it %in% A) %>% group_by(model_label) %>% summarise(c = mean(changed), .groups = "drop")
    ab <- inner_join(a, b, by = "model_label")
    suppressWarnings(stats::cor(ab$f, ab$c, method = "spearman"))
  }, numeric(1))
}
set.seed(SEED + 15)
si <- split_item_rho(en_pairs)
# Bootstrap over models for the split-item correlation
mods_a <- unique(en_pairs$model_label)
si_bs <- replicate(300, {
  m <- sample(mods_a, replace = TRUE)
  pb <- map_dfr(seq_along(m), ~ en_pairs %>% filter(model_label == m[.x]) %>% mutate(model_label = paste0("b", .x)))
  mean(split_item_rho(pb, n_split = 20), na.rm = TRUE)
})
tests_align <- list(
  list("(i) floor pinning vs instability, same pairs (partly mechanical)", align$pct_at_floor, align$pct_changed),
  list("(ii) floor pinning vs instability of NON-pinned pairs", align$pct_at_floor, align$pct_changed_nonpinned),
  list("(iv) SDO floor pinning vs VSA instability (different items)", align$pct_at_floor, align$vsa_changed),
  list("(v) SDO floor pinning vs RMET instability (discriminant)", align$pct_at_floor, align$rmet_changed))
align_tab <- map_dfr(tests_align, function(t) cor_test_full(t[[2]], t[[3]]) %>% mutate(test = t[[1]], .before = 1))
align_tab <- bind_rows(align_tab,
  tibble(test = "(iii) split-item: pinning on half the items vs instability on the other half",
         n = length(mods_a), r = mean(si, na.rm = TRUE),
         ci_low = stats::quantile(si_bs, .025, na.rm = TRUE),
         ci_high = stats::quantile(si_bs, .975, na.rm = TRUE), p_perm = NA_real_)) %>%
  arrange(test)
report_table(align_tab, "S15_alignment_tests.csv",
             "Table S15.2. Alignment-proxy correlations (Spearman; bootstrap 95% CI; permutation p)")
for (i in seq_len(nrow(align_tab))) {
  register("S15 Alignment proxy", align_tab$test[i], "Spearman correlation",
           estimate = align_tab$r[i], ci_low = align_tab$ci_low[i], ci_high = align_tab$ci_high[i],
           p = align_tab$p_perm[i], n = paste(align_tab$n[i], "models"), effect = "rho")
}

# Does pinning add to developer? (multiple regression, adjusted R2) ---------------------
lm_dev   <- stats::lm(pct_changed ~ developer_group, data = align)
lm_full  <- stats::lm(pct_changed ~ developer_group + pct_at_floor, data = align)
an_nest  <- stats::anova(lm_dev, lm_full)
sm_full  <- summary(lm_full)
subhead("Multiple regression: instability ~ developer group + floor pinning")
print(round(sm_full$coefficients, 4))
say("R2 = ", fmt_n(sm_full$r.squared, 3), ", adjusted R2 = ", fmt_n(sm_full$adj.r.squared, 3),
    " (developer only: adjusted R2 = ", fmt_n(summary(lm_dev)$adj.r.squared, 3), ")")
say("Floor pinning beyond developer: F(", an_nest$Df[2], ", ", an_nest$Res.Df[2], ") = ",
    fmt_n(an_nest$F[2]), ", ", fp(an_nest$`Pr(>F)`[2]))
register("S15 Alignment proxy", "Pinning beyond developer group", "Nested-model F test (OLS)",
         estimate = sm_full$coefficients["pct_at_floor", "Estimate"], statistic = an_nest$F[2],
         df = paste(an_nest$Df[2], an_nest$Res.Df[2], sep = ", "), p = an_nest$`Pr(>F)`[2],
         n = paste(nrow(align), "models"), effect = "slope (pp instability per pp pinned)",
         note = paste0("adj R2 ", round(summary(lm_dev)$adj.r.squared, 3), " -> ", round(sm_full$adj.r.squared, 3)))
# Difference between the non-mechanical and the discriminant correlation
ov <- align %>% filter(!is.na(rmet_changed), !is.na(pct_changed_nonpinned))
d_obs <- stats::cor(ov$pct_at_floor, ov$pct_changed_nonpinned, method = "spearman") -
  stats::cor(ov$pct_at_floor, ov$rmet_changed, method = "spearman")
d_bs <- replicate(B_BOOT, { i <- sample.int(nrow(ov), replace = TRUE)
  suppressWarnings(stats::cor(ov$pct_at_floor[i], ov$pct_changed_nonpinned[i], method = "spearman") -
                     stats::cor(ov$pct_at_floor[i], ov$rmet_changed[i], method = "spearman")) })
say("Attitude-specific minus general (test ii minus test v) on ", nrow(ov), " shared models: ",
    fmt_n(d_obs, 3), " ", fmt_ci(stats::quantile(d_bs, .025, na.rm = TRUE),
                                 stats::quantile(d_bs, .975, na.rm = TRUE), 3))
register("S15 Alignment proxy", "Specificity: (ii) minus (v)", "Bootstrap CI for a difference of dependent correlations",
         estimate = d_obs, ci_low = stats::quantile(d_bs, .025, na.rm = TRUE),
         ci_high = stats::quantile(d_bs, .975, na.rm = TRUE), n = paste(nrow(ov), "models"),
         note = "CI excluding 0 supports an attitude-specific mechanism")
FIG$align <- align
FIG$align_tests <- align_tab


# ==============================================================================
# SECTION 16. MODEL PROFILES: PCA AND CLUSTERING
# ==============================================================================

banner(16, "Model profiles: principal components and clustering")
note("Each model is described by its attitude scores, instability, pinning, ",
     "response styles and language shift. PCA summarises the profile space; the ",
     "number of components is chosen by Horn's parallel analysis. Clusters come ",
     "from Ward hierarchical clustering; the number of clusters by average ",
     "silhouette; stability by bootstrap Jaccard (Hennig, 2007: > .75 stable, ",
     "< .50 dissolved); robustness by agreement with PAM (adjusted Rand index).")

prof <- model_instr %>% select(model_label, inst_lang, pct_changed, mean_score) %>%
  pivot_wider(names_from = inst_lang, values_from = c(pct_changed, mean_score)) %>%
  left_join(styles %>% filter(inst_lang == "SDO6-EN") %>% select(model_label, ERS, ARS), by = "model_label") %>%
  left_join(align %>% select(model_label, pct_at_floor), by = "model_label") %>%
  left_join(lang_model %>% select(model_label, lang_shift = diff), by = "model_label")
feat_names <- c(`mean_score_SDO7s` = "SDO7(s) score", `mean_score_SDO6-EN` = "SDO6 score (EN)",
                `mean_score_VSA` = "VSA score", lang_shift = "Language shift (ZH - EN)",
                `pct_changed_SDO7s` = "Instability SDO7(s)", `pct_changed_SDO6-EN` = "Instability SDO6 (EN)",
                `pct_changed_SDO6-ZH` = "Instability SDO6 (ZH)", `pct_changed_VSA` = "Instability VSA",
                pct_at_floor = "Floor pinning", ERS = "Extreme responding", ARS = "Acquiescence")
X <- prof %>% select(model_label, all_of(names(feat_names))) %>% filter(stats::complete.cases(.))
Xs <- scale(as.matrix(X[, -1])); rownames(Xs) <- X$model_label
pca <- stats::prcomp(Xs, center = FALSE, scale. = FALSE)
eig <- pca$sdev^2
set.seed(SEED + 16)
pa_null <- replicate(1000, { Z <- matrix(stats::rnorm(length(Xs)), nrow(Xs)); eigen(stats::cor(Z), only.values = TRUE)$values })
pa_q95 <- apply(pa_null, 1, stats::quantile, 0.95)
n_keep <- max(1, sum(cumprod(eig > pa_q95)))
pca_var <- tibble(component = paste0("PC", seq_along(eig)), eigenvalue = eig,
                  pct_variance = 100 * eig / sum(eig), cum_pct = cumsum(100 * eig / sum(eig)),
                  parallel_95th = pa_q95, retained = seq_along(eig) <= n_keep)
report_table(pca_var, "S16_pca_variance.csv", "Table S16.1. PCA eigenvalues and Horn's parallel analysis")
loadings <- as_tibble(pca$rotation[, 1:max(2, n_keep)] %*% diag(pca$sdev[1:max(2, n_keep)]),
                      .name_repair = ~ paste0("PC", seq_along(.x))) %>%
  mutate(feature = unname(feat_names[rownames(pca$rotation)]), .before = 1)
report_table(loadings, "S16_pca_loadings.csv", "Table S16.2. PCA loadings (correlations of features with components)")
scores <- as_tibble(pca$x[, 1:max(2, n_keep)]) %>% mutate(model_label = rownames(pca$x), .before = 1)

# Supplementary variables (RMET), correlated with component scores
sup <- scores %>% left_join(rmet_model %>% filter(condition == "Pooled") %>% select(model_label, rmet_acc = accuracy),
                            by = "model_label") %>%
  left_join(rmet_instab_model %>% filter(condition == "Pooled") %>% select(model_label, rmet_inst = pct_changed),
            by = "model_label")
sup_tab <- bind_rows(
  cor_test_full(sup$PC1, sup$rmet_acc) %>% mutate(pair = "PC1 x RMET accuracy"),
  cor_test_full(sup$PC2, sup$rmet_acc) %>% mutate(pair = "PC2 x RMET accuracy"),
  cor_test_full(sup$PC1, sup$rmet_inst) %>% mutate(pair = "PC1 x RMET instability"),
  cor_test_full(sup$PC2, sup$rmet_inst) %>% mutate(pair = "PC2 x RMET instability")) %>% relocate(pair)
report_table(sup_tab, "S16_pca_supplementary_rmet.csv",
             "Table S16.3. RMET as supplementary variables (Spearman with component scores)")

# Clustering --------------------------------------------------------------------------------
D_prof <- stats::dist(Xs)
hc_prof <- stats::hclust(D_prof, method = "ward.D2")
sil_prof <- map_dfr(2:6, function(k) {
  cl <- stats::cutree(hc_prof, k)
  tibble(k = k, avg_silhouette = mean(cluster::silhouette(cl, D_prof)[, "sil_width"]))
})
k_prof <- sil_prof$k[which.max(sil_prof$avg_silhouette)]
cl_prof <- stats::cutree(hc_prof, k_prof)
pam_prof <- cluster::pam(D_prof, k_prof, diss = TRUE)$clustering
set.seed(SEED + 17)
jac <- matrix(NA_real_, 500, k_prof)
for (b in seq_len(500)) {
  idx <- sample.int(nrow(Xs), replace = TRUE)
  cb <- stats::cutree(stats::hclust(stats::dist(Xs[idx, ]), method = "ward.D2"), k_prof)
  u <- !duplicated(idx); orig <- idx[u]; cbu <- cb[u]
  for (c in seq_len(k_prof)) {
    A <- orig[cl_prof[orig] == c]
    if (!length(A)) next
    jac[b, c] <- max(vapply(unique(cbu), function(cc) {
      Bm <- orig[cbu == cc]; length(intersect(A, Bm)) / length(union(A, Bm)) }, numeric(1)))
  }
}
clus_tab <- tibble(cluster = seq_len(k_prof), n_models = as.vector(table(cl_prof)),
                   bootstrap_jaccard = colMeans(jac, na.rm = TRUE))
report_table(sil_prof, "S16_cluster_silhouette.csv", "Table S16.4. Average silhouette by number of clusters")
ari_pam <- adj_rand(cl_prof, pam_prof)
report_table(clus_tab, "S16_cluster_stability.csv",
             paste0("Table S16.5. Cluster stability (k = ", k_prof, "); ARI with PAM = ",
                    fmt_n(ari_pam, 3)))
if (ari_pam < 0.5 || min(clus_tab$bootstrap_jaccard, na.rm = TRUE) < 0.6 ||
    max(sil_prof$avg_silhouette) < 0.5) {
  note("The cluster solution is NOT robust (silhouette < .50, a cluster with ",
       "Jaccard < .60, or low agreement with PAM). Models vary along continuous ",
       "dimensions (the principal components) rather than falling into discrete ",
       "types; report the PCA and treat clusters as descriptive only.")
}
prof_clusters <- tibble(model_label = names(cl_prof), cluster = cl_prof) %>% add_meta() %>%
  left_join(prof, by = "model_label")
report_table(prof_clusters %>% group_by(cluster) %>%
               summarise(n = n(), models = paste(display, collapse = ", "),
                         across(all_of(names(feat_names)), ~ mean(.x)), .groups = "drop"),
             "S16_cluster_profiles.csv", "Table S16.6. Cluster membership and mean profile")
tab_pc <- table(prof_clusters$cluster, prof_clusters$developer_group)
ft_pc <- stats::fisher.test(tab_pc, simulate.p.value = TRUE, B = B_PERM)
say("Clusters x developer group: Fisher exact (Monte Carlo) ", fp(ft_pc$p.value),
    ", Cramer's V = ", fmt_n(cramers_v(tab_pc)))
register("S16 Model profiles", "Profile clusters vs developer group", "Fisher exact test (Monte Carlo)",
         estimate = cramers_v(tab_pc), p = ft_pc$p.value, n = paste(nrow(prof_clusters), "models"),
         effect = "Cramer's V")
FIG$pca_scores   <- scores %>% left_join(prof_clusters %>% select(model_label, cluster), by = "model_label") %>% add_meta()
FIG$pca_loadings <- loadings
FIG$pca_var      <- pca_var
FIG$profile_hclust <- hc_prof


# ==============================================================================
# SECTION 17. CROSS-INSTRUMENT CORRELATIONS (MODELS AS UNITS)
# ==============================================================================

banner(17, "Cross-instrument correlations across models")
note("Human research finds SDO and authoritarianism positively correlated and ",
     "SDO negatively related to empathy. Whether the same nomological network ",
     "appears ACROSS MODELS is an open question; between-model correlations are ",
     "not between-person correlations and should be read as descriptive of this ",
     "model sample.")
cv <- prof %>% select(model_label, `mean_score_SDO7s`, `mean_score_SDO6-EN`, `mean_score_VSA`, lang_shift,
                      `pct_changed_SDO6-EN`, `pct_changed_VSA`, pct_at_floor) %>%
  full_join(rmet_model %>% filter(condition == "Pooled") %>% select(model_label, rmet_acc = accuracy), by = "model_label") %>%
  full_join(rmet_instab_model %>% filter(condition == "Pooled") %>% select(model_label, rmet_inst = pct_changed), by = "model_label")
cv_names <- c(`mean_score_SDO7s` = "SDO7(s) score", `mean_score_SDO6-EN` = "SDO6 score (EN)",
              `mean_score_VSA` = "VSA score", lang_shift = "Language shift",
              `pct_changed_SDO6-EN` = "Instability SDO6 (EN)", `pct_changed_VSA` = "Instability VSA",
              pct_at_floor = "Floor pinning", rmet_acc = "RMET accuracy", rmet_inst = "RMET instability")
vars <- names(cv_names)
cor_mat <- map_dfr(utils::combn(vars, 2, simplify = FALSE), function(v) {
  cor_test_full(cv[[v[1]]], cv[[v[2]]], B = 1000, B_p = 2000) %>%
    mutate(var1 = cv_names[v[1]], var2 = cv_names[v[2]], .before = 1)
}) %>% mutate(p_bh = p.adjust(p_perm, "BH"))
report_table(cor_mat %>% arrange(p_perm), "S17_cross_instrument_correlations.csv",
             "Table S17.1. Spearman correlations across models (pairwise n; bootstrap CI; permutation p; BH)")
for (i in seq_len(nrow(cor_mat))) {
  register("S17 Cross-instrument", paste(cor_mat$var1[i], "x", cor_mat$var2[i]), "Spearman correlation",
           estimate = cor_mat$r[i], ci_low = cor_mat$ci_low[i], ci_high = cor_mat$ci_high[i],
           p = cor_mat$p_perm[i], n = paste(cor_mat$n[i], "models"), effect = "rho")
}
FIG$cor_mat <- cor_mat


# ==============================================================================
# SECTION 18. SENSITIVITY POWER AND SAMPLE-SIZE PLANNING
# ==============================================================================

banner(18, "Sensitivity power and sample-size planning")
note("Post-hoc 'observed power' is not reported: it is a monotone function of the ",
     "p-value and adds no information (Hoenig & Heisey, 2001). Instead: (a) the ",
     "smallest effect each test could detect with 80% and 90% power, (b) the power ",
     "of the two equivalence tests, (c) sample sizes for the follow-up studies ",
     "flagged as needed, and (d) the number of repeats, from the D-study.")

sens <- bind_rows(
  tibble(test = "RMET definitions, paired over models", n = "21 models",
         d80 = pwr::pwr.t.test(n = 21, power = .80, type = "paired")$d,
         d90 = pwr::pwr.t.test(n = 21, power = .90, type = "paired")$d,
         raw_sd = sd(defs_model$diff_pp), raw_unit = "% points"),
  tibble(test = "SDO6 language, paired over models", n = "27 models",
         d80 = pwr::pwr.t.test(n = 27, power = .80, type = "paired")$d,
         d90 = pwr::pwr.t.test(n = 27, power = .90, type = "paired")$d,
         raw_sd = sd(lang_model$diff), raw_unit = "scale points"),
  tibble(test = "Correlation across SDO models", n = "27 models",
         d80 = pwr::pwr.r.test(n = 27, power = .80)$r, d90 = pwr::pwr.r.test(n = 27, power = .90)$r,
         raw_sd = NA, raw_unit = "r"),
  tibble(test = "Correlation across RMET models", n = "21 models",
         d80 = pwr::pwr.r.test(n = 21, power = .80)$r, d90 = pwr::pwr.r.test(n = 21, power = .90)$r,
         raw_sd = NA, raw_unit = "r"),
  tibble(test = "OpenAI vs Anthropic, RMET instability", n = "8 vs 11 models",
         d80 = pwr::pwr.t2n.test(n1 = 8, n2 = 11, power = .80)$d,
         d90 = pwr::pwr.t2n.test(n1 = 8, n2 = 11, power = .90)$d,
         raw_sd = sd(dev_inst$pct_changed), raw_unit = "% points"),
  tibble(test = "LLM models vs human RMET controls", n = "21 models vs 122 humans",
         d80 = pwr::pwr.t2n.test(n1 = 21, n2 = 122, power = .80)$d,
         d90 = pwr::pwr.t2n.test(n1 = 21, n2 = 122, power = .90)$d,
         raw_sd = 3.6, raw_unit = "items (human SD)")
) %>% mutate(min_detectable_raw_80 = if_else(raw_unit == "r", NA_real_, d80 * raw_sd))
report_table(sens, "S18_sensitivity.csv",
             "Table S18.1. Smallest standardised effect detectable (alpha = .05, two-sided)")

tost_power <- function(sesoi, se, df, alpha = .05) {
  tc <- stats::qt(1 - alpha, df)
  max(0, stats::pt(sesoi / se - tc, df) - stats::pt(-sesoi / se + tc, df))
}
tp <- tibble(
  equivalence_test = c("LLM vs human benchmark (+/-1.8 items)", "Definitions null (+/-5 pp)"),
  sesoi = c(SESOI_ITEMS, SESOI_PP),
  se = c(bench$se[1], ps_defs$sd_diff / sqrt(ps_defs$n)),
  df = c(bench$df[1], ps_defs$n - 1)) %>%
  mutate(power_if_true_diff_is_0 = pmap_dbl(list(sesoi, se, df), tost_power),
         models_needed_for_80pct = c(
           { f <- function(n) tost_power(SESOI_ITEMS, sqrt(sd(llm_scores)^2 / n + 3.6^2 / 122),
                                         n - 1) - 0.80
             tryCatch(ceiling(stats::uniroot(f, c(3, 2000))$root), error = function(e) NA_real_) },
           { f <- function(n) tost_power(SESOI_PP, ps_defs$sd_diff / sqrt(n), n - 1) - 0.80
             tryCatch(ceiling(stats::uniroot(f, c(3, 2000))$root), error = function(e) NA_real_) }))
report_table(tp, "S18_tost_power.csv", "Table S18.2. Power of the equivalence tests at the achieved design")

# Developer-origin comparison: how many Chinese-developed models are needed?
g_origin <- with(lang_studies, {
  a <- yi[origin == "China"]; b <- yi[origin == "US"]
  if (length(a) >= 2) (mean(a) - mean(b)) / sqrt(((length(a) - 1) * var(a) + (length(b) - 1) * var(b)) /
                                                  (length(a) + length(b) - 2)) else NA_real_
})
n_needed <- map_dfr(c(0.5, 0.8, 1.0, 1.2, 1.5), function(d) {
  n_fixed <- tryCatch(ceiling(pwr::pwr.t2n.test(n2 = sum(lang_studies$origin == "US"), d = d, power = .80)$n1),
                      error = function(e) NA_real_)
  n_equal <- ceiling(pwr::pwr.t.test(d = d, power = .80)$n)
  tibble(effect_size_d = d,
         china_models_needed_with_current_US_sample = n_fixed,
         per_group_if_balanced = n_equal)
})
report_table(n_needed, "S18_origin_sample_size.csv",
             paste0("Table S18.3. Models needed to detect a developer-origin difference in the language shift ",
                    "(80% power); observed g with 2 China-developed models = ", fmt_n(g_origin)))
note("The observed effect from two models is too imprecise to plan on, so a range ",
     "of effect sizes is shown. NA = not achievable with the current comparison group.")

prec <- model_instr %>% group_by(inst_lang) %>% summarise(between_model_sd_pp = sd(pct_changed), .groups = "drop") %>%
  bind_rows(tibble(inst_lang = "RMET", between_model_sd_pp = sd(dev_inst$pct_changed))) %>%
  mutate(models_for_pm5pp = ceiling((1.96 * between_model_sd_pp / 5)^2),
         models_for_pm10pp = ceiling((1.96 * between_model_sd_pp / 10)^2))
report_table(prec, "S18_models_for_precision.csv",
             "Table S18.4. Models needed to estimate average instability within +/-5 or +/-10 percentage points")
say("Repeats needed (D-study, Section 8): see S08_d_study_runs_needed.csv. A single administration ",
    "is enough for scale-score claims on most instruments; claims about individual item ",
    "responses need several repeats.")
FIG$sensitivity <- sens


# ==============================================================================
# SECTION 19. TEST REGISTRY AND MULTIPLICITY CONTROL
# ==============================================================================

banner(19, "Test registry and multiplicity control")
note("Every hypothesis test above is collected here. Benjamini-Hochberg adjustment ",
     "is applied within each family (a family is one research question; per-item ",
     "and per-model families are kept separate so they do not dilute the main ",
     "tests). Holm-adjusted p-values are also given for readers who prefer ",
     "family-wise control.")
registry <- bind_rows(REGISTRY) %>%
  group_by(family) %>%
  mutate(p_bh = if_else(is.na(p), NA_real_, p.adjust(p, "BH")),
         p_holm = if_else(is.na(p), NA_real_, p.adjust(p, "holm")),
         k_in_family = sum(!is.na(p))) %>% ungroup() %>%
  mutate(id = sprintf("T%03d", row_number()), .before = 1)
readr::write_csv(registry, file.path(DIR_TABLES, "S19_test_registry.csv"), na = "")
TABLE_INDEX[["S19_test_registry.csv"]] <- "Table S19.1. Registry of every hypothesis test"
reg_sum <- registry %>% group_by(family) %>%
  summarise(tests = sum(!is.na(p)), sig_raw = sum(p < ALPHA, na.rm = TRUE),
            sig_bh = sum(p_bh < ALPHA, na.rm = TRUE), sig_holm = sum(p_holm < ALPHA, na.rm = TRUE),
            .groups = "drop")
report_table(reg_sum, "S19_registry_summary.csv", "Table S19.2. Tests per family and number significant")
subhead("Main-family tests (per-item, per-model and correlation-matrix families omitted)")
print(as.data.frame(registry %>%
  filter(!str_detect(family, "per item|per model|Cross-instrument"), !is.na(p)) %>%
  transmute(id, what = str_trunc(paste0(analysis, ": ", test), 95),
            estimate = signif(estimate, 3), p = signif(p, 3), p_bh = signif(p_bh, 3))),
  row.names = FALSE, right = FALSE)


# ==============================================================================
# SAVE OBJECTS FOR THE FIGURE SCRIPT, MANIFEST, SESSION INFO
# ==============================================================================

FIG$registry  <- registry
FIG$constants <- list(N_RUNS = N_RUNS, RMET_ITEMS = RMET_ITEMS, RMET_CHANCE = RMET_CHANCE,
                      SESOI_ITEMS = SESOI_ITEMS, SESOI_PP = SESOI_PP, generated = Sys.time())
saveRDS(FIG, file.path(DIR_OUT, "analysis_results.rds"))

manifest <- c(
  "# MS_AAI_ToM - statistical analysis output",
  paste0("Generated ", format(Sys.time(), "%Y-%m-%d %H:%M"), " by Analysis_script/02_statistical_analysis.R"),
  "", "- `stats_log.txt`: full log with every test statistic, section by section",
  "- `Stats_tables/`: every table in the log as CSV (prefix = section number)",
  "- `Stats_tables/S19_test_registry.csv`: all hypothesis tests with BH and Holm adjustment",
  "- `analysis_results.rds`: objects read by 03_figures.R", "", "## Tables", "",
  "| File | Contents |", "|---|---|",
  paste0("| ", names(TABLE_INDEX), " | ", gsub("\\|", "/", unlist(TABLE_INDEX)), " |"))
writeLines(manifest, file.path(DIR_OUT, "Stats_README.md"))

cat("\n\n", RULE_MAJOR, "\nSESSION INFORMATION\n", RULE_MAJOR, "\n", sep = "")
print(utils::sessionInfo())
cat("\n", RULE_MAJOR, "\nAnalysis complete.\n  Log     : ", LOG_FILE,
    "\n  Tables  : ", DIR_TABLES, "\n  Results : ", file.path(DIR_OUT, "analysis_results.rds"),
    "\n", RULE_MAJOR, "\n", sep = "")
close_log()
