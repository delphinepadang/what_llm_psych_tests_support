# ============================================================
# RMET Pilot Analysis — Theory of Mind across model versions
# Project: Evolution of ToM in LLMs (lead analysis script)
# ------------------------------------------------------------
# WHAT THIS DOES
#   1. Loads the RMET pilot data
#   2. Cleans it and builds family + version-order columns
#   3. Computes accuracy per model and per condition
#   4. Plots the "evolution" picture (accuracy by version, by family)
#   5. Fits a simple mixed-effects model (accuracy ~ version, by item)
#
# HOW TO RUN
#   - Put RMET_Results.csv in the same folder as this script
#   - Open in RStudio and run top-to-bottom, OR: Rscript rmet_evolution.R
#   - First run only: install the packages in the block below
# ============================================================

# ---- 0. Packages -------------------------------------------
# Run ONCE if you don't have these:
# install.packages(c("tidyverse", "lme4"))
library(tidyverse)   # data wrangling + ggplot
library(lme4)        # mixed-effects models (for the stats step)

# ---- 1. Load -----------------------------------------------
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

rmet <- read_csv(in_path("RMET_Results.csv"))

# Quick look — always sanity-check what you loaded
glimpse(rmet)

# ---- 2. Clean + label --------------------------------------
# Strip the "databricks-" prefix so model names are readable
rmet <- rmet %>%
  mutate(m = str_remove(model, "databricks-"))

# Assign each model to a FAMILY (the ecosystem/provider grouping)
rmet <- rmet %>%
  mutate(family = case_when(
    str_starts(m, "gpt-oss") ~ "GPT-OSS",
    str_starts(m, "gpt")     ~ "GPT",
    str_detect(m, "claude")  ~ "Claude",
    str_detect(m, "qwen")    ~ "Qwen",
    str_detect(m, "gemma")   ~ "Gemma",
    str_detect(m, "llama")   ~ "Llama",
    TRUE ~ "Other"
  ))

# For Claude, also tag the sub-line (opus / sonnet / haiku),
# because version order only makes sense WITHIN a lineage.
rmet <- rmet %>%
  mutate(subfam = str_extract(m, "opus|sonnet|haiku"))

# VERSION ORDER (release chronology, verified against release dates).
# Generation is only comparable *within* a family — never assume
# "gpt-5" and "claude-opus-4-5" are the same "generation number".
version_order <- c(
  # GPT lineage
  "gpt-4.1" = 1, "gpt-4o" = 2, "gpt-5" = 3, "gpt-5.1" = 4,
  "gpt-5.2" = 5, "gpt-5.4" = 6, "gpt-5.5" = 7,
  # Claude opus lineage
  "claude-opus-4-1" = 1, "claude-opus-4-5" = 2, "claude-opus-4-6" = 3,
  "claude-opus-4-7" = 4, "claude-opus-4-8" = 5,
  # Claude sonnet lineage
  "claude-sonnet-4" = 1, "claude-sonnet-4-5" = 2, "claude-sonnet-4-6" = 3,
  # Claude haiku lineage
  "claude-haiku-4-5" = 1
)
rmet <- rmet %>%
  mutate(vorder = version_order[m])

# NOTE ON THE PARSE ERROR:
# One gpt-4o row (rmet-20) could not be parsed into an option.
# It is already marked is_correct = FALSE, which is the correct
# treatment (a non-answer is not a correct answer). We keep it.
# Count how many invalid rows exist, for transparency:
rmet %>% summarise(n_parse_errors = sum(!is.na(parse_error)))

# ---- 3. Accuracy tables ------------------------------------
# Overall accuracy
rmet %>% summarise(accuracy = mean(is_correct), n = n())

# Accuracy per model (this is the core descriptive result)
# We attach a 95% CONFIDENCE INTERVAL to every accuracy, because
# with only ~72 observations per model the point estimates are
# uncertain -- the CIs make that uncertainty visible and are the
# honest way to show the version "trend" is suggestive, not firm.
#
# We use the Wilson interval for a binomial proportion: it is the
# standard, well-behaved CI for a "k correct out of n" accuracy
# (better than the naive normal approximation, especially near 0/1).
wilson_ci <- function(k, n, conf = 0.95) {
  z  <- qnorm(1 - (1 - conf) / 2)
  p  <- k / n
  den <- 1 + z^2 / n
  centre <- (p + z^2 / (2 * n)) / den
  half   <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / den
  tibble(lower = centre - half, upper = centre + half)
}

acc_by_model <- rmet %>%
  group_by(family, subfam, m, vorder) %>%
  summarise(k = sum(is_correct), n = n(), .groups = "drop") %>%
  mutate(accuracy = k / n) %>%
  bind_cols(wilson_ci(.$k, .$n)) %>%   # adds lower / upper columns
  arrange(family, subfam, vorder)

# Print with CIs rounded for readability
acc_by_model %>%
  mutate(across(c(accuracy, lower, upper), ~ round(.x, 3))) %>%
  print(n = 100)
# READING THIS: e.g. gpt-5.2 ~ 0.625 [0.51, 0.73]. The intervals are
# WIDE (~20 points) and overlap heavily across versions -- which is
# exactly why the version trend should be called suggestive, not proven.

# Effect of the DEFINITIONS condition
# (In the pilot this was essentially nil — expect ~0.715 vs ~0.710)
rmet %>%
  group_by(definitions_shown) %>%
  summarise(accuracy = mean(is_correct), n = n(), .groups = "drop")

# ---- 4. The evolution plot ---------------------------------
# We plot the GPT and Claude lineages (the ones with multiple
# versions). Small/other models have no version trajectory.
# A horizontal line marks chance (1/4, since each item has 4 options).

plot_df <- acc_by_model %>%
  filter(family %in% c("GPT", "Claude"), !is.na(vorder)) %>%
  mutate(line = if_else(family == "Claude",
                        paste0("Claude-", subfam), "GPT"))

p <- ggplot(plot_df, aes(x = vorder, y = accuracy,
                         colour = line, group = line)) +
  geom_hline(yintercept = 0.25, linetype = "dashed", colour = "grey60") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15,
                alpha = 0.5) +    # 95% Wilson CIs -- show the uncertainty
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  geom_text(aes(label = m), vjust = -0.9, size = 2.6, show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  labs(
    title = "RMET accuracy across model versions (pilot)",
    subtitle = "Within-family release order on the x-axis; dashed line = chance (25%)",
    x = "Version order (older -> newer, within family)",
    y = "Accuracy",
    colour = "Model line"
  ) +
  theme_minimal(base_size = 12)

ggsave(out_path("rmet_evolution_plot.png"), p, width = 9, height = 5.5, dpi = 150)
print(p)

# ---- 5. A first inferential model --------------------------
# Question: is there a systematic accuracy change across versions,
# once we account for some items being harder than others?
#
# We model each response (correct/incorrect) with a logistic
# mixed-effects model. Random intercept for ITEM soaks up the
# fact that some eye-photos are just harder. We test version
# WITHIN the GPT line here as a clean example (one family,
# one clear ordering). You can repeat for Claude opus.

gpt <- rmet %>% filter(family == "GPT")

m_gpt <- glmer(
  is_correct ~ vorder + (1 | question_id),
  data = gpt,
  family = binomial
)
summary(m_gpt)

# INTERPRETING THE OUTPUT:
#   - The 'vorder' coefficient is the change in log-odds of a
#     correct answer per step up the version ladder.
#   - If it's near zero / not significant (likely here), that is
#     itself the finding: GPT's RMET accuracy did NOT systematically
#     improve across versions in this pilot.
#   - Do NOT over-read this: only 1 repetition per item means a lot
#     of noise. This is a pilot signal, not a final result.

# ---- 6. Save the tidy results table ------------------------
write_csv(acc_by_model, out_path("rmet_accuracy_by_model.csv"))

# ============================================================
# WHAT TO EXPECT (verified values from the pilot data):
#   Overall accuracy ............ ~0.712
#   Definitions effect .......... ~0.715 (no) vs ~0.710 (yes)  -> negligible
#   GPT line: 0.819, 0.833, 0.750, 0.736, 0.625, 0.806, 0.750
#             (4.1, 4o, 5, 5.1, 5.2, 5.4, 5.5) -> NOT monotonic
#   Claude opus: 0.778, 0.722, 0.653, 0.708, 0.833
#             (4-1 .. 4-8) -> U-shaped
#   Gemma 0.458, Llama 0.542 -> clearly below the frontier models
#
# HEADLINE FOR YOUR MEETING:
#   High overall accuracy; frontier models clearly beat small ones;
#   but within-family "evolution" is flat / wobbly, not a clean climb.
#   With 1 repetition we can't separate signal from noise -> this is
#   the argument for the full study's 5-10 repetitions.
# ============================================================
