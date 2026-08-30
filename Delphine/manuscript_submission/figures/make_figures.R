###############################################################################
# make_figures.R
#
# Generates Figures 1-6 for:
#   "Uniform Scores, Divergent Subdimensions: Why Aggregate Political-Psychology
#    Instruments Fail to Discriminate Among Large Language Models"
#
# Everything is rebuilt from the RAW response files. No derived summary CSV is
# read. Reverse-keying, subscale membership and scoring are defined explicitly
# below so a reader can check them against the published scoring keys.
#
# Scoring keys:
#   SDO7(s), Ho et al. (2015). 8 items, 7-point Favor/Oppose, midpoint 4.
#     Dominance          q1 q2 q3 q4   (reverse: q3 q4)
#     Anti-egalitarianism q5 q6 q7 q8   (reverse: q7 q8)
#     Reverse transform: 8 - x
#
#   VSA, Bizumic & Duckitt (2018). 6 items, 9-point (-4..+4), midpoint 0.
#     Submission      q1 q2   (reverse: q1)
#     Traditionalism  q3 q4   (reverse: q4)
#     Aggression      q5 q6   (reverse: q5)
#     Reverse transform: -x
#
# Missing-data rule: model scores are means over ALL VALID observations
# (observation-level), not item-balanced. 4/1680 responses failed to parse.
# See sensitivity check printed at the end.
#
# Usage:  Rscript make_figures.R [input_dir] [output_dir]
# Default input_dir = "." ; default output_dir = "output_manuscript"
###############################################################################

suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(tidyr); library(scales)
})

# The raw exports contain a UTF-8 BOM, curly apostrophes in item text, and
# embedded newlines inside quoted raw_response fields. base::read.csv silently
# truncates on these (it read 41 of 1,680 rows). readr handles all three.
read_raw <- function(path) {
  if (requireNamespace("readr", quietly = TRUE)) {
    as.data.frame(readr::read_csv(
      path, show_col_types = FALSE, progress = FALSE, name_repair = "minimal",
      col_types = readr::cols(parse_error = readr::col_character(),
                              raw_response = readr::col_character(),
                              .default = readr::col_guess())))
  } else {
    warning("readr not installed - falling back to read.csv; VERIFY ROW COUNTS")
    read.csv(path, encoding = "UTF-8", stringsAsFactors = FALSE)
  }
}

args       <- commandArgs(trailingOnly = TRUE)
input_dir  <- if (length(args) >= 1) args[1] else "."
output_dir <- if (length(args) >= 2) args[2] else "output_manuscript"

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
cat("Input :", normalizePath(input_dir), "\n")
cat("Output:", normalizePath(output_dir), "\n\n")

# ---------------------------------------------------------------- constants --
SDO_REV <- c("sdo-q3", "sdo-q4", "sdo-q7", "sdo-q8")
VSA_REV <- c("vsa-q1", "vsa-q4", "vsa-q5")

SDO_SUB <- c(`sdo-q1` = "Dominance",           `sdo-q2` = "Dominance",
             `sdo-q3` = "Dominance",           `sdo-q4` = "Dominance",
             `sdo-q5` = "Anti-egalitarianism", `sdo-q6` = "Anti-egalitarianism",
             `sdo-q7` = "Anti-egalitarianism", `sdo-q8` = "Anti-egalitarianism")

VSA_SUB <- c(`vsa-q1` = "Submission",     `vsa-q2` = "Submission",
             `vsa-q3` = "Traditionalism", `vsa-q4` = "Traditionalism",
             `vsa-q5` = "Aggression",     `vsa-q6` = "Aggression")

SDO_MID <- 4; VSA_MID <- 0
HUMAN_RMET <- 26.2 / 36      # Baron-Cohen et al. (2001) adult mean
CHANCE_RMET <- 0.25

# Colourblind-safe. Blue = below midpoint, red = at/above, grey = neutral.
COL_BELOW <- "#2A78D6"; COL_ABOVE <- "#E34948"; COL_NEUTRAL <- "#898781"
COL_FWD   <- "#EB6834"; COL_REV   <- "#1BAF7A"

theme_ms <- function(base = 10) {
  theme_minimal(base_size = base) +
    theme(
      panel.grid.minor  = element_blank(),
      panel.grid.major.y = element_line(linewidth = 0.25, colour = "grey88"),
      axis.text         = element_text(colour = "grey25"),
      axis.title        = element_text(colour = "grey15"),
      strip.text        = element_text(face = "bold", size = base),
      plot.title        = element_text(face = "bold", size = base + 2),
      plot.subtitle     = element_text(colour = "grey35", size = base - 0.5),
      legend.position   = "top",
      legend.title       = element_blank(),
      plot.margin       = margin(8, 12, 8, 8)
    )
}

save_fig <- function(p, name, w, h) {
  for (ext in c("png", "pdf")) {
    ggsave(file.path(output_dir, paste0(name, ".", ext)), p,
           width = w, height = h, dpi = 600,
           device = if (ext == "pdf") cairo_pdf else NULL)
  }
  cat("  wrote", name, "\n")
}

short_name <- function(x) sub("^databricks-", "", x)

# --------------------------------------------------------------------- load --
sdo_path  <- file.path(input_dir, "SDO_VSA_Results.csv")
rmet_path <- file.path(input_dir, "RMET_Results.csv")
stopifnot(file.exists(sdo_path), file.exists(rmet_path))

sd_raw <- read_raw(sdo_path)
rm_raw <- read_raw(rmet_path)

# strip any BOM left on the first column name
names(sd_raw) <- sub("^\ufeff", "", names(sd_raw))
names(rm_raw) <- sub("^\ufeff", "", names(rm_raw))

# fail loudly rather than silently plotting a truncated dataset
stopifnot(
  "SDO/VSA row count != 1680 - check the CSV reader" = nrow(sd_raw) == 1680,
  "RMET row count != 1296 - check the CSV reader"    = nrow(rm_raw) == 1296
)

sd <- sd_raw %>%
  mutate(
    m      = short_name(model),
    scored = case_when(
      question_id %in% SDO_REV ~ 8 - value,
      question_id %in% VSA_REV ~ -value,
      TRUE                     ~ value
    ),
    keying   = ifelse(question_id %in% c(SDO_REV, VSA_REV), "Reverse-worded", "Forward-worded"),
    subscale = ifelse(category == "sdo", SDO_SUB[question_id], VSA_SUB[question_id])
  )

sd_ok <- sd %>% filter(!is.na(scored))

cat("SDO/VSA:", nrow(sd_raw), "responses,", n_distinct(sd$m), "models,",
    sum(is.na(sd$scored)), "unparsed\n")
cat("RMET   :", nrow(rm_raw), "responses,", n_distinct(short_name(rm_raw$model)), "models\n\n")

# ------------------------------------------------------- derived data frames --
totals <- sd_ok %>%
  group_by(m, category) %>%
  summarise(score = mean(scored), .groups = "drop") %>%
  mutate(instrument = ifelse(category == "sdo",
                             "Social dominance (SDO7s)", "Authoritarianism (VSA)"),
         midpoint   = ifelse(category == "sdo", SDO_MID, VSA_MID))

subs <- sd_ok %>%
  group_by(m, category, subscale) %>%
  summarise(score = mean(scored), .groups = "drop") %>%
  mutate(midpoint = ifelse(category == "sdo", SDO_MID, VSA_MID),
         side = case_when(score >  midpoint ~ "Above midpoint",
                          score == midpoint ~ "Exactly at midpoint",
                          TRUE              ~ "Below midpoint"))

disp <- sd_ok %>%
  group_by(m, category, question_id) %>%
  summarise(sd_item = sd(scored), .groups = "drop") %>%
  group_by(m, category) %>%
  summarise(dispersion = round(mean(sd_item, na.rm = TRUE), 12), .groups = "drop")
# NOTE: rounding at 12 dp is deliberate — prevents summation-order artifacts
# producing mathematically identical values stored as distinct floats.

rmet <- rm_raw %>%
  mutate(m = short_name(model), ok = as.logical(is_correct))

# ==============================================================================
# FIGURE 1 - Study design
# ==============================================================================
cat("Figure 1: design schematic\n")

items <- sd %>%
  distinct(category, question_id, subscale, keying) %>%
  arrange(category, question_id) %>%
  mutate(
    instrument = ifelse(category == "sdo",
                        "SDO7(s)  ·  8 items  ·  1-7, midpoint 4",
                        "VSA  ·  6 items  ·  -4 to +4, midpoint 0"),
    idx = as.integer(sub(".*q", "", question_id))
  )

f1 <- ggplot(items, aes(x = idx, y = subscale, fill = keying)) +
  geom_tile(colour = "white", linewidth = 1.4, height = 0.72) +
  geom_text(aes(label = paste0("q", idx)), colour = "white",
            fontface = "bold", size = 3.1) +
  facet_wrap(~instrument, scales = "free", ncol = 1) +
  scale_fill_manual(values = c("Forward-worded" = COL_FWD,
                               "Reverse-worded" = COL_REV)) +
  scale_x_continuous(breaks = NULL) +
  labs(x = NULL, y = NULL,
       title = "Instrument structure",
       subtitle = paste0("24 instruction-tuned models  x  14 items  x  5 independent ",
                         "administrations  =  1,680 responses\n",
                         "Subscale membership and keying direction both structure the analysis")) +
  theme_ms() +
  theme(panel.grid.major.y = element_blank(),
        axis.text.x = element_blank())

save_fig(f1, "Figure1_design", 8.0, 5.2)

# ==============================================================================
# FIGURE 2 - Aggregate scores by model
# ==============================================================================
cat("Figure 2: aggregate scores\n")

ord2 <- totals %>% filter(category == "sdo") %>% arrange(score) %>% pull(m)

f2dat <- totals %>%
  mutate(m = factor(m, levels = ord2),
         instrument = factor(instrument,
                             levels = c("Social dominance (SDO7s)",
                                        "Authoritarianism (VSA)")))

# usable scale range per panel, drawn as a light band
bands <- data.frame(
  instrument = factor(c("Social dominance (SDO7s)", "Authoritarianism (VSA)"),
                      levels = levels(f2dat$instrument)),
  lo = c(1, -4), hi = c(7, 4), midpoint = c(SDO_MID, VSA_MID)
)

f2 <- ggplot(f2dat, aes(x = score, y = m)) +
  geom_rect(data = bands, inherit.aes = FALSE,
            aes(xmin = lo, xmax = hi, ymin = -Inf, ymax = Inf),
            fill = "grey95") +
  geom_vline(data = bands, aes(xintercept = midpoint),
             linetype = "dashed", colour = "grey35", linewidth = 0.45) +
  geom_segment(aes(x = midpoint, xend = score, yend = m),
               colour = "grey72", linewidth = 0.35) +
  geom_point(aes(colour = score < midpoint), size = 2.1) +
  facet_wrap(~instrument, scales = "free_x") +
  scale_colour_manual(values = c(`TRUE` = COL_BELOW, `FALSE` = COL_ABOVE),
                      labels = c(`TRUE` = "Below midpoint", `FALSE` = "At or above midpoint")) +
  labs(x = "Mean score (higher = more dominance / more authoritarian)", y = NULL,
       title = "Aggregate scores place every model at the egalitarian pole",
       subtitle = paste0("Grey band = full usable scale range. Dashed line = neutral midpoint.\n",
                         "24/24 below the dominance midpoint; 21/24 below the authoritarianism midpoint")) +
  theme_ms() +
  theme(axis.text.y = element_text(size = 7.2))

save_fig(f2, "Figure2_aggregate_scores", 9.0, 6.4)

# ==============================================================================
# FIGURE 3 - Subscale decomposition  [KEY FIGURE]
# ==============================================================================
cat("Figure 3: subscale decomposition  [key figure]\n")

vsa_ord <- subs %>% filter(subscale == "Aggression") %>% arrange(score) %>% pull(m)

f3_vsa <- subs %>%
  filter(category == "vsa") %>%
  mutate(m = factor(m, levels = vsa_ord),
         subscale = factor(subscale,
                           levels = c("Submission", "Traditionalism", "Aggression")))

# Report the three-way split explicitly. Collapsing "exactly at midpoint" into
# "at or above" inflates the aggression count from 10 to 18 and overstates the
# claim - the paper's wording depends on keeping these separate.
lab_vsa <- f3_vsa %>%
  group_by(subscale) %>%
  summarise(above = sum(score > 0), at = sum(score == 0),
            below = sum(score < 0), .groups = "drop") %>%
  mutate(lab = sprintf("%d above  \u00b7  %d at  \u00b7  %d below", above, at, below))

f3a <- ggplot(f3_vsa, aes(x = score, y = m)) +
  geom_vline(xintercept = VSA_MID, linetype = "dashed",
             colour = "grey30", linewidth = 0.5) +
  geom_segment(aes(x = VSA_MID, xend = score, yend = m),
               colour = "grey78", linewidth = 0.35) +
  geom_point(aes(colour = side), size = 2.1) +
  geom_text(data = lab_vsa, inherit.aes = FALSE,
            aes(x = -4.3, y = -Inf, label = lab),
            hjust = 0, vjust = -0.8, size = 2.9, colour = "grey20",
            fontface = "bold") +
  facet_wrap(~subscale, nrow = 1) +
  expand_limits(y = 0) +
  scale_colour_manual(values = c("Below midpoint" = COL_BELOW,
                                 "Exactly at midpoint" = COL_NEUTRAL,
                                 "Above midpoint" = COL_ABOVE)) +
  scale_x_continuous(limits = c(-4.3, 2.4)) +
  labs(x = "Mean subscale score  (-4 to +4, higher = more authoritarian)", y = NULL,
       title = "The aggregate authoritarianism score conceals a reversal",
       subtitle = paste0("No model exceeds the midpoint on submission or traditionalism. ",
                         "Ten of 24 do on authoritarian aggression.")) +
  theme_ms() +
  theme(axis.text.y = element_text(size = 6.6))

save_fig(f3a, "Figure3_subscales_VSA", 10.0, 6.6)

# companion panel: SDO subscales
sdo_ord <- subs %>% filter(subscale == "Anti-egalitarianism") %>% arrange(score) %>% pull(m)
f3_sdo <- subs %>%
  filter(category == "sdo") %>%
  mutate(m = factor(m, levels = sdo_ord))

f3b <- ggplot(f3_sdo, aes(x = score, y = m)) +
  geom_vline(xintercept = SDO_MID, linetype = "dashed",
             colour = "grey30", linewidth = 0.5) +
  geom_segment(aes(x = SDO_MID, xend = score, yend = m),
               colour = "grey78", linewidth = 0.35) +
  geom_point(aes(colour = side), size = 2.1) +
  facet_wrap(~subscale, nrow = 1) +
  scale_colour_manual(values = c("Below midpoint" = COL_BELOW,
                                 "Exactly at midpoint" = COL_NEUTRAL,
                                 "Above midpoint" = COL_ABOVE)) +
  scale_x_continuous(limits = c(0.8, 7.2)) +
  labs(x = "Mean subscale score  (1-7, higher = more dominance)", y = NULL,
       title = "Social dominance subscales",
       subtitle = "Both subscales sit below the midpoint, but anti-egalitarianism is consistently higher") +
  theme_ms() +
  theme(axis.text.y = element_text(size = 6.6))

save_fig(f3b, "Figure3b_subscales_SDO", 7.2, 6.6)

# ==============================================================================
# FIGURE 4 - Placement vs stability
# ==============================================================================
cat("Figure 4: placement vs stability\n")

f4dat <- totals %>%
  select(m, category, score, midpoint) %>%
  left_join(disp, by = c("m", "category")) %>%
  mutate(instrument = ifelse(category == "sdo",
                             "Social dominance (SDO7s)", "Authoritarianism (VSA)"),
         instrument = factor(instrument,
                             levels = c("Social dominance (SDO7s)", "Authoritarianism (VSA)")),
         deterministic = dispersion == 0)

f4 <- ggplot(f4dat, aes(x = score, y = dispersion)) +
  geom_vline(aes(xintercept = midpoint), linetype = "dashed",
             colour = "grey55", linewidth = 0.4) +
  geom_hline(yintercept = 0, colour = "grey80", linewidth = 0.3) +
  geom_point(aes(colour = deterministic), size = 2.3, alpha = 0.85) +
  facet_wrap(~instrument, scales = "free") +
  scale_colour_manual(values = c(`TRUE` = COL_NEUTRAL, `FALSE` = COL_ABOVE),
                      labels = c(`TRUE` = "Identical across all 5 administrations",
                                 `FALSE` = "Varies across administrations")) +
  labs(x = "Mean score", y = "Within-model dispersion (mean SD across repeats)",
       title = "Score placement and response stability are independent",
       subtitle = paste0("Models at comparable score positions differ by more than an order of ",
                         "magnitude in variability.\n8/24 models never varied on dominance; ",
                         "6/24 on authoritarianism")) +
  theme_ms()

save_fig(f4, "Figure4_placement_vs_stability", 9.0, 4.8)

# ==============================================================================
# FIGURE 5 - Item-level response surface
# ==============================================================================
cat("Figure 5: item-level heatmap\n")

f5dat <- sd_ok %>%
  group_by(m, category, question_id, keying) %>%
  summarise(mean_scored = mean(scored), .groups = "drop") %>%
  group_by(category) %>%
  mutate(
    # rescale each instrument to 0-1 of its own usable range so panels compare
    z = if (first(category) == "sdo") (mean_scored - 1) / 6 else (mean_scored + 4) / 8
  ) %>%
  ungroup() %>%
  mutate(m = factor(m, levels = ord2),
         item_lab = paste0(sub(".*q", "q", question_id),
                           ifelse(keying == "Reverse-worded", " (R)", "")),
         instrument = ifelse(category == "sdo", "SDO7(s)", "VSA"))

f5 <- ggplot(f5dat, aes(x = item_lab, y = m, fill = z)) +
  geom_tile(colour = "white", linewidth = 0.4) +
  facet_grid(~instrument, scales = "free_x", space = "free_x") +
  scale_fill_gradient2(low = COL_BELOW, mid = "grey93", high = COL_ABOVE,
                       midpoint = 0.5, limits = c(0, 1),
                       labels = c("0.0\negalitarian pole", "0.25", "0.50\nmidpoint",
                                  "0.75", "1.00\ndominance pole"),
                       breaks = c(0, .25, .5, .75, 1)) +
  labs(x = "Item   ((R) = reverse-worded)", y = NULL,
       title = "Discriminative variance concentrates in a minority of items",
       subtitle = paste0("Each instrument rescaled to 0-1 of its own usable range. ",
                         "Reverse-worded dominance items\nsit uniformly at floor; ",
                         "cross-model variance survives mainly in forward-worded items")) +
  theme_ms() +
  theme(axis.text.y = element_text(size = 6.6),
        panel.grid.major.y = element_blank(),
        legend.key.width = unit(1.5, "cm"),
        legend.text = element_text(size = 6.5))

save_fig(f5, "Figure5_item_heatmap", 8.6, 6.6)

# ==============================================================================
# FIGURE 6 - RMET accuracy, single administration
# ==============================================================================
cat("Figure 6: eyes-test accuracy\n")

wilson <- function(k, n, conf = 0.95) {
  z  <- qnorm(1 - (1 - conf) / 2); p <- k / n
  d  <- 1 + z^2 / n
  ctr <- (p + z^2 / (2 * n)) / d
  hw <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / d
  c(lo = ctr - hw, hi = ctr + hw)
}

acc <- rmet %>%
  group_by(m) %>%
  summarise(k = sum(ok), n = n(), acc = k / n, .groups = "drop") %>%
  rowwise() %>%
  mutate(lo = wilson(k, n)["lo"], hi = wilson(k, n)["hi"]) %>%
  ungroup() %>%
  arrange(acc) %>%
  mutate(m = factor(m, levels = m),
         spans_human = lo <= HUMAN_RMET & hi >= HUMAN_RMET)

f6 <- ggplot(acc, aes(x = acc, y = m)) +
  annotate("rect", xmin = HUMAN_RMET - 0.005, xmax = HUMAN_RMET + 0.005,
           ymin = -Inf, ymax = Inf, fill = COL_NEUTRAL, alpha = 0.25) +
  geom_vline(xintercept = HUMAN_RMET, colour = "grey30",
             linetype = "dashed", linewidth = 0.45) +
  geom_vline(xintercept = CHANCE_RMET, colour = "grey55",
             linetype = "dotted", linewidth = 0.45) +
  geom_errorbarh(aes(xmin = lo, xmax = hi, colour = spans_human),
                 height = 0, linewidth = 1.0) +
  geom_point(aes(colour = spans_human), size = 2.2) +
  annotate("text", x = CHANCE_RMET, y = nrow(acc) + 0.85, label = "chance",
           size = 2.7, colour = "grey40", hjust = -0.15) +
  annotate("text", x = HUMAN_RMET, y = nrow(acc) + 0.85, label = "human adult norm",
           size = 2.7, colour = "grey25", hjust = 1.08) +
  expand_limits(y = nrow(acc) + 1.4) +
  scale_colour_manual(values = c(`TRUE` = COL_NEUTRAL, `FALSE` = COL_ABOVE),
                      labels = c(`TRUE` = "Interval includes human norm",
                                 `FALSE` = "Interval excludes human norm")) +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0.2, 1)) +
  labs(x = "Accuracy (95% Wilson interval)", y = NULL,
       title = "Visual mental-state inference accuracy, single administration",
       subtitle = paste0("All 18 models above chance. 13 of 18 intervals include the ",
                         "human adult norm.\nInterval width reflects single administration ",
                         "(n = 72 per model) - do not tidy these bars")) +
  theme_ms() +
  theme(axis.text.y = element_text(size = 7.2))

save_fig(f6, "Figure6_rmet_accuracy", 8.6, 5.4)

# inset: definitions condition, paired
defs <- rmet %>%
  group_by(m, definitions_shown) %>%
  summarise(acc = mean(ok), .groups = "drop") %>%
  mutate(cond = ifelse(definitions_shown, "With definitions", "Without definitions")) %>%
  select(m, cond, acc) %>%
  pivot_wider(names_from = cond, values_from = acc)

f6b <- ggplot(defs, aes(x = `Without definitions`, y = `With definitions`)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey60", linetype = "dashed") +
  geom_point(size = 2.2, colour = COL_BELOW, alpha = 0.85) +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0.35, 0.9)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0.35, 0.9)) +
  coord_equal() +
  labs(title = "Definitional scaffolding had no effect",
       subtitle = "Exact McNemar over 648 paired cells, p = .79; null in every individual model") +
  theme_ms()

save_fig(f6b, "Figure6b_definitions_null", 4.6, 4.8)

# ==============================================================================
# VERIFICATION BLOCK - printed to console and written to a log
# ==============================================================================
log_path <- file.path(output_dir, "figure_verification_log.txt")
con <- file(log_path, open = "wt"); sink(con, split = TRUE)

cat("\n===============================================================\n")
cat("VERIFICATION - numbers underlying the figures\n")
cat("===============================================================\n")
cat("Generated:", format(Sys.time(), tz = "Australia/Sydney", usetz = TRUE), "\n")
cat(R.version.string, "\n\n")

s <- totals %>% filter(category == "sdo") %>% pull(score)
v <- totals %>% filter(category == "vsa") %>% pull(score)
cat(sprintf("R1  SDO : %d/24 below midpoint 4 | median %.2f | range %.2f-%.2f\n",
            sum(s < SDO_MID), median(s), min(s), max(s)))
cat(sprintf("R1  VSA : %d/24 below midpoint 0 | median %.2f | range %.2f-%.2f\n",
            sum(v < VSA_MID), median(v), min(v), max(v)))
print(wilcox.test(s - SDO_MID, alternative = "less"))
print(wilcox.test(v - VSA_MID, alternative = "less"))

cat("\nR2  VSA subscales (midpoint 0):\n")
subs %>% filter(category == "vsa") %>%
  group_by(subscale) %>%
  summarise(mean = mean(score), median = median(score),
            above = sum(score > 0), at = sum(score == 0), below = sum(score < 0),
            p = wilcox.test(score)$p.value, .groups = "drop") %>%
  as.data.frame() %>% print(digits = 4)

cat("\nR2  SDO subscales (midpoint 4):\n")
subs %>% filter(category == "sdo") %>%
  group_by(subscale) %>%
  summarise(mean = mean(score), median = median(score),
            above = sum(score >= 4), .groups = "drop") %>%
  as.data.frame() %>% print(digits = 4)

cat("\nR3  keying direction, SDO (across-model SD of subscore):\n")
key <- sd_ok %>% filter(category == "sdo") %>%
  group_by(m, keying) %>% summarise(sub = mean(scored), .groups = "drop") %>%
  pivot_wider(names_from = keying, values_from = sub)
cat(sprintf("    forward SD = %.3f | reverse SD = %.3f | forward > reverse in %d/24\n",
            sd(key$`Forward-worded`), sd(key$`Reverse-worded`),
            sum(key$`Forward-worded` > key$`Reverse-worded`)))
print(wilcox.test(key$`Forward-worded`, key$`Reverse-worded`, paired = TRUE))

cat("\nR4  dispersion:\n")
cat(sprintf("    deterministic: SDO %d/24, VSA %d/24 | max SDO %.3f, max VSA %.3f\n",
            sum(disp$dispersion[disp$category == "sdo"] == 0),
            sum(disp$dispersion[disp$category == "vsa"] == 0),
            max(disp$dispersion[disp$category == "sdo"]),
            max(disp$dispersion[disp$category == "vsa"])))

cat("\nR6  RMET:\n")
cat(sprintf("    pooled %.4f | range %.4f-%.4f | intervals spanning human norm: %d/18\n",
            mean(rmet$ok), min(acc$acc), max(acc$acc), sum(acc$spans_human)))

cat("\nSENSITIVITY - missing-data averaging route (affects 2 models only):\n")
bal <- sd_ok %>% group_by(m, category, question_id) %>%
  summarise(im = mean(scored), .groups = "drop") %>%
  group_by(m, category) %>% summarise(balanced = mean(im), .groups = "drop")
comp <- totals %>% select(m, category, obs_level = score) %>%
  left_join(bal, by = c("m", "category")) %>%
  mutate(diff = abs(obs_level - balanced)) %>% filter(diff > 1e-12)
print(as.data.frame(comp), digits = 8)
cat(sprintf("    max absolute difference = %.10f\n", max(c(0, comp$diff))))

sink(); close(con)
cat("\nDone. Figures and log written to:", normalizePath(output_dir), "\n")
