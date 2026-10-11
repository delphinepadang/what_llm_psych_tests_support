# ==============================================================================
# MS_AAI_ToM  |  03_figures.R
# Publication figures for the LLM psychometrics paper
# ------------------------------------------------------------------------------
# Reads  : Analysis_output/analysis_results.rds  (written by 02_statistical_analysis.R)
# Writes : Analysis_output/Figures/Fig##_*.pdf  (vector, for submission)
#          Analysis_output/Figures/Fig##_*.png  (raster preview, DPI below)
#
# Design rules applied to every figure
#   - No titles, subtitles or captions inside the image (those go in the paper).
#   - Every encoding has a legend or a direct label; axis titles say what is
#     measured and in what unit.
#   - One colour language across figures: developer colours (Okabe-Ito, colour-
#     blind safe) for models; a separate palette for instruments; teal/rose for
#     English/Chinese.
#   - Sizes are set for print: 7.2 in = double column, 3.5 in = single column.
#
# Run:  Rscript Analysis_script/03_figures.R   (after 02_statistical_analysis.R)
# ==============================================================================

PROJECT_ROOT <- path.expand("~/Documents/research_projects/AI Bias paper/MS_AAI_ToM")
if (!dir.exists(PROJECT_ROOT)) {
  wd <- normalizePath(getwd())
  PROJECT_ROOT <- if (basename(wd) == "Analysis_script") dirname(wd) else wd
}
DIR_OUT <- file.path(PROJECT_ROOT, "Analysis_output")
DIR_FIG <- file.path(DIR_OUT, "Figures")
RDS     <- file.path(DIR_OUT, "analysis_results.rds")
if (!file.exists(RDS)) stop("Run 02_statistical_analysis.R first: ", RDS, " not found.", call. = FALSE)
if (!dir.exists(DIR_FIG)) dir.create(DIR_FIG, recursive = TRUE)

DPI <- 400          # PNG resolution; use 600 for final raster submissions
SEED <- 20260922

PKGS <- c("ggplot2", "dplyr", "tidyr", "stringr", "forcats", "purrr", "tibble",
          "patchwork", "ggrepel", "scales")
missing_pkgs <- PKGS[!vapply(PKGS, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs)) install.packages(missing_pkgs)
suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(tidyr); library(stringr)
  library(forcats); library(purrr); library(tibble); library(patchwork)
  library(ggrepel)
})
if (utils::packageVersion("ggplot2") < "3.4.0") stop("Please update ggplot2 to >= 3.4.0", call. = FALSE)
HAS_RAGG  <- requireNamespace("ragg", quietly = TRUE)
HAS_CAIRO <- isTRUE(capabilities("cairo"))

F <- readRDS(RDS)
# Figures use a few typographic characters (en dash, Greek letters, multiplication
# sign); make sure the session can encode them.
if (!isTRUE(l10n_info()$`UTF-8`)) invisible(suppressWarnings(Sys.setlocale("LC_CTYPE", "en_US.UTF-8")))

# ------------------------------------------------------------------------------
# DESIGN SYSTEM
# ------------------------------------------------------------------------------

pick_font <- function() {
  wanted <- c("Arial", "Helvetica", "Liberation Sans")
  if (requireNamespace("systemfonts", quietly = TRUE)) {
    have <- unique(systemfonts::system_fonts()$family)
    hit <- wanted[wanted %in% have]
    if (length(hit)) return(hit[1])
  }
  "sans"
}
FONT <- pick_font()

INK   <- "grey15"
MUTED <- "grey45"
GRID  <- "grey91"

DEV_COLS <- c(OpenAI = "#0072B2", Anthropic = "#D55E00", Google = "#009E73",
              Meta = "#CC79A7", Alibaba = "#E69F00")
INSTR_COLS <- c(`SDO7(s)` = "#332288", `SDO6 (EN)` = "#44AA99", `SDO6 (ZH)` = "#CC6677",
                VSA = "#999933", RMET = "#882255")
LANG_COLS <- c(English = "#44AA99", Chinese = "#CC6677")
INSTR_LABEL <- c(SDO7s = "SDO7(s)", `SDO6-EN` = "SDO6 (EN)", `SDO6-ZH` = "SDO6 (ZH)",
                 VSA = "VSA", RMET = "RMET")

theme_pub <- function(base_size = 8.5) {
  theme_minimal(base_size = base_size, base_family = FONT) +
    theme(
      text               = element_text(colour = INK),
      axis.text          = element_text(colour = "grey30", size = rel(0.92)),
      axis.title         = element_text(colour = INK, size = rel(1)),
      axis.title.x       = element_text(margin = margin(t = 5)),
      axis.title.y       = element_text(margin = margin(r = 5)),
      panel.grid.major   = element_line(colour = GRID, linewidth = 0.3),
      panel.grid.minor   = element_blank(),
      axis.ticks         = element_line(colour = "grey75", linewidth = 0.3),
      axis.ticks.length  = unit(2, "pt"),
      legend.position    = "top",
      legend.justification = "left",
      legend.title       = element_text(size = rel(0.92), face = "bold"),
      legend.text        = element_text(size = rel(0.9)),
      legend.key.size    = unit(9, "pt"),
      legend.spacing.x   = unit(3, "pt"),
      legend.margin      = margin(0, 0, 2, 0),
      legend.box.spacing = unit(4, "pt"),
      strip.text         = element_text(face = "bold", size = rel(0.95), hjust = 0,
                                        colour = INK, margin = margin(b = 4)),
      panel.spacing      = unit(12, "pt"),
      plot.tag           = element_text(face = "bold", size = rel(1.25), colour = INK),
      plot.tag.position  = c(0, 1),
      plot.margin        = margin(8, 10, 6, 6)
    )
}
theme_set(theme_pub())
update_geom_defaults("text", list(family = FONT, colour = INK))

pct_lab <- function(x) paste0(x, "%")
fmt2 <- function(x, d = 2) formatC(x, format = "f", digits = d)
rho_lab <- function(r, lo, hi) paste0("\u03c1 = ", fmt2(r), "  [", fmt2(lo), ", ", fmt2(hi), "]")

save_fig <- function(p, name, width, height) {
  # On macOS, Cairo may be advertised even when its XQuartz libraries are absent.
  # Quartz writes PDFs directly and supports the same installed fonts as ragg.
  pdf_dev <- if (isTRUE(capabilities("aqua"))) {
    function(filename, width, height, bg = "white", ...) {
      grDevices::quartz(type = "pdf", file = filename, width = width,
                       height = height, bg = bg, ...)
    }
  } else if (HAS_CAIRO) grDevices::cairo_pdf else grDevices::pdf
  ggsave(file.path(DIR_FIG, paste0(name, ".pdf")), p, width = width, height = height,
         units = "in", device = pdf_dev, bg = "white")
  if (HAS_RAGG) {
    ggsave(file.path(DIR_FIG, paste0(name, ".png")), p, width = width, height = height,
           units = "in", dpi = DPI, device = ragg::agg_png, bg = "white")
  } else {
    ggsave(file.path(DIR_FIG, paste0(name, ".png")), p, width = width, height = height,
           units = "in", dpi = DPI, bg = "white")
  }
  message("  saved ", name)
  invisible(p)
}

dev_scale_colour <- scale_colour_manual(values = DEV_COLS, name = "Developer", drop = FALSE)
dev_scale_fill   <- scale_fill_manual(values = DEV_COLS, name = "Developer", drop = FALSE)

# Keep only developer levels that occur (in this figure: pass `pool` when several
# panels must share one legend), so legends never show empty keys.
dev_factor <- function(x, pool = x) {
  factor(as.character(x), levels = names(DEV_COLS)[names(DEV_COLS) %in% as.character(pool)])
}

human <- F$human_rmet[1, ]
H_MEAN <- 100 * human$mean / F$constants$RMET_ITEMS
H_LO   <- 100 * (human$mean - human$sd) / F$constants$RMET_ITEMS
H_HI   <- 100 * (human$mean + human$sd) / F$constants$RMET_ITEMS

message("Writing figures to ", DIR_FIG)


# ==============================================================================
# FIGURE 1. RMET accuracy against the human benchmark, and answer stability
# ==============================================================================

acc  <- F$rmet_model %>% filter(condition == "Pooled") %>% arrange(accuracy) %>%
  mutate(display = factor(display, levels = display), developer = dev_factor(developer))
accc <- F$rmet_model %>% filter(condition != "Pooled") %>%
  mutate(display = factor(display, levels = levels(acc$display)),
         condition = factor(condition, levels = c("No definitions", "Definitions")))
ins  <- F$rmet_instab_model %>% filter(condition == "Pooled") %>%
  mutate(display = factor(display, levels = levels(acc$display)), developer = dev_factor(developer))
insc <- F$rmet_instab_model %>% filter(condition != "Pooled") %>%
  mutate(display = factor(display, levels = levels(acc$display)),
         condition = factor(condition, levels = c("No definitions", "Definitions")))
n_mod <- nlevels(acc$display)

p1a <- ggplot(acc, aes(y = display)) +
  annotate("rect", xmin = H_LO, xmax = H_HI, ymin = -Inf, ymax = Inf, fill = "grey93") +
  geom_vline(xintercept = H_MEAN, colour = "grey55", linewidth = 0.4, linetype = "22") +
  geom_vline(xintercept = 100 * F$constants$RMET_CHANCE, colour = "grey60", linewidth = 0.4,
             linetype = "13") +
  annotate("text", x = H_MEAN, y = -0.35, label = "Human adults\n(mean \u00b1 1 SD, shaded)",
           size = 2.3, colour = MUTED, hjust = 0.5, vjust = 0, lineheight = 0.9) +
  annotate("text", x = 100 * F$constants$RMET_CHANCE + 1.2, y = -0.35, label = "Chance",
           size = 2.3, colour = MUTED, hjust = 0, vjust = 0) +
  geom_linerange(aes(xmin = 100 * ci_low, xmax = 100 * ci_high, colour = developer),
                 linewidth = 0.55, alpha = 0.55) +
  geom_point(data = accc, aes(x = 100 * accuracy, shape = condition), size = 1.3,
             colour = "grey30", stroke = 0.45) +
  geom_point(aes(x = 100 * accuracy, fill = developer), shape = 21, size = 2.5,
             colour = "white", stroke = 0.5) +
  scale_shape_manual(values = c(`No definitions` = 1, Definitions = 2), name = "Condition") +
  dev_scale_colour + dev_scale_fill +
  scale_x_continuous(limits = c(20, 100), breaks = seq(20, 100, 20), labels = pct_lab,
                     expand = expansion(mult = c(0, 0.01))) +
  scale_y_discrete(expand = expansion(add = c(2, 0.6))) +
  labs(x = "RMET accuracy (% correct)", y = NULL) +
  guides(fill = guide_legend(order = 1, override.aes = list(size = 2.6)),
         colour = "none", shape = guide_legend(order = 2)) +
  theme(panel.grid.major.y = element_blank())

p1b <- ggplot(ins, aes(y = display)) +
  geom_linerange(aes(xmin = ci_low, xmax = ci_high, colour = developer),
                 linewidth = 0.55, alpha = 0.55) +
  geom_point(data = insc, aes(x = pct_changed, shape = condition), size = 1.3,
             colour = "grey30", stroke = 0.45) +
  geom_point(aes(x = pct_changed, fill = developer), shape = 21, size = 2.5,
             colour = "white", stroke = 0.5) +
  scale_shape_manual(values = c(`No definitions` = 1, Definitions = 2), name = "Condition") +
  dev_scale_colour + dev_scale_fill +
  scale_x_continuous(limits = c(0, NA), labels = pct_lab, expand = expansion(mult = c(0.01, 0.05))) +
  scale_y_discrete(expand = expansion(add = c(2, 0.6))) +
  labs(x = "Items whose answer changed\nacross 5 identical repeats (%)", y = NULL) +
  guides(fill = "none", colour = "none", shape = "none") +
  theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank(),
        plot.margin = margin(8, 10, 6, 18))

fig1 <- (p1a + p1b + plot_layout(widths = c(1.75, 1), guides = "collect")) +
  plot_annotation(tag_levels = "A") &
  theme(legend.position = "top", legend.box = "vertical", legend.box.just = "left",
        legend.spacing.y = unit(1, "pt"))
save_fig(fig1, "Fig01_rmet_accuracy_and_stability", 7.2, 0.22 * n_mod + 1.5)


# ==============================================================================
# FIGURE 2. Answer instability by model and instrument (heatmap), with the
#           population-level estimate and its prediction interval on top
# ==============================================================================

heat <- bind_rows(
  F$model_instr %>% transmute(model_label, instrument = as.character(inst_lang), pct = pct_changed),
  F$rmet_instab_model %>% filter(condition == "Pooled") %>%
    transmute(model_label, instrument = "RMET", pct = pct_changed)) %>%
  left_join(F$models %>% select(model_label, display, developer), by = "model_label") %>%
  mutate(instrument = factor(INSTR_LABEL[instrument], levels = INSTR_LABEL))
ord <- heat %>% group_by(display) %>% summarise(m = mean(pct, na.rm = TRUE), .groups = "drop") %>%
  arrange(m) %>% pull(display)
heat <- heat %>% complete(display = ord, instrument) %>%
  group_by(display) %>% fill(developer, .direction = "downup") %>% ungroup() %>%
  mutate(display = factor(display, levels = ord), developer = dev_factor(developer))
HEAT_COLS <- c("#F7F7F7", "#FDE0C5", "#F9A66C", "#E0584B", "#A3203C", "#5C0A2E")

p2_main <- ggplot(heat, aes(x = instrument, y = display)) +
  geom_tile(aes(fill = pct), colour = "white", linewidth = 0.6) +
  geom_text(aes(label = ifelse(is.na(pct), "\u2013", round(pct)),
                colour = !is.na(pct) & pct >= 45), size = 2.3, show.legend = FALSE) +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "grey20")) +
  scale_fill_gradientn(colours = HEAT_COLS, limits = c(0, 100), na.value = "grey97",
                       labels = pct_lab, name = "Items whose answer changed across 5 repeats",
                       guide = guide_colourbar(barwidth = unit(50, "mm"), barheight = unit(2.4, "mm"),
                                               title.position = "top", order = 2)) +
  scale_x_discrete(position = "bottom", expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(x = NULL, y = NULL) +
  theme(panel.grid.major = element_blank(), axis.ticks = element_blank(),
        axis.text.x = element_text(face = "bold", colour = INK))

p2_strip <- ggplot(distinct(heat, display, developer), aes(x = 1, y = display, fill = developer)) +
  geom_tile(width = 0.8, colour = "white", linewidth = 0.6) +
  scale_fill_manual(values = DEV_COLS, name = "Developer", drop = TRUE,
                    guide = guide_legend(order = 1, nrow = 1)) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) +
  labs(x = NULL, y = NULL) +
  theme(axis.text = element_blank(), panel.grid.major = element_blank(), axis.ticks = element_blank(),
        plot.margin = margin(8, 6, 6, 1))

pooled <- F$rel_general %>%
  mutate(instrument = factor(INSTR_LABEL[as.character(instrument)], levels = INSTR_LABEL))
p2_top <- ggplot(pooled, aes(x = instrument)) +
  geom_linerange(aes(ymin = pi_low, ymax = pi_high), colour = "grey70", linewidth = 2.2) +
  geom_linerange(aes(ymin = ci_low, ymax = ci_high), colour = "grey25", linewidth = 0.7) +
  geom_point(aes(y = pooled_pct), shape = 21, fill = "white", colour = "grey15", size = 1.9, stroke = 0.6) +
  annotate("text", x = 0.55, y = 112, hjust = 0, vjust = 1, size = 2.2, colour = MUTED,
           label = "All models pooled: estimate, 95% CI (black) and 95% prediction interval (grey)") +
  scale_y_continuous(limits = c(0, 112), breaks = c(0, 50, 100), labels = pct_lab) +
  scale_x_discrete(expand = expansion(add = 0.5)) +
  labs(x = NULL, y = NULL) +
  theme(axis.text.x = element_blank(), panel.grid.major.x = element_blank())

fig2 <- (p2_top + plot_spacer() + p2_main + p2_strip) +
  plot_layout(ncol = 2, widths = c(1, 0.035), heights = c(1, 6), guides = "collect") &
  theme(legend.position = "top", legend.box = "vertical", legend.box.just = "left",
        legend.spacing.y = unit(2, "pt"))
save_fig(fig2, "Fig02_instability_heatmap", 5.4, 0.2 * length(ord) + 2.1)


# ==============================================================================
# FIGURE 3. Reliability at each level of aggregation, and the D-study
# ==============================================================================

ds <- F$dstudy %>%
  filter(instrument != "RMET (definitions)") %>%
  mutate(instrument = recode(instrument, `RMET (no definitions)` = "RMET"),
         instrument = factor(INSTR_LABEL[instrument], levels = INSTR_LABEL))
rel3 <- ds %>%
  filter((coefficient == "item_rel_models" & n_runs == 1) |
           (coefficient == "scale_rel_items_fixed" & n_runs %in% c(1, 5))) %>%
  mutate(type = case_when(coefficient == "item_rel_models" ~ "Single item answer, one run",
                          n_runs == 1 ~ "Scale score, one run",
                          TRUE ~ "Scale score, mean of 5 runs"),
         type = factor(type, levels = c("Single item answer, one run", "Scale score, one run",
                                        "Scale score, mean of 5 runs")),
         instrument = fct_rev(instrument))
TYPE_SHAPES <- c(`Single item answer, one run` = 21, `Scale score, one run` = 22,
                 `Scale score, mean of 5 runs` = 23)
TYPE_FILLS <- c(`Single item answer, one run` = "white", `Scale score, one run` = "grey65",
                `Scale score, mean of 5 runs` = "grey15")

p3a <- ggplot(rel3, aes(x = value, y = instrument)) +
  geom_vline(xintercept = c(0.8, 0.9), colour = "grey80", linewidth = 0.35, linetype = "22") +
  geom_line(aes(group = instrument), colour = "grey75", linewidth = 0.5) +
  geom_point(aes(shape = type, fill = type), size = 2.3, colour = "grey15", stroke = 0.5) +
  scale_shape_manual(values = TYPE_SHAPES, name = NULL) +
  scale_fill_manual(values = TYPE_FILLS, name = NULL) +
  scale_x_continuous(limits = c(0.5, 1), breaks = seq(0.5, 1, 0.1)) +
  labs(x = "Reliability of comparing models (generalizability coefficient)", y = NULL) +
  guides(shape = guide_legend(nrow = 1), fill = guide_legend(nrow = 1)) +
  theme(panel.grid.major.y = element_blank())

ds_plot <- ds %>% filter(coefficient %in% c("scale_rel_items_fixed", "item_rel_models"), n_runs <= 10) %>%
  mutate(panel = if_else(coefficient == "item_rel_models", "Single item answer", "Scale score"),
         panel = factor(panel, levels = c("Scale score", "Single item answer")))
p3b <- ggplot(ds_plot, aes(x = n_runs, y = value, colour = instrument)) +
  geom_hline(yintercept = c(0.8, 0.9), colour = "grey80", linewidth = 0.35, linetype = "22") +
  geom_text(data = expand_grid(panel = factor(levels(ds_plot$panel), levels = levels(ds_plot$panel)),
                               y = c(0.8, 0.9)) %>% mutate(label = if_else(y == 0.8, ".80", ".90")),
            aes(x = 10, y = y, label = label), inherit.aes = FALSE, hjust = 1, vjust = -0.4,
            size = 2.3, colour = MUTED) +
  geom_line(linewidth = 0.7) +
  geom_point(data = ds_plot %>% filter(n_runs %in% c(1, 5)), size = 1.4) +
  facet_wrap(~ panel, nrow = 1) +
  scale_colour_manual(values = INSTR_COLS, name = NULL) +
  scale_x_continuous(breaks = c(1, 2, 3, 5, 10), limits = c(1, 10)) +
  scale_y_continuous(limits = c(0.5, 1), breaks = seq(0.5, 1, 0.1)) +
  labs(x = "Number of repeats averaged", y = "Reliability of comparing models") +
  guides(colour = guide_legend(nrow = 1, override.aes = list(size = 1.6)))

fig3 <- (p3a / p3b) + plot_layout(heights = c(1, 1.35)) + plot_annotation(tag_levels = "A")
save_fig(fig3, "Fig03_reliability_and_dstudy", 7.2, 5.6)


# ==============================================================================
# FIGURE 4. Apparent stability depends on where the first answer sits
# ==============================================================================

pc <- F$pos_curves %>%
  mutate(instrument = factor(INSTR_LABEL[as.character(inst_lang)], levels = INSTR_LABEL))
p4 <- ggplot(pc, aes(x = pos1, colour = instrument, fill = instrument)) +
  geom_ribbon(aes(ymin = 100 * lo, ymax = 100 * hi), alpha = 0.14, colour = NA) +
  geom_line(aes(y = 100 * fit), linewidth = 0.75) +
  geom_linerange(aes(ymin = 100 * obs_lo, ymax = 100 * obs_hi), linewidth = 0.35, alpha = 0.7) +
  geom_point(aes(y = 100 * obs, size = n), shape = 21, fill = "white", stroke = 0.6) +
  facet_wrap(~ instrument, nrow = 1) +
  scale_colour_manual(values = INSTR_COLS, guide = "none") +
  scale_fill_manual(values = INSTR_COLS, guide = "none") +
  scale_size_area(max_size = 4.2, breaks = c(10, 50, 150, 300), name = "Model\u2013item pairs") +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("Floor", "Mid", "Ceiling"),
                     expand = expansion(mult = 0.06)) +
  scale_y_continuous(limits = c(0, 100), labels = pct_lab) +
  labs(x = "Position of the first answer on the scale (construct direction)",
       y = "Pairs whose answer changed\nin a later repeat (%)") +
  theme(legend.position = "top", panel.spacing = unit(20, "pt"))
save_fig(p4, "Fig04_instability_by_scale_position", 7.2, 3.0)


# ==============================================================================
# FIGURE 5. English vs Chinese SDO6: models and items
# ==============================================================================

SDO6_SHORT <- c(
  `1` = "Some groups are simply inferior", `2` = "Force against other groups is sometimes needed",
  `3` = "OK if some groups have more chance in life", `4` = "Sometimes necessary to step on other groups",
  `5` = "Fewer problems if groups stayed in place", `6` = "Good that some groups are on top",
  `7` = "Inferior groups should stay in their place", `8` = "Other groups must be kept in their place",
  `9` = "Good if groups could be equal (R)", `10` = "Group equality should be our ideal (R)",
  `11` = "All groups should get an equal chance (R)", `12` = "Equalize conditions for groups (R)",
  `13` = "More social equality benefits society (R)", `14` = "Fewer problems if treated more equally (R)",
  `15` = "Make incomes as equal as possible (R)", `16` = "No group should dominate (R)")

lm5 <- F$lang_model %>% mutate(developer = dev_factor(developer))
# Label the 4 largest shifts, models with no or a negative shift, and high scorers.
# round() so that tied shifts tie on every computer (see tie_round() in 02).
lab5 <- lm5 %>% filter(rank(-round(diff, 10)) <= 4 | diff <= 0 | en > 2)
lim5 <- c(1, ceiling(max(c(lm5$en, lm5$zh)) * 2) / 2)
p5a <- ggplot(lm5, aes(x = en, y = zh)) +
  geom_abline(data = tibble(l = "Equal in both languages"), aes(slope = 1, intercept = 0, linetype = l),
              colour = "grey60", linewidth = 0.4) +
  geom_point(aes(fill = developer), shape = 21, colour = "white", size = 2.5, stroke = 0.45) +
  geom_text_repel(data = lm5 %>% mutate(lab = if_else(model_label %in% lab5$model_label, display, "")),
                  aes(label = lab), size = 2.2, colour = "grey30",
                  segment.colour = "grey70", segment.size = 0.25, min.segment.length = 0,
                  box.padding = 0.5, point.padding = 0.25, force = 6, max.overlaps = Inf,
                  seed = SEED, family = FONT) +
  dev_scale_fill +
  scale_linetype_manual(values = "22", name = NULL) +
  scale_x_continuous(breaks = seq(1, lim5[2], 0.5)) + scale_y_continuous(breaks = seq(1, lim5[2], 0.5)) +
  coord_equal(xlim = lim5, ylim = lim5) +
  labs(x = "SDO6 score, English (1\u20137)", y = "SDO6 score, Chinese (1\u20137)") +
  guides(fill = guide_legend(order = 1, nrow = 2, byrow = TRUE, override.aes = list(size = 2.6)),
         linetype = guide_legend(order = 2)) +
  theme(legend.position = "bottom", legend.box = "vertical", legend.box.just = "left",
        legend.spacing.y = unit(1, "pt"))

it5 <- F$lang_item %>%
  left_join(F$dif %>% select(item_no, flagged, direction), by = "item_no") %>%
  mutate(sub_lab = if_else(subscale == "dominance", "Dominance", "Anti-egalitarianism (R)"),
         sub_lab = factor(sub_lab, levels = c("Dominance", "Anti-egalitarianism (R)")),
         label = paste0(item_no, ". ", SDO6_SHORT[as.character(item_no)]),
         label = fct_reorder(label, -item_no))
it5_long <- it5 %>% select(label, sub_lab, English = mean_en, Chinese = mean_zh) %>%
  pivot_longer(c(English, Chinese), names_to = "language", values_to = "mean") %>%
  mutate(language = factor(language, levels = c("English", "Chinese")))
p5b <- ggplot(it5, aes(y = label)) +
  geom_segment(aes(x = mean_en, xend = mean_zh, yend = label), colour = "grey70", linewidth = 0.7) +
  geom_point(data = it5_long, aes(x = mean, colour = language), size = 2.1) +
  geom_point(data = it5 %>% filter(flagged %in% TRUE) %>%
               mutate(flag = "Item shifts differently from the scale"),
             aes(x = 4.3, shape = flag), colour = "#882255", fill = "#882255", size = 1.5) +
  facet_grid(sub_lab ~ ., scales = "free_y", space = "free_y") +
  scale_colour_manual(values = LANG_COLS, name = "Language") +
  scale_shape_manual(values = 23, name = NULL) +
  scale_x_continuous(limits = c(1, 4.45), breaks = 1:4) +
  labs(x = "Mean response (1 = lowest SDO)", y = NULL) +
  guides(colour = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  theme(strip.text.y = element_text(angle = -90, hjust = 0.5), panel.grid.major.y = element_blank(),
        legend.position = "bottom", legend.box = "vertical", legend.box.just = "left",
        legend.spacing.y = unit(1, "pt"), axis.text.y = element_text(size = rel(0.86)))

fig5 <- (p5a + p5b + plot_layout(widths = c(1.5, 1))) + plot_annotation(tag_levels = "A")
save_fig(fig5, "Fig05_language_models_and_items", 7.4, 4.6)


# ==============================================================================
# FIGURE 6. How the Chinese shift happens: softer disagreement, not reversal
# ==============================================================================

# Response distribution on the construct metric (all scored responses)
dist6 <- F$resp_counts %>% filter(instrument == "SDO6") %>%
  group_by(subscale, language, resp = value_key) %>% summarise(n = sum(n), .groups = "drop") %>%
  group_by(subscale, language) %>% mutate(pct = 100 * n / sum(n)) %>% ungroup() %>%
  mutate(language = factor(if_else(language == "en", "English", "Chinese"), levels = c("English", "Chinese")),
         sub_lab = factor(if_else(subscale == "dominance", "Dominance items", "Anti-egalitarianism items (R)"),
                          levels = c("Dominance items", "Anti-egalitarianism items (R)")))
p6a <- ggplot(dist6, aes(x = factor(resp, levels = 1:7), y = pct, fill = language)) +
  geom_col(position = position_dodge(width = 0.78), width = 0.72) +
  facet_wrap(~ sub_lab, nrow = 1) +
  scale_fill_manual(values = LANG_COLS, name = "Language") +
  scale_y_continuous(labels = pct_lab, expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Response (construct direction; 1 = strongest rejection of group dominance)",
       y = "Responses (%)") +
  theme(panel.grid.major.x = element_blank())

PAT_LEVELS <- c("unchanged", "same side, less extreme in Chinese", "same side, more extreme in Chinese",
                "at or through midpoint", "direction reversed")
PAT_LABELS <- c("Unchanged", "Softer in Chinese (same side)", "More extreme in Chinese (same side)",
                "At the midpoint in either language", "Direction reversed")
PAT_COLS <- setNames(c("grey85", "#CC6677", "#44AA99", "#DDCC77", "#332288"), PAT_LABELS)
dec <- F$lang_decomp %>%
  mutate(pattern = factor(PAT_LABELS[match(pattern, PAT_LEVELS)], levels = rev(PAT_LABELS)),
         sub_lab = factor(if_else(subscale == "dominance", "Dominance", "Anti-egalitarianism"),
                          levels = c("Anti-egalitarianism", "Dominance")))
p6b <- ggplot(dec, aes(x = pct_pairs, y = sub_lab, fill = pattern)) +
  geom_col(width = 0.62, colour = "white", linewidth = 0.3) +
  geom_text(aes(label = ifelse(pct_pairs >= 6, paste0(round(pct_pairs), "%"), "")),
            position = position_stack(vjust = 0.5), size = 2.3,
            colour = ifelse(dec$pattern %in% c("Direction reversed", "Softer in Chinese (same side)"), "white", "grey15")) +
  scale_fill_manual(values = PAT_COLS, name = NULL, breaks = PAT_LABELS) +
  scale_x_continuous(labels = pct_lab, expand = c(0, 0)) +
  labs(x = "Model\u2013item pairs, English to Chinese (%)", y = NULL) +
  guides(fill = guide_legend(nrow = 2)) +
  theme(panel.grid.major.y = element_blank())

fig6 <- (p6a / p6b) + plot_layout(heights = c(1.5, 0.75)) + plot_annotation(tag_levels = "A")
save_fig(fig6, "Fig06_language_mechanism", 7.2, 4.6)


# ==============================================================================
# FIGURE 7. Forest plots: effects vary between models (random-effects meta-analysis)
# ==============================================================================

pool7 <- unique(c(as.character(F$defs_studies$developer), as.character(F$lang_studies$developer)))
forest <- function(st, meta, xlab, band = NULL, scale = 1) {
  st <- st %>% mutate(yi_s = yi * scale, lo = (yi - 1.96 * sei) * scale, hi = (yi + 1.96 * sei) * scale,
                      developer = dev_factor(developer, pool7)) %>%
    arrange(yi_s) %>% mutate(y = row_number() + 1.5)
  pool_y <- 0
  diamond <- tibble(x = c(meta$ci_low, meta$estimate, meta$ci_high, meta$estimate),
                    y = c(pool_y, pool_y + 0.45, pool_y, pool_y - 0.45))
  g <- ggplot(st, aes(y = y))
  if (!is.null(band)) g <- g + annotate("rect", xmin = -band, xmax = band, ymin = -Inf, ymax = Inf,
                                        fill = "grey94")
  g + geom_vline(xintercept = 0, colour = "grey55", linewidth = 0.35) +
    geom_linerange(aes(xmin = lo, xmax = hi, colour = developer), linewidth = 0.5, alpha = 0.8) +
    geom_point(aes(x = yi_s, fill = developer), shape = 22, size = 1.9, colour = "white", stroke = 0.3, show.legend = TRUE) +
    annotate("segment", x = meta$pi_low, xend = meta$pi_high, y = pool_y, yend = pool_y,
             colour = "grey45", linewidth = 0.5) +
    annotate("segment", x = c(meta$pi_low, meta$pi_high), xend = c(meta$pi_low, meta$pi_high),
             y = pool_y - 0.25, yend = pool_y + 0.25, colour = "grey45", linewidth = 0.5) +
    geom_polygon(data = diamond, aes(x = x, y = y), inherit.aes = FALSE, fill = "grey20") +
    scale_y_continuous(breaks = c(pool_y, st$y),
                       labels = c("Pooled: 95% CI (diamond),\nprediction interval (line)", as.character(st$display)),
                       expand = expansion(add = 0.8)) +
    scale_colour_manual(values = DEV_COLS, limits = pool7, name = "Developer") +
    scale_fill_manual(values = DEV_COLS, limits = pool7, name = "Developer") +
    guides(colour = "none", fill = guide_legend(override.aes = list(shape = 22))) +
    labs(x = xlab, y = NULL) +
    theme(panel.grid.major.y = element_blank())
}
p7a <- forest(F$defs_studies, F$defs_meta, "Definitions effect on accuracy\n(percentage points)",
              band = F$constants$SESOI_PP, scale = 100) +
  annotate("text", x = 0, y = 1.3, label = paste0("shaded: equivalence bounds (\u00b1", F$constants$SESOI_PP, " pp)"),
           size = 2.1, colour = MUTED)
p7b <- forest(F$lang_studies, F$lang_meta, "Chinese minus English SDO6 score\n(scale points)") +
  guides(fill = "none", colour = "none")
fig7 <- (p7a + p7b + plot_layout(guides = "collect")) + plot_annotation(tag_levels = "A") &
  theme(legend.position = "bottom")
save_fig(fig7, "Fig07_forest_definitions_and_language", 7.4, 5.8)


# ==============================================================================
# FIGURE 8. The alignment-proxy correlation is largely mechanical
# ==============================================================================

al <- F$align %>% mutate(developer = dev_factor(developer))
at <- F$align_tests
get_rho <- function(pattern) at %>% filter(str_detect(test, fixed(pattern)))
panel8 <- function(yvar, ylab, pat, xlab = NULL) {
  r <- get_rho(pat)
  d <- al %>% filter(!is.na(.data[[yvar]]))
  ggplot(d, aes(x = pct_at_floor, y = .data[[yvar]])) +
    geom_smooth(method = "lm", formula = y ~ x, colour = "grey35", fill = "grey88",
                linewidth = 0.5) +
    geom_point(aes(fill = developer), shape = 21, colour = "white", size = 2.3, stroke = 0.4) +
    annotate("text", x = 62.5, y = 118, label = paste0(rho_lab(r$r, r$ci_low, r$ci_high), ",  n = ", r$n),
             size = 2.35, colour = INK) +
    scale_fill_manual(values = DEV_COLS, limits = names(DEV_COLS), name = "Developer") +
    scale_x_continuous(limits = c(25, 100), labels = pct_lab) +
    scale_y_continuous(labels = pct_lab, breaks = seq(0, 100, 25)) +
    coord_cartesian(ylim = c(-3, 122), clip = "off") +
    labs(x = xlab, y = ylab)
}
p8a <- panel8("pct_changed", "SDO answers that changed (%)", "(i)")
p8b <- panel8("pct_changed_nonpinned", "SDO answers that changed, among\nitems not pinned at the floor (%)", "(ii)",
              xlab = "SDO items pinned at the construct floor on all 5 repeats (%)") + guides(fill = "none")
p8c <- panel8("rmet_changed", "RMET answers that changed (%)", "(v)") + guides(fill = "none")
fig8 <- (p8a + p8b + p8c + plot_layout(guides = "collect")) + plot_annotation(tag_levels = "A") &
  theme(legend.position = "bottom")
save_fig(fig8, "Fig08_alignment_proxy_tests", 7.2, 3.2)


# ==============================================================================
# FIGURE 9. RMET items: where LLM answers depart from the human consensus
# ==============================================================================

items <- F$rmet_items %>% mutate(other = pmax(0, 1 - target_share - top_foil_share)) %>%
  arrange(target_share) %>%
  mutate(label = paste0(item, "  ", target), label = factor(label, levels = label))
items_long <- items %>% select(label, Target = target_share, `Most chosen foil` = top_foil_share,
                               `Other two foils` = other, retained) %>%
  pivot_longer(c(Target, `Most chosen foil`, `Other two foils`), names_to = "part", values_to = "share") %>%
  mutate(part = factor(part, levels = rev(c("Target", "Most chosen foil", "Other two foils"))))
p9 <- ggplot(items_long, aes(x = 100 * share, y = label, fill = part)) +
  geom_col(width = 0.72) +
  geom_vline(xintercept = 50, colour = "grey20", linewidth = 0.35, linetype = "22") +
  geom_point(data = items %>% filter(!retained) %>% mutate(flag = "Fails Baron-Cohen et al. (2001) retention rule"),
             aes(x = 103, y = label, shape = flag), inherit.aes = FALSE,
             size = 1.6, fill = "#882255", colour = "#882255") +
  annotate("text", x = 50, y = nrow(items) + 1.1, label = "50% target rule", size = 2.3,
           colour = MUTED, hjust = 0.5) +
  scale_shape_manual(values = 23, name = NULL) +
  scale_fill_manual(values = c(Target = "#44AA99", `Most chosen foil` = "#CC6677",
                               `Other two foils` = "grey85"), breaks = c("Target", "Most chosen foil", "Other two foils"),
                    name = "Share of LLM answers") +
  scale_x_continuous(limits = c(0, 106), breaks = seq(0, 100, 25), labels = pct_lab,
                     expand = c(0, 0)) +
  scale_y_discrete(expand = expansion(add = c(0.6, 1.8))) +
  labs(x = "Answers across models, conditions and repeats (%)",
       y = "RMET item (target word)") +
  theme(panel.grid.major.y = element_blank(), legend.position = "top", legend.box = "vertical",
        legend.box.just = "left") +
  guides(fill = guide_legend(order = 1),
         shape = guide_legend(order = 2, override.aes = list(fill = "#882255", colour = "#882255", size = 2)))
save_fig(p9, "Fig09_rmet_items_consensus", 5.6, 6.6)


# ==============================================================================
# FIGURE 10. Do models make the same mistakes? Error consistency matrix
# ==============================================================================

EC <- F$rmet_EC; ordr <- F$rmet_EC_order
disp <- setNames(F$models$display, F$models$model_label)
devm <- setNames(as.character(F$models$developer), F$models$model_label)
ec <- as_tibble(EC, rownames = "a") %>% pivot_longer(-a, names_to = "b", values_to = "kappa") %>%
  mutate(ia = match(a, ordr), ib = match(b, ordr)) %>% filter(ia > ib) %>%
  mutate(a = factor(disp[a], levels = disp[ordr]), b = factor(disp[b], levels = disp[ordr]))
lim <- max(abs(ec$kappa))
p10 <- ggplot(ec, aes(x = b, y = a, fill = kappa)) +
  geom_tile(colour = "white", linewidth = 0.4) +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0,
                       limits = c(-lim, lim), name = "Error consistency (\u03ba)",
                       guide = guide_colourbar(barwidth = unit(45, "mm"), barheight = unit(2.4, "mm"),
                                               title.position = "top")) +
  scale_x_discrete(position = "bottom", drop = FALSE) +
  scale_y_discrete(drop = FALSE) +
  labs(x = NULL, y = NULL) +
  theme(panel.grid.major = element_blank(), axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1),
        legend.position = "top")
strip_y <- ggplot(tibble(m = factor(disp[ordr], levels = disp[ordr]), dev = dev_factor(devm[ordr])),
                  aes(x = 1, y = m, fill = dev)) +
  geom_tile(colour = "white", linewidth = 0.4) + dev_scale_fill +
  scale_x_continuous(expand = c(0, 0)) + labs(x = NULL, y = NULL) +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid.major = element_blank(),
        plot.margin = margin(8, 6, 6, 0))
fig10 <- (p10 + strip_y + plot_layout(widths = c(1, 0.035), guides = "collect")) &
  theme(legend.position = "top")
save_fig(fig10, "Fig10_rmet_error_consistency", 6.0, 6.0)


# ==============================================================================
# FIGURE 11. Stability across months (June vs August/September 2026)
# ==============================================================================

dr <- F$drift_rmet %>% arrange(excess_disagreement) %>%
  mutate(display = factor(display, levels = display), sig = p_bh < 0.05,
         developer = dev_factor(developer))
dr_long <- dr %>% select(display, developer, sig, `Within September` = within_sep,
                         `June vs September` = between_jun_sep) %>%
  pivot_longer(c(`Within September`, `June vs September`), names_to = "comparison", values_to = "agree") %>%
  mutate(comparison = factor(comparison, levels = c("Within September", "June vs September")))
p11a <- ggplot(dr, aes(y = display)) +
  geom_segment(aes(x = within_sep * 100, xend = between_jun_sep * 100, yend = display),
               colour = "grey70", linewidth = 0.6) +
  geom_point(data = dr_long, aes(x = agree * 100, shape = comparison, fill = comparison),
             size = 2, colour = "grey15", stroke = 0.4) +
  geom_point(data = dr %>% filter(sig) %>% mutate(flag = "Drift beyond repeat-to-repeat variation (BH p < .05)"),
             aes(x = pmin(within_sep, between_jun_sep) * 100 - 1.2, colour = flag),
             shape = 8, size = 1.6, stroke = 0.6) +
  scale_colour_manual(values = "#882255", name = NULL) +
  scale_shape_manual(values = c(`Within September` = 21, `June vs September` = 24), name = NULL) +
  scale_fill_manual(values = c(`Within September` = "white", `June vs September` = "grey20"), name = NULL) +
  scale_x_continuous(labels = pct_lab) +
  labs(x = "Probability that two answers to the same item agree (%)", y = NULL) +
  theme(panel.grid.major.y = element_blank())

ds11 <- F$drift_sdo_scores %>% select(display, developer, inst_lang, session, score) %>%
  pivot_wider(names_from = session, values_from = score) %>%
  mutate(instrument = factor(INSTR_LABEL[as.character(inst_lang)], levels = INSTR_LABEL),
         developer = dev_factor(developer))
p11b <- ggplot(ds11, aes(x = 100 * Jun, y = 100 * Aug)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey60", linewidth = 0.4, linetype = "22") +
  geom_point(aes(fill = developer), shape = 21, colour = "white", size = 2.1, stroke = 0.4) +
  facet_wrap(~ instrument, nrow = 2, scales = "free") +
  dev_scale_fill +
  labs(x = "Scale position, June (% of range)", y = "Scale position, August (% of range)")
fig11 <- (p11a + p11b + plot_layout(widths = c(1.35, 1), guides = "collect")) +
  plot_annotation(tag_levels = "A") &
  theme(legend.position = "top", legend.box = "vertical", legend.box.just = "left",
        legend.spacing.y = unit(1, "pt"))
save_fig(fig11, "Fig11_longitudinal_drift", 7.2, 4.4)


# ==============================================================================
# FIGURE 12. Subscales order models differently (what a total score hides)
# ==============================================================================

sm <- F$sub_models %>% mutate(instrument = factor(INSTR_LABEL[as.character(inst_lang)], levels = INSTR_LABEL),
                              developer = dev_factor(developer)) %>%
  filter(!is.na(instrument)) %>% mutate(instrument = droplevels(instrument))
rv <- F$reversals %>%
  mutate(instrument = INSTR_LABEL[as.character(inst_lang)],
         strip = paste0(instrument, "\nKendall \u03c4 = ", fmt2(kendall_tau_D_AE), ";  ",
                        round(pct_total_order_reversed_by_a_subscale), "% of pairs reversed"))
strip_map <- setNames(rv$strip, rv$instrument)
sm <- sm %>% mutate(strip = factor(strip_map[as.character(instrument)], levels = strip_map[levels(instrument)]))
p12 <- ggplot(sm, aes(x = dominance, y = anti_egalitarianism)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey60", linewidth = 0.4, linetype = "22") +
  geom_point(aes(fill = developer), shape = 21, colour = "white", size = 2.1, stroke = 0.4,
             position = position_jitter(width = 0.015, height = 0.015, seed = SEED)) +
  facet_wrap(~ strip, nrow = 1) +
  dev_scale_fill +
  scale_x_continuous(trans = "log2", breaks = c(1, 1.5, 2, 3, 4, 6)) +
  scale_y_continuous(trans = "log2", breaks = c(1, 1.5, 2, 3, 4, 6)) +
  labs(x = "Dominance subscale score (1\u20137, log scale)",
       y = "Anti-egalitarianism score\n(1\u20137, log scale)") +
  theme(strip.text = element_text(size = rel(0.9), lineheight = 1.05))
save_fig(p12, "Fig12_subscale_divergence", 7.2, 3.3)


# ==============================================================================
# FIGURE 13. Model profiles: principal components
# ==============================================================================

sc <- F$pca_scores %>% mutate(developer = dev_factor(developer))
ld <- F$pca_loadings
pv <- F$pca_var
p13a <- ggplot(sc, aes(x = PC1, y = PC2)) +
  geom_hline(yintercept = 0, colour = "grey85", linewidth = 0.3) +
  geom_vline(xintercept = 0, colour = "grey85", linewidth = 0.3) +
  geom_point(aes(fill = developer), shape = 21, colour = "white", size = 2.5, stroke = 0.45) +
  geom_text_repel(aes(label = display), size = 2.1, colour = "grey25", family = FONT, seed = SEED,
                  segment.colour = "grey75", segment.size = 0.2, max.overlaps = Inf,
                  box.padding = 0.4, point.padding = 0.2, force = 5) +
  dev_scale_fill +
  labs(x = paste0("PC1 (", round(pv$pct_variance[1]), "% of variance)"),
       y = paste0("PC2 (", round(pv$pct_variance[2]), "% of variance)")) +
  guides(fill = guide_legend(override.aes = list(size = 2.6)))
circle <- tibble(t = seq(0, 2 * pi, length.out = 200), x = cos(t), y = sin(t))
p13b <- ggplot(ld, aes(x = PC1, y = PC2)) +
  geom_path(data = circle, aes(x = x, y = y), colour = "grey80", linewidth = 0.35) +
  geom_hline(yintercept = 0, colour = "grey88", linewidth = 0.3) +
  geom_vline(xintercept = 0, colour = "grey88", linewidth = 0.3) +
  geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2), colour = "grey35", linewidth = 0.4,
               arrow = arrow(length = unit(1.5, "mm"), type = "closed")) +
  geom_text_repel(aes(label = feature), size = 2.1, colour = INK, family = FONT, seed = SEED,
                  segment.colour = "grey80", segment.size = 0.2, box.padding = 0.25,
                  max.overlaps = Inf, min.segment.length = 0.2,
                  force = 8, max.time = 5, max.iter = 20000,
                  xlim = c(-1.35, 1.35), ylim = c(-1.3, 1.3)) +
  coord_equal(xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5)) +
  scale_x_continuous(breaks = c(-1, 0, 1)) + scale_y_continuous(breaks = c(-1, 0, 1)) +
  labs(x = "Correlation with PC1", y = "Correlation with PC2")
fig13 <- (p13a / p13b + plot_layout(heights = c(1, 1))) + plot_annotation(tag_levels = "A") &
  theme(legend.position = "bottom")
save_fig(fig13, "Fig13_model_profiles_pca", 7.4, 8.5)


# ==============================================================================
# SUPPLEMENTARY FIGURES
# ==============================================================================

# S1. Repeats are exchangeable: mean deviation by run ---------------------------------
rd <- F$run_dev %>% mutate(data = factor(data, levels = c("SDO/VSA", "RMET"),
                                         labels = c("SDO and VSA (position, 0\u20131)", "RMET (correct, 0/1)")))
pS1 <- ggplot(rd, aes(x = run, y = mean_dev)) +
  geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.35, linetype = "22") +
  geom_linerange(aes(ymin = mean_dev - 1.96 * se, ymax = mean_dev + 1.96 * se), linewidth = 0.5) +
  geom_point(size = 2, shape = 21, fill = "white", stroke = 0.6) +
  facet_wrap(~ data, nrow = 1, scales = "free_y") +
  scale_x_continuous(breaks = 1:5) +
  labs(x = "Repeat (order of administration)", y = "Mean deviation from the pair's own mean\n(95% CI)")
save_fig(pS1, "FigS1_run_order", 6.0, 2.6)

# S2. Response styles by language: extreme responding, English vs Chinese -------------
ers <- F$styles %>% filter(inst_lang %in% c("SDO6-EN", "SDO6-ZH")) %>%
  select(display, developer, inst_lang, ERS) %>%
  pivot_wider(names_from = inst_lang, values_from = ERS) %>%
  arrange(`SDO6-EN` - `SDO6-ZH`) %>% mutate(display = factor(display, levels = display))
ers_long <- ers %>% pivot_longer(c(`SDO6-EN`, `SDO6-ZH`), names_to = "lang", values_to = "ERS") %>%
  mutate(lang = factor(if_else(lang == "SDO6-EN", "English", "Chinese"), levels = c("English", "Chinese")))
pS2 <- ggplot(ers, aes(y = display)) +
  geom_segment(aes(x = 100 * `SDO6-EN`, xend = 100 * `SDO6-ZH`, yend = display), colour = "grey70",
               linewidth = 0.6) +
  geom_point(data = ers_long, aes(x = 100 * ERS, colour = lang), size = 2) +
  scale_colour_manual(values = LANG_COLS, name = "Language") +
  scale_x_continuous(labels = pct_lab, limits = c(0, 100)) +
  labs(x = "SDO6 answers at a scale endpoint (%)", y = NULL) +
  theme(panel.grid.major.y = element_blank())
save_fig(pS2, "FigS2_extreme_responding_by_language", 4.6, 5.2)

# S3. Correlation matrix across models ---------------------------------------------------
cm <- F$cor_mat
vars <- unique(c(cm$var1, cm$var2))
cm_full <- bind_rows(cm, cm %>% rename(var1 = var2, var2 = var1)) %>%
  mutate(var1 = factor(var1, levels = vars), var2 = factor(var2, levels = rev(vars))) %>%
  filter(as.integer(var1) < (length(vars) + 1 - as.integer(var2)))
pS3 <- ggplot(cm_full, aes(x = var1, y = var2, fill = r)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = paste0(fmt2(r), ifelse(p_bh < 0.05, "*", ""))), size = 2.3,
            colour = ifelse(abs(cm_full$r) > 0.55, "white", INK)) +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0, limits = c(-1, 1),
                       name = "Spearman \u03c1 across models  (* BH-adjusted p < .05)",
                       guide = guide_colourbar(barwidth = unit(50, "mm"), barheight = unit(2.4, "mm"),
                                               title.position = "top")) +
  labs(x = NULL, y = NULL) +
  theme(panel.grid.major = element_blank(), axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 40, hjust = 1, vjust = 1), legend.position = "top")
save_fig(pS3, "FigS3_cross_instrument_correlations", 5.6, 5.2)

# S4. Variance components (G-study) ----------------------------------------------------------
gc <- F$g_components %>% filter(instrument != "RMET (definitions)") %>%
  mutate(instrument = recode(instrument, `RMET (no definitions)` = "RMET"),
         instrument = factor(INSTR_LABEL[instrument], levels = rev(INSTR_LABEL)),
         component = recode(component, "residual (model x item x run)" = "Run-to-run noise (residual)",
                            "model x item" = "Model \u00d7 item", "model x run" = "Model \u00d7 run",
                            "item x run" = "Item \u00d7 run", "model" = "Model", "item" = "Item", "run" = "Run"),
         component = factor(component, levels = c("Model", "Item", "Model \u00d7 item",
                                                  "Run-to-run noise (residual)", "Model \u00d7 run",
                                                  "Item \u00d7 run", "Run")))
COMP_COLS <- c("Model" = "#332288", "Item" = "#88CCEE", "Model \u00d7 item" = "#44AA99",
               "Run-to-run noise (residual)" = "#CC6677", "Model \u00d7 run" = "#DDCC77",
               "Item \u00d7 run" = "#AA4499", "Run" = "grey70")
pS4 <- ggplot(gc, aes(x = 100 * share, y = instrument, fill = component)) +
  geom_col(width = 0.66, colour = "white", linewidth = 0.3) +
  scale_fill_manual(values = COMP_COLS, name = NULL) +
  scale_x_continuous(labels = pct_lab, expand = c(0, 0)) +
  labs(x = "Share of response variance", y = NULL) +
  guides(fill = guide_legend(nrow = 2)) +
  theme(panel.grid.major.y = element_blank())
save_fig(pS4, "FigS4_variance_components", 6.4, 3.0)

message("Done: ", length(list.files(DIR_FIG, pattern = "\\.pdf$")), " PDF figures in ", DIR_FIG)
