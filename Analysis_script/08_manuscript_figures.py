"""Manuscript figures for 'Do large language models respond like human participants?
Evidence from social attitude and mental-state measures in two languages'.

Run from the repository root (no arguments needed):

    python3 Analysis_script/08_manuscript_figures.py

Reads
    Results/SDO_VSA_Results_20260830.csv                      attitude data (August 2026)
    Results/RMET_Results_20260912.csv                         RMET data (September 2026)
    Analysis_output/Reframing_checks/R3_agreement_by_model.csv  per-model Krippendorff's alpha
                                                               (written by 04_reframing_checks.R)
Writes
    Manuscript_figures/Fig1_study_design.{png,pdf}
    Manuscript_figures/Fig2_language.{png,pdf}            panel B as dots (one per run)
    Manuscript_figures/Fig2_language_bar.{png,pdf}        panel B as bars (mean) + range
    Manuscript_figures/Fig2_language_boxplot.{png,pdf}    panel B as boxplots
    Manuscript_figures/Fig2_language_simple.{png,pdf}     plain-language version (A, example B, all models C)
    Manuscript_figures/Fig3_answer_changes.{png,pdf}
    Manuscript_figures/FigICC_scale_scores.{png,pdf}      ICC explained: values, why high, what it hides
    Manuscript_figures/Fig4_rmet.{png,pdf}
    Manuscript_figures/source_data/*.csv                      the numbers plotted in each panel

PNG files are 600 dpi; PDF files are vector. The script also prints the numbers quoted in the
manuscript text so they can be checked against the figures.

Optional arguments: python3 08_manuscript_figures.py <repository folder> <output folder>

Requires Python 3.8+ with pandas, numpy and matplotlib (see requirements.txt).
"""
import os
import sys

import numpy as np
import pandas as pd
import matplotlib as mpl
mpl.use("Agg")
import matplotlib.pyplot as plt
from matplotlib import font_manager
from matplotlib.lines import Line2D
from matplotlib.patches import FancyArrowPatch, FancyBboxPatch, Rectangle

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = sys.argv[1] if len(sys.argv) > 1 else os.path.dirname(HERE)
OUT = sys.argv[2] if len(sys.argv) > 2 else os.path.join(ROOT, "Manuscript_figures")
SRC = os.path.join(OUT, "source_data")
os.makedirs(SRC, exist_ok=True)

EXCLUDED = "databricks-claude-sonnet-4"   # every call failed in both collections


# ---------------------------------------------------------------- style
def pick_font():
    """First installed font from a list of Helvetica-like faces."""
    have = {f.name for f in font_manager.fontManager.ttflist}
    for name in ("TeX Gyre Heros", "Helvetica", "Arial", "Liberation Sans", "DejaVu Sans"):
        if name in have:
            return name
    return "sans-serif"


FONT = pick_font()
mpl.rcParams.update({
    "font.family": FONT, "font.size": 8,
    "axes.titlesize": 8.5, "axes.labelsize": 8,
    "xtick.labelsize": 7.5, "ytick.labelsize": 7.5, "legend.fontsize": 7.5,
    "axes.edgecolor": "#b9b8b0", "axes.linewidth": 0.6,
    "xtick.color": "#52514e", "ytick.color": "#52514e",
    "xtick.major.width": 0.6, "ytick.major.width": 0.6,
    "xtick.major.size": 2.5, "ytick.major.size": 2.5,
    "axes.labelcolor": "#1a1a1a", "text.color": "#1a1a1a",
    "mathtext.fontset": "custom", "mathtext.rm": FONT,
    "mathtext.it": f"{FONT}:italic", "mathtext.bf": f"{FONT}:bold",
    "pdf.fonttype": 42, "savefig.dpi": 600,
})
INK, SEC, MUTED = "#1a1a1a", "#52514e", "#898781"
GRID, AXIS, RULE = "#ebeae4", "#b9b8b0", "#8f8d86"
BLUE, ORANGE, GREY = "#2a78d6", "#eb6834", "#bdbbb3"
TINT_BLUE, TINT_ORANGE = "#eef4fc", "#fdeee6"
# diverging ramp for the 1-7 SDO scale: blue = egalitarian, red = hierarchy-endorsing
RAMP = ["#1c5cab", "#5598e7", "#b7d3f6", "#dcdbd5", "#f6c3bb", "#e8776f", "#b42f2e"]


def tidy(ax, grid_axis="y"):
    for s in ("top", "right"):
        ax.spines[s].set_visible(False)
    if grid_axis:
        ax.grid(axis=grid_axis, color=GRID, linewidth=0.6, zorder=0)
        ax.set_axisbelow(True)


def title(ax, letter, text):
    ax.set_title(f"{letter}   {text}" if letter else text, loc="left",
                 fontweight="bold", fontsize=8.5, pad=8, color=INK)


def save(fig, name):
    for ext in ("png", "pdf"):
        fig.savefig(os.path.join(OUT, f"{name}.{ext}"), bbox_inches="tight",
                    pad_inches=0.04, facecolor="white")
    plt.close(fig)
    print(f"  saved {name}.png and {name}.pdf")


def wilson(k, n, z=1.96):
    """Wilson 95% interval for a proportion."""
    k, n = np.asarray(k, float), np.asarray(n, float)
    p = k / n
    d = 1 + z * z / n
    c = (p + z * z / (2 * n)) / d
    h = z * np.sqrt(p * (1 - p) / n + z * z / (4 * n * n)) / d
    return c - h, c + h


# ---------------------------------------------------------------- data
def read_csv(rel):
    path = os.path.join(ROOT, rel)
    if not os.path.exists(path):
        sys.exit(f"Not found: {path}\nRun this script from the repository root, "
                 "or pass the repository folder as the first argument.")
    df = pd.read_csv(path)
    df.columns = [c.replace("﻿", "") for c in df.columns]
    return df


sv = read_csv("Results/SDO_VSA_Results_20260830.csv")
sv = sv[sv.model_label != EXCLUDED].copy()
raw = pd.to_numeric(sv["value"], errors="coerce")
v = pd.to_numeric(sv["value_recoded"], errors="coerce")       # correct for SDO6 only
is7, isv = sv.instrument.eq("SDO"), sv.instrument.eq("VSA")
rev7 = is7 & sv.question_id.isin(["sdo-q3", "sdo-q4", "sdo-q7", "sdo-q8"])
revv = isv & sv.question_id.isin(["vsa-q1", "vsa-q4", "vsa-q5"])
v[is7] = raw[is7]; v[rev7] = 8 - raw[rev7]                    # SDO7(s): items 3, 4, 7, 8 reversed
v[isv] = raw[isv]; v[revv] = -raw[revv]                       # VSA: items 1, 4, 5 sign-reversed
sv["v"] = v
sv["inst"] = np.where(sv.instrument.eq("SDO6"), "SDO6-" + sv.language.str.upper(),
                      np.where(is7, "SDO7s", "VSA"))

# SDO6 answer distribution by language (Fig. 2a)
s6 = sv[sv.instrument.eq("SDO6")].dropna(subset=["v"])
dist = (s6.groupby("language").v.value_counts(normalize=True).unstack().fillna(0) * 100)
dist = dist.reindex(columns=[1, 2, 3, 4, 5, 6, 7], fill_value=0)

# Each model's Chinese-minus-English SDO6 score in each run (Fig. 2b)
run = s6.groupby(["model_label", "repeat", "language"]).v.mean().unstack()
run["diff"] = (run["zh"] - run["en"]).round(10)
runs = run["diff"].unstack()                                   # models x 5 runs
lang = pd.DataFrame({"mean": runs.mean(axis=1), "lo": runs.min(axis=1), "hi": runs.max(axis=1)})
lang["reversed"] = (runs.gt(0).any(axis=1) & runs.lt(0).any(axis=1))
lang = lang.sort_values("mean")

# Model-item pairs with five scored answers (Fig. 3b)
pairs = []
for (m, inst, q), g in sv.groupby(["model_label", "inst", "question_id"]):
    vals = g.sort_values("repeat").v.values
    if len(vals) < 5 or np.isnan(vals).any():
        continue
    pairs.append(dict(model=m, inst=inst, question_id=q, first=vals[0],
                      changed_any=int(len(set(vals)) > 1),          # any change across all five runs
                      changed_later=int(len(set(vals[1:])) > 1)))   # change among runs 2-5 only
pairs = pd.DataFrame(pairs)
sdo_pairs = pairs[pairs.inst != "VSA"].copy()
sdo_pairs["first_answer"] = pd.cut(sdo_pairs["first"], [0, 1, 2, 3, 4, 7],
                                   labels=["1", "2", "3", "4", "5–7"])
# The first run only classifies each pair; changes are counted among the four LATER runs, so
# an unusual first answer cannot create a 'change' by regression to the mean.
bycat = sdo_pairs.groupby("first_answer", observed=False).agg(
    n=("changed_later", "size"), changed=("changed_later", "sum"),
    changed_any5=("changed_any", "sum"))

# Per-model agreement across runs (Fig. 3a), from 04_reframing_checks.R
agree = read_csv("Analysis_output/Reframing_checks/R3_agreement_by_model.csv")
agree = agree.dropna(subset=["alpha_interval"])

# RMET accuracy and answer changes by model (Fig. 4)
rm = read_csv("Results/RMET_Results_20260912.csv")
rm = rm[rm.model_label != EXCLUDED]
rm = rm[rm["choice"].notna() & rm["choice"].astype(str).str.len().gt(0)].copy()
rm["correct"] = rm["is_correct"].map({True: 1, False: 0, "True": 1, "False": 0}).astype(float)
cell = rm.groupby(["model_label", "question_id", "definitions_shown"])
rmet = pd.DataFrame({
    "accuracy_pct": cell["correct"].mean().groupby("model_label").mean() * 100,
    "pairs_changed_pct": cell["choice"].nunique().gt(1).groupby("model_label").mean() * 100,
})
rho = rmet["accuracy_pct"].corr(rmet["pairs_changed_pct"], method="spearman")
HUMAN_RMET = 26.2   # general-population adult mean, Baron-Cohen et al. (2001), Table 3


# ---------------------------------------------------------------- Fig 1: design
def fig1():
    fig = plt.figure(figsize=(7.2, 3.0))
    ax = fig.add_axes([0, 0, 1, 1]); ax.set_xlim(0, 100); ax.set_ylim(0, 41.7); ax.axis("off")

    def card(x, y, w, h, face=TINT_BLUE):
        ax.add_patch(FancyBboxPatch((x, y), w, h, boxstyle="round,pad=0,rounding_size=1.2",
                                    facecolor=face, edgecolor="none"))

    def head(x, num, text):
        ax.text(x, 38.6, num, fontsize=9, fontweight="bold", color=BLUE, va="center")
        ax.text(x + 2.6, 38.6, text, fontsize=9, fontweight="bold", color=INK, va="center")

    c1, w1 = 2.0, 33.0
    c2, w2 = 40.0, 27.0
    c3, w3 = 72.0, 26.0
    head(c1, "1", "Measures"); head(c2, "2", "Administration"); head(c3, "3", "Research questions")

    top, h, gap = 35.2, 7.0, 1.4
    measures = [
        ("Social dominance orientation", r"SDO$_7$(s) · 8 items · English"),
        ("Social dominance orientation", r"SDO$_6$ · 16 items · English and Chinese"),
        ("Authoritarianism", "VSA · 6 items · English"),
        ("Mental-state attribution", "RMET · 36 items · with and without definitions"),
    ]
    for i, (name, detail) in enumerate(measures):
        y = top - h - i * (h + gap)
        card(c1, y, w1, h)
        ax.text(c1 + 1.6, y + h - 2.3, name, fontsize=8, fontweight="bold", va="center")
        ax.text(c1 + 1.6, y + 2.2, detail, fontsize=7.2, color=SEC, va="center")

    y_bot = top - 4 * h - 3 * gap
    card(c2, y_bot, w2, top - y_bot)
    lines = [("27 LLMs from 5 developers", True), ("(21 for the RMET)", False), ("", False),
             ("One item per request", False), ("No memory of earlier answers", False),
             ("Provider default settings", False)]
    yy = top - 3.0
    for text, bold in lines:
        if text:
            ax.text(c2 + 1.6, yy, text, fontsize=8 if bold else 7.4,
                    fontweight="bold" if bold else "normal", color=INK if bold else SEC, va="center")
        yy -= 2.7
    ax.text(c2 + 1.6, 14.0, "Each item asked five times", fontsize=7.6, fontweight="bold", va="center")
    answers = ["2", "2", "3", "2", "2"]
    bw, bgap = 3.9, 1.05
    x0 = c2 + (w2 - (5 * bw + 4 * bgap)) / 2
    for k, a in enumerate(answers):
        x = x0 + k * (bw + bgap)
        changed = k == 2
        ax.add_patch(FancyBboxPatch((x, 6.6), bw, 4.0, boxstyle="round,pad=0,rounding_size=0.6",
                                    facecolor=ORANGE if changed else "white",
                                    edgecolor="none" if changed else "#c9d9ee", linewidth=0.8))
        ax.text(x + bw / 2, 8.6, a, ha="center", va="center", fontsize=8.5, fontweight="bold",
                color="white" if changed else INK)
        ax.text(x + bw / 2, 11.5, f"Run {k + 1}", ha="center", va="center", fontsize=6.4, color=SEC)
    ax.text(c2 + w2 / 2, 4.6, "Illustrative answers to one item", ha="center",
            va="center", fontsize=6.4, color=MUTED, style="italic")

    questions = [
        ("RQ1", "Human-like?", "Average scores vs human norms"),
        ("RQ2", "Language?", "English vs Chinese answers"),
        ("RQ3", "Stable?", "Same answer, same conclusion?"),
        ("RQ4", "What do scores mean?", "Accuracy, consistency, item pattern"),
    ]
    for i, (rq, name, detail) in enumerate(questions):
        y = top - h - i * (h + gap)
        card(c3, y, w3, h)
        ax.text(c3 + 1.6, y + h - 2.3, rq, fontsize=7.2, fontweight="bold", color=BLUE, va="center")
        ax.text(c3 + 6.6, y + h - 2.3, name, fontsize=8, fontweight="bold", va="center")
        ax.text(c3 + 1.6, y + 2.2, detail, fontsize=7.2, color=SEC, va="center")

    ymid = (top + y_bot) / 2
    for xa, xb in ((c1 + w1 + 0.8, c2 - 0.8), (c2 + w2 + 0.8, c3 - 0.8)):
        ax.add_patch(FancyArrowPatch((xa, ymid), (xb, ymid), arrowstyle="-|>", mutation_scale=9,
                                     color=RULE, linewidth=1.0))
    save(fig, "Fig1_study_design")


# ---------------------------------------------------------------- Fig 2: language
# Display names, matching MODEL_LOOKUP in 02_statistical_analysis.R
DISPLAY = {
    "gpt-4.1": "GPT-4.1", "gpt-4o": "GPT-4o", "gpt-5": "GPT-5", "gpt-5.1": "GPT-5.1",
    "gpt-5.2": "GPT-5.2", "gpt-5.4": "GPT-5.4", "gpt-5.5": "GPT-5.5", "gpt-5.6-sol": "GPT-5.6-sol",
    "gpt-oss-20b": "GPT-OSS 20B", "gpt-oss-120b": "GPT-OSS 120B",
    "claude-fable-5": "Claude Fable 5", "claude-haiku-4-5": "Claude Haiku 4.5",
    "claude-opus-4-1": "Claude Opus 4.1", "claude-opus-4-5": "Claude Opus 4.5",
    "claude-opus-4-6": "Claude Opus 4.6", "claude-opus-4-7": "Claude Opus 4.7",
    "claude-opus-4-8": "Claude Opus 4.8", "claude-opus-5": "Claude Opus 5",
    "claude-sonnet-4-5": "Claude Sonnet 4.5", "claude-sonnet-4-6": "Claude Sonnet 4.6",
    "claude-sonnet-5": "Claude Sonnet 5", "gemma-3-12b": "Gemma 3 12B",
    "meta-llama-3-1-8b-instruct": "Llama 3.1 8B", "meta-llama-3-3-70b-instruct": "Llama 3.3 70B",
    "llama-4-maverick": "Llama 4 Maverick", "qwen3-next-80b-a3b-instruct": "Qwen3-Next 80B",
    "qwen35-122b-a10b": "Qwen3.5 122B",
}


def display_name(label):
    short = label.split("-", 1)[1] if label.split("-", 1)[0] in ("azure", "databricks") else label
    return DISPLAY.get(short, short)


def panel_letter(fig, ax, letter, x=0.0):
    """Bold panel letter at the figure's left edge, level with the top of the panel."""
    fig.text(x, ax.get_position().y1 + 0.012, letter, fontsize=11, fontweight="bold",
             ha="left", va="bottom", color=INK)


def _fig2_panel_a(a):
    """Share of all SDO6 answers at each scale point, English vs Chinese."""
    BAR_H = 0.40
    ys = {"en": 1.0, "zh": 0.45}
    for langc, yy in ys.items():
        left = 0.0
        for k in range(7):
            w = dist.loc[langc, k + 1]
            a.barh(yy, w, left=left, height=BAR_H, color=RAMP[k], edgecolor="white",
                   linewidth=0.8, zorder=2)
            if k < 2:
                a.text(left + w / 2, yy, f"{w:.0f}%", ha="center", va="center", fontsize=8,
                       fontweight="bold", color="white" if k == 0 else INK, zorder=3)
            elif k < 4 and w >= 3:
                a.text(left + w / 2, yy, f"{w:.0f}%", ha="center", va="center", fontsize=6.6,
                       color=INK, zorder=3)
            left += w
    a.set_yticks([ys["en"], ys["zh"]], ["English", "Chinese"], fontsize=8, color=INK)
    a.tick_params(axis="y", length=0)
    a.set_xlim(0, 100); a.set_ylim(-0.42, 1.24)
    a.set_xticks([])
    for s_ in ("top", "right", "bottom", "left"):
        a.spines[s_].set_visible(False)
    # key for the 1–7 scale, directly under the bars
    ky, kh, kw, kgap = -0.17, 0.17, 3.2, 0.45
    kx0 = 15.5
    a.text(kx0 - 1.4, ky, "Most egalitarian", ha="right", va="center", fontsize=7, color=SEC)
    for k in range(7):
        x = kx0 + k * (kw + kgap)
        a.add_patch(Rectangle((x, ky - kh / 2), kw, kh, facecolor=RAMP[k], edgecolor="none",
                              clip_on=False))
        a.text(x + kw / 2, ky, str(k + 1), ha="center", va="center", fontsize=6.6,
               fontweight="bold", color="white" if k in (0, 6) else INK)
    kx_end = kx0 + 7 * kw + 6 * kgap
    a.text(kx_end + 1.4, ky, "Most hierarchy-endorsing", ha="left", va="center", fontsize=7,
           color=SEC)


CONS, REV = "#77756f", ORANGE          # same direction in every run / direction changed
BAR_GREY = "#a7a59e"


def _fig2_b_axes(b, order):
    """Shared layout for panel B: model rows, zero line, average line, axis labels."""
    n = len(order)
    m_all = lang["mean"].mean()
    b.axvline(0, color=RULE, linewidth=0.9, zorder=1)
    b.axvline(m_all, color=SEC, linewidth=0.7, ls=(0, (3, 2)), zorder=1)
    b.text(m_all + 0.025, 0.0, f"Average of all {n} models: +{m_all:.2f}", ha="left",
           va="center", fontsize=7, color=SEC)
    b.set_yticks(range(n), [display_name(m) for m in order], fontsize=7)
    for t, rev in zip(b.get_yticklabels(), lang.loc[order, "reversed"].values):
        t.set_color(REV if rev else INK)
        if rev:
            t.set_fontweight("bold")
    b.tick_params(axis="y", length=0, pad=3)
    b.set_ylim(-0.7, n - 0.3)
    b.set_xlim(-0.45, 1.8)
    b.set_xticks([-0.25, 0, 0.25, 0.5, 0.75, 1.0, 1.25, 1.5, 1.75],
                 ["−0.25", "0", "0.25", "0.50", "0.75", "1.00", "1.25", "1.50", "1.75"])
    b.set_xlabel(r"Chinese minus English SDO$_6$ score (points on the 1–7 scale)", labelpad=15)
    tidy(b, grid_axis="x")
    b.spines["left"].set_visible(False)
    tf = b.get_xaxis_transform()
    b.text(-0.03, -0.075, "← Higher in English", transform=tf, ha="right", va="top",
           fontsize=7, color=SEC)
    b.text(0.03, -0.075, "Higher in Chinese →", transform=tf, ha="left", va="top",
           fontsize=7, color=SEC)


def _fig2_legend(b, handles, labels, n):
    b.legend(handles, labels, loc="lower right", bbox_to_anchor=(1.0, (4.65 + 0.7) / (n + 0.4)),
             frameon=True, facecolor="white", edgecolor="none", framealpha=1,
             handlelength=1.6, borderaxespad=0.3, fontsize=7)


def _spread(vals, step=0.17):
    """Vertical offsets that separate runs with identical values."""
    offs = np.zeros(len(vals))
    for val in np.unique(vals):
        idx = np.where(np.isclose(vals, val))[0]
        if len(idx) > 1:
            offs[idx] = (np.arange(len(idx)) - (len(idx) - 1) / 2) * step
    return offs


def _fig2_b_dots(b):
    """Every run as a dot, the range as a line, the mean as a tick."""
    order = list(lang.index)
    for i, m in enumerate(order):
        r = lang.loc[m]
        col = REV if r.reversed else CONS
        if r.reversed:
            b.axhspan(i - 0.5, i + 0.5, color=TINT_ORANGE, lw=0, zorder=0)
        b.hlines(i, r.lo, r.hi, color=col, linewidth=1.0, alpha=0.7, zorder=2)
        vals = runs.loc[m].values.astype(float)
        b.scatter(vals, i + _spread(vals), s=10, color=col, edgecolor="white", linewidth=0.35,
                  zorder=3)
        b.scatter([r["mean"]], [i], marker="|", s=55, color=INK, linewidths=1.3, zorder=4)
    _fig2_b_axes(b, order)
    _fig2_legend(b, [
        Line2D([0], [0], ls="none", marker="o", ms=3.6, color=CONS, mec="white", mew=0.35),
        Line2D([0], [0], color=CONS, lw=1.0, alpha=0.7),
        Line2D([0], [0], ls="none", marker="|", ms=7, mew=1.3, color=INK),
        Line2D([0], [0], ls="none", marker="s", ms=7, color=TINT_ORANGE, mec=ORANGE, mew=0.6),
    ], ["One run", "Range across the five runs", "Mean of the five runs",
        "Direction changed between runs"], len(order))


def _fig2_b_bar(b):
    """Mean of the five runs as a bar, lowest-to-highest run as an error bar."""
    order = list(lang.index)
    for i, m in enumerate(order):
        r = lang.loc[m]
        b.barh(i, r["mean"], height=0.64, color=REV if r.reversed else BAR_GREY,
               edgecolor="none", zorder=2)
        b.errorbar(r["mean"], i, xerr=[[r["mean"] - r.lo], [r.hi - r["mean"]]], fmt="none",
                   ecolor=INK, elinewidth=0.8, capsize=2.2, capthick=0.8, zorder=3)
    _fig2_b_axes(b, order)
    _fig2_legend(b, [
        Rectangle((0, 0), 1, 1, facecolor=BAR_GREY, edgecolor="none"),
        Rectangle((0, 0), 1, 1, facecolor=REV, edgecolor="none"),
        Line2D([0], [0], color=INK, lw=0.8, marker="|", ms=5, mew=0.8),
    ], ["Mean of five runs: same direction in every run",
        "Mean of five runs: direction changed between runs",
        "Lowest to highest of the five runs"], len(order))


def _fig2_b_box(b):
    """Boxplot of the five runs per model; whiskers reach the lowest and highest run."""
    med = runs.median(axis=1)
    order = list(lang.assign(med=med).sort_values(["med", "mean"]).index)
    data = [runs.loc[m].values.astype(float) for m in order]
    try:
        bp = b.boxplot(data, positions=range(len(order)), orientation="horizontal",
                       widths=0.62, whis=(0, 100), showfliers=False, patch_artist=True)
    except TypeError:                                   # matplotlib < 3.10
        bp = b.boxplot(data, positions=range(len(order)), vert=False,
                       widths=0.62, whis=(0, 100), showfliers=False, patch_artist=True)
    for i, m in enumerate(order):
        rev = lang.loc[m, "reversed"]
        col = REV if rev else CONS
        bp["boxes"][i].set(facecolor=TINT_ORANGE if rev else "#f1f0eb", edgecolor=col,
                           linewidth=0.8, zorder=2)
        for part in ("whiskers", "caps"):
            for ln in bp[part][2 * i: 2 * i + 2]:
                ln.set(color=col, linewidth=0.8, zorder=2)
        bp["medians"][i].set(color=INK, linewidth=1.3, zorder=4)
        vals = data[i]
        b.scatter(vals, i + _spread(vals, 0.13), s=6, color=col, edgecolor="white",
                  linewidth=0.3, zorder=3)
    _fig2_b_axes(b, order)
    b.set_yticks(range(len(order)), [display_name(m) for m in order], fontsize=7)
    for t, m in zip(b.get_yticklabels(), order):
        rev = lang.loc[m, "reversed"]
        t.set_color(REV if rev else INK)
        t.set_fontweight("bold" if rev else "normal")
    _fig2_legend(b, [
        Rectangle((0, 0), 1, 1, facecolor="#f1f0eb", edgecolor=CONS, linewidth=0.8),
        Line2D([0], [0], color=INK, lw=1.3),
        Line2D([0], [0], color=CONS, lw=0.8),
        Line2D([0], [0], ls="none", marker="o", ms=3, color=CONS, mec="white", mew=0.3),
        Rectangle((0, 0), 1, 1, facecolor=TINT_ORANGE, edgecolor=REV, linewidth=0.8),
    ], ["Middle three of the five runs", "Median run", "Lowest to highest run", "One run",
        "Direction changed between runs"], len(order))


def fig2():
    """Fig. 2 in three versions that differ only in panel B (dots, bar, boxplot)."""
    for variant, draw, name in (("dots", _fig2_b_dots, "Fig2_language"),
                                ("bar", _fig2_b_bar, "Fig2_language_bar"),
                                ("box", _fig2_b_box, "Fig2_language_boxplot")):
        fig = plt.figure(figsize=(7.2, 5.7))
        gs = fig.add_gridspec(2, 1, height_ratios=[0.95, 3.6], hspace=0.20)
        a, b = fig.add_subplot(gs[0]), fig.add_subplot(gs[1])
        _fig2_panel_a(a)
        draw(b)
        panel_letter(fig, a, "A"); panel_letter(fig, b, "B")
        save(fig, name)

    d = dist.copy(); d.index = d.index.map({"en": "English", "zh": "Chinese"})
    d.columns = [f"pct_answer_{c}" for c in d.columns]
    d.round(2).to_csv(os.path.join(SRC, "Fig2a_answer_distribution.csv"), index_label="language")
    r = runs.copy(); r.columns = [f"run_{int(c) + 1}" for c in r.columns]
    r = r.join(lang[["mean", "reversed"]]).sort_values("mean")
    r.insert(r.columns.get_loc("mean") + 1, "median", runs.median(axis=1).reindex(r.index))
    r.insert(0, "display", [display_name(m) for m in r.index])
    r.round(4).to_csv(os.path.join(SRC, "Fig2b_chinese_minus_english_by_run.csv"), index_label="model")


# ---------------------------------------------------------------- Fig 2, plain-language version
from decimal import Decimal, ROUND_HALF_UP


def signed2(x):
    """+0.63 / −0.06: two decimals, halves rounded up, typographic minus."""
    q = Decimal(str(abs(x))).quantize(Decimal("0.01"), rounding=ROUND_HALF_UP)
    return ("" if q == 0 else ("+" if x > 0 else "−")) + str(q)


FIG2_EXAMPLE = ("azure-gpt-5", "GPT-5")
EN_COL, ZH_COL = "#3d4b5c", "#18a091"
CAT_ORDER = ["always", "mostly", "switched", "english"]
CAT_LABEL = {"always": "Higher in Chinese in every run",
             "mostly": "Higher in Chinese or equal in every run",
             "switched": "Direction switched between runs",
             "english": "Higher in English or equal in every run"}
CAT_FACE = {"always": "#4f5d6e", "mostly": "#a3adb8", "switched": ORANGE, "english": "#dcdbd5"}
CAT_TEXT = {"always": "white", "mostly": INK, "switched": "white", "english": INK}


def _fig2_categories():
    """Classify each model by the sign of its Chinese-minus-English difference across runs."""
    pos, neg = runs.gt(0).any(axis=1), runs.lt(0).any(axis=1)
    cat = pd.Series("always", index=runs.index)
    cat[pos & neg] = "switched"
    cat[~neg & runs.eq(0).any(axis=1)] = "mostly"
    cat[~pos] = "english"
    return cat


def fmt2(x):
    """Two decimals, halves rounded up."""
    return str(Decimal(str(x)).quantize(Decimal("0.01"), rounding=ROUND_HALF_UP))


def _fig2_panel_example(ax):
    """One model: its English and Chinese average (16 items) in each run, and the subtraction."""
    m, name = FIG2_EXAMPLE
    r = run.loc[m].sort_index()
    n = len(r)
    ys = [n + 0.4 - k for k in range(n)]                 # runs, top to bottom
    y_mean = 0.0
    rows = [(f"Run {k + 1}", y, row["en"], row["zh"], False) for k, (y, (_, row))
            in enumerate(zip(ys, r.iterrows()))]
    rows.append(("Average of\n5 runs", y_mean, r["en"].mean(), r["zh"].mean(), True))
    tcol = ax.get_yaxis_transform()                      # x in axes units, y in data units
    for label, y, en, zh, is_mean in rows:
        dlt = zh - en
        flip = dlt < 0
        if flip:
            ax.axhspan(y - 0.45, y + 0.45, xmin=0, xmax=1, color=TINT_ORANGE, lw=0, zorder=0)
        ax.annotate("", xy=(zh, y), xytext=(en, y),
                    arrowprops=dict(arrowstyle="-|>", color=ORANGE if flip else "#9a988f",
                                    lw=1.2 if is_mean else 1.0, mutation_scale=7,
                                    shrinkA=3.5, shrinkB=3.5), zorder=2)
        size = 42 if is_mean else 32
        ax.scatter([en], [y], s=size, color=EN_COL, edgecolor="white", linewidth=0.6, zorder=3)
        ax.scatter([zh], [y], s=size, color=ZH_COL, edgecolor="white", linewidth=0.6, zorder=3)
        col = ORANGE if flip else INK
        ax.text(1.03, y, f"{fmt2(zh)} − {fmt2(en)}", transform=tcol, ha="left", va="center",
                fontsize=7, color=SEC, clip_on=False)
        ax.text(1.36, y, f"= {signed2(dlt)}", transform=tcol, ha="right", va="center",
                fontsize=7.4, fontweight="bold", color=col, clip_on=False)
    ax.axhline(0.85, xmin=0, xmax=1, color=AXIS, lw=0.6)
    ax.plot([1.02, 1.36], [0.85, 0.85], transform=tcol, color=AXIS, lw=0.6, clip_on=False)
    ax.text(1.03, n + 1.05, "Chinese − English", transform=tcol, ha="left", va="bottom",
            fontsize=6.8, color=SEC, clip_on=False)
    ax.set_yticks([y for _, y, *_ in rows], [lab for lab, *_ in rows], fontsize=7.2)
    for t, (_, _, _, _, is_mean) in zip(ax.get_yticklabels(), rows):
        t.set_fontweight("bold" if is_mean else "normal")
    ax.tick_params(axis="y", length=0)
    ax.set_xlim(1.0, 2.05); ax.set_ylim(-0.6, n + 1.9)
    ax.set_xticks([1.0, 1.25, 1.5, 1.75, 2.0], ["1.00", "1.25", "1.50", "1.75", "2.00"])
    ax.set_xlabel(r"Average of the 16 SDO$_6$ answers (1 = most egalitarian)")
    tidy(ax, grid_axis="x"); ax.spines["left"].set_visible(False)
    ax.text(0.0, 1.075, f"Example: {name}", transform=ax.transAxes, ha="left", va="bottom",
            fontsize=7.4, fontweight="bold", color=INK)
    ax.text(0.0, 1.02, "In each run, the model answered all 16 items once, in each language",
            transform=ax.transAxes, ha="left", va="bottom", fontsize=6.7, color=SEC)
    ax.legend([Line2D([0], [0], ls="none", marker="o", ms=5, color=EN_COL, mec="white"),
               Line2D([0], [0], ls="none", marker="o", ms=5, color=ZH_COL, mec="white")],
              ["English average", "Chinese average"], loc="upper left",
              bbox_to_anchor=(0.0, 1.0), ncol=2, frameon=False, handletextpad=0.2,
              columnspacing=1.0, fontsize=6.9, borderaxespad=0.1)


# Model families, each ordered by version number (older to newer)
FAMILIES = [
    ("OpenAI GPT-4", ["gpt-4o", "gpt-4.1"]),
    ("OpenAI GPT-5", ["gpt-5", "gpt-5.1", "gpt-5.2", "gpt-5.4", "gpt-5.5", "gpt-5.6-sol"]),
    ("OpenAI GPT-OSS", ["gpt-oss-20b", "gpt-oss-120b"]),
    ("Anthropic Claude 4", ["claude-opus-4-1", "claude-sonnet-4-5", "claude-haiku-4-5",
                            "claude-opus-4-5", "claude-opus-4-6", "claude-sonnet-4-6",
                            "claude-opus-4-7", "claude-opus-4-8"]),
    ("Anthropic Claude 5", ["claude-opus-5", "claude-sonnet-5", "claude-fable-5"]),
    ("Google Gemma", ["gemma-3-12b"]),
    ("Meta Llama", ["meta-llama-3-1-8b-instruct", "meta-llama-3-3-70b-instruct",
                    "llama-4-maverick"]),
    ("Alibaba Qwen", ["qwen3-next-80b-a3b-instruct", "qwen35-122b-a10b"]),
]


def _short(label):
    return label.split("-", 1)[1] if label.split("-", 1)[0] in ("azure", "databricks") else label


def _fig2_panel_tiles(ax):
    """All 27 models as tiles, one row per family, coloured by the sign of the difference."""
    cat = _fig2_categories()
    by_short = {_short(m): m for m in cat.index}
    placed = [s_ for _, ms in FAMILIES for s_ in ms]
    missing = sorted(set(by_short) - set(placed))
    fams = FAMILIES + ([("Other", missing)] if missing else [])
    ncol = max(len(ms) for _, ms in fams)
    w, h, gx, gy = 1.0, 0.78, 0.06, 0.22
    for r_, (fam, members) in enumerate(fams):
        y = (len(fams) - 1 - r_) * (h + gy)
        ax.text(-0.12, y + h / 2, fam, ha="right", va="center", fontsize=7, color=INK)
        for c_, s_ in enumerate([s_ for s_ in members if s_ in by_short]):
            m = by_short[s_]
            x = c_ * (w + gx)
            cc = cat[m]
            is_ex = m == FIG2_EXAMPLE[0]
            ax.add_patch(FancyBboxPatch((x, y), w, h, boxstyle="round,pad=0,rounding_size=0.06",
                                        facecolor=CAT_FACE[cc], edgecolor=INK if is_ex else "none",
                                        linewidth=1.4 if is_ex else 0))
            label = display_name(m)
            if fam.startswith("Anthropic Claude"):
                label = label.replace("Claude ", "")        # family already named on the left
            ax.text(x + w / 2, y + h / 2, label + (" (B)" if is_ex else ""),
                    ha="center", va="center", fontsize=6.2 if len(label) <= 13 else 5.6,
                    color=CAT_TEXT[cc],
                    fontweight="bold" if cc == "switched" else "normal")
    ax.set_xlim(-0.02, ncol * (w + gx)); ax.set_ylim(-0.05, len(fams) * (h + gy))
    ax.axis("off")
    ax.annotate("", xy=(ncol * (w + gx) - 0.1, len(fams) * (h + gy) + 0.05),
                xytext=(0.0, len(fams) * (h + gy) + 0.05),
                arrowprops=dict(arrowstyle="-|>", color=MUTED, lw=0.7, mutation_scale=7),
                annotation_clip=False)
    ax.text(ncol * (w + gx) / 2, len(fams) * (h + gy) + 0.2, "Older → newer version within each family",
            ha="center", va="bottom", fontsize=6.7, color=SEC)
    ax.text(0.0, 1.16, f"All {len(cat)} models: sign of the Chinese − English difference in each of the 5 runs",
            transform=ax.transAxes, ha="left", va="bottom", fontsize=7.4, fontweight="bold",
            color=INK)
    counts = cat.value_counts()
    handles = [Rectangle((0, 0), 1, 1, facecolor=CAT_FACE[c], edgecolor="none") for c in CAT_ORDER]
    labels = [f"{CAT_LABEL[c]} ({counts.get(c, 0)})" for c in CAT_ORDER]
    ax.legend(handles, labels, loc="upper left", bbox_to_anchor=(0.0, -0.02), ncol=2,
              frameon=False, handlelength=1.1, handleheight=0.9, fontsize=7, labelspacing=0.35,
              columnspacing=2.0, borderaxespad=0.0)


def fig2_simple():
    """Fig. 2 for a general audience: answers in each language, one example, all models."""
    fig = plt.figure(figsize=(7.2, 7.4))
    a = fig.add_axes([0.11, 0.85, 0.80, 0.12])
    b = fig.add_axes([0.14, 0.445, 0.46, 0.30])
    c = fig.add_axes([0.19, 0.08, 0.80, 0.22])
    _fig2_panel_a(a)
    _fig2_panel_example(b)
    _fig2_panel_tiles(c)
    for letter, x, y in (("A", 0.0, 0.975), ("B", 0.0, 0.80), ("C", 0.0, 0.35)):
        fig.text(x, y, letter, fontsize=11, fontweight="bold", ha="left", va="bottom", color=INK)
    save(fig, "Fig2_language_simple")
    m, _ = FIG2_EXAMPLE
    ex = run.loc[m].sort_index()[["en", "zh"]].rename(columns={"en": "english", "zh": "chinese"})
    ex["chinese_minus_english"] = ex["chinese"] - ex["english"]
    ex.index = [f"run_{int(k) + 1}" for k in ex.index]
    ex.round(4).to_csv(os.path.join(SRC, "Fig2B_simple_example_gpt5.csv"), index_label="run")
    pd.DataFrame({"display": [display_name(i) for i in runs.index],
                  "category": _fig2_categories().map(CAT_LABEL)}, index=runs.index).to_csv(
        os.path.join(SRC, "Fig2C_simple_model_categories.csv"), index_label="model")


# ---------------------------------------------------------------- Fig 3: stability
def example_grid(model, inst):
    """Rescored answers of one model on one scale version: items x runs."""
    g = sv[(sv.model_label == model) & (sv.inst == inst)]
    return g.pivot_table(index="question_id", columns="repeat", values="v").sort_index().values


EXAMPLE_MODEL, EXAMPLE_DISPLAY, EXAMPLE_INST = "azure-gpt-5.5", "GPT-5.5", "VSA"
CHANGE_COLS = {"none": "#dcdbd5", "some": "#f6c3bb", "many": ORANGE}


def minus(x):
    """Format a number with a typographic minus sign."""
    s = f"{x:g}"
    return s.replace("-", "−")


def _fig3_panel_a(ax, M, alpha):
    """One real model x scale: the five answers to every item."""
    n_items, n_runs = M.shape
    ax.set_xlim(-0.2, n_runs + 3.2); ax.set_ylim(-2.5, n_items + 1.75)
    ax.axis("off")
    changed = [len(set(row)) > 1 for row in M]
    for i, row in enumerate(M):
        y = n_items - 1 - i
        vals, cnt = np.unique(row, return_counts=True)
        mode = vals[np.argmax(cnt)]
        if changed[i]:
            ax.add_patch(Rectangle((0.15, y - 0.47), n_runs + 3.0, 0.94, facecolor=TINT_ORANGE,
                                   edgecolor="none", zorder=0))
        ax.text(0.95, y, f"Item {i + 1}", ha="right", va="center", fontsize=7.2, color=INK)
        for j, v in enumerate(row):
            x = 1.6 + j
            odd = v != mode
            ax.add_patch(FancyBboxPatch((x - 0.4, y - 0.34), 0.8, 0.68,
                                        boxstyle="round,pad=0,rounding_size=0.08",
                                        facecolor=ORANGE if odd else "white",
                                        edgecolor="none" if odd else "#cfcec7",
                                        linewidth=0.6, zorder=2))
            ax.text(x, y, minus(v), ha="center", va="center", fontsize=7.4, zorder=3,
                    fontweight="bold" if odd else "normal", color="white" if odd else INK)
        ax.text(1.6 + n_runs - 0.25, y, "No: changed" if changed[i] else "Yes",
                ha="left", va="center", fontsize=7, color=ORANGE if changed[i] else MUTED,
                fontweight="bold" if changed[i] else "normal")
    for j in range(n_runs):
        ax.text(1.6 + j, n_items - 0.25, f"Run {j + 1}", ha="center", va="bottom", fontsize=6.8,
                color=SEC)
    ax.text(0.15, n_items + 1.05, f"Example: {EXAMPLE_DISPLAY}, authoritarianism scale (VSA)",
            ha="left", va="bottom", fontsize=7.4, color=INK, fontweight="bold")
    ax.text(0.15, n_items + 0.45, f"In each run, the model answered all {n_items} items once "
            "(scale −4 to +4)", ha="left", va="bottom", fontsize=6.7, color=SEC)
    ax.text(1.6 + n_runs - 0.25, n_items - 0.25, "All 5 the same?", ha="left", va="bottom",
            fontsize=6.4, color=SEC)
    n_ch = int(sum(changed))
    a_txt = f"{alpha:.2f}".replace("0.", ".")
    ax.text(0.15, -1.0, f"{EXAMPLE_DISPLAY}'s reliability score across its 5 runs: "
            f"α = {a_txt} (1 = perfect)", ha="left", va="center",
            fontsize=7.0, color=INK)
    ax.text(0.15, -1.6, f"→ {EXAMPLE_DISPLAY} passes the usual .80 benchmark on this scale",
            ha="left", va="center", fontsize=7.2, color=BLUE, fontweight="bold")
    ax.text(0.15, -2.25, f"Yet {n_ch} of its {n_items} items got a different answer "
            f"in at least one of the 5 runs", ha="left", va="center", fontsize=7.2,
            color=ORANGE, fontweight="bold")


def _fig3_change_groups():
    a = agree.copy()
    a["passed"] = a.alpha_interval >= 0.8
    a["grp"] = np.select([a.pct_changed == 0, a.pct_changed < 20], ["none", "some"], "many")
    tab = pd.crosstab(a.passed, a.grp).reindex(index=[True, False], columns=["none", "some", "many"],
                                                 fill_value=0)
    return a, tab


def _fig3_panel_b(ax):
    """Model x scale tests that passed / did not pass the .80 benchmark, by items changed."""
    _, tab = _fig3_change_groups()
    ys = {True: 1.0, False: 0.0}
    for passed, row in tab.iterrows():
        left = 0
        for grp in ("none", "some", "many"):
            w = int(row[grp])
            if w:
                ax.barh(ys[passed], w, left=left, height=0.56, color=CHANGE_COLS[grp],
                        edgecolor="white", linewidth=0.8, zorder=2)
                ax.text(left + w / 2, ys[passed], str(w), ha="center", va="center", fontsize=7.6,
                        fontweight="bold", color="white" if grp == "many" else INK, zorder=3)
            left += w
    n_pass = int(tab.loc[True].sum()); n_fail = int(tab.loc[False].sum())
    moved = int(tab.loc[True, ["some", "many"]].sum())
    ax.text(n_pass + 1.5, 1.0, f"{moved} of {n_pass}\nstill changed\nat least one\nanswer", ha="left",
            va="center", fontsize=7, color=INK, linespacing=1.15)
    ax.set_yticks([1, 0], [f"Reliability score\n.80 or higher\nn = {n_pass}",
                           f"Reliability score\nbelow .80\nn = {n_fail}"],
                  fontsize=7.2)
    ax.tick_params(axis="y", length=0)
    ax.set_xlim(0, 72); ax.set_ylim(-0.42, 1.42)
    ax.set_xticks([0, 10, 20, 30, 40, 50, 60, 70])
    ax.set_xlabel("Number of model–scale combinations")
    tidy(ax, grid_axis=None); ax.spines["left"].set_visible(False)
    handles = [Rectangle((0, 0), 1, 1, facecolor=CHANGE_COLS[g], edgecolor="none")
               for g in ("none", "some", "many")]
    leg = ax.legend(handles, ["None", "Under 20%", "20% or more"],
                    title="Share of the scale's items whose answer changed across the 5 runs",
                    loc="lower left", bbox_to_anchor=(-0.02, 1.02), ncol=3, frameon=False,
                    handlelength=1.1, handleheight=0.9, borderaxespad=0.0, fontsize=7,
                    columnspacing=1.2, title_fontsize=6.7, alignment="left")
    leg.get_title().set_color(SEC)


def _fig3_panel_c(ax):
    """SDO model x item pairs: did the four later answers differ, by the first answer."""
    labels = ["1  Most egalitarian", "2", "3", "4  Neutral", "5–7  Hierarchy-endorsing"]
    cols = [RAMP[0], RAMP[1], RAMP[2], RAMP[3], RAMP[5]]
    pct = (bycat["changed"] / bycat["n"] * 100).values
    lo, hi = wilson(bycat["changed"].values, bycat["n"].values)
    ys = np.arange(len(pct))[::-1]
    for y, p, l, h, c in zip(ys, pct, lo * 100, hi * 100, cols):
        ax.barh(y, p, height=0.62, color=c, edgecolor="#b9b8b0" if c in (RAMP[2], RAMP[3]) else "none",
                linewidth=0.5, zorder=2)
        ax.plot([l, h], [y, y], color=SEC, lw=0.8, zorder=3)
        for xx in (l, h):
            ax.plot([xx, xx], [y - 0.1, y + 0.1], color=SEC, lw=0.8, zorder=3)
        ax.text(h + 1.5, y, f"{p:.0f}%", ha="left", va="center", fontsize=7.8, fontweight="bold",
                color=INK)
    ns = bycat["n"].values
    ax.set_yticks(ys, [f"{lab}  (n = {n})" for lab, n in zip(labels, ns)], fontsize=7.2)
    ax.tick_params(axis="y", length=0)
    ax.set_xlim(0, 100); ax.set_ylim(-0.6, len(pct) - 0.4)
    ax.set_xticks([0, 25, 50, 75, 100], ["0", "25", "50", "75", "100%"])
    ax.set_xlabel("Items whose next four answers (runs 2–5) were not all the same (%)")
    tidy(ax, grid_axis="x"); ax.spines["left"].set_visible(False)


def _fig3_explainer(ax):
    """Two simple cases showing how to read panel C."""
    ax.set_xlim(0, 7.2); ax.set_ylim(-0.2, 4.6); ax.axis("off")
    ax.text(0.0, 4.35, "How to read the bars", ha="left", va="center", fontsize=7.2,
            fontweight="bold", color=INK)
    ax.text(0.0, 3.8, "1  Sort each item by the first answer", ha="left", va="center",
            fontsize=6.6, color=SEC)
    ax.text(0.0, 3.35, "2  Check: were the next four answers all the same?", ha="left",
            va="center", fontsize=6.6, color=SEC)
    ax.text(0.55, 2.65, "First", ha="center", va="center", fontsize=6.2, color=SEC)
    ax.text(3.05, 2.65, "Next four", ha="center", va="center", fontsize=6.2, color=SEC)
    cases = [(1.85, 1, [1, 1, 1, 1], False), (0.75, 4, [4, 3, 4, 4], True)]
    for y, first, later, ch in cases:
        ax.add_patch(FancyBboxPatch((0.2, y - 0.32), 0.7, 0.64, boxstyle="round,pad=0,rounding_size=0.08",
                                    facecolor="#f1f0eb", edgecolor="#b9b8b0", linewidth=0.6))
        ax.text(0.55, y, str(first), ha="center", va="center", fontsize=7.2, color=INK)
        ax.annotate("", xy=(1.5, y), xytext=(1.0, y),
                    arrowprops=dict(arrowstyle="-|>", color=MUTED, lw=0.7, mutation_scale=6))
        for j, v in enumerate(later):
            odd = v != first
            x = 1.95 + j * 0.75
            ax.add_patch(FancyBboxPatch((x - 0.32, y - 0.32), 0.64, 0.64,
                                        boxstyle="round,pad=0,rounding_size=0.08",
                                        facecolor=ORANGE if odd else "white",
                                        edgecolor="none" if odd else "#cfcec7", linewidth=0.6))
            ax.text(x, y, str(v), ha="center", va="center", fontsize=7.2,
                    color="white" if odd else INK, fontweight="bold" if odd else "normal")
        ax.text(4.85, y, "Changed" if ch else "All the same", ha="left", va="center", fontsize=7,
                color=ORANGE if ch else MUTED, fontweight="bold" if ch else "normal")


def fig3():
    M = example_grid(EXAMPLE_MODEL, EXAMPLE_INST)
    alpha = agree.loc[(agree.display == EXAMPLE_DISPLAY) & (agree.inst_lang == EXAMPLE_INST),
                      "alpha_interval"].iloc[0]
    fig = plt.figure(figsize=(7.2, 6.0))
    a = fig.add_axes([0.00, 0.50, 0.44, 0.44])
    b = fig.add_axes([0.665, 0.565, 0.30, 0.20])
    c = fig.add_axes([0.30, 0.07, 0.40, 0.29])
    e = fig.add_axes([0.745, 0.10, 0.255, 0.27])
    _fig3_panel_a(a, M, alpha)
    _fig3_panel_b(b)
    _fig3_panel_c(c)
    _fig3_explainer(e)
    fig.text(0.515, 0.905, "All models on all scale versions", ha="left", va="bottom",
             fontsize=7.4, fontweight="bold", color=INK)
    fig.text(0.515, 0.88, "Each model on each scale version is counted once: 27 models × 4 versions\n"
             "= 108. Two are not shown: the model gave the same answer to every item\n"
             "in every run, so a reliability score cannot be calculated.", ha="left", va="top",
             fontsize=6.5, color=SEC, linespacing=1.15)
    fig.text(0.0, 0.415, "SDO scales: how often the next four answers changed, by the first answer",
             ha="left", va="bottom", fontsize=7.4, fontweight="bold", color=INK)
    fig.text(0.0, 0.392, "All models pooled; n = number of model–item combinations in each row",
             ha="left", va="top", fontsize=6.7, color=SEC)
    for letter, x, y in (("A", 0.0, 0.955), ("B", 0.49, 0.955), ("C", 0.0, 0.445)):
        fig.text(x, y, letter, fontsize=11, fontweight="bold", ha="left", va="bottom", color=INK)
    save(fig, "Fig3_answer_changes")

    g, tab = _fig3_change_groups()
    cols = [c_ for c_ in ("model_label", "display", "inst_lang", "alpha_interval", "pct_changed")
            if c_ in g.columns]
    g[cols + ["passed", "grp"]].rename(columns={"grp": "items_changed_group"}).to_csv(
        os.path.join(SRC, "Fig3b_passed_vs_items_changed.csv"), index=False)
    pd.DataFrame(M, index=[f"item_{i + 1}" for i in range(M.shape[0])],
                 columns=[f"run_{j + 1}" for j in range(M.shape[1])]).to_csv(
        os.path.join(SRC, "Fig3a_example_gpt55_vsa.csv"), index_label="item")
    out = bycat.copy()
    lo, hi = wilson(bycat["changed"].values, bycat["n"].values)
    out["pct_later_changed"] = (out["changed"] / out["n"] * 100).round(2)
    out["ci_low"], out["ci_high"] = (lo * 100).round(2), (hi * 100).round(2)
    out["pct_any_change_all_five_runs"] = (out["changed_any5"] / out["n"] * 100).round(2)
    out.to_csv(os.path.join(SRC, "Fig3c_change_by_first_answer.csv"), index_label="first_answer")


# ---------------------------------------------------------------- ICC figure (separate)
ICC_PANELS = [("SDO7s", r"SDO$_7$(s), English"),
              ("SDO6-EN", r"SDO$_6$, English"),
              ("SDO6-ZH", r"SDO$_6$, Chinese"),
              ("VSA", "VSA, English")]
ICC_EXAMPLE = "SDO6-EN"
# Usual guideline for ICC values (Koo & Li, 2016)
ICC_ZONES = [(0.0, 0.5, "Poor"), (0.5, 0.75, "Moderate"), (0.75, 0.9, "Good"), (0.9, 1.0, "Excellent")]
ZONE_FACE = ["#f4f3ef", "#ecebe6", "#e0e8f3", "#cfdcef"]


def icc_scores(inst):
    """Each model's full-scale score in each run (items answered in all five runs)."""
    d = sv[sv.inst == inst].dropna(subset=["v"])
    W = d.pivot_table(index=["model_label", "question_id"], columns="repeat", values="v").dropna()
    return W.groupby(level=0).mean()


def icc1(M):
    """ICC(1,1): one-way random effects, single run (as icc_suite in 02_statistical_analysis.R)."""
    n, k = M.shape
    gm, rm = M.mean(), M.mean(axis=1)
    msr = k * ((rm - gm) ** 2).sum() / (n - 1)
    msw = (((M - gm) ** 2).sum() - k * ((rm - gm) ** 2).sum()) / (n * (k - 1))
    return (msr - msw) / (msr + (k - 1) * msw)


def _f2(x):
    return f"{x:.2f}".replace("0.", ".")


def _icc_panel_a(ax, res):
    """ICC of each scale version against the usual guideline bands."""
    n = len(res)
    for (lo, hi, lab), face in zip(ICC_ZONES, ZONE_FACE):
        ax.axvspan(lo, hi, color=face, lw=0, zorder=0)
        ax.text((lo + hi) / 2, n - 0.35, lab, ha="center", va="bottom", fontsize=6.8, color=SEC)
    for i, r in enumerate(res):
        y = n - 1 - i
        ax.plot([r["lo"], r["hi"]], [y, y], color=INK, lw=0.9, zorder=2)
        ax.scatter([r["icc"]], [y], s=34, color=BLUE, edgecolor="white", linewidth=0.7, zorder=3)
        ax.text(r["hi"] + 0.012, y, _f2(r["icc"]), ha="left", va="center", fontsize=7.4,
                fontweight="bold", color=INK)
    ax.set_yticks(range(n), [r["name"] for r in res][::-1], fontsize=7.2)
    ax.tick_params(axis="y", length=0)
    ax.set_xlim(0, 1.0); ax.set_ylim(-0.6, n + 0.15)
    ax.set_xticks([0, 0.25, 0.5, 0.75, 0.9, 1.0], ["0", ".25", ".50", ".75", ".90", "1"])
    ax.set_xlabel("Intraclass correlation (ICC)")
    for s_ in ("top", "right", "left"):
        ax.spines[s_].set_visible(False)


def _icc_panel_b(ax, S):
    """Example scale: each model's score in each run; spread between vs within models."""
    S = S.loc[S.mean(axis=1).sort_values().index]
    M, means = S.values, S.mean(axis=1).values
    rng = M.max(axis=1) - M.min(axis=1)
    mid = [i for i in range(4, len(S) - 4) if rng[i] > 0]
    typical = np.median(rng[[i for i in range(len(S)) if rng[i] > 0]])
    ex = int(min(mid, key=lambda i: abs(rng[i] - typical)))  # a model with a typical wobble
    for i, vals in enumerate(M):
        col = BLUE if i == ex else "#8f8d86"
        ax.vlines(i, vals.min(), vals.max(), color=col, lw=1.2 if i == ex else 0.9, zorder=2)
        ax.scatter(np.full(len(vals), i), vals, s=12 if i == ex else 8, color=col,
                   edgecolor="white", linewidth=0.3, zorder=3)
    xb = len(S) + 0.4
    ax.plot([xb, xb], [means.min(), means.max()], color=INK, lw=1.0, clip_on=False)
    for yy in (means.min(), means.max()):
        ax.plot([xb - 0.5, xb], [yy, yy], color=INK, lw=1.0, clip_on=False)
    ax.text(xb + 0.5, (means.min() + means.max()) / 2, "Between\nmodels:\nlarge\nspread",
            ha="left", va="center", fontsize=6.8, color=INK, fontweight="bold", linespacing=1.1)
    ax.annotate(f"Within one model\n({display_name(S.index[ex])}):\nsmall run-to-run wobble",
                xy=(ex, M[ex].max() + 0.04), xytext=(ex - 2.0, M.max() - 0.1),
                ha="left", va="top", fontsize=6.8, color=BLUE, fontweight="bold",
                arrowprops=dict(arrowstyle="-", color=BLUE, lw=0.7, shrinkA=2, shrinkB=2))
    ax.set_xlim(-0.8, len(S) + 0.5); ax.set_xticks([])
    ax.set_ylim(M.min() - 0.15, M.max() + 0.15)
    ax.set_xlabel(f"{len(S)} models, ordered by average score")
    ax.set_ylabel(r"SDO$_6$ score (1 to 7)")
    tidy(ax); ax.spines["bottom"].set_visible(False)


def _icc_panel_c(ax, res):
    """Models whose scale score was not identical in all five runs."""
    n = len(res)
    for i, r in enumerate(res):
        y = n - 1 - i
        ax.barh(y, r["changed"], height=0.58, color=ORANGE, edgecolor="white", lw=0.8, zorder=2)
        ax.barh(y, r["models"] - r["changed"], left=r["changed"], height=0.58, color=GREY,
                edgecolor="white", lw=0.8, zorder=2)
        ax.text(r["changed"] / 2, y, f"{r['changed']} of {r['models']}", ha="center",
                va="center", fontsize=7.2, fontweight="bold", color="white", zorder=3)
    ax.set_yticks(range(n), [r["name"] for r in res][::-1], fontsize=7.2)
    ax.tick_params(axis="y", length=0)
    ax.set_xlim(0, 27); ax.set_ylim(-0.6, n - 0.4)
    ax.set_xticks([0, 9, 18, 27])
    ax.set_xlabel("Number of models")
    tidy(ax, grid_axis=None); ax.spines["left"].set_visible(False)
    ax.legend([Rectangle((0, 0), 1, 1, facecolor=ORANGE), Rectangle((0, 0), 1, 1, facecolor=GREY)],
              ["Score changed between runs", "Same score in all 5 runs"], loc="upper left",
              bbox_to_anchor=(-0.02, -0.28), ncol=1, frameon=False, fontsize=6.8,
              handlelength=1.0, handleheight=0.9, labelspacing=0.3)


def fig_icc():
    """Separate figure for a general audience: high ICC, why, and what it does not show."""
    ci = read_csv("Analysis_output/Stats_tables/S07_reliability_by_aggregation_level.csv")
    ci = ci[ci.level == "3 full-scale score"].set_index("instrument")
    res, scores = [], {}
    for inst, name in ICC_PANELS:
        S = icc_scores(inst)
        scores[inst] = S
        res.append(dict(scale=inst, name=name, icc=icc1(S.values), lo=ci.loc[inst, "icc1_lo"],
                        hi=ci.loc[inst, "icc1_hi"], models=len(S),
                        changed=int(S.nunique(axis=1).gt(1).sum())))
    fig = plt.figure(figsize=(7.2, 6.3))
    a = fig.add_axes([0.20, 0.745, 0.74, 0.165])
    b = fig.add_axes([0.09, 0.13, 0.46, 0.43])
    c = fig.add_axes([0.80, 0.30, 0.18, 0.26])
    _icc_panel_a(a, res)
    _icc_panel_b(b, scores[ICC_EXAMPLE])
    _icc_panel_c(c, res)
    ex = next(r for r in res if r["scale"] == ICC_EXAMPLE)
    heads = [
        (0.0, 0.975, "All four scale versions look reliable by the usual guideline",
         "Dots show the ICC; lines show its 95% confidence interval"),
        (0.0, 0.645, f"Why: models differ far more from each other than from run to run "
         f"(ICC = {_f2(ex['icc'])})", "Each column of dots is one model; each dot is its score "
         "in one of the 5 runs"),
        (0.665, 0.645, "But most models' scores still changed",
         "Models whose score was not the same\nin all 5 runs"),
    ]
    for x, y, h1, h2 in heads:
        fig.text(x + 0.035, y, h1, ha="left", va="top", fontsize=7.6, fontweight="bold", color=INK)
        fig.text(x + 0.035, y - 0.027, h2, ha="left", va="top", fontsize=6.8, color=SEC,
                 linespacing=1.15)
    for letter, x, y in (("A", 0.0, 0.975), ("B", 0.0, 0.645), ("C", 0.665, 0.645)):
        fig.text(x, y, letter, fontsize=11, fontweight="bold", ha="left", va="top", color=INK)
    fig.text(0.0, 0.035, "Takeaway: a high ICC shows that models can be told apart, "
             "not that a model gives the same score every time.", ha="left", va="center",
             fontsize=7.6, fontweight="bold", color=INK)
    save(fig, "FigICC_scale_scores")
    pd.DataFrame(res).drop(columns="name").round(4).to_csv(
        os.path.join(SRC, "FigICC_summary.csv"), index=False)
    for inst, S in scores.items():
        out = S.copy(); out.columns = [f"run_{int(cc) + 1}" for cc in out.columns]
        out.insert(0, "display", [display_name(m) for m in out.index])
        out["score_changed"] = S.nunique(axis=1).gt(1)
        out.round(4).to_csv(os.path.join(SRC, f"FigICC_scores_{inst}.csv"), index_label="model")


# ---------------------------------------------------------------- Fig 4: RMET
def fig4():
    fig, ax = plt.subplots(figsize=(3.5, 3.0))
    human = HUMAN_RMET / 36 * 100
    ax.axvline(human, color=RULE, linewidth=0.8, zorder=1)
    ax.scatter(rmet.accuracy_pct, rmet.pairs_changed_pct, s=24, color=BLUE, edgecolor="white",
               linewidth=0.6, zorder=3)
    ax.set_xlim(40, 92); ax.set_ylim(-2, 38)
    ax.set_xticks([40, 50, 60, 70, 80, 90]); ax.set_yticks([0, 10, 20, 30])
    ax.set_xlabel("RMET accuracy (% correct)")
    ax.set_ylabel("Answers that changed across runs (%)")
    tidy(ax)
    ax.text(human - 1.0, 37, f"Adult average\n({HUMAN_RMET} of 36)", ha="right", va="top",
            fontsize=6.8, color=SEC)
    ax.text(41, 37, f"Spearman ρ = {rho:.2f}".replace("0.", "."), ha="left", va="top",
            fontsize=7.2, color=INK)
    title(ax, "", "Accuracy did not predict consistency")
    save(fig, "Fig4_rmet")
    rmet.round(2).to_csv(os.path.join(SRC, "Fig4_rmet_by_model.csv"), index_label="model")


def report_numbers():
    """Numbers quoted in the manuscript text, recomputed from the same data."""
    m = s6.groupby(["model_label", "language"]).v.mean().unstack()
    print("SDO6 answer distribution (%):", dist.round(1).to_dict("index"))
    print("SDO6 mean, English %.2f vs Chinese %.2f; higher in Chinese for %d of %d models"
          % (s6[s6.language == "en"].v.mean(), s6[s6.language == "zh"].v.mean(),
             int((m.zh > m.en).sum()), len(m)))
    print("Models whose language difference reversed between runs:", int(lang.reversed.sum()))
    print("Complete model-item pairs: %d; changed at least once: %.1f%%"
          % (len(pairs), 100 * pairs.changed_any.mean()))
    hit = (agree.alpha_interval >= 0.8) & (agree.pct_changed >= 20)
    print("Fig 3a: %d of %d combinations at alpha >= .80 changed >= 20%% of items"
          % (int(hit.sum()), int((agree.alpha_interval >= 0.8).sum())))
    print("Fig 3b, later four runs (%):", (bycat.changed / bycat.n * 100).round(1).to_dict(),
          "| n:", bycat.n.to_dict())
    print("        all five runs  (%):", (bycat.changed_any5 / bycat.n * 100).round(1).to_dict())
    print("RMET: accuracy %.1f%%, pairs changed %.1f%%, Spearman rho %.3f (%d models)"
          % (rmet.accuracy_pct.mean(), rmet.pairs_changed_pct.mean(), rho, len(rmet)))


if __name__ == "__main__":
    print(f"Repository: {ROOT}\nOutput:     {OUT}\nFont:       {FONT}\n")
    report_numbers()
    print()
    fig1(); fig2(); fig2_simple(); fig3(); fig_icc(); fig4()
    print(f"\nDone: four figures and their source data in {OUT}")
