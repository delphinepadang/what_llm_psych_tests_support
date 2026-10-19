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
    Manuscript_figures/Fig2_language.{png,pdf}
    Manuscript_figures/Fig3_answer_changes.{png,pdf}
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
def fig2():
    fig, (a, b) = plt.subplots(1, 2, figsize=(7.2, 3.05), gridspec_kw={"width_ratios": [1.12, 1]})
    fig.subplots_adjust(wspace=0.32, bottom=0.27)

    # (a) distribution of answers by language
    for yy, langc in ((1, "en"), (0, "zh")):
        left = 0.0
        for k in range(7):
            w = dist.loc[langc, k + 1]
            a.barh(yy, w, left=left, height=0.46, color=RAMP[k], edgecolor="white",
                   linewidth=0.9, zorder=2)
            if w >= 8:
                a.text(left + w / 2, yy, f"{w:.0f}%", ha="center", va="center", fontsize=7.6,
                       fontweight="bold", color="white" if k == 0 else INK, zorder=3)
            left += w
    a.set_yticks([1, 0], ["English", "Chinese"], fontsize=8, color=INK)
    a.tick_params(axis="y", length=0)
    a.set_xlim(0, 100); a.set_ylim(-0.42, 1.42)
    a.set_xticks([0, 25, 50, 75, 100])
    a.set_xlabel(r"Share of all SDO$_6$ answers (%)")
    tidy(a, grid_axis=None); a.spines["left"].set_visible(False)
    title(a, "a", "Answers softened in Chinese")
    kx0, ky, kw, kh = 0.0, -0.40, (1 - 6 * 0.012) / 7, 0.075
    for k in range(7):
        x = kx0 + k * (kw + 0.012)
        a.add_patch(Rectangle((x, ky), kw, kh, transform=a.transAxes, clip_on=False,
                              facecolor=RAMP[k], edgecolor="none"))
        a.text(x + kw / 2, ky + kh / 2, str(k + 1), transform=a.transAxes, ha="center",
               va="center", fontsize=7, fontweight="bold",
               color="white" if k in (0, 6) else INK)
    a.text(kx0, ky - 0.06, "Most egalitarian", transform=a.transAxes, ha="left", va="top",
           fontsize=7, color=SEC)
    a.text(kx0 + 3 * (kw + 0.012) + kw / 2, ky - 0.06, "Neutral", transform=a.transAxes,
           ha="center", va="top", fontsize=7, color=SEC)
    a.text(kx0 + 6 * (kw + 0.012) + kw, ky - 0.06, "Most hierarchy-\nendorsing",
           transform=a.transAxes, ha="right", va="top", fontsize=7, color=SEC, linespacing=1.15)

    # (b) each model's Chinese-minus-English difference in each run
    for i, (m, r) in enumerate(lang.iterrows()):
        col = ORANGE if r.reversed else BLUE
        b.vlines(i, r.lo, r.hi, color=col, linewidth=1.6, alpha=0.85, zorder=2)
        b.scatter(np.full(5, i), runs.loc[m].values, s=11, color=col, edgecolor="white",
                  linewidth=0.4, zorder=3)
    b.axhline(0, color=RULE, linewidth=0.8, zorder=1)
    b.text(len(lang) - 0.4, -0.06, "No difference", ha="right", va="top", fontsize=6.8, color=SEC)
    b.set_xlim(-1, len(lang)); b.set_xticks([])
    b.set_ylim(-0.4, 1.85); b.set_yticks([0, 0.5, 1.0, 1.5], ["0", "0.5", "1.0", "1.5"])
    b.set_ylabel("Chinese minus English score\n(points on the 1–7 scale)")
    b.set_xlabel(f"{len(lang)} models, ordered by average difference")
    tidy(b)
    b.spines["bottom"].set_visible(False)
    title(b, "b", "Five models reversed direction between runs")
    handles = [Line2D([0], [0], color=BLUE, lw=1.6, marker="o", ms=3.5, mec="white", mew=0.4),
               Line2D([0], [0], color=ORANGE, lw=1.6, marker="o", ms=3.5, mec="white", mew=0.4)]
    b.legend(handles, ["Same direction in all five runs", "Direction reversed between runs"],
             loc="upper left", frameon=False, handlelength=1.6, borderaxespad=0.2)
    save(fig, "Fig2_language")

    d = dist.copy(); d.index = d.index.map({"en": "English", "zh": "Chinese"})
    d.columns = [f"pct_answer_{c}" for c in d.columns]
    d.round(2).to_csv(os.path.join(SRC, "Fig2a_answer_distribution.csv"), index_label="language")
    r = runs.copy(); r.columns = [f"run_{int(c) + 1}" for c in r.columns]
    r = r.join(lang[["mean", "reversed"]]).sort_values("mean")
    r.round(4).to_csv(os.path.join(SRC, "Fig2b_chinese_minus_english_by_run.csv"), index_label="model")


# ---------------------------------------------------------------- Fig 3: stability
def fig3():
    fig, (a, b) = plt.subplots(1, 2, figsize=(7.2, 3.05), gridspec_kw={"width_ratios": [1.08, 1]})
    fig.subplots_adjust(wspace=0.3)

    # (a) conventional agreement vs items changed
    hit = (agree.alpha_interval >= 0.8) & (agree.pct_changed >= 20)
    a.add_patch(Rectangle((0.8, 20), 0.26, 90, facecolor=TINT_ORANGE, edgecolor="none", zorder=0))
    a.axvline(0.8, color=RULE, linewidth=0.8, zorder=1)
    a.scatter(agree.alpha_interval[~hit], agree.pct_changed[~hit], s=15, color=GREY,
              edgecolor="white", linewidth=0.5, zorder=2)
    a.scatter(agree.alpha_interval[hit], agree.pct_changed[hit], s=17, color=ORANGE,
              edgecolor="white", linewidth=0.5, zorder=3)
    a.set_xlim(-0.04, 1.06); a.set_ylim(-4, 104)
    a.set_xticks([0, 0.2, 0.4, 0.6, 0.8, 1.0], ["0", ".2", ".4", ".6", ".8", "1"])
    a.set_xlabel("Agreement across the five runs (Krippendorff’s α)")
    a.set_ylabel("Items whose answer changed (%)")
    tidy(a)
    a.text(0.785, 101, "Usual .80\nbenchmark", ha="right", va="top", fontsize=6.8, color=SEC)
    n_hit, n_pass = int(hit.sum()), int((agree.alpha_interval >= 0.8).sum())
    a.text(0.815, 78, f"{n_hit} of {n_pass} that\nmet the benchmark\nchanged ≥ 20%\nof their items",
           ha="left", va="top", fontsize=6.8, color=INK)
    n_flat = int(((agree.alpha_interval >= 0.9999) & (agree.pct_changed == 0)).sum())
    a.annotate(f"{n_flat} overlapping points:\nno answer changed", xy=(0.993, 0.8), xytext=(0.2, 17),
               fontsize=6.8, color=SEC, ha="center", va="center",
               arrowprops=dict(arrowstyle="-", color=RULE, lw=0.6, shrinkA=2, shrinkB=3))
    title(a, "a", "Reliable by convention, yet answers changed")

    # (b) SDO pairs whose four later answers differed, by the answer given in the first run
    pct = (bycat["changed"] / bycat["n"] * 100).values
    lo, hi = wilson(bycat["changed"].values, bycat["n"].values)
    xs = np.arange(len(bycat))
    b.bar(xs, pct, width=0.56, color=BLUE, zorder=2)
    b.errorbar(xs, pct, yerr=[pct - lo * 100, hi * 100 - pct], fmt="none", ecolor=SEC,
               elinewidth=0.8, capsize=2.2, capthick=0.8, zorder=3)
    for xi, p, h in zip(xs, pct, hi * 100):
        b.text(xi, h + 2.5, f"{p:.0f}%", ha="center", va="bottom", fontsize=7.6,
               fontweight="bold", color=INK)
    labels = [f"{c}\nn = {n}" for c, n in zip(bycat.index.astype(str), bycat["n"].values)]
    b.set_xticks(xs, labels, fontsize=7.2)
    b.tick_params(axis="x", length=0)
    b.set_ylim(0, 100); b.set_yticks([0, 25, 50, 75, 100])
    b.set_ylabel("Pairs whose four later answers differed (%)")
    b.set_xlabel("Answer in the first run (1 = most egalitarian, 4 = neutral)")
    tidy(b)
    title(b, "b", "Egalitarian answers held; others moved")
    save(fig, "Fig3_answer_changes")

    cols = [c for c in ("model_label", "display", "inst_lang", "alpha_interval", "pct_changed")
            if c in agree.columns]
    agree[cols].assign(flagged=hit.values).to_csv(
        os.path.join(SRC, "Fig3a_agreement_vs_items_changed.csv"), index=False)
    out = bycat.copy()
    out["pct_later_changed"] = (out["changed"] / out["n"] * 100).round(2)
    out["ci_low"], out["ci_high"] = (lo * 100).round(2), (hi * 100).round(2)
    out["pct_any_change_all_five_runs"] = (out["changed_any5"] / out["n"] * 100).round(2)
    out.to_csv(os.path.join(SRC, "Fig3b_change_by_first_answer.csv"), index_label="first_answer")


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
    fig1(); fig2(); fig3(); fig4()
    print(f"\nDone: four figures and their source data in {OUT}")
