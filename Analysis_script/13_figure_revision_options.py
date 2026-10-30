"""Build two data-based designs of Figures 2 and 3 and supplementary Figure S14.

The original figures and analyses are preserved. Requires reportlab; PNG export
uses Poppler's pdftoppm. Run from any directory with the bundled Python runtime.
All plotted quantities are recalculated from the same inputs as script 08.
"""
from __future__ import annotations

import csv
import html
import io
import json
import math
import re
from collections import Counter, defaultdict
from decimal import Decimal, ROUND_HALF_UP
from pathlib import Path
import shutil
import subprocess

from reportlab.lib import colors
from reportlab.pdfbase import pdfmetrics
from reportlab.pdfbase.ttfonts import TTFont
from reportlab.pdfgen import canvas
from reportlab.platypus import Paragraph
from reportlab.lib.styles import ParagraphStyle

ROOT = Path(__file__).resolve().parents[1]
BASE = ROOT / "Figures/Main_figures/Revision_options"
REVISED = BASE / "01_Revised"
ALTERNATIVE = BASE / "02_Alternative_charts"
SUPP = ROOT / "Figures/Supplementary_figures"
TMP = ROOT / "tmp/pdfs/figure_revision"
for folder in (REVISED, ALTERNATIVE, SUPP, TMP, BASE / "source_data"):
    folder.mkdir(parents=True, exist_ok=True)

INK = "#20272D"
SEC = "#59616B"
GRID = "#E5E8EC"
EDGE = "#C6CCD3"
BLUE = "#245FA5"
EN = "#42566D"
ZH = "#138B82"
ORANGE = "#D95B2B"
PALE = "#FFF0E7"
GREY = "#D8DADF"
PINK = "#F3BCAF"
RAMP = ["#245FA5", "#5896DE", "#BCD5F0", "#DADBD7", "#F4C5BD", "#E67870", "#AF302F"]
FONT_DIR = Path("/System/Library/Fonts/Supplemental")
REGULAR = FONT_DIR / "Arial.ttf"
BOLD = FONT_DIR / "Arial Bold.ttf"
FONT = "Arial" if REGULAR.exists() and BOLD.exists() else "Helvetica"
if FONT == "Arial":
    pdfmetrics.registerFont(TTFont("Arial", str(REGULAR)))
    pdfmetrics.registerFont(TTFont("Arial-Bold", str(BOLD)))
    pdfmetrics.registerFontFamily("Arial", normal="Arial", bold="Arial-Bold", italic="Arial", boldItalic="Arial-Bold")


def fmt(value, signed=False, digits=2):
    q = Decimal(str(abs(value))).quantize(Decimal("1").scaleb(-digits), rounding=ROUND_HALF_UP)
    prefix = ("+" if value > 0 else "-") if signed and q else ("-" if value < 0 and q else "")
    return prefix + str(q)


def read_csv(path):
    with (ROOT / path).open(encoding="utf-8-sig", newline="") as f:
        return list(csv.DictReader(f))


def number(value):
    try:
        out = float(value)
        return out if math.isfinite(out) else None
    except (TypeError, ValueError):
        return None


raw = [r for r in read_csv("Results/SDO_VSA_Results_20260830.csv")
       if r["model_label"] != "databricks-claude-sonnet-4"]
cells = defaultdict(dict)
language_runs = defaultdict(list)
distribution = {lang: Counter() for lang in ("en", "zh")}
for row in raw:
    instrument = row["instrument"]
    value = number(row["value"])
    if instrument == "SDO6":
        value = number(row["value_recoded"])
        inst = "SDO6-" + row["language"].upper()
        if value is not None:
            distribution[row["language"]][int(value)] += 1
            language_runs[(row["model_label"], int(row["repeat"]), row["language"])].append(value)
    elif instrument == "SDO":
        inst = "SDO7s"
        if value is not None and row["question_id"] in {"sdo-q3", "sdo-q4", "sdo-q7", "sdo-q8"}:
            value = 8 - value
    elif instrument == "VSA":
        inst = "VSA"
        if value is not None and row["question_id"] in {"vsa-q1", "vsa-q4", "vsa-q5"}:
            value = -value
    else:
        continue
    if value == 0:
        value = 0.0
    key = (row["model_label"], inst, row["question_id"])
    run = int(row["repeat"])
    if run in cells[key]:
        raise ValueError(f"Duplicate model-item-run: {key}, {run}")
    cells[key][run] = value

totals = {lang: sum(counts.values()) for lang, counts in distribution.items()}
pct = {lang: [100 * distribution[lang][i] / totals[lang] for i in range(1, 8)]
       for lang in distribution}
scores = {key: sum(values) / len(values) for key, values in language_runs.items()}
models = sorted({key[0] for key in language_runs})
example2 = [(scores[("azure-gpt-5", run, "en")], scores[("azure-gpt-5", run, "zh")])
            for run in range(5)]
# Explicitly order by run, independent of the input CSV row order.
example3 = [[cells[("azure-gpt-5.5", "VSA", f"vsa-q{i}")][run] for run in range(5)]
            for i in range(1, 7)]
agreement_all = read_csv("Analysis_output/Reframing_checks/R3_agreement_by_model.csv")
agreement = [r for r in agreement_all if number(r["alpha_interval"]) is not None]
alpha = float(next(r["alpha_interval"] for r in agreement
                   if r["display"] == "GPT-5.5" and r["inst_lang"] == "VSA"))
groups = {True: [0, 0, 0], False: [0, 0, 0]}
for row in agreement:
    p = float(row["pct_changed"])
    group = 0 if p == 0 else 1 if p < 20 else 2
    groups[float(row["alpha_interval"]) >= .8][group] += 1

bycat = {label: {"n": 0, "changed": 0} for label in ("1", "2", "3", "4", "5-7")}
for (model, inst, item), runs in cells.items():
    if inst == "VSA" or set(runs) != set(range(5)) or any(v is None for v in runs.values()):
        continue
    vals = [runs[r] for r in range(5)]
    label = str(int(vals[0])) if vals[0] <= 4 else "5-7"
    bycat[label]["n"] += 1
    bycat[label]["changed"] += len(set(vals[1:])) > 1


def wilson(k, n):
    p, z = k / n, 1.96
    den = 1 + z * z / n
    center = (p + z * z / (2 * n)) / den
    half = z * math.sqrt(p * (1 - p) / n + z * z / (4 * n * n)) / den
    return 100 * (center - half), 100 * (center + half)


for group in bycat.values():
    group["percent"] = 100 * group["changed"] / group["n"]
    group["ci_low"], group["ci_high"] = wilson(group["changed"], group["n"])

# This is a figure revision: check that the recomputed values match the originals.
assert totals == {"en": 2156, "zh": 2146}
assert groups == {True: [29, 29, 12], False: [0, 4, 32]}
assert [g["n"] for g in bycat.values()] == [682, 244, 43, 66, 30]
assert sum(len(set(v)) > 1 for v in example3) == 3
assert len(models) == 27 and len(agreement_all) == 108 and len(agreement) == 106
assert all(len(language_runs[("azure-gpt-5", run, lang)]) == 16
           for run in range(5) for lang in ("en", "zh"))


class Figure:
    """A small vector drawing surface shared by PDF and editable SVG exports."""
    def __init__(self, width, height, name):
        self.w, self.h, self.name = width, height, name
        self.buffer = io.BytesIO()
        self.pdf = canvas.Canvas(self.buffer, pagesize=(width, height))
        self.pdf.setTitle(name)
        self.svg = [f'<svg xmlns="http://www.w3.org/2000/svg" width="{width}pt" height="{height}pt" '
                    f'viewBox="0 0 {width} {height}" role="img">',
                    f'<title>{html.escape(name)}</title>',
                    f'<rect width="{width}" height="{height}" fill="white"/>']

    def text(self, x, y, text, size=11, color=INK, bold=False, anchor="start", angle=0):
        text = str(text)
        font = FONT + ("-Bold" if bold else "")
        self.pdf.setFillColor(colors.toColor(color))
        # Typeset scale subscripts explicitly: the installed Arial font has no
        # Unicode subscript-6 glyph. Keep SVG text editable and PDF glyphs sound.
        runs = []
        for part in re.split(r"(SDO[67])", text):
            if re.fullmatch(r"SDO[67]", part):
                runs.extend([("SDO", size, 0), (part[-1], size*.72, size*.22)])
            elif part:
                runs.append((part, size, 0))
        width = sum(pdfmetrics.stringWidth(t, font, sz) for t, sz, _ in runs)
        offset = width/2 if anchor == "middle" else width if anchor == "end" else 0
        xx = -offset
        if angle == 0:
            if x-offset < 0 or x-offset+width > self.w or y > self.h-3 or y-size < 0:
                raise ValueError(f"Text outside figure bounds: {text}")
        elif angle == 90:
            if x-size < 0 or y-width+offset < 0 or y+offset > self.h:
                raise ValueError(f"Vertical text outside figure bounds: {text}")
        else:
            raise ValueError("Only horizontal text and vertical axis labels are supported")
        self.pdf.saveState()
        self.pdf.translate(x, self.h-y)
        self.pdf.rotate(angle)
        spans = []
        previous_dy = 0
        for t, sz, dy in runs:
            self.pdf.setFont(font, sz)
            self.pdf.drawString(xx, -dy, t)
            xx += pdfmetrics.stringWidth(t, font, sz)
            spans.append(f'<tspan font-size="{sz}" dy="{dy-previous_dy}">{html.escape(t)}</tspan>')
            previous_dy = dy
        self.pdf.restoreState()
        transform = f' transform="rotate({-angle} {x} {y})"' if angle else ""
        self.svg.append(f'<text x="{x:.3f}" y="{y:.3f}"{transform} font-family="Arial, Helvetica, sans-serif" '
                        f'font-size="{size}" font-weight="{"700" if bold else "400"}" '
                        f'fill="{color}" text-anchor="{anchor}">{"".join(spans)}</text>')

    def rect(self, x, y, width, height, fill="white", stroke=None, sw=.7, radius=0):
        self.pdf.setFillColor(colors.toColor(fill))
        if stroke:
            self.pdf.setStrokeColor(colors.toColor(stroke))
        self.pdf.setLineWidth(sw)
        if radius:
            self.pdf.roundRect(x, self.h-y-height, width, height, radius,
                               fill=1, stroke=bool(stroke))
        else:
            self.pdf.rect(x, self.h-y-height, width, height, fill=1, stroke=bool(stroke))
        self.svg.append(f'<rect x="{x:.3f}" y="{y:.3f}" width="{width:.3f}" height="{height:.3f}" '
                        f'rx="{radius}" fill="{fill}" stroke="{stroke or "none"}" stroke-width="{sw}"/>')

    def line(self, x1, y1, x2, y2, color=EDGE, sw=1, dash=None):
        self.pdf.setStrokeColor(colors.toColor(color))
        self.pdf.setLineWidth(sw)
        self.pdf.setDash(dash or [])
        self.pdf.line(x1, self.h-y1, x2, self.h-y2)
        self.pdf.setDash([])
        dash_svg = f' stroke-dasharray="{",".join(map(str, dash))}"' if dash else ""
        self.svg.append(f'<line x1="{x1:.3f}" y1="{y1:.3f}" x2="{x2:.3f}" y2="{y2:.3f}" '
                        f'stroke="{color}" stroke-width="{sw}"{dash_svg}/>')

    def circle(self, x, y, r, fill, stroke="white", sw=1):
        self.pdf.setFillColor(colors.toColor(fill))
        self.pdf.setStrokeColor(colors.toColor(stroke))
        self.pdf.setLineWidth(sw)
        self.pdf.circle(x, self.h-y, r, fill=1, stroke=1)
        self.svg.append(f'<circle cx="{x:.3f}" cy="{y:.3f}" r="{r}" fill="{fill}" '
                        f'stroke="{stroke}" stroke-width="{sw}"/>')

    def arrow(self, x1, y1, x2, y2, color=EDGE, sw=1.3):
        angle = math.atan2(y2-y1, x2-x1)
        x1, y1 = x1+6*math.cos(angle), y1+6*math.sin(angle)
        x2, y2 = x2-6*math.cos(angle), y2-6*math.sin(angle)
        self.line(x1, y1, x2, y2, color, sw)
        for side in (-1, 1):
            a = angle + side * .45
            self.line(x2, y2, x2-5*math.cos(a), y2-5*math.sin(a), color, sw)

    def heading(self, letter, text, y, x=18, size=14):
        self.text(x, y, letter, size=19, bold=True)
        self.text(x+28, y-1, text, size=size, bold=True)

    def export(self, folder, stem):
        svg_path = folder / f"{stem}.svg"
        svg_path.write_text("\n".join(self.svg + ["</svg>"]), encoding="utf-8")
        self.pdf.showPage()
        self.pdf.save()
        pdf_path = TMP / f"{stem}.pdf"
        pdf_path.write_bytes(self.buffer.getvalue())
        render = shutil.which("pdftoppm") or str(Path.home() / ".cache/codex-runtimes/codex-primary-runtime/dependencies/bin/override/pdftoppm")
        subprocess.run([render, "-png", "-r", "600", "-singlefile", str(pdf_path), str(folder / stem)],
                       check=True, capture_output=True)
        return {"svg": str(svg_path), "png": str(folder / f"{stem}.png"), "pdf": str(pdf_path)}


def language_legend(fig, x, y, size=11, en_color=EN, zh_color=ZH):
    fig.circle(x, y-4, 4, en_color)
    fig.text(x+11, y, "English", size=size)
    fig.circle(x+99, y-4, 4, zh_color)
    fig.text(x+110, y, "Chinese", size=size)


def scale_key(fig, y, x=278):
    fig.text(x-10, y+11, "Most egalitarian", size=10, anchor="end", color=SEC)
    for i, col in enumerate(RAMP):
        fig.rect(x+i*22, y, 20, 16, fill=col)
        fig.text(x+i*22+10, y+12, str(i+1), size=10, bold=True,
                 color="white" if i in (0, 6) else INK, anchor="middle")
    fig.text(x+7*22+8, y+11, "Most hierarchy-endorsing", size=10, color=SEC)


def figure2_revised():
    f = Figure(720, 450, "Figure 2: revised layout")
    f.heading("A", "SDO6 response distributions by language", 28)
    for i, lang in enumerate(("en", "zh")):
        y = 57+i*44
        f.text(101, y+20, {"en": "English", "zh": "Chinese"}[lang], size=12, anchor="end")
        left = 115
        for k, w in enumerate(pct[lang]):
            bw = 565*w/100
            f.rect(left, y, bw, 29, fill=RAMP[k], stroke="white", sw=.8)
            if w >= 3:
                f.text(left+bw/2, y+20, f"{w:.0f}%", size=11 if w >= 10 else 9,
                       color="white" if k == 0 else INK, bold=w>=10, anchor="middle")
            left += bw
    scale_key(f, 148)
    f.heading("B", "SDO6 scores across runs: GPT-5", 211)
    language_legend(f, 120, 240)
    x0, x1, y0, y1 = 115, 680, 258, 395
    x = lambda v: x0+(v-1)/1.05*(x1-x0)
    for tick in (1, 1.25, 1.5, 1.75, 2):
        f.line(x(tick), y0, x(tick), y1, GRID, .7)
        f.text(x(tick), y1+17, f"{tick:.2f}", size=10, color=SEC, anchor="middle")
    for i, (en, zh) in enumerate(example2):
        y = 269+i*28
        flip = zh < en
        if flip:
            f.rect(x0, y-11, x1-x0, 22, fill=PALE)
        f.text(101, y+4, f"Run {i+1}", size=11.5, anchor="end")
        f.arrow(x(en), y, x(zh), y, ORANGE if flip else "#9AA3AC", 1.3)
        f.circle(x(en), y, 4.8, EN)
        f.circle(x(zh), y, 4.8, ZH)
    f.line(x0, y1, x1, y1)
    f.text((x0+x1)/2, 432, "Mean SDO6 score", size=12, anchor="middle")
    return f.export(REVISED, "Fig2_language_revised")


def figure2_alternative():
    f = Figure(720, 610, "Figure 2: alternative chart design")
    f.heading("A", "SDO6 response distributions by language", 28)
    language_legend(f, 487, 53, en_color=BLUE, zh_color=ORANGE)
    x0, x1, y0, y1 = 76, 690, 71, 250
    for tick in (0, 20, 40, 60, 80):
        y = y1-tick/80*(y1-y0)
        f.line(x0, y, x1, y, GRID, .7)
        f.text(x0-10, y+4, str(tick), size=10, anchor="end", color=SEC)
    f.text(22, (y0+y1)/2, "Responses (%)", size=11.5, anchor="middle", angle=90)
    for k in range(7):
        center = x0+(k+.5)*(x1-x0)/7
        for lang, offset, color in (("en", -12, BLUE), ("zh", 12, ORANGE)):
            h = pct[lang][k]/80*(y1-y0)
            f.rect(center+offset-10, y1-h, 20, h, fill=color)
            # Small categories retain decimal precision to preserve differences.
            value = f"{pct[lang][k]:.0f}" if pct[lang][k] >= 10 else f"{pct[lang][k]:.1f}"
            f.text(center+offset, y1-h-7, value, size=9, anchor="middle", color=color, bold=True)
        f.text(center, y1+18, str(k+1), size=11, anchor="middle")
    f.line(x0, y1, x1, y1)
    f.text((x0+x1)/2, y1+39, "SDO6 response category", size=12, anchor="middle")
    f.text(x0, y1+55, "1 = most egalitarian", size=9.5, color=SEC)
    f.text(x1, y1+55, "7 = most hierarchy-endorsing", size=9.5, color=SEC, anchor="end")
    f.heading("B", "SDO6 scores across runs: GPT-5", 348)
    f.text(22, 485, "Mean SDO6 score", size=11.5, anchor="middle", angle=90)
    ya = lambda value: 530-(value-1)*96
    for tick in (1, 1.5, 2):
        f.text(66, ya(tick)+3, f"{tick:.2f}", size=9, color=SEC, anchor="end")
    for i, (en, zh) in enumerate(example2):
        left = 76+i*124
        flip = zh < en
        f.rect(left, 371, 112, 204, fill=PALE if flip else "#F7F9FB", radius=5)
        f.text(left+56, 394, f"Run {i+1}", size=12, bold=True, anchor="middle")
        f.text(left+56, 412, fmt(zh-en, signed=True), size=11, bold=True,
               color=ORANGE if flip else INK, anchor="middle")
        for tick in (1, 1.5, 2):
            f.line(left+12, ya(tick), left+101, ya(tick), GRID, .6)
        f.arrow(left+28, ya(en), left+87, ya(zh), ORANGE if flip else "#929DA8", 1.2)
        f.circle(left+28, ya(en), 4.5, BLUE)
        f.circle(left+87, ya(zh), 4.5, ORANGE)
        f.text(left+19, ya(en)-10, fmt(en), size=9.5, color=BLUE, anchor="middle")
        f.text(left+93, ya(zh)+16, fmt(zh), size=9.5, color=ORANGE, anchor="middle")
        f.text(left+28, 560, "English", size=9.5, color=BLUE, anchor="middle")
        f.text(left+87, 560, "Chinese", size=9.5, color=ORANGE, anchor="middle")
    f.text(382, 598, "Language", size=12, anchor="middle")
    return f.export(ALTERNATIVE, "Fig2_language_alternative")


def figure3_revised():
    f = Figure(720, 604, "Figure 3: revised layout")
    f.heading("A", "VSA responses across runs: GPT-5.5", 28, size=12.8)
    f.heading("B", "Reliability and response changes", 28, x=380, size=12.8)
    for j in range(5):
        f.text(94+j*45, 65, f"Run {j+1}", size=10.5, anchor="middle", color=SEC)
    f.text(326, 65, "Changed", size=10, anchor="middle", color=SEC)
    for i, vals in enumerate(example3):
        y = 88+i*34
        changed = len(set(vals)) > 1
        if changed:
            f.rect(20, y-15, 335, 29, fill=PALE)
        f.text(64, y+4, f"Item {i+1}", size=10.5, anchor="end")
        mode = Counter(vals).most_common(1)[0][0]
        for j, value in enumerate(vals):
            odd = value != mode
            f.rect(77+j*45, y-12, 34, 25, fill=ORANGE if odd else "white",
                   stroke=None if odd else EDGE, radius=3)
            f.text(94+j*45, y+5, f"{value:g}", size=12, color="white" if odd else INK,
                   bold=odd, anchor="middle")
        f.text(326, y+4, "Yes" if changed else "No", size=10.5,
               color=ORANGE if changed else SEC, bold=changed, anchor="middle")
    f.text(21, 304, f"Krippendorff's α = {alpha:.2f}", size=12, bold=True, color=BLUE)
    f.text(352, 304, "3 of 6 items changed", size=11.5, bold=True, color=ORANGE, anchor="end")
    # Short key; denominators and exclusions are explained in the caption.
    f.text(408, 63, "Items changed", size=10.5, color=SEC)
    for i, (label, color) in enumerate((("0%", GREY), ("<20%", PINK), ("≥20%", ORANGE))):
        x = 408+i*90
        f.rect(x, 76, 11, 11, fill=color)
        f.text(x+16, 86, label, size=10.5)
    x0, x1 = 479, 697
    for idx, passed in enumerate((True, False)):
        y = 132+idx*93
        f.text(464, y+2, "α ≥ .80" if passed else "α < .80", size=11, anchor="end")
        f.text(464, y+19, f"n = {sum(groups[passed])}", size=10, anchor="end", color=SEC)
        left = x0
        for count, color in zip(groups[passed], (GREY, PINK, ORANGE)):
            if not count:
                continue
            width = (x1-x0)*count/70
            f.rect(left, y-14, width, 40, fill=color, stroke="white", sw=.8)
            f.text(left+width/2, y+11, count, size=11, anchor="middle", bold=True,
                   color="white" if color == ORANGE else INK)
            left += width
    for tick in (0, 20, 40, 60):
        x = x0+(x1-x0)*tick/70
        f.line(x, 261, x, 265, EDGE, .7)
        f.text(x, 279, tick, size=10, anchor="middle", color=SEC)
    f.line(x0, 261, x1, 261)
    f.text((x0+x1)/2, 300, "Model-scale combinations", size=10.5, anchor="middle")
    f.heading("C", "SDO response variation by initial response", 358)
    f.text(690, 357, "All models pooled", size=11, color=SEC, anchor="end")
    x0, x1, y0, y1 = 257, 678, 382, 542
    for tick in (0, 25, 50, 75, 100):
        x = x0+(x1-x0)*tick/100
        f.line(x, y0, x, y1+5, GRID, .7)
        f.text(x, y1+22, tick, size=10, color=SEC, anchor="middle")
    labels = ["1  Most egalitarian", "2", "3", "4  Neutral", "5-7  Hierarchy-endorsing"]
    for i, (label, group, col) in enumerate(zip(labels, bycat.values(), [RAMP[0], RAMP[1], RAMP[2], RAMP[3], RAMP[5]])):
        y = 396+i*33
        f.text(x0-13, y+4, f"{label}  (n = {group['n']})", size=11, anchor="end")
        p = x0+(x1-x0)*group["percent"]/100
        lo = x0+(x1-x0)*group["ci_low"]/100
        hi = x0+(x1-x0)*group["ci_high"]/100
        f.rect(x0, y-10, p-x0, 21, fill=col)
        f.line(lo, y, hi, y, SEC, 1)
        f.line(lo, y-4, lo, y+4, SEC, 1)
        f.line(hi, y-4, hi, y+4, SEC, 1)
        f.text(hi+8, y+4, f"{group['percent']:.0f}%", size=11, bold=True)
    f.line(x0, y1+5, x1, y1+5)
    f.text((x0+x1)/2, 586, "Items with response variation in Runs 2-5 (%)", size=11.5, anchor="middle")
    return f.export(REVISED, "Fig3_answer_changes_revised")


def figure3_alternative():
    f = Figure(720, 778, "Figure 3: alternative chart design")
    f.heading("A", "VSA response trajectories: GPT-5.5", 28)
    f.text(694, 27, f"Krippendorff's α = {alpha:.2f}", size=11.5, color=INK, bold=True, anchor="end")
    f.text(22, 130, "Rescored item response", size=11, anchor="middle", angle=90)
    for i, vals in enumerate(example3):
        x = 65+i*106
        changed = len(set(vals)) > 1
        col = ORANGE if changed else BLUE
        f.rect(x, 49, 98, 159, fill=PALE if changed else "#F7F9FB", radius=5)
        f.text(x+49, 69, f"Item {i+1}", size=11.5, bold=True, anchor="middle")
        # All six small multiples use exactly the same response-axis range.
        yy = lambda v: 156-(v+3)*22
        for tick in (-3, -2, -1, 0):
            f.line(x+19, yy(tick), x+88, yy(tick), GRID, .6)
            f.text(x+13, yy(tick)+3, tick, size=8, color=SEC, anchor="end")
        for j in range(4):
            f.line(x+22+j*16, yy(vals[j]), x+22+(j+1)*16, yy(vals[j+1]), col, 1.6)
        for j, value in enumerate(vals):
            f.circle(x+22+j*16, yy(value), 2.9, col, stroke=col, sw=.5)
            f.text(x+22+j*16, 176, j+1, size=8, anchor="middle", color=SEC)
        f.text(x+49, 195, "Changed" if changed else "Unchanged", size=9.5,
               color=col, bold=changed, anchor="middle")
    f.text(379, 231, "Repeated run", size=11.5, anchor="middle")
    f.heading("B", "Reliability and response changes", 270)
    # A continuous reliability axis retains every one of the 106 defined values.
    f.text(689, 269, "106 model-scale combinations", size=10.5, color=SEC, anchor="end")
    x0, x1, y0, y1 = 88, 675, 311, 433
    X = lambda val: x0+(val+.025)/1.06*(x1-x0)
    Y = lambda val: y1-val/100*(y1-y0)
    f.rect(X(.8), y0, X(1)-X(.8), y1-y0, fill="#F5F6F7")
    for tick in (0, 50, 100):
        f.line(x0, Y(tick), x1, Y(tick), GRID, .6)
        f.text(x0-10, Y(tick)+3, tick, size=9.5, color=SEC, anchor="end")
    f.text(22, (y0+y1)/2, "Items changed across runs (%)", size=11, anchor="middle", angle=90)
    f.line(X(.8), y0, X(.8), y1, SEC, 1, dash=[3, 3])
    f.text(X(.8)+8, y0-12, "α = .80", size=10, color=SEC)
    f.line(x0, y1, x1, y1)
    repeated = Counter((float(row["alpha_interval"]), float(row["pct_changed"])) for row in agreement)
    for (a, p), n in sorted(repeated.items()):
        # Preserve exact coordinates and equal marker sizes. Explain complete
        # overlaps with a direct annotation, rather than encoding count by area.
        color = ORANGE if a >= .8 and p > 0 else (BLUE if a >= .8 else "#A9B1BA")
        f.circle(X(a), Y(p), 3.1, color, stroke="white", sw=.6)
    for tick in (0, .2, .4, .6, .8, 1):
        f.text(X(tick), y1+25, f"{tick:.1f}", size=9.5, color=SEC, anchor="middle")
    f.text((x0+x1)/2, 480, "Response reliability (Krippendorff's interval α)", size=11.5, anchor="middle")
    f.line(X(1)+5, Y(0)+2, 690, 452, SEC, .7)
    f.line(690, 452, 690, 495, SEC, .7)
    f.line(690, 495, 672, 495, SEC, .7)
    f.text(663, 494, "29 overlapping combinations:", size=10.5, color=BLUE, anchor="end")
    f.text(663, 509, "no response changes across runs", size=10.5, color=BLUE, anchor="end")
    f.heading("C", "SDO response variation by initial response", 552)
    f.text(690, 551, "All models pooled", size=11, color=SEC, anchor="end")
    x0, x1, y0, y1 = 88, 672, 595, 720
    f.text(22, (y0+y1)/2, "Items varying in Runs 2-5 (%)", size=11, anchor="middle", angle=90)
    Y = lambda p: y1-p/100*(y1-y0)
    for tick in (0, 50, 100):
        f.line(x0, Y(tick), x1, Y(tick), GRID, .7)
        f.text(x0-10, Y(tick)+3, f"{tick}%", size=9, color=SEC, anchor="end")
    for i, (label, group, color) in enumerate(zip(bycat, bycat.values(), [RAMP[0], RAMP[1], RAMP[2], RAMP[3], RAMP[5]])):
        x = x0+45+i*119
        f.line(x, Y(group["ci_high"]), x, Y(group["ci_low"]), SEC, 1.1)
        f.line(x-4, Y(group["ci_high"]), x+4, Y(group["ci_high"]), SEC, 1.1)
        f.line(x-4, Y(group["ci_low"]), x+4, Y(group["ci_low"]), SEC, 1.1)
        f.circle(x, Y(group["percent"]), 4.8, color, stroke=SEC, sw=.5)
        f.text(x+11, Y(group["percent"])+3, f"{group['percent']:.0f}%", size=10.5, bold=True)
        f.text(x, y1+17, f"{label}  (n = {group['n']})", size=9.5, anchor="middle")
    f.text((x0+x1)/2, 760, "Initial response (Run 1)", size=11.5, anchor="middle")
    return f.export(ALTERNATIVE, "Fig3_answer_changes_alternative")


NAMES = {
    "gpt-4o": "GPT-4o", "gpt-4.1": "GPT-4.1", "gpt-5": "GPT-5",
    "gpt-5.1": "GPT-5.1", "gpt-5.2": "GPT-5.2", "gpt-5.4": "GPT-5.4",
    "gpt-5.5": "GPT-5.5", "gpt-5.6-sol": "GPT-5.6-sol",
    "gpt-oss-20b": "GPT-OSS 20B", "gpt-oss-120b": "GPT-OSS 120B",
    "claude-opus-4-1": "Opus 4.1", "claude-sonnet-4-5": "Sonnet 4.5",
    "claude-haiku-4-5": "Haiku 4.5", "claude-opus-4-5": "Opus 4.5",
    "claude-opus-4-6": "Opus 4.6", "claude-sonnet-4-6": "Sonnet 4.6",
    "claude-opus-4-7": "Opus 4.7", "claude-opus-4-8": "Opus 4.8",
    "claude-opus-5": "Opus 5", "claude-sonnet-5": "Sonnet 5", "claude-fable-5": "Fable 5",
    "gemma-3-12b": "Gemma 3 12B", "meta-llama-3-1-8b-instruct": "Llama 3.1 8B",
    "meta-llama-3-3-70b-instruct": "Llama 3.3 70B", "llama-4-maverick": "Llama 4 Maverick",
    "qwen3-next-80b-a3b-instruct": "Qwen3-Next 80B", "qwen35-122b-a10b": "Qwen3.5 122B",
}
FAMILIES = [
    ("OpenAI GPT-4", ["gpt-4o", "gpt-4.1"]),
    ("OpenAI GPT-5", ["gpt-5", "gpt-5.1", "gpt-5.2", "gpt-5.4", "gpt-5.5", "gpt-5.6-sol"]),
    ("OpenAI GPT-OSS", ["gpt-oss-20b", "gpt-oss-120b"]),
    ("Anthropic Claude 4", ["claude-opus-4-1", "claude-sonnet-4-5", "claude-haiku-4-5", "claude-opus-4-5",
                            "claude-opus-4-6", "claude-sonnet-4-6", "claude-opus-4-7", "claude-opus-4-8"]),
    ("Anthropic Claude 5", ["claude-opus-5", "claude-sonnet-5", "claude-fable-5"]),
    ("Google Gemma", ["gemma-3-12b"]),
    ("Meta Llama", ["meta-llama-3-1-8b-instruct", "meta-llama-3-3-70b-instruct", "llama-4-maverick"]),
    ("Alibaba Qwen", ["qwen3-next-80b-a3b-instruct", "qwen35-122b-a10b"]),
]


def short(label):
    return label.split("-", 1)[1] if label.split("-", 1)[0] in ("azure", "databricks") else label


categories = {}
differences = {}
for model in models:
    d = [round(scores[(model, r, "zh")]-scores[(model, r, "en")], 10) for r in range(5)]
    differences[model] = d
    pos, neg, zero = any(v>0 for v in d), any(v<0 for v in d), any(v==0 for v in d)
    categories[model] = "switched" if pos and neg else "english" if not pos else "mostly" if zero else "always"
assert Counter(categories.values()) == {"always": 20, "mostly": 1, "english": 1, "switched": 5}


def supplementary():
    f = Figure(960, 394, "Supplementary Figure S14: language-effect direction across runs")
    f.text(20, 28, "Language-effect direction across repeated runs", size=17, bold=True)
    colors_by_category = {"always": "#4F5D6E", "mostly": "#A3ADB8", "english": "#DADBD7", "switched": ORANGE}
    model_lookup = {short(m): m for m in models}
    for i, (family, members) in enumerate(FAMILIES):
        y = 53+i*32
        f.text(166, y+16, family, size=11, anchor="end")
        for j, member in enumerate(members):
            model = model_lookup[member]
            category = categories[model]
            x = 180+j*95
            f.rect(x, y, 90, 24, fill=colors_by_category[category], radius=3)
            f.text(x+45, y+16, NAMES[member], size=9.3 if len(NAMES[member])<13 else 8.4,
                   color=INK if category in ("mostly", "english") else "white", anchor="middle",
                   bold=category=="switched")
    legend = [("always", "Chinese > English in all five runs (20)"),
              ("switched", "Direction reversed across runs (5)"),
              ("mostly", "Chinese ≥ English; at least one tie (1)"),
              ("english", "English ≥ Chinese; at least one tie (1)")]
    for i, (category, label) in enumerate(legend):
        x = 180+(i%2)*375
        y = 329+(i//2)*26
        f.rect(x, y, 12, 12, fill=colors_by_category[category])
        f.text(x+20, y+11, label, size=11)
    return f.export(SUPP, "FigS14_language_direction")


TITLE2 = "Models gave less strongly egalitarian responses in Chinese on average, but the direction of the language difference varied across runs for some models."
CAPTION2 = (
    "<b>Fig. 2. " + TITLE2 + "</b> "
    "<b>(A)</b> Proportion of SDO6 responses at each scale point, shown separately by language and pooled across 27 models, "
    "16 items and five runs. Reverse-keyed items were rescored so that 1 indicates the most egalitarian response and 7 the most "
    "hierarchy-endorsing response. Percentages use all valid responses within each language (English, n = 2,156; Chinese, n = 2,146). "
    "<b>(B)</b> GPT-5's mean SDO6 score in English and Chinese in each of five runs. In each run, GPT-5 answered all 16 items once "
    "in each language; the score is the mean of the 16 rescored responses. Arrows connect the English score to the Chinese score; "
    "signed values indicate the Chinese score minus the English score, calculated before rounding. Orange highlights Run 4, "
    "in which the direction of the language difference reversed. Model-level patterns across runs are shown in Supplementary Fig. S14."
)
TITLE3 = "High reliability did not imply identical responses across repeated runs."
CAPTION3 = (
    "<b>Fig. 3. " + TITLE3 + "</b> "
    "<b>(A)</b> GPT-5.5's rescored responses to the six Very Short Authoritarianism (VSA) items across five runs. In each run, the model "
    "answered each item once on the -4 to +4 response scale. Orange cells mark responses that differ from the modal response for that item; "
    "shaded rows identify items with at least one response change. Krippendorff's interval α was .94, exceeding the .80 benchmark, "
    "although three of six items changed across runs. <b>(B)</b> Model-scale combinations grouped by whether Krippendorff's interval α "
    "met the .80 benchmark and by the proportion of items with at least one response change across five runs. The four scale versions were "
    "SDO7(s) in English, SDO6 in English, SDO6 in Chinese and VSA in English. Of 108 combinations (27 models × four versions), two had "
    "invariant responses and undefined α and were omitted. Among the 70 combinations with α ≥ .80, 41 changed at least one item, "
    "including 12 that changed at least 20% of items. <b>(C)</b> Proportion of model-item combinations with at least two distinct responses "
    "among Runs 2-5, grouped by the rescored response in Run 1. All 27 models and the three social dominance orientation (SDO) scale versions were pooled; only "
    "model-item combinations with five valid responses were included. Run 1 defines the groups and is excluded from the response-variation "
    "calculation. Values of n indicate model-item combinations per group; error bars show 95% Wilson intervals for the pooled proportions."
)
CAPTION2_ALT = CAPTION2.replace(
    "shown separately by language and pooled across 27 models,",
    "shown separately by language (English, blue; Chinese, orange) and pooled across 27 models,"
).replace(
    "In each run, GPT-5 answered all 16 items once in each language; the score is the mean of the 16 rescored responses.",
    "In each run, GPT-5 answered all 16 items once in each language; the score is the mean of the 16 rescored responses. "
    "In each run-specific plot, the horizontal axis identifies language and the vertical axis shows the mean SDO6 score."
).replace("Arrows connect the English score to the Chinese score;", "In each small multiple, an arrow connects the English score to the Chinese score;")
CAPTION3_ALT = CAPTION3.replace(
    "Orange cells mark responses that differ from the modal response for that item; shaded rows identify items with at least one response change.",
    "Each small multiple shows one item's response trajectory: the horizontal axis is repeated run (1-5), and the vertical axis is the "
    "rescored response to that individual item, rather than an average. All items use a shared vertical scale; orange indicates items with "
    "at least one response change, and blue indicates unchanged items."
).replace(
    "Model-scale combinations grouped by whether Krippendorff's interval α met the .80 benchmark and by the proportion of items with at least one response change across five runs.",
    "Points show model-scale combinations, positioned by Krippendorff's interval α and the percentage of items with at least one response change across five runs. "
    "The dashed line marks α = .80. Orange points meet this benchmark despite response changes; blue points meet it without changes; grey points fall below it. "
    "All markers have the same size. The labelled point at α = 1.0 and 0% response change represents 29 combinations "
    "at identical coordinates; the label identifies their overlap."
)
CAPTION2_REVISED = CAPTION2.replace(
    "signed values indicate the Chinese score minus the English score, calculated before rounding. ", ""
).replace("Chinese score; Orange", "Chinese score. Orange")
CAPTIONS = {
    "Fig2_language_revised": CAPTION2_REVISED,
    "Fig3_answer_changes_revised": CAPTION3,
    "Fig2_language_alternative": CAPTION2_ALT,
    "Fig3_answer_changes_alternative": CAPTION3_ALT,
    "FigS14_language_direction": (
        "<b>Supplementary Fig. S14. Language-effect direction across repeated runs.</b> "
        "Each tile represents one of the 27 models, grouped by model family and coloured according to the direction of its mean "
        "Chinese-minus-English SDO6 score difference across five runs. Scores use rescored valid responses. Twenty models scored higher "
        "in Chinese in every run; one scored at least as high in Chinese in all runs, with one tie; one scored at least as high in English "
        "in all runs, with one tie. "
        "Orange identifies the five models with both positive and negative differences across runs. This panel was moved from the original "
        "Figure 2C; S14 continues the existing S1-S13 supplementary numbering."
    ),
}


def export_source_data():
    dest = BASE / "source_data"
    def write(name, fields, rows):
        with (dest / name).open("w", encoding="utf-8", newline="") as f:
            writer = csv.DictWriter(f, fieldnames=fields)
            writer.writeheader()
            writer.writerows(rows)
    write("Fig2A_distributions.csv", ["language", "response", "count", "valid_responses", "percent"],
          [dict(language=lang, response=i, count=distribution[lang][i], valid_responses=totals[lang], percent=pct[lang][i-1])
           for lang in ("en", "zh") for i in range(1, 8)])
    write("Fig2B_GPT5_scores.csv", ["run", "english", "chinese", "difference"],
          [dict(run=i+1, english=en, chinese=zh, difference=zh-en) for i, (en, zh) in enumerate(example2)])
    write("Fig3A_GPT55_VSA.csv", ["item"]+[f"run_{i+1}" for i in range(5)],
          [{"item": i+1, **{f"run_{j+1}": value for j, value in enumerate(values)}} for i, values in enumerate(example3)])
    write("Fig3B_groups.csv", ["alpha_group", "items_changed_group", "count"],
          [dict(alpha_group="at_least_080" if passed else "below_080", items_changed_group=label, count=count)
           for passed in (True, False) for label, count in zip(("none", "under_20_percent", "at_least_20_percent"), groups[passed])])
    write("Fig3B_all_points.csv", list(agreement[0]), agreement)
    write("Fig3C_response_variation.csv", ["initial_response", "n", "changed", "percent", "ci_low", "ci_high"],
          [dict(initial_response=label, **group) for label, group in bycat.items()])
    write("FigS14_model_categories.csv", ["model", "category"]+[f"run_{i+1}" for i in range(5)],
          [dict(model=m, category=categories[m], **{f"run_{i+1}": value for i, value in enumerate(differences[m])}) for m in models])


def review_pdf(artifacts):
    """Keep figure artwork vector by merging temporary figure PDFs as forms."""
    from pypdf import PdfReader, PdfWriter, Transformation
    output = PdfWriter()
    style = ParagraphStyle("caption", fontName=FONT, fontSize=9, leading=12, textColor=colors.HexColor(INK))
    pages = [
        ("Revised figures | Figure 2", "Fig2_language_revised"),
        ("Revised figures | Figure 3", "Fig3_answer_changes_revised"),
        ("Alternative charts | Figure 2", "Fig2_language_alternative"),
        ("Alternative charts | Figure 3", "Fig3_answer_changes_alternative"),
        ("Supplementary figure | Former Figure 2C", "FigS14_language_direction"),
    ]
    for heading, stem in pages:
        # A generous page allows full-width figures and the expanded captions.
        page_w, page_h = 792, 1008
        b = io.BytesIO()
        c = canvas.Canvas(b, pagesize=(page_w, page_h))
        c.setFillColor(colors.HexColor(INK))
        c.setFont(FONT+"-Bold", 16)
        c.drawString(36, page_h-35, heading)
        chart = PdfReader(artifacts[stem]["pdf"]).pages[0]
        w, h = float(chart.mediabox.width), float(chart.mediabox.height)
        scale = min(720/w, 690/h)
        chart_y = page_h-62-h*scale
        caption = CAPTIONS[stem]
        # Scientific subscripts are kept in the manuscript captions and review.
        caption = caption.replace("SDO7(s)", "SDO<sub>7</sub>(s)").replace("SDO6", "SDO<sub>6</sub>")
        para = Paragraph(caption, style)
        _, ph = para.wrap(720, 900)
        if chart_y-ph-22 < 30:
            raise ValueError(f"Review caption overflows: {stem}")
        para.drawOn(c, 36, chart_y-ph-22)
        c.setFont(FONT, 8)
        c.setFillColor(colors.HexColor(SEC))
        c.drawString(36, 20, "Figure revision options | 29 October 2026 | PNG: 600 dpi; SVG: editable vector artwork")
        c.save()
        page = PdfReader(b).pages[0]
        page.merge_transformed_page(chart, Transformation().scale(scale).translate((page_w-w*scale)/2, chart_y))
        output.add_page(page)
    path = BASE / "Figures_2_and_3_review.pdf"
    output.add_metadata({"/Title": "Figures 2 and 3: revision options", "/Author": "Figure revision"})
    with path.open("wb") as f:
        output.write(f)
    return path


def publish_selected_figures(artifacts):
    """Install the user's selected pair and export an artwork-only two-page PDF."""
    from pypdf import PdfReader, PdfWriter
    destination = ROOT / "Figures/Main_figures"
    backup = BASE / "00_Originals"
    backup.mkdir(exist_ok=True)
    selected = [("Fig2_language_revised", "Fig2_language"),
                ("Fig3_answer_changes_alternative", "Fig3_answer_changes")]
    combined = PdfWriter()
    captions = []
    for source, target in selected:
        for ext in ("png", "svg"):
            path = destination / f"{target}.{ext}"
            original = backup / path.name
            if path.exists() and not original.exists():
                shutil.copy2(path, original)
            shutil.copy2(artifacts[source][ext], path)
        page = PdfReader(artifacts[source]["pdf"]).pages[0]
        combined.add_page(page)
        single = PdfWriter()
        single.add_page(page)
        single.add_metadata({"/Title": "Figure 2" if target.startswith("Fig2") else "Figure 3"})
        with (destination / f"{target}.pdf").open("wb") as f:
            single.write(f)
        caption = CAPTIONS[source]
        captions.append(re.sub("<[^>]+>", "", caption))
    path = destination / "Figures_2_and_3.pdf"
    combined.add_metadata({"/Title": "Figures 2 and 3", "/Author": ""})
    with path.open("wb") as f:
        combined.write(f)
    (destination / "Figures_2_and_3_captions.md").write_text("\n\n".join(captions)+"\n", encoding="utf-8")
    return path


def write_notes():
    clean = lambda text: re.sub("<[^>]+>", "", text)
    lines = [
        "# Figures 2 and 3: revision options", "",
        "Two chart-based versions are provided. Both use the original study data.", "",
        "## 01_Revised", "",
        "Preserves the original chart types, removes long explanations, adds formal panel headings, "
        "and retains only Runs 1-5 in Figure 2B. The former Figure 2C is now Supplementary Figure S14.", "",
        "## 02_Alternative_charts", "",
        "Figure 2: grouped response-distribution bars and one paired score plot per run. "
        "Figure 3: item-level response trajectories, a scatterplot of all 106 defined reliability values, "
        "and a point-and-interval plot of later-run response variation.", "",
        "The scatterplot retains the 106 combinations shown in the original grouped bars. "
        "All markers have equal size. A direct annotation identifies 29 combinations at the exact same coordinates "
        "(alpha = 1.0, response change = 0%); exact values are in source_data/Fig3B_all_points.csv.", "",
        "## Selected main figures", "",
        "The chosen pair is Figure 2 revised and Figure 3 alternative. They are saved as "
        "Figures/Main_figures/Fig2_language.{png,svg} and Fig3_answer_changes.{png,svg}. "
        "Figures/Main_figures/Figures_2_and_3.pdf contains artwork only: no full captions, review headings or footers. "
        "Captions are saved separately in Figures_2_and_3_captions.md. "
        "The difference column has been removed from Figure 2B. "
        "Original main PNGs are preserved in Revision_options/00_Originals/.", "",
        "## Alternative-chart axes and colours", "",
        "| Panel | Horizontal axis | Vertical axis |",
        "|---|---|---|",
        "| Fig. 2A | SDO6 response category (1-7) | Responses within each language (%) |",
        "| Fig. 2B | Language, separately for each run | Mean of the 16 rescored SDO6 responses |",
        "| Fig. 3A | Repeated run (1-5) | Rescored item response |",
        "| Fig. 3B | Response reliability (Krippendorff's interval alpha) | Items changed across five runs (%) |",
        "| Fig. 3C | Initial response (Run 1) | Items varying among Runs 2-5 (%) |", "",
        "In Figure 2, English is blue and Chinese is orange, using the same blue and orange as Figure 3. "
        "In Figure 3A, orange identifies items that changed and blue identifies items that did not change; "
        "Figure 3B retains its reliability-group colour key, and Figure 3C retains its response-category colour scale.", "",
        "The selected Figure 3 retains the VSA and SDO panel headings, with the scale names defined in the separate caption. "
        "Axis labels use plain wording without these acronyms. The alpha annotation and the .80 reference guide are neutral in colour; "
        "blue identifies unchanged response data in panels A and B rather than the reliability statistic.", "",
        "## Panel headings", "",
        "- Fig. 2A: SDO6 response distributions by language", 
        "- Fig. 2B: SDO6 scores across runs: GPT-5",
        "- Fig. 3A: VSA responses across runs: GPT-5.5 (alternative: VSA response trajectories: GPT-5.5)",
        "- Fig. 3B: Reliability and response changes",
        "- Fig. 3C: SDO response variation by initial response", "",
        "## Captions", "",
    ]
    for name, caption in CAPTIONS.items():
        lines += [f"### {name}", "", clean(caption), ""]
    lines += [
        "## Files and reproduction", "",
        "- Each figure has a 600 dpi PNG and an editable SVG with text preserved.",
        "- Figures_2_and_3_review.pdf contains all five figures, with their captions.",
        "- Supplementary Figure S14 is in Figures/Supplementary_figures/.",
        "- Source CSVs contain the plotted values and denominators.",
        "- Run Analysis_script/13_figure_revision_options.py with reportlab, pypdf and Poppler to regenerate.",
        "- Original main figures and script 08 are preserved.", "",
        "## Data checks", "",
        "Distribution denominators: English 2,156; Chinese 2,146 (all valid rescored responses). "
        "The caption distinguishes these from the nominal 27 × 16 × 5 design. GPT-5 has all 16 items "
        "in each language in each run. Figure 3B counts: α ≥ .80, 29/29/12; α < .80, 0/4/32. "
        "Two invariant combinations have undefined α and are excluded from both designs. "
        "Figure 3C denominators are 682, 244, 43, 66 and 30. Variation is calculated only among Runs 2-5, "
        "not as a difference from Run 1. Confidence intervals reproduce the original pooled Wilson intervals.", "",
    ]
    (BASE / "Captions_and_revision_notes.md").write_text("\n".join(lines), encoding="utf-8")


if __name__ == "__main__":
    artifacts = {}
    for build in (figure2_revised, figure3_revised, figure2_alternative, figure3_alternative, supplementary):
        record = build()
        stem = Path(record["png"]).stem
        artifacts[stem] = record
        print("Created", stem)
    export_source_data()
    write_notes()
    review = review_pdf(artifacts)
    selected = publish_selected_figures(artifacts)
    (TMP / "manifest.json").write_text(json.dumps(artifacts, indent=2), encoding="utf-8")
    print("Review PDF:", review)
    print("Selected figure artwork:", selected)
