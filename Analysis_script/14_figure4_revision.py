"""Rebuild Figure 4 with a descriptive title and explicit response-change axis.

Uses the same valid RMET responses and model-level aggregation as script 08.
The vector drawing helper from script 13 supplies the manuscript typography.
Writes PNG, SVG and PDF artwork, a separate caption, and plotted source data.
"""
from __future__ import annotations

from collections import Counter, defaultdict
import csv
import importlib.util
from pathlib import Path
import shutil
import statistics

ROOT = Path(__file__).resolve().parents[1]
MAIN = ROOT / "Figures/Main_figures"
OPTIONS = MAIN / "Revision_options"
OUT = OPTIONS / "01_Revised"
SOURCE = OPTIONS / "source_data"
TMP = ROOT / "tmp/pdfs/figure4_revision"
for folder in (MAIN, OUT, SOURCE, TMP):
    folder.mkdir(parents=True, exist_ok=True)

spec = importlib.util.spec_from_file_location("revision_drawing", Path(__file__).with_name("13_figure_revision_options.py"))
drawing = importlib.util.module_from_spec(spec)
spec.loader.exec_module(drawing)
drawing.TMP = TMP
Figure, INK, SEC, GRID, EDGE = drawing.Figure, drawing.INK, drawing.SEC, drawing.GRID, drawing.EDGE

TITLE = "Relationship between accuracy and response variability"
HUMAN_MEAN = 26.2 / 36 * 100  # Retains the reference used in the original figure.


def mean(values):
    return statistics.mean(values)


def ranks(values):
    """Average ranks, retaining ties at the precision of the response counts."""
    values = [round(v, 12) for v in values]
    return [1 + sum(x < v for x in values) + (sum(x == v for x in values)-1)/2 for v in values]


def load_data():
    with (ROOT / "Results/RMET_Results_20260912.csv").open(encoding="utf-8-sig", newline="") as f:
        rows = list(csv.DictReader(f))
    cells = defaultdict(list)
    for row in rows:
        if row["model_label"] == "databricks-claude-sonnet-4" or not row["choice"]:
            continue
        if row["is_correct"] not in {"True", "False"}:
            raise ValueError("Unrecognised correctness coding")
        cells[(row["model_label"], row["question_id"], row["definitions_shown"])].append(row)

    models = defaultdict(list)
    for (model, question, condition), responses in cells.items():
        if len({r["repeat"] for r in responses}) != len(responses):
            raise ValueError("Duplicate response in a model-item-condition-run cell")
        models[model].append({
            "accuracy": mean(r["is_correct"] == "True" for r in responses),
            "changed": len({r["choice"] for r in responses}) > 1,
            "valid_responses": len(responses),
        })
    result = [dict(
        model=model,
        accuracy_pct=100 * mean(cell["accuracy"] for cell in values),
        items_changed_pct=100 * mean(cell["changed"] for cell in values),
        item_condition_combinations=len(values),
        combinations_with_four_valid_responses=sum(cell["valid_responses"] == 4 for cell in values),
        valid_responses=sum(cell["valid_responses"] for cell in values),
    ) for model, values in sorted(models.items())]
    counts = Counter(len(values) for values in cells.values())
    assert len(result) == 21
    assert all(r["item_condition_combinations"] == 72 for r in result)
    assert counts == {5: 1504, 4: 8}
    rho = statistics.correlation(ranks([r["accuracy_pct"] for r in result]),
                                 ranks([r["items_changed_pct"] for r in result]))
    assert round(rho, 2) == .05  # Reproduces the original figure's annotation.
    return result, rho


def make_figure(data, rho):
    f = Figure(720, 484, "Figure 4: " + TITLE)
    f.text(80, 28, TITLE, size=16, bold=True)
    f.text(80, 51, "Reading the Mind in the Eyes Test", size=11.5, color=SEC)
    x0, x1, y0, y1 = 86, 688, 100, 420
    X = lambda value: x0 + (value-40)/52*(x1-x0)
    Y = lambda value: y1 - value/40*(y1-y0)

    for tick in (0, 10, 20, 30, 40):
        y = Y(tick)
        f.line(x0, y, x1, y, GRID, .7)
        f.text(x0-13, y+4, tick, size=11, color=SEC, anchor="end")
    for tick in (40, 50, 60, 70, 80, 90):
        x = X(tick)
        f.line(x, y1, x, y1+4, EDGE, .8)
        f.text(x, y1+21, tick, size=11, color=SEC, anchor="middle")
    f.line(x0, y0, x0, y1, EDGE, .8)
    f.line(x0, y1, x1, y1, EDGE, .8)
    f.text((x0+x1)/2, 470, "Accuracy (% correct)", size=12.5, anchor="middle")
    f.text(25, (y0+y1)/2, "Items with response changes across runs (%)", size=12,
           anchor="middle", angle=90)

    f.line(X(HUMAN_MEAN), y0, X(HUMAN_MEAN), y1, SEC, 1, dash=[4, 3])
    f.text(X(HUMAN_MEAN)-10, 73, "Adult reference mean", size=10.5, color=SEC, anchor="end")
    f.text(X(HUMAN_MEAN)-10, 89, "72.8% (26.2/36)", size=10.5, color=SEC, anchor="end")
    f.text(x0+1, 85, f"Spearman's ρ = {rho:.2f}", size=11.5, color=INK)
    f.text(x1, 85, "21 models", size=11, color=SEC, anchor="end")
    for row in data:
        f.circle(X(row["accuracy_pct"]), Y(row["items_changed_pct"]), 4.7,
                 fill="#42566D", stroke="white", sw=.9)
    return f.export(OUT, "Fig4_rmet_revised")


def publish(artifacts, data, rho):
    backup = OPTIONS / "00_Originals"
    backup.mkdir(exist_ok=True)
    for ext in ("png", "svg", "pdf"):
        target = MAIN / f"Fig4_rmet.{ext}"
        if target.exists() and not (backup / target.name).exists():
            shutil.copy2(target, backup / target.name)
        shutil.copy2(artifacts[ext], target)
    fields = list(data[0])
    with (SOURCE / "Fig4_rmet_by_model.csv").open("w", encoding="utf-8", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fields)
        writer.writeheader()
        writer.writerows(data)

    caption = (
        f"Fig. 4. {TITLE}. Each point represents one of 21 models on the Reading the Mind in the Eyes Test. "
        "Accuracy is the mean proportion of correct responses, first averaged within each model-item-condition combination "
        "across valid responses and then across the 72 item-condition combinations (36 items with and without word definitions). "
        "Response variability is the percentage of those combinations with more than one distinct answer across repeated runs. "
        "Each item was administered in five runs; eight combinations had four valid responses, while the remaining 1,504 had five. "
        f"Spearman's rank correlation was {rho:.2f}. The dashed vertical line marks the adult reference mean of 26.2/36 "
        "(72.8%), retained from the original figure (Baron-Cohen et al., 2001)."
    )
    (MAIN / "Fig4_caption.md").write_text(caption+"\n", encoding="utf-8")
    (OPTIONS / "Figure4_revision_notes.md").write_text(
        "# Figure 4 revision\n\n"
        "The title describes the relationship rather than asserting a conclusion or prediction. "
        "The term response variability matches the response-change percentage on the vertical axis. "
        "The axis labels contain no unexplained test acronym. All model points use one neutral colour; "
        "the correlation annotation and human reference line are also neutral. "
        "The figure artwork contains no full caption, review heading or footer.\n\n"
        "The original model aggregation is retained, including available responses in the eight incomplete combinations. "
        "The plotted coordinates reproduce script 08, and the rank correlation rounds to 0.05. "
        "Source values are in source_data/Fig4_rmet_by_model.csv.\n\n"
        "Regenerate with Analysis_script/14_figure4_revision.py. The original main figure is preserved "
        "in 00_Originals/Fig4_rmet.png.\n", encoding="utf-8")


if __name__ == "__main__":
    data, rho = load_data()
    artifacts = make_figure(data, rho)
    publish(artifacts, data, rho)
    print("Created:", MAIN / "Fig4_rmet.pdf")
    print("PNG, editable SVG, separate caption and source values also saved.")
