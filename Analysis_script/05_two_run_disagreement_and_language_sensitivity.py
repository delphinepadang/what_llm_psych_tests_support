"""
05_two_run_disagreement_and_language_sensitivity.py

Two quantities reported in the manuscript that 02_statistical_analysis.R does
not yet produce:

  1. Two-run disagreement for the attitude instruments: for each model-item pair
     with five scored responses, the share of the 10 pairs of runs that gave
     different answers, averaged over pairs (overall, by instrument, and by
     model x instrument). Also the share of answers that differ from their
     pair's most common answer.
  2. The English-Chinese SDO6 language shift with and without Llama 3.1 8B
     (model-level mean of Chinese-minus-English scores, using reverse-keyed
     item means across runs).

Input : Results/SDO_VSA_Results_20260830.csv
Output: Analysis_output/Additional_checks/*.csv and printed summary

Uses only the Python standard library (Python >= 3.8). Run from the repository
root:  python3 Analysis_script/05_two_run_disagreement_and_language_sensitivity.py
"""

import csv
import itertools
import math
import os
import statistics as st
from collections import defaultdict

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
DATA = os.path.join(ROOT, "Results", "SDO_VSA_Results_20260830.csv")
OUT = os.path.join(ROOT, "Analysis_output", "Additional_checks")
EXCLUDED = {"databricks-claude-sonnet-4"}          # API failure on every call
LLAMA_31_8B = "databricks-meta-llama-3-1-8b-instruct"

os.makedirs(OUT, exist_ok=True)
rows = [r for r in csv.DictReader(open(DATA, encoding="utf-8-sig"))
        if r["model_label"] not in EXCLUDED]


def instrument(r):
    if r["instrument"] == "SDO":
        return "SDO7s"
    if r["instrument"] == "VSA":
        return "VSA"
    return "SDO6-" + r["language"].upper()


# Two-run disagreement does not depend on keying, so raw values are used.
# (Note: the reverse_scored / value_recoded columns in the CSV are reliable for
# SDO6 only; 02 applies the SDO7s and VSA keys itself.)
pairs = defaultdict(list)
for r in rows:
    if r["value"] != "":
        pairs[(r["model_label"], instrument(r), r["question_id"])].append(float(r["value"]))
complete = {k: v for k, v in pairs.items() if len(v) == 5}


def two_run(v):
    return sum(a != b for a, b in itertools.combinations(v, 2)) / 10


summary = []
for inst in ["SDO7s", "SDO6-EN", "SDO6-ZH", "VSA", "All"]:
    ks = [k for k in complete if inst == "All" or k[1] == inst]
    summary.append({
        "instrument": inst,
        "n_pairs": len(ks),
        "pct_pairs_changed": round(100 * sum(len(set(complete[k])) > 1 for k in ks) / len(ks), 3),
        "pct_two_run_disagreement": round(100 * st.mean(two_run(complete[k]) for k in ks), 3),
    })
dev = sum(5 - max(v.count(a) for a in set(v)) for v in complete.values())
modal_pct = 100 * dev / (5 * len(complete))

by_model = defaultdict(list)
for k, v in complete.items():
    by_model[(k[0], k[1])].append(two_run(v))
model_rows = [{"model_label": m, "instrument": i, "n_pairs": len(v),
               "pct_two_run_disagreement": round(100 * st.mean(v), 3)}
              for (m, i), v in sorted(by_model.items())]

with open(os.path.join(OUT, "two_run_disagreement_summary.csv"), "w", newline="") as f:
    w = csv.DictWriter(f, fieldnames=list(summary[0])); w.writeheader(); w.writerows(summary)
with open(os.path.join(OUT, "two_run_disagreement_by_model.csv"), "w", newline="") as f:
    w = csv.DictWriter(f, fieldnames=list(model_rows[0])); w.writeheader(); w.writerows(model_rows)

# Language shift (SDO6 uses value_recoded, which is already reverse-keyed)
item_means = defaultdict(list)
for r in rows:
    if r["instrument"] == "SDO6" and r["value_recoded"] != "":
        item_means[(r["model_label"], r["language"], int(float(r["item_no"])))].append(float(r["value_recoded"]))


def score(m, lang):
    vals = [st.mean(item_means[(m, lang, i)]) for i in range(1, 17) if (m, lang, i) in item_means]
    return st.mean(vals)


models = sorted({k[0] for k in item_means})
shift = {m: score(m, "zh") - score(m, "en") for m in models}


def paired(d):
    v = list(d.values())
    mean, sd = st.mean(v), st.stdev(v)
    return {"n_models": len(v), "mean_shift": round(mean, 4),
            "t": round(mean / (sd / math.sqrt(len(v))), 3),
            "models_higher_in_zh": sum(x > 0 for x in v)}


lang = [dict(analysis="All models", **paired(shift)),
        dict(analysis="Excluding Llama 3.1 8B", **paired({m: s for m, s in shift.items() if m != LLAMA_31_8B}))]
with open(os.path.join(OUT, "language_shift_sensitivity.csv"), "w", newline="") as f:
    w = csv.DictWriter(f, fieldnames=list(lang[0])); w.writeheader(); w.writerows(lang)

print("Complete model-item pairs:", len(complete))
for s in summary:
    print(f"  {s['instrument']:8s} pairs={s['n_pairs']:5d}  changed={s['pct_pairs_changed']:.1f}%  two-run={s['pct_two_run_disagreement']:.1f}%")
vals = [r["pct_two_run_disagreement"] for r in model_rows]
print(f"  per model x instrument: min {min(vals):.1f}%, median {st.median(vals):.1f}%, max {max(vals):.1f}%")
print(f"  answers differing from their pair's modal answer: {modal_pct:.1f}%")
for l in lang:
    print(f"Language shift ({l['analysis']}): {l['mean_shift']:.3f}, t = {l['t']}, {l['models_higher_in_zh']} of {l['n_models']} higher in Chinese")
