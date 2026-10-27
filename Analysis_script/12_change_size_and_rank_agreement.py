"""Size of answer changes between runs, and agreement between model rankings across runs.

Run from the repository root:
    python3 Analysis_script/12_change_size_and_rank_agreement.py

Reads   Results/SDO_VSA_Results_20260830.csv
Writes  Analysis_output/Additional_checks/change_size_by_scale.csv
        Analysis_output/Additional_checks/rank_agreement_by_scale.csv

Used for Section 3.3 (size of changes), Section 3.4 (Kendall's tau-b between runs,
rank spans, highest-scoring model) and Supplementary Tables S6 and S8 (part C).

Definitions
- A model-item pair is "complete" when it has a scored answer in all five runs.
  Claude Sonnet 4 is excluded (no scored answers).
- Change size: absolute difference in scale points for all ten pairs of runs within
  each complete pair. The overall mean includes zero differences; the conditional
  mean and the 1-point / 2+-point shares use only pairs of runs that disagree.
- Rankings: within each scale version and run, models are ranked by their mean
  reverse-keyed score over items that are complete for that model (rank 1 = highest
  score; ties share the best rank). Kendall's tau-b is averaged over the ten pairs
  of runs. A reversal is a pair of models whose order is strictly opposite in two runs.
"""
import itertools
import os

import numpy as np
import pandas as pd
from scipy.stats import kendalltau

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
OUT = os.path.join(ROOT, "Analysis_output", "Additional_checks")
os.makedirs(OUT, exist_ok=True)

d = pd.read_csv(os.path.join(ROOT, "Results", "SDO_VSA_Results_20260830.csv"),
                low_memory=False, encoding="utf-8-sig")
d = d[d.model_label != "databricks-claude-sonnet-4"]
d = d[d["value"].notna()].copy()
d["scale"] = np.select(
    [d.instrument.eq("SDO"),
     d.instrument.eq("SDO6") & d.language.eq("en"),
     d.instrument.eq("SDO6") & d.language.eq("zh"),
     d.instrument.eq("VSA")],
    ["SDO7(s)", "SDO6 English", "SDO6 Chinese", "VSA"], "other")
# item number: from item_no where given, otherwise from the question id (e.g. sdo-q3)
d["item"] = d.item_no.fillna(d.question_id.str.extract(r"q(\d+)$")[0].astype(float)).astype(int)

REVERSED = {"SDO7(s)": {3, 4, 7, 8}, "SDO6 English": set(range(9, 17)),
            "SDO6 Chinese": set(range(9, 17)), "VSA": {1, 4, 5}}


def keyed(row):
    if row["item"] in REVERSED[row["scale"]]:
        return -row["value"] if row["scale"] == "VSA" else 8 - row["value"]
    return row["value"]


d["keyed"] = d.apply(keyed, axis=1)
n_runs = d.groupby(["scale", "model_label", "item"])["repeat"].transform("nunique")
d = d[n_runs == 5]
ORDER = ["SDO7(s)", "SDO6 English", "SDO6 Chinese", "VSA"]


def run_differences(sub):
    out = []
    for _, s in sub.groupby(["scale", "model_label", "item"]):
        v = s.sort_values("repeat")["value"].to_numpy()
        out.extend(abs(a - b) for a, b in itertools.combinations(v, 2))
    return np.array(out)


rows = []
for label, sub in [(s, d[d.scale == s]) for s in ORDER] + [("All SDO versions", d[d.scale != "VSA"])]:
    diff = run_differences(sub)
    dis = diff[diff > 0]
    rows.append({"scale": label,
                 "complete_model_item_pairs": len(diff) // 10,
                 "run_comparisons": len(diff),
                 "mean_abs_difference": round(diff.mean(), 3),
                 "mean_abs_difference_among_disagreements": round(dis.mean(), 2),
                 "pct_disagreements_1_point": round(100 * (dis == 1).mean(), 1),
                 "pct_disagreements_2plus_points": round(100 * (dis >= 2).mean(), 1)})
size = pd.DataFrame(rows)
size.to_csv(os.path.join(OUT, "change_size_by_scale.csv"), index=False)

rows = []
for sc in ORDER:
    scores = d[d.scale == sc].groupby(["model_label", "repeat"])["keyed"].mean().unstack()
    ranks = scores.rank(ascending=False, method="min")
    taus, reversals = [], []
    x_all = scores.to_numpy()
    model_pairs = list(itertools.combinations(range(len(scores)), 2))
    for a, b in itertools.combinations(range(scores.shape[1]), 2):
        taus.append(kendalltau(x_all[:, a], x_all[:, b]).statistic)  # tau-b
        n_rev = sum((x_all[i, a] - x_all[j, a]) * (x_all[i, b] - x_all[j, b]) < 0 for i, j in model_pairs)
        reversals.append(100 * n_rev / len(model_pairs))
    span = ranks.max(axis=1) - ranks.min(axis=1)
    top = [sorted(ranks.index[ranks[c] == 1]) for c in ranks.columns]
    rows.append({"scale": sc, "models": len(scores),
                 "mean_kendall_tau_b": round(float(np.mean(taus)), 2),
                 "model_pairs_reversing_mean_pct": round(float(np.mean(reversals)), 1),
                 "model_pairs_reversing_max_pct": round(float(np.max(reversals)), 1),
                 "models_spanning_5plus_ranks": int((span >= 5).sum()),
                 "largest_rank_span": int(span.max()),
                 "highest_scoring_model_by_run": " | ".join("+".join(t) for t in top)})
rank = pd.DataFrame(rows)
rank.to_csv(os.path.join(OUT, "rank_agreement_by_scale.csv"), index=False)

pd.set_option("display.width", 200)
print(size.to_string(index=False))
print()
print(rank.drop(columns="highest_scoring_model_by_run").to_string(index=False))
print("\nModel-scale combinations spanning 5+ ranks:", int(rank.models_spanning_5plus_ranks.sum()))
