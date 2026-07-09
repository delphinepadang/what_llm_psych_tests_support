"""
Attitude (SDO + VSA) analysis for the lead paper.
Produces methods-ready numbers + a generation-trend figure mirroring the RMET evolution plot.

Inputs (from /mnt/user-data/uploads):
  - SDO_VSA_Results.csv               raw, 24 models x 14 items x 5 reps
  - attitude_sdo_by_model.csv         scored SDO mean per model (reverse-coding already applied)
  - attitude_rwa_by_model.csv         scored RWA(=VSA) mean per model
  - attitude_consistency_by_model.csv per-model within-model dispersion (SD), labelled "consistency"

NOTE on naming: the "consistency_*" columns are a DISPERSION measure (0 = perfectly
deterministic; higher = MORE variable). Higher value = LESS consistent. Verified below.
"""
import pandas as pd, numpy as np
from scipy import stats
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

# ---- data location (portable; mirrors the R scripts' path block) -------------
# Reads inputs from, and writes the figure to, the `data_summaries` folder next to
# `analysis_code/`. Order: (1) env AAI_DATA_DIR (2) first CLI arg (3) a
# `data_summaries` folder found next to this script or one level up.
import os, sys
def _find_data_dir():
    env = os.environ.get("AAI_DATA_DIR")
    if env:
        return os.path.abspath(env)
    if len(sys.argv) > 1 and sys.argv[1]:
        return os.path.abspath(sys.argv[1])
    here = os.path.dirname(os.path.abspath(__file__))
    for cand in (os.path.join(here, "data_summaries"),
                 os.path.join(here, "..", "data_summaries"),
                 os.path.join(os.getcwd(), "data_summaries")):
        if os.path.isdir(cand):
            return os.path.abspath(cand)
    sys.exit("Could not find the 'data_summaries' folder. "
             "Set AAI_DATA_DIR or pass it as the first argument.")
U = _find_data_dir()
print(f"[paths] data_summaries -> {U}")
# ------------------------------------------------------------------------------
raw  = pd.read_csv(f"{U}/SDO_VSA_Results.csv")
sdo  = pd.read_csv(f"{U}/attitude_sdo_by_model.csv")
rwa  = pd.read_csv(f"{U}/attitude_rwa_by_model.csv")
cons = pd.read_csv(f"{U}/attitude_consistency_by_model.csv")

line = lambda c="-": print(c*78)

# ----------------------------------------------------------------------------
# 0. METRIC VERIFICATION — what exactly is "consistency_*"?
#    Recompute mean within-item SD across reps, averaged over items, per model.
# ----------------------------------------------------------------------------
line("=")
print("0. METRIC CHECK: is consistency_* = mean within-item SD across the 5 reps?")
line()
def mean_within_item_sd(sub):
    # SD across reps for each item (ddof=1, pandas default; population SD if ddof=0)
    per_item = sub.groupby("question_id")["value"].std(ddof=1)
    return per_item.mean()

raw["short"] = raw["model"].str.replace("databricks-","",regex=False).str.replace("azure-","",regex=False)
chk = []
for m, sub in raw.groupby("short"):
    chk.append({
        "m": m,
        "recomputed_sdo": mean_within_item_sd(sub[sub.category=="sdo"]),
        "recomputed_vsa": mean_within_item_sd(sub[sub.category=="vsa"]),
    })
chk = pd.DataFrame(chk).merge(cons, on="m", how="left")
chk["match_sdo"] = np.isclose(chk.recomputed_sdo, chk.consistency_sdo, atol=1e-6, equal_nan=True)
chk["match_vsa"] = np.isclose(chk.recomputed_vsa, chk.consistency_vsa, atol=1e-6, equal_nan=True)
print(f"  SDO matches mean-within-item-SD (ddof=1): {chk.match_sdo.mean()*100:.0f}% of models")
print(f"  VSA matches mean-within-item-SD (ddof=1): {chk.match_vsa.mean()*100:.0f}% of models")
if not chk.match_sdo.all():
    print("  (mismatch -> trying population SD, ddof=0)")
    chk2=[]
    for m, sub in raw.groupby("short"):
        s=sub[sub.category=="sdo"]; per=s.groupby("question_id")["value"].std(ddof=0)
        chk2.append((m, per.mean()))
    c2=pd.DataFrame(chk2,columns=["m","recomp0"]).merge(cons[["m","consistency_sdo"]],on="m")
    print(f"  SDO matches with ddof=0: {np.isclose(c2.recomp0,c2.consistency_sdo,atol=1e-6,equal_nan=True).mean()*100:.0f}%")

# ----------------------------------------------------------------------------
# 1. REFUSALS / NON-PARSES
# ----------------------------------------------------------------------------
line("=")
print("1. REFUSALS / NON-PARSES")
line()
ref = raw[raw.value.isna()]
n_total = len(raw)
print(f"  Total item-administrations: {n_total}  (24 models x 14 items x 5 reps)")
print(f"  Non-parsed / refused:       {len(ref)}  ({len(ref)/n_total*100:.2f}%)")
print("  Breakdown:")
for _, r in ref.iterrows():
    tag = "explicit refusal" if "Refused" in str(r.raw_response) else "unparseable"
    print(f"    {r['short']:24s} {r['question_id']:8s} rep{r['repeat']}  [{tag}]")
print("  -> All on SDO items (the most overtly hierarchy-endorsing). None on VSA.")
print("  -> Handling: treat as MISSING (non-response), NOT 'incorrect' (no correct answer on an attitude item).")

# ----------------------------------------------------------------------------
# 2. DETERMINISM / WITHIN-MODEL DISPERSION SPLIT  (the robust headline)
# ----------------------------------------------------------------------------
line("=")
print("2. WITHIN-MODEL DISPERSION (response stability) — the determinism split")
line()
# transparent grouping
PROPRIETARY = set(sdo[sdo.family.isin(["Claude","GPT"])].m)   # all Claude + all GPT
cons["access"] = np.where(cons.m.isin(PROPRIETARY), "proprietary-flagship", "open-weight")
# provider path (for robustness): Azure = GPT only; everything else served via Databricks
cons["served_via"] = np.where(cons.family=="GPT", "azure", "databricks")

for scale in ["sdo","vsa"]:
    col=f"consistency_{scale}"
    print(f"\n  -- {scale.upper()} dispersion (mean within-item SD; 0 = deterministic) --")
    g = cons.groupby("access")[col].agg(["median","mean","max","count"])
    print(g.to_string())
    a = cons[cons.access=="proprietary-flagship"][col]
    b = cons[cons.access=="open-weight"][col]
    u,p = stats.mannwhitneyu(a,b,alternative="two-sided")
    print(f"    Mann-Whitney proprietary vs open: U={u:.0f}, p={p:.4g}")

# ROBUSTNESS: split must hold WITHIN Databricks (drops the temperature/provider confound)
print("\n  -- ROBUSTNESS: within Databricks-served models only (same client, same default sampling) --")
dbx = cons[cons.served_via=="databricks"].copy()
dbx["dbx_access"] = np.where(dbx.family=="Claude","Claude (flagship)","small open-weight")
for scale in ["sdo","vsa"]:
    col=f"consistency_{scale}"
    g = dbx.groupby("dbx_access")[col].agg(["median","mean","max","count"])
    a = dbx[dbx.dbx_access=="Claude (flagship)"][col]
    b = dbx[dbx.dbx_access=="small open-weight"][col]
    u,p = stats.mannwhitneyu(a,b,alternative="two-sided")
    print(f"    {scale.upper()}: Claude median={g.loc['Claude (flagship)','median']:.3f} "
          f"vs open median={g.loc['small open-weight','median']:.3f}  (U={u:.0f}, p={p:.4g})")

print("\n  -- Most variable models (top of the dispersion tail) --")
print(cons.sort_values("consistency_vsa",ascending=False)
        [["m","family","access","consistency_sdo","consistency_vsa"]].head(6).to_string(index=False))

# ----------------------------------------------------------------------------
# 3. ECOSYSTEM CONTRAST (descriptive only; Eastern n=2)
# ----------------------------------------------------------------------------
line("=")
print("3. ECOSYSTEM CONTRAST (Eastern = 2 Qwen models; Western = 22). DESCRIPTIVE ONLY.")
line()
for name, d, val in [("SDO",sdo,"SDO"), ("RWA(VSA)",rwa,"RWA")]:
    east = d[d.ecosystem=="Eastern"]
    west = d[d.ecosystem=="Western"]
    print(f"  {name}:")
    print(f"    Eastern (n={len(east)}): " + ", ".join(f"{r.m}={r[val]:.2f}" for _,r in east.iterrows()))
    print(f"    Western (n={len(west)}): mean={west[val].mean():.2f}, range [{west[val].min():.2f}, {west[val].max():.2f}]")
print("  -> Qwen straddles the Western distribution (one near the low end, one mid-high).")
print("  -> n=2 on the Eastern side: report descriptively, NO inferential ecosystem claim. Defer to paper 2.")

# ----------------------------------------------------------------------------
# 4. GENERATION TREND + FIGURE (mirrors RMET evolution plot)
# ----------------------------------------------------------------------------
line("=")
print("4. GENERATION TREND (within-family version order)")
line()
# version-order maps for the families with clean version chains
order = {
    "GPT":          ["gpt-4o","gpt-4.1","gpt-5","gpt-5.1","gpt-5.2","gpt-5.4","gpt-5.5"],
    "Claude-opus":  ["claude-opus-4-1","claude-opus-4-5","claude-opus-4-6","claude-opus-4-7","claude-opus-4-8"],
    "Claude-sonnet":["claude-sonnet-4","claude-sonnet-4-5","claude-sonnet-4-6"],
    "Claude-haiku": ["claude-haiku-4-5"],
}
scores = sdo[["m","SDO"]].merge(rwa[["m","RWA"]], on="m")
rows=[]
for sub, members in order.items():
    for i,mm in enumerate(members,1):
        r = scores[scores.m==mm]
        if len(r):
            rows.append({"subline":sub,"order":i,"m":mm,
                         "SDO":r.SDO.values[0],"VSA":r.RWA.values[0]})
trend = pd.DataFrame(rows)
print(trend.to_string(index=False))

# Llama trend (size-confounded) reported in text, not on the version axis
print("\n  Llama (version order confounded with size):")
for mm in ["meta-llama-3-1-8b-instruct","meta-llama-3-3-70b-instruct","llama-4-maverick"]:
    r=scores[scores.m==mm]
    print(f"    {mm:32s} SDO={r.SDO.values[0]:.2f}  VSA={r.RWA.values[0]:.2f}")

# ---- figure ----
colors = {"GPT":"#9467bd","Claude-opus":"#8c9f1a","Claude-sonnet":"#17becf","Claude-haiku":"#e377c2"}
fig, axes = plt.subplots(2,1, figsize=(10,8.5), sharex=True)
for ax, (col, mid, ylab, ttl) in zip(
        axes,
        [("SDO",4.0,"SDO score (1\u20137)","Social Dominance Orientation"),
         ("VSA",0.0,"VSA score (\u22124\u2026+4)","Authoritarianism (VSA)")]):
    for sub in ["GPT","Claude-opus","Claude-sonnet"]:
        d = trend[trend.subline==sub].sort_values("order")
        ax.plot(d["order"], d[col], "-o", color=colors[sub], label=sub, lw=2, ms=7)
        for _,r in d.iterrows():
            ax.annotate(r.m.replace("claude-","").replace("gpt-",""),
                        (r["order"], r[col]), fontsize=7, color=colors[sub],
                        xytext=(0,7), textcoords="offset points", ha="center")
    h = trend[trend.subline=="Claude-haiku"]
    ax.plot(h["order"], h[col], "o", color=colors["Claude-haiku"], ms=8, label="Claude-haiku")
    ax.axhline(mid, ls="--", color="grey", lw=1)
    ax.text(0.6, mid, "scale midpoint", fontsize=7, color="grey", va="bottom")
    ax.set_ylabel(ylab); ax.set_title(ttl, loc="left", fontsize=11, fontweight="bold")
    ax.grid(axis="y", alpha=0.25)
axes[0].set_ylim(0.5,7.5); axes[1].set_ylim(-4.2,4.2)
axes[1].set_xlabel("Version order (older \u2192 newer, within family)")
axes[0].legend(title="Model line", bbox_to_anchor=(1.01,1), loc="upper left", fontsize=9)
fig.suptitle("Political dispositions across model versions (pilot)", x=0.01, ha="left",
             fontsize=14, fontweight="bold")
fig.tight_layout(rect=[0,0,1,0.98])
out=os.path.join(U, "attitude_evolution_plot.png")
fig.savefig(out, dpi=150, bbox_inches="tight")
print(f"\n  figure saved: {out}")
line("=")
print("DONE")
