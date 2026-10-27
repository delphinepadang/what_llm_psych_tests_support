"""Supplementary Fig. S3: how SDO6 answers changed between English and Chinese.

Run from the repository root:  python3 Analysis_script/11_supplementary_fig_s3.py
Reads   Results/SDO_VSA_Results_20260830.csv
        Analysis_output/Stats_tables/S13_extremity_vs_direction.csv   (made by 02_statistical_analysis.R)
Writes  Figures/Supplementary_figures/FigS03_language_mechanism.png

Panel A: share of SDO6 answers at each scale point, by language, after reverse-keying
(1 = strongest rejection of group dominance). Panel B: how each model-item mean answer
changed from English to Chinese. Claude Sonnet 4 is excluded (no valid answers).
"""
import os
import csv
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib import rcParams

rcParams['font.family'] = 'sans-serif'
rcParams['font.sans-serif'] = ['Arial', 'Liberation Sans', 'DejaVu Sans']
rcParams['font.size'] = 9

D = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
OUT = os.path.join(D, 'Figures', 'Supplementary_figures', 'FigS03_language_mechanism.png')
os.makedirs(os.path.dirname(OUT), exist_ok=True)

d = pd.read_csv(D + '/Results/SDO_VSA_Results_20260830.csv', low_memory=False)
d = d[(d.model_label != 'databricks-claude-sonnet-4') & (d.instrument == 'SDO6') & d.value.notna()].copy()
d['item_no'] = d.item_no.astype(int)
d['k'] = np.where(d.item_no >= 9, 8 - d.value, d.value).astype(int)
d['sub'] = np.where(d.item_no <= 8, 'Dominance items', 'Anti-egalitarianism items (reverse-keyed)')

EN, ZH = '#3AA89A', '#C8576A'
fig = plt.figure(figsize=(7.2, 4.6), dpi=400)
gs = fig.add_gridspec(2, 2, height_ratios=[1.55, 1], hspace=0.55, wspace=0.12)

for j, sub in enumerate(['Dominance items', 'Anti-egalitarianism items (reverse-keyed)']):
    ax = fig.add_subplot(gs[0, j])
    x = np.arange(1, 8)
    for off, lang, col, lab in [(-0.2, 'en', EN, 'English'), (0.2, 'zh', ZH, 'Chinese')]:
        s = d[(d['sub'] == sub) & (d.language == lang)].k
        pct = [100 * (s == v).mean() for v in x]
        ax.bar(x + off, pct, width=0.38, color=col, label=lab)
    ax.set_title(sub, fontsize=9, fontweight='bold', loc='left')
    ax.set_xticks(x)
    ax.set_ylim(0, 95)
    ax.set_yticks([0, 25, 50, 75])
    ax.set_yticklabels(['0%', '25%', '50%', '75%'] if j == 0 else [])
    ax.grid(axis='y', color='#E5E5E5', linewidth=0.6)
    ax.set_axisbelow(True)
    for sp in ['top', 'right', 'left']:
        ax.spines[sp].set_visible(False)
    ax.tick_params(axis='y', length=0)
    ax.set_xlabel('Answer after reverse-keying', fontsize=8)
    if j == 0:
        ax.set_ylabel('Answers (%)')
        ax.legend(frameon=False, ncol=2, loc='upper right', fontsize=8, handlelength=1.2)
fig.text(0.02, 0.97, 'A', fontsize=12, fontweight='bold')
fig.text(0.02, 0.40, 'B', fontsize=12, fontweight='bold')

rows = list(csv.DictReader(open(D + '/Analysis_output/Stats_tables/S13_extremity_vs_direction.csv')))
LESS, MORE = '#E08A3C', '#4F7FB0'
cats = [('unchanged', 'Unchanged', '#D9D9D9'),
        ('same side, less extreme in Chinese', 'Less extreme in Chinese (same side)', LESS),
        ('same side, more extreme in Chinese', 'More extreme in Chinese (same side)', MORE),
        ('at or through midpoint', 'At the midpoint in either language', '#E3C76B'),
        ('direction reversed', 'Direction reversed', '#3B2A84')]
ax = fig.add_subplot(gs[1, :])
labels = [('dominance', 'Dominance'), ('anti_egalitarianism', 'Anti-egalitarianism')]
for yi, (key, lab) in enumerate(labels):
    left = 0
    for c, name, col in cats:
        v = [float(r['pct_pairs']) for r in rows if r['subscale'] == key and r['pattern'] == c][0]
        ax.barh(1 - yi, v, left=left, color=col, height=0.62, label=name if yi == 0 else None, edgecolor='white', linewidth=0.6)
        if v >= 5:
            ax.text(left + v / 2, 1 - yi, '%d%%' % round(v), ha='center', va='center', fontsize=7.5,
                    color='white' if col in (MORE, '#3B2A84') else '#222222')
        left += v
ax.set_yticks([1, 0])
ax.set_yticklabels([l for _, l in labels])
ax.set_xlim(0, 100)
ax.set_xticks([0, 25, 50, 75, 100])
ax.set_xticklabels(['0%', '25%', '50%', '75%', '100%'])
ax.set_xlabel('Model–item pairs: change in mean answer from English to Chinese (%)', fontsize=8.5)
for sp in ['top', 'right', 'left']:
    ax.spines[sp].set_visible(False)
ax.tick_params(axis='y', length=0)
ax.legend(frameon=False, ncol=3, loc='upper left', bbox_to_anchor=(0, -0.42), fontsize=7.5, handlelength=1.0,
          columnspacing=1.2)
fig.savefig(OUT, dpi=400, bbox_inches='tight', facecolor='white')
print('saved', OUT)
