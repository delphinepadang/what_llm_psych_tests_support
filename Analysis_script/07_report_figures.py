"""MS_AAI_ToM  |  07_report_figures.py
Two new figures for the report version, drawn from Analysis_output/Report_outputs/
(produced by 06_report_descriptives.py). Requires Python 3 with pandas, numpy and matplotlib.
Run from the project root:  python3 Analysis_script/07_report_figures.py
Writes Analysis_output/Figures/Report_Fig2_zoom_out_zoom_in.(png|pdf) and
       Analysis_output/Figures/Report_Fig5_older_vs_newer.(png|pdf).

Fig. 2  Zoom out and zoom in: each model's scale score in each of the five runs (grey) and the
        mean of all 27 models (blue).
Fig. 5  Older and newer model generations (GPT-4 vs GPT-5; Claude 4 vs Claude 5).
Palette: categorical slots 1-2 of the reference palette (validated, light surface).
"""
import sys, os
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

OUT = sys.argv[1] if len(sys.argv) > 1 else os.path.join('Analysis_output', 'Report_outputs')
FIG = sys.argv[2] if len(sys.argv) > 2 else os.path.join('Analysis_output', 'Figures')
os.makedirs(FIG, exist_ok=True)
BLUE, ORANGE = '#2a78d6', '#eb6834'
INK, INK2, GRID, GREY = '#0b0b0b', '#52514e', '#e4e3df', '#a9a8a2'
plt.rcParams.update({'font.family': 'DejaVu Sans', 'font.size': 8, 'axes.edgecolor': GRID,
                     'axes.labelcolor': INK, 'xtick.color': INK2, 'ytick.color': INK2,
                     'axes.titlesize': 9, 'axes.titleweight': 'bold', 'axes.titlecolor': INK,
                     'figure.dpi': 100, 'savefig.dpi': 400})

# ------------------------------------------------------------------ Fig. 2 zoom out / zoom in
ss = pd.read_csv(os.path.join(OUT, 'scale_scores_by_model_run.csv'))
panels = [('SDO7s', 'SDO₇(s)', (1, 7), 4), ('SDO6-EN', 'SDO₆, English', (1, 7), 4),
          ('SDO6-ZH', 'SDO₆, Chinese', (1, 7), 4), ('VSA', 'VSA', (-4, 4), 0)]
fig, axes = plt.subplots(1, 4, figsize=(7.09, 2.7), sharey=False)
for ax, (key, title, ylim, mid) in zip(axes, panels):
    d = ss[ss.inst == key]
    for _, g in d.groupby('display'):
        g = g.sort_values('run')
        ax.plot(g.run + 1, g.score, color=GREY, lw=0.8, alpha=0.75, zorder=1)
    pooled = d.groupby('run').score.mean()
    ax.plot(pooled.index + 1, pooled.values, color=BLUE, lw=2.2, marker='o', ms=4,
            markeredgecolor='white', markeredgewidth=0.8, zorder=3)
    ax.axhline(mid, color=INK2, lw=0.8, ls=(0, (3, 3)), zorder=0)
    ax.set_xlim(0.7, 5.3); ax.set_ylim(*ylim)
    ax.set_xticks([1, 2, 3, 4, 5]); ax.set_xlabel('Run')
    ax.set_title(title, loc='left')
    ax.grid(axis='y', color=GRID, lw=0.6); ax.set_axisbelow(True)
    for s in ('top', 'right'): ax.spines[s].set_visible(False)
axes[0].set_ylabel('Scale score (1 = lowest SDO)')
axes[3].set_ylabel('Scale score (−4 = lowest)')
from matplotlib.lines import Line2D
handles = [Line2D([0], [0], color=GREY, lw=1, label='Each model (27 lines)'),
           Line2D([0], [0], color=BLUE, lw=2.2, marker='o', ms=4, markeredgecolor='white', label='Mean of all 27 models'),
           Line2D([0], [0], color=INK2, lw=0.8, ls=(0, (3, 3)), label='Scale midpoint')]
fig.legend(handles=handles, loc='upper center', ncol=3, frameon=False, bbox_to_anchor=(0.5, 1.02), fontsize=7.5)
fig.tight_layout(rect=(0, 0, 1, 0.93), w_pad=1.2)
for ext in ('png', 'pdf'):
    fig.savefig(os.path.join(FIG, f'Report_Fig2_zoom_out_zoom_in.{ext}'), bbox_inches='tight', facecolor='white')
plt.close(fig)

# ------------------------------------------------------------------ Fig. 5 generations
gt = pd.read_csv(os.path.join(OUT, 'generation_table.csv'))
gt = gt[gt.family.isin(['GPT-4', 'GPT-5', 'Claude 4', 'Claude 5'])]
groups = ['GPT-4', 'GPT-5', 'Claude 4', 'Claude 5']
xpos = {'GPT-4': 0, 'GPT-5': 1.3, 'Claude 4': 3.0, 'Claude 5': 4.8}
col = {'GPT-4': BLUE, 'Claude 4': BLUE, 'GPT-5': ORANGE, 'Claude 5': ORANGE}
metrics = [('att_instab', 'Attitude items whose answer\nchanged across five runs (%)', (0, 50)),
           ('rmet_instab', 'RMET items whose answer\nchanged across five runs (%)', (0, 50)),
           ('rmet_acc', 'RMET accuracy (%)', (40, 100))]
fig, axes = plt.subplots(1, 3, figsize=(7.09, 2.9))
rng = np.random.default_rng(7)
for ax, (m, lab, ylim) in zip(axes, metrics):
    labels = []
    for gname in groups:
        v = gt.loc[gt.family == gname, m].dropna().values
        x0 = xpos[gname]
        jit = (rng.random(len(v)) - 0.5) * 0.36
        ax.scatter(x0 + jit, v, s=26, color=col[gname], edgecolor='white', linewidth=0.8, zorder=3)
        if len(v):
            ax.plot([x0 - 0.28, x0 + 0.28], [np.median(v)] * 2, color=INK, lw=1.6, zorder=4)
        labels.append(f'{gname}\nn = {len(v)}')
    if m == 'rmet_acc':
        ax.axhline(100 * 26.2 / 36, color=INK2, lw=0.8, ls=(0, (3, 3)), zorder=0)
        ax.text(-0.5, 100 * 26.2 / 36 - 0.8, 'human mean', fontsize=6, color=INK2, va='top', ha='left')
    ax.set_xticks([xpos[g] for g in groups]); ax.set_xticklabels(labels, fontsize=6.5)
    ax.set_xlim(-0.6, 5.6); ax.set_ylim(*ylim); ax.set_ylabel(lab)
    ax.grid(axis='y', color=GRID, lw=0.6); ax.set_axisbelow(True)
    for s in ('top', 'right'): ax.spines[s].set_visible(False)
handles = [Line2D([0], [0], marker='o', color='none', markerfacecolor=BLUE, markeredgecolor='white', ms=6, label='Older generation'),
           Line2D([0], [0], marker='o', color='none', markerfacecolor=ORANGE, markeredgecolor='white', ms=6, label='Newer generation'),
           Line2D([0], [0], color=INK, lw=1.6, label='Group median')]
fig.legend(handles=handles, loc='upper center', ncol=3, frameon=False, bbox_to_anchor=(0.5, 1.02), fontsize=7.5)
for ax, letter in zip(axes, 'ABC'):
    ax.text(-0.22, 1.04, letter, transform=ax.transAxes, fontsize=10, fontweight='bold', color=INK)
fig.tight_layout(rect=(0, 0, 1, 0.92), w_pad=1.5)
for ext in ('png', 'pdf'):
    fig.savefig(os.path.join(FIG, f'Report_Fig5_older_vs_newer.{ext}'), bbox_inches='tight', facecolor='white')
plt.close(fig)
print('figures written to', FIG)
