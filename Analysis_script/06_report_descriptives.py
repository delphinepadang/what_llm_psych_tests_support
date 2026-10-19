"""
MS_AAI_ToM  |  06_report_descriptives.py
Analyses for the report version of the manuscript (October 2026).
Requires Python 3 with pandas, numpy and scipy. Run from the project root:
    python3 Analysis_script/06_report_descriptives.py
Outputs go to Analysis_output/Report_outputs/ (CSV tables and analysis_report_log.txt).

Reads the raw response files in Results/ and
  (1) re-derives headline numbers already reported in Analysis_output (as a check), and
  (2) computes the additional descriptives used in the report:
      scale scores by model, pooled scale scores by run ("zoom out"),
      the attitude summary by instrument (report Table 2), item-level answer
      distributions, and the older- vs newer-generation comparison.

Scoring follows Analysis_script/02_statistical_analysis.R:
  - Claude Sonnet 4 excluded (API error on every call).
  - Reverse keys: SDO7(s) items 3, 4, 7, 8 (8 - x); SDO6 items 9-16 (8 - x); VSA items 1, 4, 5 (-x).
  - A model-item pair is "complete" if all five runs were scored; it "changed" if the five answers
    were not identical.
  - Scale score for a model in a run = mean of that model's complete items in that run
    (as in 04_reframing_checks.R). A model's overall score on an instrument = mean of all
    its scored answers (as in 02's language comparison).
  - RMET accuracy: mean over scored runs within model-item(-condition), then over items.
  - Generation: family labels from MODEL_LOOKUP in 02_statistical_analysis.R
    (GPT-4 vs GPT-5; Claude 4 vs Claude 5). Llama and Qwen are reported descriptively only.

Usage: python3 Analysis_script/06_report_descriptives.py [Results dir] [output dir]
"""
import sys, os, json, itertools
import numpy as np
import pandas as pd
from scipy import stats

RES = sys.argv[1] if len(sys.argv) > 1 else 'Results'
OUT = sys.argv[2] if len(sys.argv) > 2 else os.path.join('Analysis_output', 'Report_outputs')
os.makedirs(OUT, exist_ok=True)
rng = np.random.default_rng(20260922)
LOG = []
def say(*a):
    s = ' '.join(str(x) for x in a)
    print(s); LOG.append(s)

LOOKUP = {
 'gpt-4o': ('GPT-4o', 'OpenAI', 'GPT-4'), 'gpt-4.1': ('GPT-4.1', 'OpenAI', 'GPT-4'),
 'gpt-5': ('GPT-5', 'OpenAI', 'GPT-5'), 'gpt-5.1': ('GPT-5.1', 'OpenAI', 'GPT-5'),
 'gpt-5.2': ('GPT-5.2', 'OpenAI', 'GPT-5'), 'gpt-5.4': ('GPT-5.4', 'OpenAI', 'GPT-5'),
 'gpt-5.5': ('GPT-5.5', 'OpenAI', 'GPT-5'), 'gpt-5.6-sol': ('GPT-5.6-sol', 'OpenAI', 'GPT-5'),
 'gpt-oss-20b': ('GPT-OSS 20B', 'OpenAI', 'GPT-OSS'), 'gpt-oss-120b': ('GPT-OSS 120B', 'OpenAI', 'GPT-OSS'),
 'claude-haiku-4-5': ('Claude Haiku 4.5', 'Anthropic', 'Claude 4'), 'claude-sonnet-4': ('Claude Sonnet 4', 'Anthropic', 'Claude 4'),
 'claude-sonnet-4-5': ('Claude Sonnet 4.5', 'Anthropic', 'Claude 4'), 'claude-sonnet-4-6': ('Claude Sonnet 4.6', 'Anthropic', 'Claude 4'),
 'claude-opus-4-1': ('Claude Opus 4.1', 'Anthropic', 'Claude 4'), 'claude-opus-4-5': ('Claude Opus 4.5', 'Anthropic', 'Claude 4'),
 'claude-opus-4-6': ('Claude Opus 4.6', 'Anthropic', 'Claude 4'), 'claude-opus-4-7': ('Claude Opus 4.7', 'Anthropic', 'Claude 4'),
 'claude-opus-4-8': ('Claude Opus 4.8', 'Anthropic', 'Claude 4'), 'claude-sonnet-5': ('Claude Sonnet 5', 'Anthropic', 'Claude 5'),
 'claude-opus-5': ('Claude Opus 5', 'Anthropic', 'Claude 5'), 'claude-fable-5': ('Claude Fable 5', 'Anthropic', 'Claude 5'),
 'claude-fable-5-1': ('Claude Fable 5.1', 'Anthropic', 'Claude 5'), 'gemma-3-12b': ('Gemma 3 12B', 'Google', 'Gemma'),
 'llama-4-maverick': ('Llama 4 Maverick', 'Meta', 'Llama'), 'meta-llama-3-1-8b-instruct': ('Llama 3.1 8B', 'Meta', 'Llama'),
 'meta-llama-3-3-70b-instruct': ('Llama 3.3 70B', 'Meta', 'Llama'), 'qwen3-next-80b-a3b-instruct': ('Qwen3-Next 80B', 'Alibaba', 'Qwen'),
 'qwen35-122b-a10b': ('Qwen3.5 122B', 'Alibaba', 'Qwen'),
}
GEN = {'GPT-4': 'older', 'Claude 4': 'older', 'GPT-5': 'newer', 'Claude 5': 'newer'}

def meta(label):
    short = label.split('-', 1)[1]
    d, dev, fam = LOOKUP[short]
    return d, dev, fam

# ----------------------------------------------------------------------------- attitude data
a = pd.read_csv(os.path.join(RES, 'SDO_VSA_Results_20260830.csv'), encoding='utf-8-sig')
a = a.rename(columns={'repeat': 'run'})
a = a[a.model_label != 'databricks-claude-sonnet-4'].copy()
a['instrument'] = a['instrument'].replace({'SDO': 'SDO7s'})
a['item_no'] = a['question_id'].str.extract(r'(\d+)$')[0].astype(int)
a['inst'] = np.where(a.instrument == 'SDO6', 'SDO6-' + a.language.str.upper(), a.instrument)
rev = {('SDO7s', i) for i in (3, 4, 7, 8)} | {('VSA', i) for i in (1, 4, 5)} | {('SDO6', i) for i in range(9, 17)}
a['rev'] = [(ins, it) in rev for ins, it in zip(a.instrument, a.item_no)]
a['scored'] = a['value'].notna()
a['key'] = np.where(~a.scored, np.nan,
            np.where(a.rev & (a.instrument == 'VSA'), -a['value'],
            np.where(a.rev, 8 - a['value'], a['value'])))
a[['display', 'developer', 'family']] = pd.DataFrame([meta(m) for m in a.model_label], index=a.index)

say('ATTITUDE: models', a.model_label.nunique(), '| calls', len(a), '| scored', int(a.scored.sum()),
    f'({100*a.scored.mean():.1f}%)')

pairs = (a.groupby(['display', 'developer', 'family', 'inst', 'item_no'])
           .agg(n_scored=('scored', 'sum'), n_unique=('key', 'nunique'),
                kmin=('key', 'min'), kmax=('key', 'max'), kmean=('key', 'mean')).reset_index())
pairs['complete'] = pairs.n_scored == 5
cp = pairs[pairs.complete].copy()
cp['changed'] = cp.n_unique > 1
cp['changed2'] = (cp.kmax - cp.kmin) >= 2
say('Complete pairs', len(cp), 'of', len(pairs))
say('Pairs changed: %.3f%%; by >=2 points: %.3f%%' % (100*cp.changed.mean(), 100*cp.changed2.mean()))
for ins, g in cp.groupby('inst'):
    say(f'  {ins}: pairs {len(g)}, changed {100*g.changed.mean():.1f}%, >=2pt {100*g.changed2.mean():.1f}%')

# per-model instability across all attitude items
mi = cp.groupby(['display', 'developer', 'family']).agg(n=('changed', 'size'), k=('changed', 'sum')).reset_index()
mi['pct'] = 100 * mi.k / mi.n
say('Per-model instability (all attitude items): min %.1f%% (%s), max %.1f%% (%s), median %.1f%%' % (
    mi.pct.min(), ', '.join(mi.loc[mi.pct == mi.pct.min(), 'display']),
    mi.pct.max(), ', '.join(mi.loc[mi.pct == mi.pct.max(), 'display']), mi.pct.median()))
say('  models with 0 changes:', ', '.join(mi.loc[mi.k == 0, 'display']) or 'none')
mi.sort_values('pct').to_csv(os.path.join(OUT, 'attitude_instability_by_model.csv'), index=False)

# scale scores: model x instrument x run, using complete items only
comp_keys = set(zip(cp.display, cp.inst, cp.item_no))
a['complete'] = [(d, i, it) in comp_keys for d, i, it in zip(a.display, a.inst, a.item_no)]
ss = (a[a.complete].groupby(['display', 'developer', 'family', 'inst', 'run'])['key'].mean()
        .reset_index(name='score'))
MID = {'SDO7s': 4, 'SDO6-EN': 4, 'SDO6-ZH': 4, 'VSA': 0}
ss['mid'] = ss.inst.map(MID)
ss.to_csv(os.path.join(OUT, 'scale_scores_by_model_run.csv'), index=False)

say('\nZOOM OUT: pooled mean scale score across models, by run')
pooled = ss.groupby(['inst', 'run'])['score'].mean().unstack('run')
for ins, row in pooled.iterrows():
    say(f'  {ins}: ' + ', '.join(f'{v:.3f}' for v in row.values) +
        f' | range {row.max()-row.min():.3f} | overall {ss[ss.inst==ins].groupby("display").score.mean().mean():.3f}')
pooled.to_csv(os.path.join(OUT, 'pooled_scale_score_by_run.csv'))

say('\nScale scores by model (mean of 5 runs) and side of midpoint')
ms = ss.groupby(['display', 'developer', 'family', 'inst']).agg(
        mean=('score', 'mean'), lo=('score', 'min'), hi=('score', 'max'), mid=('mid', 'first')).reset_index()
for ins, g in ms.groupby('inst'):
    m = MID[ins]
    runs = ss[ss.inst == ins]
    side = runs.assign(below=runs.score < m, above=runs.score > m, at=runs.score == m)
    agg = side.groupby('display').agg(below=('below', 'all'), above=('above', 'all'),
                                      any_below=('below', 'any'), any_above=('above', 'any'), any_at=('at', 'any'))
    say(f'  {ins}: mean of model means {g["mean"].mean():.3f} (model range {g["mean"].min():.3f} to {g["mean"].max():.3f}); '
        f'below midpoint every run {int(agg.below.sum())}, above every run {int(agg.above.sum())}, '
        f'both sides across runs {int((agg.any_below & agg.any_above).sum())}, '
        f'touching midpoint in some run {int(agg.any_at.sum())}; '
        f'model means above midpoint: {int((g["mean"] > m).sum())}')
    if ins == 'VSA':
        odd = agg[~agg.below & ~agg.above]
        for d, r in odd.iterrows():
            sc = runs[runs.display == d].sort_values('run').score.round(3).tolist()
            say(f'     VSA not below every run: {d}: runs {sc}')
ms.to_csv(os.path.join(OUT, 'scale_scores_by_model.csv'), index=False)

# overall model score = mean of all scored answers (as in 02's language comparison)
ms_all = (a[a.scored].groupby(['display', 'developer', 'family', 'inst'])['key'].mean()
            .reset_index(name='score_all'))
ms_all.to_csv(os.path.join(OUT, 'overall_scores_by_model.csv'), index=False)

# ----------------------------------------------------------------------------- report Table 2
say('\nTABLE 2: attitude answers by instrument')
BOUNDS = {'SDO7s': (1, 7, 4), 'SDO6-EN': (1, 7, 4), 'SDO6-ZH': (1, 7, 4), 'VSA': (-4, 4, 0)}
t2 = []
for ins in ['SDO7s', 'SDO6-EN', 'SDO6-ZH', 'VSA']:
    lo, hi, mid = BOUNDS[ins]
    resp = a[(a.inst == ins) & a.scored]
    mm = ms_all[ms_all.inst == ins].set_index('display').score_all
    runs = ss[ss.inst == ins]
    sd_ = runs.assign(below=runs.score < mid, above=runs.score > mid, at=runs.score == mid).groupby('display').agg(
        below_all=('below', 'all'), any_below=('below', 'any'), any_above=('above', 'any'), any_at=('at', 'any'),
        n_above=('above', 'sum'), n_at=('at', 'sum'))
    pr = cp[cp.inst == ins]
    row = dict(instrument=ins, n_scored=len(resp), mean_response=resp.key.mean(),
               model_mean_min=mm.min(), model_mean_min_model=mm.idxmin(),
               model_mean_max=mm.max(), model_mean_max_model=mm.idxmax(),
               models_mean_above_midpoint=int((mm > mid).sum()),
               models_below_midpoint_every_run=int(sd_.below_all.sum()),
               models_both_sides_across_runs=int((sd_.any_below & sd_.any_above).sum()),
               models_at_midpoint_in_some_run=int(sd_.any_at.sum()),
               pct_answers_at_floor=100 * (resp.key == lo).mean(),
               pct_answers_at_midpoint=100 * (resp.key == mid).mean(),
               complete_pairs=len(pr), pct_pairs_changed=100 * pr.changed.mean(),
               pct_pairs_changed_2plus=100 * pr.changed2.mean())
    t2.append(row)
    say('  ' + ins + ': ' + ', '.join(f'{k} {v:.3f}' if isinstance(v, float) else f'{k} {v}' for k, v in row.items() if k != 'instrument'))
    odd = sd_[~sd_.below_all]
    for d_, r_ in odd.iterrows():
        sc = runs[runs.display == d_].sort_values('run').score.round(3).tolist()
        say(f'     not below midpoint in every run: {d_}: runs {sc}')
pd.DataFrame(t2).to_csv(os.path.join(OUT, 'table2_attitude_by_instrument.csv'), index=False)

# ----------------------------------------------------------------------------- item-level answers
say('\nITEM-LEVEL ANSWERS (raw response scale; keyed mean = after reverse-keying)')
txt = a[a.language.eq('en')].drop_duplicates(['instrument', 'item_no']).set_index(['instrument', 'item_no']).question_text
irows = []
for (inst, ins, it), g in a[a.scored].groupby(['instrument', 'inst', 'item_no']):
    lo, hi, mid = BOUNDS[ins]
    v = g['value']
    rowm = g.groupby('display')['value'].mean()
    irows.append(dict(inst=ins, item_no=it, reverse_keyed=bool(g.rev.iloc[0]), n=len(g),
                      text_en=txt.get((inst, it), ''), mean_raw=v.mean(), mean_keyed=g.key.mean(),
                      pct_lowest=100 * (v == lo).mean(), pct_below_mid=100 * (v < mid).mean(),
                      pct_mid=100 * (v == mid).mean(), pct_above_mid=100 * (v > mid).mean(),
                      pct_highest=100 * (v == hi).mean(),
                      models_mean_below_mid=int((rowm < mid).sum()), models_mean_above_mid=int((rowm > mid).sum())))
items = pd.DataFrame(irows)
items.to_csv(os.path.join(OUT, 'item_answer_distribution.csv'), index=False)
for _, r_ in items.iterrows():
    say(f"  {r_.inst} {r_.item_no:>2} rev={int(r_.reverse_keyed)} n={r_.n}: raw mean {r_.mean_raw:.2f}, keyed {r_.mean_keyed:.2f}; "
        f"lowest {r_.pct_lowest:.1f}%, below mid {r_.pct_below_mid:.1f}%, mid {r_.pct_mid:.1f}%, above mid {r_.pct_above_mid:.1f}%, "
        f"highest {r_.pct_highest:.1f}%; models mean below/above mid {r_.models_mean_below_mid}/{r_.models_mean_above_mid} | {str(r_.text_en)[:70]}")
# SDO7(s) subscales (dominance items 1-4, anti-egalitarianism items 5-8), mean of model means as in 02 S11
s7 = a[(a.inst == 'SDO7s') & a.scored].assign(sub=lambda x: np.where(x.item_no <= 4, 'D', 'AE'))
s7m = s7.groupby(['display', 'item_no']).key.mean().reset_index().assign(sub=lambda x: np.where(x.item_no <= 4, 'D', 'AE'))
s7s = s7m.groupby(['display', 'sub']).key.mean().unstack()
say(f"  SDO7(s) dominance {s7s['D'].mean():.3f} vs anti-egalitarianism {s7s['AE'].mean():.3f} (mean of model means)")
# models answering neutral to every VSA item
vv = a[(a.inst == 'VSA') & a.scored].groupby('display')['value'].agg(lambda s: bool((s == 0).all()))
say('  models answering 0 (Unsure/neutral) to every VSA request: ' + ', '.join(vv[vv].index))

# response distribution on VSA (native coding, keyed)
v = a[(a.inst == 'VSA') & a.scored]
say('\nVSA keyed responses: share at 0 (neutral) %.1f%%, below 0 %.1f%%, above 0 %.1f%%' % (
    100*(v.key == 0).mean(), 100*(v.key < 0).mean(), 100*(v.key > 0).mean()))
for it, g in v.groupby('item_no'):
    say(f'   VSA item {it} (rev={g.rev.iloc[0]}): mean keyed {g.key.mean():.2f}; neutral {100*(g.key==0).mean():.1f}%')

# ----------------------------------------------------------------------------- language
say('\nLANGUAGE (SDO6)')
l = ms_all[ms_all.inst.isin(['SDO6-EN', 'SDO6-ZH'])].pivot_table(index='display', columns='inst', values='score_all')
d = l['SDO6-ZH'] - l['SDO6-EN']   # model-level shift from all scored answers, as in 02
say(f'  model-level mean EN {l["SDO6-EN"].mean():.3f}, ZH {l["SDO6-ZH"].mean():.3f}; diff {d.mean():.3f}; '
    f'higher in ZH {int((d>0).sum())} of {len(d)}')
t = stats.ttest_rel(l['SDO6-ZH'], l['SDO6-EN'])
say(f'  paired t({len(d)-1}) = {t.statistic:.2f}, p = {t.pvalue:.2g}')
resp = a[a.instrument.eq('SDO6') & a.scored]
say('  response-level mean EN %.3f ZH %.3f' % (resp[resp.language == 'en'].key.mean(), resp[resp.language == 'zh'].key.mean()))

# ----------------------------------------------------------------------------- RMET
r = pd.read_csv(os.path.join(RES, 'RMET_Results_20260912.csv'), encoding='utf-8-sig').rename(columns={'repeat': 'run'})
r = r[r.model_label != 'databricks-claude-sonnet-4'].copy()
r['item'] = r.question_id.str.extract(r'(\d+)').astype(int)
opts = r.options.str.lower().str.split(r',\s*')
ch = r.choice.astype(str).str.strip().str.lower()
r['scored'] = [c in [o.strip() for o in op] for c, op in zip(ch, opts)]
r['correct'] = np.where(r.scored, (ch == r.correct_answer.str.strip().str.lower()).astype(float), np.nan)
r['cond'] = np.where(r.definitions_shown.astype(str).str.lower() == 'true', 'Definitions', 'No definitions')
r[['display', 'developer', 'family']] = pd.DataFrame([meta(m) for m in r.model_label], index=r.index)
say('\nRMET: models', r.model_label.nunique(), '| calls', len(r), '| scored', int(r.scored.sum()))
pi = r[r.scored].groupby(['display', 'developer', 'family', 'cond', 'item']).agg(
        acc=('correct', 'mean'), n=('correct', 'size'), nu=('choice', lambda s: s.str.lower().str.strip().nunique())).reset_index()
pi['changed'] = pi.nu > 1
acc_mc = pi.groupby(['display', 'developer', 'family', 'cond']).acc.mean().unstack('cond')
acc_mc['pooled'] = pi.groupby(['display', 'developer', 'family']).acc.mean()
for c in ['No definitions', 'Definitions', 'pooled']:
    say(f'  accuracy {c}: {100*acc_mc[c].mean():.2f}% = {36*acc_mc[c].mean():.2f}/36')
say('  model range pooled: %.1f%% to %.1f%%' % (100*acc_mc.pooled.min(), 100*acc_mc.pooled.max()))
HUMAN_MEAN, HUMAN_SD = 26.2, 3.6   # Baron-Cohen et al. (2001), general population, n = 122
s36 = 36 * acc_mc.pooled
say(f'  human general-population mean {HUMAN_MEAN}/36 = {100*HUMAN_MEAN/36:.1f}%')
say(f'  models within 1 human SD of the human mean: {int(((s36 - HUMAN_MEAN).abs() <= HUMAN_SD).sum())} of {len(s36)}; '
    f'above +1 SD: {int((s36 > HUMAN_MEAN + HUMAN_SD).sum())}; below -1 SD: {int((s36 < HUMAN_MEAN - HUMAN_SD).sum())}')
dd = 100 * (acc_mc['Definitions'] - acc_mc['No definitions'])
say(f'  definitions minus none: mean {dd.mean():.2f} pp; range {dd.min():.1f} to {dd.max():.1f}; '
    f'higher with defs {int((dd>0).sum())}, lower {int((dd<0).sum())}, equal {int((dd==0).sum())}')
say('  pairs changed (%%): none %.2f, defs %.2f, pooled %.2f' % (
    100*pi[pi.cond=='No definitions'].changed.mean(), 100*pi[pi.cond=='Definitions'].changed.mean(),
    100*pi.groupby(['display','cond']).changed.mean().groupby('display').mean().mean()))
rmi = pi.groupby(['display', 'developer', 'family']).changed.mean().reset_index(name='rmet_instab')
rmi['rmet_instab'] *= 100
# pooled accuracy by sitting (zoom out)
sit = r[r.scored].groupby(['display', 'run']).correct.mean().groupby('run').mean()
say('  pooled RMET accuracy by sitting (%%): ' + ', '.join(f'{100*x:.1f}' for x in sit.values))
item_acc = pi.groupby('item').acc.mean()
say('  item accuracy range: %.1f%% (item %d) to %.1f%%; items >= 90%%: %d; items < 50%%: %d' % (
    100*item_acc.min(), item_acc.idxmin(), 100*item_acc.max(), (item_acc >= .9).sum(), (item_acc < .5).sum()))

# ----------------------------------------------------------------------------- generations
say('\nOLDER vs NEWER GENERATION (GPT-4 vs GPT-5; Claude 4 vs Claude 5)')
gen_tab = mi[['display', 'developer', 'family', 'pct']].rename(columns={'pct': 'att_instab'})
ls = d.rename('lang_shift').reset_index()
sc = ms_all.pivot_table(index='display', columns='inst', values='score_all').reset_index()
gen_tab = gen_tab.merge(ls, on='display', how='left').merge(sc, on='display', how='left')
gen_tab = gen_tab.merge(acc_mc.reset_index()[['display', 'pooled']].rename(columns={'pooled': 'rmet_acc'}), on='display', how='outer')
gen_tab = gen_tab.merge(rmi[['display', 'rmet_instab']], on='display', how='left')
gen_tab['family'] = gen_tab['display'].map({v[0]: v[2] for v in LOOKUP.values()})
gen_tab['developer'] = gen_tab['display'].map({v[0]: v[1] for v in LOOKUP.values()})
gen_tab['rmet_acc'] *= 100
gen_tab['gen'] = gen_tab.family.map(GEN)
gen_tab.to_csv(os.path.join(OUT, 'generation_table.csv'), index=False)

def strat_perm(df, col, B=20000):
    """Difference newer - older, averaged over developers (OpenAI, Anthropic) with equal weight;
    p from permuting generation labels within developer."""
    df = df.dropna(subset=[col, 'gen'])
    def stat(x):
        out = []
        for dev, g in x.groupby('developer'):
            if g.gen.nunique() < 2: continue
            out.append(g.loc[g.gen == 'newer', col].mean() - g.loc[g.gen == 'older', col].mean())
        return np.mean(out)
    obs = stat(df)
    cnt = 0
    for _ in range(B):
        x = df.copy()
        x['gen'] = x.groupby('developer')['gen'].transform(lambda s: rng.permutation(s.values))
        if abs(stat(x)) >= abs(obs) - 1e-12: cnt += 1
    return obs, (cnt + 1) / (B + 1), len(df)

outcomes = [('rmet_acc', 'RMET accuracy (%)'), ('rmet_instab', 'RMET items changed (%)'),
            ('att_instab', 'Attitude items changed (%)'), ('SDO7s', 'SDO7(s) score'),
            ('SDO6-EN', 'SDO6 English score'), ('SDO6-ZH', 'SDO6 Chinese score'), ('VSA', 'VSA score'),
            ('lang_shift', 'Chinese minus English SDO6')]
gt = gen_tab[gen_tab.gen.notna()]
rows = []
for col, lab in outcomes:
    g = gt.dropna(subset=[col])
    o = g[g.gen == 'older'][col]; nw = g[g.gen == 'newer'][col]
    diff, p, n = strat_perm(g, col)
    per_dev = {dev: (round(x[x.gen=='older'][col].mean(), 3), round(x[x.gen=='newer'][col].mean(), 3),
                     int((x.gen=='older').sum()), int((x.gen=='newer').sum()))
               for dev, x in g.groupby('developer')}
    rows.append(dict(outcome=lab, n_older=len(o), n_newer=len(nw), older_mean=o.mean(), newer_mean=nw.mean(),
                     older_median=o.median(), newer_median=nw.median(), dev_balanced_diff=diff, p_perm=p,
                     by_developer=json.dumps(per_dev)))
    say(f'  {lab}: older {o.mean():.2f} (n={len(o)}), newer {nw.mean():.2f} (n={len(nw)}); '
        f'developer-balanced diff {diff:+.2f}; stratified permutation p = {p:.3f}; by developer {per_dev}')
gc = pd.DataFrame(rows)
_p = gc.p_perm.values; _o = np.argsort(_p); _m = len(_p)
_adj = np.minimum.accumulate((_p[_o] * _m / np.arange(1, _m + 1))[::-1])[::-1]
gc.loc[gc.index[_o], 'p_bh'] = np.minimum(_adj, 1)
for _, r_ in gc.iterrows():
    say(f'  BH-adjusted p, {r_.outcome}: {r_.p_bh:.3f}')
gc.to_csv(os.path.join(OUT, 'generation_comparison.csv'), index=False)

# version order within the GPT and Claude Opus lines (Spearman)
say('\nVersion order within lines (Spearman rho, exact permutation p)')
lines = {'GPT (4o<4.1<5<5.1<5.2<5.4<5.5<5.6-sol)': ['GPT-4o', 'GPT-4.1', 'GPT-5', 'GPT-5.1', 'GPT-5.2', 'GPT-5.4', 'GPT-5.5', 'GPT-5.6-sol'],
         'Claude Opus (4.1<4.5<4.6<4.7<4.8<5)': ['Claude Opus 4.1', 'Claude Opus 4.5', 'Claude Opus 4.6', 'Claude Opus 4.7', 'Claude Opus 4.8', 'Claude Opus 5']}
for name, order in lines.items():
    sub = gen_tab.set_index('display').loc[order]
    for col, lab in [('rmet_acc', 'RMET accuracy'), ('rmet_instab', 'RMET changed'), ('att_instab', 'attitude changed')]:
        y = sub[col].values; x = np.arange(len(order))
        rho = stats.spearmanr(x, y).statistic
        perms = [stats.spearmanr(x, p).statistic for p in itertools.permutations(y)] if len(y) <= 8 else None
        pval = np.mean([abs(pp) >= abs(rho) - 1e-12 for pp in perms]) if perms else np.nan
        say(f'  {name} | {lab}: rho = {rho:.2f}, exact p = {pval:.3f}; values ' + ', '.join(f'{v:.1f}' for v in y))

# Llama / Qwen descriptive
say('\nLlama and Qwen (descriptive only)')
for disp in ['Llama 3.1 8B', 'Llama 3.3 70B', 'Llama 4 Maverick', 'Qwen3-Next 80B', 'Qwen3.5 122B']:
    rrow = gen_tab[gen_tab.display == disp].iloc[0]
    say(f'  {disp}: attitude changed {rrow.att_instab:.1f}%; SDO6 lang shift {rrow.lang_shift:.3f}; '
        f'RMET acc {rrow.rmet_acc if pd.notna(rrow.rmet_acc) else "n/a"}')

open(os.path.join(OUT, 'analysis_report_log.txt'), 'w').write('\n'.join(LOG))
