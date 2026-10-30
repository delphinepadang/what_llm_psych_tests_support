# Figures 2 and 3: revision options

Two chart-based versions are provided. Both use the original study data.

## 01_Revised

Preserves the original chart types, removes long explanations, adds formal panel headings, and retains only Runs 1-5 in Figure 2B. The former Figure 2C is now Supplementary Figure S14.

## 02_Alternative_charts

Figure 2: grouped response-distribution bars and one paired score plot per run. Figure 3: item-level response trajectories, a scatterplot of all 106 defined reliability values, and a point-and-interval plot of later-run response variation.

The scatterplot retains the 106 combinations shown in the original grouped bars. All markers have equal size. A direct annotation identifies 29 combinations at the exact same coordinates (alpha = 1.0, response change = 0%); exact values are in source_data/Fig3B_all_points.csv.

## Selected main figures

The chosen pair is Figure 2 revised and Figure 3 alternative. They are saved as Figures/Main_figures/Fig2_language.{png,svg} and Fig3_answer_changes.{png,svg}. Figures/Main_figures/Figures_2_and_3.pdf contains artwork only: no full captions, review headings or footers. Captions are saved separately in Figures_2_and_3_captions.md. The difference column has been removed from Figure 2B. Original main PNGs are preserved in Revision_options/00_Originals/.

## Alternative-chart axes and colours

| Panel | Horizontal axis | Vertical axis |
|---|---|---|
| Fig. 2A | SDO6 response category (1-7) | Responses within each language (%) |
| Fig. 2B | Language, separately for each run | Mean of the 16 rescored SDO6 responses |
| Fig. 3A | Repeated run (1-5) | Rescored item response |
| Fig. 3B | Response reliability (Krippendorff's interval alpha) | Items changed across five runs (%) |
| Fig. 3C | Initial response (Run 1) | Items varying among Runs 2-5 (%) |

In Figure 2, English is blue and Chinese is orange, using the same blue and orange as Figure 3. In Figure 3A, orange identifies items that changed and blue identifies items that did not change; Figure 3B retains its reliability-group colour key, and Figure 3C retains its response-category colour scale.

The selected Figure 3 retains the VSA and SDO panel headings, with the scale names defined in the separate caption. Axis labels use plain wording without these acronyms. The alpha annotation and the .80 reference guide are neutral in colour; blue identifies unchanged response data in panels A and B rather than the reliability statistic.

## Panel headings

- Fig. 2A: SDO6 response distributions by language
- Fig. 2B: SDO6 scores across runs: GPT-5
- Fig. 3A: VSA responses across runs: GPT-5.5 (alternative: VSA response trajectories: GPT-5.5)
- Fig. 3B: Reliability and response changes
- Fig. 3C: SDO response variation by initial response

## Captions

### Fig2_language_revised

Fig. 2. Models gave less strongly egalitarian responses in Chinese on average, but the direction of the language difference varied across runs for some models. (A) Proportion of SDO6 responses at each scale point, shown separately by language and pooled across 27 models, 16 items and five runs. Reverse-keyed items were rescored so that 1 indicates the most egalitarian response and 7 the most hierarchy-endorsing response. Percentages use all valid responses within each language (English, n = 2,156; Chinese, n = 2,146). (B) GPT-5's mean SDO6 score in English and Chinese in each of five runs. In each run, GPT-5 answered all 16 items once in each language; the score is the mean of the 16 rescored responses. Arrows connect the English score to the Chinese score. Orange highlights Run 4, in which the direction of the language difference reversed. Model-level patterns across runs are shown in Supplementary Fig. S14.

### Fig3_answer_changes_revised

Fig. 3. High reliability did not imply identical responses across repeated runs. (A) GPT-5.5's rescored responses to the six Very Short Authoritarianism (VSA) items across five runs. In each run, the model answered each item once on the -4 to +4 response scale. Orange cells mark responses that differ from the modal response for that item; shaded rows identify items with at least one response change. Krippendorff's interval α was .94, exceeding the .80 benchmark, although three of six items changed across runs. (B) Model-scale combinations grouped by whether Krippendorff's interval α met the .80 benchmark and by the proportion of items with at least one response change across five runs. The four scale versions were SDO7(s) in English, SDO6 in English, SDO6 in Chinese and VSA in English. Of 108 combinations (27 models × four versions), two had invariant responses and undefined α and were omitted. Among the 70 combinations with α ≥ .80, 41 changed at least one item, including 12 that changed at least 20% of items. (C) Proportion of model-item combinations with at least two distinct responses among Runs 2-5, grouped by the rescored response in Run 1. All 27 models and the three social dominance orientation (SDO) scale versions were pooled; only model-item combinations with five valid responses were included. Run 1 defines the groups and is excluded from the response-variation calculation. Values of n indicate model-item combinations per group; error bars show 95% Wilson intervals for the pooled proportions.

### Fig2_language_alternative

Fig. 2. Models gave less strongly egalitarian responses in Chinese on average, but the direction of the language difference varied across runs for some models. (A) Proportion of SDO6 responses at each scale point, shown separately by language (English, blue; Chinese, orange) and pooled across 27 models, 16 items and five runs. Reverse-keyed items were rescored so that 1 indicates the most egalitarian response and 7 the most hierarchy-endorsing response. Percentages use all valid responses within each language (English, n = 2,156; Chinese, n = 2,146). (B) GPT-5's mean SDO6 score in English and Chinese in each of five runs. In each run, GPT-5 answered all 16 items once in each language; the score is the mean of the 16 rescored responses. In each run-specific plot, the horizontal axis identifies language and the vertical axis shows the mean SDO6 score. In each small multiple, an arrow connects the English score to the Chinese score; signed values indicate the Chinese score minus the English score, calculated before rounding. Orange highlights Run 4, in which the direction of the language difference reversed. Model-level patterns across runs are shown in Supplementary Fig. S14.

### Fig3_answer_changes_alternative

Fig. 3. High reliability did not imply identical responses across repeated runs. (A) GPT-5.5's rescored responses to the six Very Short Authoritarianism (VSA) items across five runs. In each run, the model answered each item once on the -4 to +4 response scale. Each small multiple shows one item's response trajectory: the horizontal axis is repeated run (1-5), and the vertical axis is the rescored response to that individual item, rather than an average. All items use a shared vertical scale; orange indicates items with at least one response change, and blue indicates unchanged items. Krippendorff's interval α was .94, exceeding the .80 benchmark, although three of six items changed across runs. (B) Points show model-scale combinations, positioned by Krippendorff's interval α and the percentage of items with at least one response change across five runs. The dashed line marks α = .80. Orange points meet this benchmark despite response changes; blue points meet it without changes; grey points fall below it. All markers have the same size. The labelled point at α = 1.0 and 0% response change represents 29 combinations at identical coordinates; the label identifies their overlap. The four scale versions were SDO7(s) in English, SDO6 in English, SDO6 in Chinese and VSA in English. Of 108 combinations (27 models × four versions), two had invariant responses and undefined α and were omitted. Among the 70 combinations with α ≥ .80, 41 changed at least one item, including 12 that changed at least 20% of items. (C) Proportion of model-item combinations with at least two distinct responses among Runs 2-5, grouped by the rescored response in Run 1. All 27 models and the three social dominance orientation (SDO) scale versions were pooled; only model-item combinations with five valid responses were included. Run 1 defines the groups and is excluded from the response-variation calculation. Values of n indicate model-item combinations per group; error bars show 95% Wilson intervals for the pooled proportions.

### FigS14_language_direction

Supplementary Fig. S14. Language-effect direction across repeated runs. Each tile represents one of the 27 models, grouped by model family and coloured according to the direction of its mean Chinese-minus-English SDO6 score difference across five runs. Scores use rescored valid responses. Twenty models scored higher in Chinese in every run; one scored at least as high in Chinese in all runs, with one tie; one scored at least as high in English in all runs, with one tie. Orange identifies the five models with both positive and negative differences across runs. This panel was moved from the original Figure 2C; S14 continues the existing S1-S13 supplementary numbering.

## Files and reproduction

- Each figure has a 600 dpi PNG and an editable SVG with text preserved.
- Figures_2_and_3_review.pdf contains all five figures, with their captions.
- Supplementary Figure S14 is in Figures/Supplementary_figures/.
- Source CSVs contain the plotted values and denominators.
- Run Analysis_script/13_figure_revision_options.py with reportlab, pypdf and Poppler to regenerate.
- Original main figures and script 08 are preserved.

## Data checks

Distribution denominators: English 2,156; Chinese 2,146 (all valid rescored responses). The caption distinguishes these from the nominal 27 × 16 × 5 design. GPT-5 has all 16 items in each language in each run. Figure 3B counts: α ≥ .80, 29/29/12; α < .80, 0/4/32. Two invariant combinations have undefined α and are excluded from both designs. Figure 3C denominators are 682, 244, 43, 66 and 30. Variation is calculated only among Runs 2-5, not as a difference from Run 1. Confidence intervals reproduce the original pooled Wilson intervals.
