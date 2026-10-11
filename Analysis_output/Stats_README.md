# MS_AAI_ToM - statistical analysis output
Generated 2026-10-06 16:11 by Analysis_script/02_statistical_analysis.R

- `stats_log.txt`: full log with every test statistic, section by section
- `Stats_tables/`: every table in the log as CSV (prefix = section number)
- `Stats_tables/S19_test_registry.csv`: all hypothesis tests with BH and Holm adjustment
- `analysis_results.rds`: objects read by 03_figures.R

## Tables

| File | Contents |
|---|---|
| S01_design.csv | Table S1.1. Analysis design after exclusions |
| S01_excluded_models.csv | Table S1.2. Models excluded (API failure > 50% of calls) |
| S01_audit_rmet.csv | Table S1.3. RMET data-integrity checks |
| S01_audit_sdo_recoding.csv | Table S1.4. Reverse-key recoding audit (source value_recoded vs re-derived key) |
| S01_nonscored_by_model.csv | Table S1.5. Non-scored responses by model (models with any) |
| S01_nonscored_by_item.csv | Table S1.6. SDO/VSA refusals, empty and unparseable responses by item |
| S02_rmet_accuracy_overall.csv | Table S2.1. RMET accuracy with four interval schemes (95%) |
| S02_rmet_accuracy_by_model.csv | Table S2.2. RMET accuracy by model, % [95% item-bootstrap CI]; score out of 36 |
| S02_rmet_vs_human.csv | Table S2.3. LLM models vs human RMET benchmarks (Welch t, Hedges g, TOST with SESOI = +/-1.8 items) |
| S03_rmet_glmm_fixed.csv | Table S3.1. GLMM fixed effects (odds ratios) |
| S03_rmet_glmm_varcomp.csv | Table S3.2. GLMM variance components (logit scale) |
| S03_rmet_definitions_paired.csv | Table S3.3. Model-level paired comparison, definitions minus none (% points); TOST SESOI = +/-5.0 pp |
| S03_rmet_rm_anova.csv | Table S3.4. Repeated-measures ANOVA on sitting accuracy (Greenhouse-Geisser) |
| S03_rmet_definitions_meta.csv | Table S3.5. Random-effects meta-analysis of per-model definition effects (% points) |
| S03_rmet_definitions_by_model.csv | Table S3.6. Per-model definition effect (paired over items), BH-adjusted |
| S04_rmet_instability_overall.csv | Table S4.1. RMET model-item pairs that changed across 5 repeats (%, pigeonhole 95% CI) |
| S04_rmet_reliability_by_model.csv | Table S4.2. RMET reliability by model and condition |
| S04_rmet_aggregate_reliability.csv | Table S4.3. Aggregate reliability: sitting-score ICCs vs pair-level alpha |
| S04_rmet_instability_by_developer.csv | Table S4.4. RMET instability by developer group (pooled over conditions) |
| S05_rmet_items.csv | Table S5.1. RMET item analysis (sorted by accuracy) |
| S06_rmet_wrong_answer_agreement.csv | Table S6.1. Probability that two erring models choose the same foil |
| S06_rmet_cluster_silhouette.csv | Table S6.2. Average silhouette width by number of clusters (error consistency) |
| S06_rmet_error_clusters.csv | Table S6.3. Error-consistency clusters |
| S07_instability_by_instrument.csv | Table S7.1. Model-item pairs that changed across 5 runs (%, pigeonhole 95% CI) |
| S07_reliability_by_aggregation_level.csv | Table S7.2. Reliability at three levels of aggregation (ICC(1,1), ICC(A,1), 95% CI) |
| S07_rank_stability.csv | Table S7.3. Rank-order stability of model scale scores between any two runs |
| S07_instability_by_model.csv | Table S7.4. Instability and item-profile reliability by model and instrument |
| S07_reliability_generalisation.csv | Table S7.5. Random-effects meta-analysis of instability rates across models (logit, back-transformed) |
| S08_g_study_variance_shares.csv | Table S8.1. G-study: share of response variance by source |
| S08_g_coefficients_observed.csv | Table S8.2. G coefficients for the observed design (5 runs) |
| S08_d_study_runs_needed.csv | Table S8.3. D-study: runs needed to reach 0.80 / 0.90 / 0.95 (NA = not reached by 20) |
| S09_order_permutation_tests.csv | Table S9.1. Within-pair permutation tests of run order |
| S09_mean_deviation_by_run.csv | Table S9.2. Mean within-pair deviation by run |
| S09_sdo_rm_anova.csv | Table S9.3. RM-ANOVA on SDO/VSA scale scores (0-1): instrument x run, models as subjects |
| S09_rmet_first_sitting_by_model.csv | Table S9.4. RMET first sitting minus mean of sittings 2-5, by model (% points) |
| S10_response_position.csv | Table S10.1. Response position by instrument (recoded metric) |
| S10_instability_by_position_band.csv | Table S10.2. Instability by mean scale position (descriptive) |
| S10_instrument_or_by_position.csv | Table S10.3. Odds of a pair changing, relative to SDO6-EN, before and after conditioning on position |
| S10_unpinned_comparison.csv | Table S10.4. Instability among pairs that never touched an endpoint |
| S10_position_curves.csv | Table S10.5. Probability a pair changes, by first-run position (observed and GLMM) |
| S07_icc_raw_vs_recoded.csv | Table S7.6. Item-level ICC on raw vs reverse-key-corrected responses; single run vs mean of 5 |
| S11_subscale_summary.csv | Table S11.1. Dominance vs anti-egalitarianism within models (exact permutation, BH) |
| S11_subscales_by_model.csv | Table S11.2. Subscale scores by model |
| S11_rank_reversals.csv | Table S11.3. How often subscales order models differently from each other and from the total |
| S11_response_styles.csv | Table S11.4. Response styles (raw metric): acquiescence (ARS, -1 to 1), extreme (ERS) and midpoint (MRS) responding |
| S11_response_styles_language.csv | Table S11.5. Response styles, Chinese minus English SDO6 (paired over models) |
| S11_ers_vs_instability.csv | Table S11.6. Extreme responding vs instability across models (Spearman, bootstrap CI) |
| S12_internal_structure.csv | Table S12.1. Internal consistency (bootstrap CI) and exact coherence tests |
| S12_item_rest_correlations.csv | Table S12.2. Item-rest correlations across models (low values = item does not cohere) |
| S12_factorability.csv | Table S12.3. Factorability (KMO, Bartlett) and parallel analysis |
| S12_efa_loadings.csv | Table S12.4. Exploratory factor loadings (minres; 2-factor oblimin) - exploratory, n = models |
| S12_language_score_equivalence.csv | Table S12.5. Are English and Chinese SDO6 scores interchangeable? (models as subjects) |
| S13_language_descriptives.csv | Table S13.1. SDO6 responses by language (recoded, 1-7) |
| S13_lmm_varcomp.csv | Table S13.2. LMM variance components |
| S13_language_meta.csv | Table S13.3. Random-effects meta-analysis of per-model language shifts (scale points) |
| S13_language_by_item.csv | Table S13.4. Language shift by SDO6 item (paired over models, BH-adjusted) |
| S13_dif_robust_linking.csv | Table S13.5. Item-level DIF by robust (median) linking, bootstrap over models, BH-adjusted |
| S13_extremity_vs_direction.csv | Table S13.6. Decomposition of the language shift: extremity change vs direction reversal |
| S14_rmet_drift_by_model.csv | Table S14.1. RMET: agreement within September vs between June and September, by model |
| S14_sdo_vsa_drift.csv | Table S14.2. SDO7s and VSA, June vs August (5 runs each) |
| S15_alignment_by_model.csv | Table S15.1. Floor pinning (SDO7s + SDO6-EN) and instability by model |
| S15_alignment_tests.csv | Table S15.2. Alignment-proxy correlations (Spearman; bootstrap 95% CI; permutation p) |
| S16_pca_variance.csv | Table S16.1. PCA eigenvalues and Horn's parallel analysis |
| S16_pca_loadings.csv | Table S16.2. PCA loadings (correlations of features with components) |
| S16_pca_supplementary_rmet.csv | Table S16.3. RMET as supplementary variables (Spearman with component scores) |
| S16_cluster_silhouette.csv | Table S16.4. Average silhouette by number of clusters |
| S16_cluster_stability.csv | Table S16.5. Cluster stability (k = 2); ARI with PAM = -0.081 |
| S16_cluster_profiles.csv | Table S16.6. Cluster membership and mean profile |
| S17_cross_instrument_correlations.csv | Table S17.1. Spearman correlations across models (pairwise n; bootstrap CI; permutation p; BH) |
| S18_sensitivity.csv | Table S18.1. Smallest standardised effect detectable (alpha = .05, two-sided) |
| S18_tost_power.csv | Table S18.2. Power of the equivalence tests at the achieved design |
| S18_origin_sample_size.csv | Table S18.3. Models needed to detect a developer-origin difference in the language shift (80% power); observed g with 2 China-developed models = -0.63 |
| S18_models_for_precision.csv | Table S18.4. Models needed to estimate average instability within +/-5 or +/-10 percentage points |
| S19_test_registry.csv | Table S19.1. Registry of every hypothesis test |
| S19_registry_summary.csv | Table S19.2. Tests per family and number significant |
