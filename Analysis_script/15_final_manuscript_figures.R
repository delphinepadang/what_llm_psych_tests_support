# Final manuscript Figures 2-4 and Supplementary Figure S14 (former Figure 2C).
# Run from the project root:
#   Rscript Analysis_script/15_final_manuscript_figures.R
# Optional: --output-root /path/to/staging --verify-only --skip-reference-checks
# Uses base R, grid and viridisLite; ragg is used for PNGs/TIFFs when available.
# No Python or cached statistics supplies the data. Quartz handles PDF on macOS;
# Cairo is used elsewhere. Optional colorspace verifies colour-vision simulations.
# Raw files are read only. All coordinates, counts, alpha values and correlations
# are derived below. Archived plotted values are used ONLY for verification.
# External constants: published scoring keys; 5-run design; alpha benchmark .80;
# adult reference mean 26.2/36 (Baron-Cohen et al., 2001, as in the original figure).

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i)) return(default)
  if (i == length(args)) stop("Missing value after ", flag)
  args[i + 1L]
}
script_arg <- grep("^--file=", commandArgs(), value = TRUE)
script_file <- if (length(script_arg)) sub("^--file=", "", script_arg[1]) else ""
candidates <- c(arg_value("--project-root"), getwd(), dirname(getwd()),
                if (nzchar(script_file)) dirname(dirname(normalizePath(script_file))))
candidates <- candidates[vapply(candidates, function(p)
  file.exists(file.path(p, "Results", "SDO_VSA_Results_20260830.csv")), logical(1))]
if (!length(candidates)) stop("Run from the project folder or provide --project-root.")
ROOT <- normalizePath(candidates[1])
OUT <- normalizePath(arg_value("--output-root", ROOT), mustWork = FALSE)
DIR_MAIN <- file.path(OUT, "Figures", "Main_figures")
DIR_SUPP <- file.path(OUT, "Figures", "Supplementary_figures")
DIR_DATA <- file.path(OUT, "Analysis_output", "Final_figure_data")
DIR_CAP <- file.path(OUT, "Figures", "Captions")
for (p in c(DIR_DATA, DIR_CAP)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
raw_paths <- file.path(ROOT, "Results", c("SDO_VSA_Results_20260830.csv", "RMET_Results_20260912.csv"))
raw_hash <- tools::md5sum(raw_paths)
read_csv <- function(path) read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
                                   na.strings = c("", "NA", "NaN"), fileEncoding = "UTF-8-BOM")
write_csv <- function(x, name) write.csv(x, file.path(DIR_DATA, name), row.names = FALSE, na = "")
checks <- list()
check <- function(name, passed, detail = "") {
  checks[[length(checks) + 1L]] <<- data.frame(check = name, passed = isTRUE(passed), detail = as.character(detail))
  if (!isTRUE(passed)) {
    write_csv(do.call(rbind, checks), "validation_checks.csv")
    stop(name, ": ", detail, call. = FALSE)
  }
}
normalise <- function(x) tolower(trimws(as.character(x)))
N_RUNS <- 5L
MAX_API_FAIL <- 0.50  # Exclude only when MORE THAN 50% of requests are API failures.
ALPHA_BENCHMARK <- .80
HUMAN_MEAN <- 100 * 26.2 / 36
# Verified in Table 3, Group 2 (122 general-population controls):
# https://docs.autismresearchcentre.com/papers/2001_BCetal_adulteyes.pdf
key <- function(d, cols) do.call(paste, c(d[cols], sep = "\034"))
short <- function(x) sub("^(azure|databricks)-", "", x)
DISPLAY <- c(
  "gpt-4o"="GPT-4o", "gpt-4.1"="GPT-4.1", "gpt-5"="GPT-5", "gpt-5.1"="GPT-5.1",
  "gpt-5.2"="GPT-5.2", "gpt-5.4"="GPT-5.4", "gpt-5.5"="GPT-5.5", "gpt-5.6-sol"="GPT-5.6-sol",
  "gpt-oss-20b"="GPT-OSS 20B", "gpt-oss-120b"="GPT-OSS 120B",
  "claude-opus-4-1"="Claude Opus 4.1", "claude-sonnet-4-5"="Claude Sonnet 4.5",
  "claude-haiku-4-5"="Claude Haiku 4.5", "claude-opus-4-5"="Claude Opus 4.5",
  "claude-opus-4-6"="Claude Opus 4.6", "claude-sonnet-4-6"="Claude Sonnet 4.6",
  "claude-opus-4-7"="Claude Opus 4.7", "claude-opus-4-8"="Claude Opus 4.8",
  "claude-opus-5"="Claude Opus 5", "claude-sonnet-5"="Claude Sonnet 5", "claude-fable-5"="Claude Fable 5",
  "claude-fable-5-1"="Claude Fable 5.1", "gemma-3-12b"="Gemma 3 12B",
  "meta-llama-3-1-8b-instruct"="Llama 3.1 8B", "meta-llama-3-3-70b-instruct"="Llama 3.3 70B",
  "llama-4-maverick"="Llama 4 Maverick", "qwen3-next-80b-a3b-instruct"="Qwen3-Next 80B",
  "qwen35-122b-a10b"="Qwen3.5 122B")
display <- function(x) {
  y <- unname(DISPLAY[short(x)])
  if (anyNA(y)) stop("Unmapped model label: ", paste(x[is.na(y)], collapse = ", "))
  y
}

# Match the manuscript and 02_statistical_analysis.R: more than 50% API failures.
# Exactly 50% is retained. Refusals and other unscored responses are handled
# separately as missing answers; they are not relabelled as API failures.
exclude_failed <- function(d, dataset) {
  failure <- !is.na(d$parse_error) & d$parse_error == "API_ERROR"
  rate <- tapply(failure, d$model_label, mean)
  excluded <- names(rate)[rate > MAX_API_FAIL]
  audit <- data.frame(dataset = dataset, model = names(rate), api_failure_share = as.numeric(rate),
                      api_failure_threshold = MAX_API_FAIL,
                      excluded = names(rate) %in% excluded)
  list(data = d[!d$model_label %in% excluded, ], audit = audit)
}
# Boundary checks use small test fixtures only; they never enter plotted data.
boundary_fixture <- data.frame(
  model_label = rep(c("exactly_half", "three_quarters"), each = 4),
  parse_error = c("API_ERROR", "API_ERROR", NA, NA,
                  "API_ERROR", "API_ERROR", "API_ERROR", NA))
boundary_result <- exclude_failed(boundary_fixture, "validation_only")
check("API failure boundary: exactly 50% retained",
      "exactly_half" %in% boundary_result$data$model_label)
check("API failure boundary: 75% excluded",
      !"three_quarters" %in% boundary_result$data$model_label)
rm(boundary_fixture, boundary_result)
att_all <- read_csv(raw_paths[1]); rm_all <- read_csv(raw_paths[2])
ae <- exclude_failed(att_all, "attitude"); re <- exclude_failed(rm_all, "RMET")
d <- ae$data; rm <- re$data
write_csv(rbind(ae$audit, re$audit), "model_exclusions.csv")
check("Unique attitude requests", !anyDuplicated(key(d, c("model_label", "instrument", "language", "question_id", "repeat"))))
check("Unique RMET requests", !anyDuplicated(key(rm, c("model_label", "question_id", "definitions_shown", "repeat"))))
d$run <- as.integer(d[["repeat"]]) + 1L
check("Attitude run identifiers", all(d$run %in% seq_len(N_RUNS)))
d$item <- as.integer(ifelse(is.na(d$item_no), sub(".*?([0-9]+)$", "\\1", d$question_id), d$item_no))
d$inst <- ifelse(d$instrument == "SDO6", paste0("SDO6-", toupper(d$language)),
                 ifelse(d$instrument == "SDO", "SDO7s", "VSA"))
v <- suppressWarnings(as.numeric(d$value))
ok <- is.finite(v)
check("Valid parsed responses have no parse error", all(is.na(d$parse_error[ok]) | d$parse_error[ok] == ""))
check("Raw responses fall within scale bounds", all(v[ok] == round(v[ok])) &&
        all(v[ok & d$instrument != "VSA"] >= 1 & v[ok & d$instrument != "VSA"] <= 7) &&
        all(v[ok & d$instrument == "VSA"] >= -4 & v[ok & d$instrument == "VSA"] <= 4))
reverse <- (d$instrument == "SDO6" & d$item %in% 9:16) |
  (d$instrument == "SDO" & d$item %in% c(3,4,7,8)) |
  (d$instrument == "VSA" & d$item %in% c(1,4,5))
d$score <- v
d$score[reverse & d$instrument != "VSA"] <- 8 - v[reverse & d$instrument != "VSA"]
d$score[reverse & d$instrument == "VSA"] <- -v[reverse & d$instrument == "VSA"]
s6_rows <- ok & d$instrument == "SDO6"
check("SDO6 reverse flags agree with item keys", all((normalise(d$reverse_scored[s6_rows]) == "true") == reverse[s6_rows]))
check("SDO6 raw-value rescoring agrees with source recoding", all(abs(d$score[s6_rows] - d$value_recoded[s6_rows]) < 1e-12))
s6 <- d[s6_rows, ]

# Figure 2A: each language's denominator contains only its valid responses.
dist <- do.call(rbind, lapply(c("en", "zh"), function(lang) {
  counts <- tabulate(s6$score[s6$language == lang], nbins = 7)
  data.frame(language = lang, response = 1:7, count = counts,
             valid_responses = sum(counts), percent = 100 * counts / sum(counts))
}))
scores <- aggregate(score ~ model_label + run + language, s6, mean)
names(scores)[4] <- "mean_score"
paired <- reshape(scores, idvar = c("model_label", "run"), timevar = "language", direction = "wide")
check("All model-run language means are paired", !anyNA(paired))
paired$diff <- round(paired$mean_score.zh - paired$mean_score.en, 10)
ex2 <- paired[paired$model_label == "azure-gpt-5", ]; ex2 <- ex2[order(ex2$run), ]
fig2b <- data.frame(run = ex2$run, english = ex2$mean_score.en, chinese = ex2$mean_score.zh,
                    difference = ex2$diff)
fig2b_all <- data.frame(model=paired$model_label,run=paired$run,english=paired$mean_score.en,
                       chinese=paired$mean_score.zh,difference=paired$diff)
count_scores<-aggregate(score~model_label+run+language,s6,length)
count_scores<-reshape(count_scores,idvar=c("model_label","run"),timevar="language",direction="wide")
ii<-match(key(fig2b_all,c("model","run")),key(transform(count_scores,model=model_label),c("model","run")))
fig2b_all$n_english<-count_scores$score.en[ii];fig2b_all$n_chinese<-count_scores$score.zh[ii]
check("GPT-5 example contains every run", identical(fig2b$run, 1:5))
check("GPT-5 has 16 valid items per language-run", all(table(s6$language[s6$model_label == "azure-gpt-5"],
                                                           s6$run[s6$model_label == "azure-gpt-5"]) == 16))

# Complete five-response item profiles. Incomplete items are excluded from
# Figure 3 and counted explicitly, not imputed or replaced with zeros.
pair_groups <- split(seq_len(nrow(d)), key(d, c("model_label", "inst", "question_id")))
profiles <- lapply(pair_groups, function(i) {
  x <- d[i, ]; x <- x[order(x$run), ]
  if (nrow(x) != N_RUNS || !identical(x$run, 1:5) || any(!is.finite(x$score))) return(NULL)
  list(model = x$model_label[1], inst = x$inst[1], item = x$item[1], values = x$score)
})
complete <- Filter(Negate(is.null), profiles)
profile_meta <- do.call(rbind, lapply(complete, function(x)
  data.frame(model = x$model, inst = x$inst, item = x$item, first = x$values[1],
             changed = length(unique(x$values)) > 1, changed_later = length(unique(x$values[-1])) > 1)))
values <- do.call(rbind, lapply(complete, `[[`, "values"))
colnames(values) <- paste0("run_", 1:5)
write_csv(cbind(profile_meta, values), "complete_attitude_item_profiles.csv")
write_csv(data.frame(total_item_profiles = length(profiles), complete_profiles = length(complete),
                     incomplete_profiles = sum(vapply(profiles, is.null, logical(1)))), "attitude_completeness.csv")

# Independent interval-alpha implementation from squared pairwise distances.
# Units are items; coders are runs. No expected disagreement => alpha undefined.
interval_alpha <- function(M) {
  pooled <- as.numeric(M); n <- length(pooled)
  expected <- 2 * n * sum((pooled - mean(pooled))^2) / (n - 1)
  if (expected == 0) return(NA_real_)
  observed <- sum(apply(M, 1, function(x) sum(outer(x, x, "-")^2)/(length(x)-1)))
  1 - observed / expected
}
model_groups <- split(seq_len(nrow(profile_meta)), key(profile_meta, c("model", "inst")))
agreement <- do.call(rbind, lapply(model_groups, function(i) {
  M <- values[i, , drop = FALSE]
  data.frame(model = profile_meta$model[i[1]], display = display(profile_meta$model[i[1]]),
             inst_lang = profile_meta$inst[i[1]], n_items = length(i),
             all_identical = length(unique(as.numeric(M))) == 1,
             alpha_interval = interval_alpha(M), n_changed = sum(profile_meta$changed[i]),
             pct_changed = 100 * mean(profile_meta$changed[i]))
}))
rownames(agreement) <- NULL
a_defined <- agreement[is.finite(agreement$alpha_interval), ]
ex_i <- which(profile_meta$model == "azure-gpt-5.5" & profile_meta$inst == "VSA")
ex_i <- ex_i[order(profile_meta$item[ex_i])]
M <- values[ex_i, , drop = FALSE]
alpha_example <- interval_alpha(M)
fig3a <- data.frame(item = profile_meta$item[ex_i], M, check.names = FALSE)
groups <- expand.grid(alpha_group = c("at_least_080", "below_080"),
                      items_changed_group = c("none", "under_20_percent", "at_least_20_percent"), stringsAsFactors = FALSE)
a_defined$alpha_group <- ifelse(a_defined$alpha_interval >= ALPHA_BENCHMARK, "at_least_080", "below_080")
a_defined$items_changed_group <- ifelse(a_defined$pct_changed == 0, "none",
                                        ifelse(a_defined$pct_changed < 20, "under_20_percent", "at_least_20_percent"))
groups$count <- vapply(seq_len(nrow(groups)), function(i)
  sum(a_defined$alpha_group == groups$alpha_group[i] & a_defined$items_changed_group == groups$items_changed_group[i]), integer(1))

wilson <- function(k, n) {
  z <- 1.96; p <- k/n; den <- 1+z*z/n
  ctr <- (p+z*z/(2*n))/den
  half <- z*sqrt(p*(1-p)/n+z*z/(4*n*n))/den
  c(100*(ctr-half), 100*(ctr+half))
}
sp <- profile_meta[profile_meta$inst != "VSA", ]
sp$initial_response <- ifelse(sp$first <= 4, as.character(sp$first), "5-7")
fig3c <- do.call(rbind, lapply(c("1","2","3","4","5-7"), function(label) {
  x <- sp$changed_later[sp$initial_response == label]; n <- length(x); k <- sum(x)
  ci <- wilson(k, n)
  data.frame(initial_response = label, n = n, changed = k, percent = 100*k/n,
             ci_low = ci[1], ci_high = ci[2])
}))
SCALES <- c("SDO7s","SDO6-EN","SDO6-ZH","VSA")
SCALE_LABELS <- c("SDO7s"="Social dominance orientation: 8-item, English",
                  "SDO6-EN"="Social dominance orientation: 16-item, English",
                  "SDO6-ZH"="Social dominance orientation: 16-item, Chinese",
                  "VSA"="Very Short Authoritarianism: 6-item, English")
fig3c_scale <- do.call(rbind,lapply(c(SCALES[1:3],"Pooled"),function(scale)
  do.call(rbind,lapply(c("1","2","3","4","5-7"),function(label){
    x<-sp[sp$initial_response==label & (scale=="Pooled" | sp$inst==scale),]
    n<-nrow(x);k<-sum(x$changed_later)
    data.frame(scale=scale,initial_response=label,n=n,changed=k,percent=if(n)100*k/n else NA_real_)
  }))))
for(label in fig3c$initial_response){
  z<-fig3c_scale[fig3c_scale$initial_response==label,]
  check(paste("Scale-specific heatmap denominators",label),sum(z$n[z$scale!="Pooled"])==z$n[z$scale=="Pooled"])
  check(paste("Scale-specific heatmap numerators",label),sum(z$changed[z$scale!="Pooled"])==z$changed[z$scale=="Pooled"])
}
# Every raw VSA item-run response, including explicit missing cells, is shown.
vsa_grid<-expand.grid(model=sort(unique(d$model_label)),item=1:6,run=1:5,stringsAsFactors=FALSE)
vsa_raw<-d[d$inst=="VSA",c("model_label","item","run","score")]
names(vsa_raw)[1]<-"model"
vsa_match<-match(key(vsa_grid,c("model","item","run")),key(vsa_raw,c("model","item","run")))
check("All VSA heatmap cells correspond to actual raw requests",!anyNA(vsa_match))
vsa_grid$response<-vsa_raw$score[vsa_match]

# Former Figure 2C: actual signs of language differences in each of five runs.
s14 <- do.call(rbind, lapply(split(paired, paired$model_label), function(x) {
  x <- x[order(x$run), ]; dd <- x$diff
  category <- if (any(dd > 0) && any(dd < 0)) "switched" else if (!any(dd > 0)) "english" else if (any(dd == 0)) "mostly" else "always"
  data.frame(model = x$model_label[1], category = category,
             as.list(setNames(dd, paste0("run_", x$run))), check.names = FALSE)
}))
rownames(s14) <- NULL

# Figure 4: verify correctness from the raw selected option and answer key.
valid_choice <- !is.na(rm$choice) & nzchar(trimws(rm$choice))
rc <- rm[valid_choice, ]; rc$run <- as.integer(rc[["repeat"]])+1L
check("RMET run identifiers", all(rc$run %in% 1:5))
correct_raw <- normalise(rc$choice) == normalise(rc$correct_answer)
check("RMET correctness matches the raw answer key", all(correct_raw == (normalise(rc$is_correct) == "true")))
valid_option <- vapply(seq_len(nrow(rc)), function(i) normalise(rc$choice[i]) %in%
                         normalise(strsplit(rc$options[i], ",", fixed = TRUE)[[1]]), logical(1))
check("RMET choices belong to their listed options", all(valid_option))
rc$correct <- correct_raw
rm_cells <- split(seq_len(nrow(rc)), key(rc, c("model_label", "question_id", "definitions_shown")))
rm_profiles <- do.call(rbind, lapply(rm_cells, function(i)
  data.frame(model = rc$model_label[i[1]], question = rc$question_id[i[1]], condition = rc$definitions_shown[i[1]],
             accuracy = mean(rc$correct[i]), changed = length(unique(rc$choice[i])) > 1, n_valid = length(i))))
fig4 <- do.call(rbind, lapply(split(rm_profiles, rm_profiles$model), function(x)
  data.frame(model = x$model[1], accuracy_pct = 100*mean(x$accuracy), items_changed_pct = 100*mean(x$changed),
             item_condition_combinations = nrow(x), combinations_with_four_valid_responses = sum(x$n_valid == 4),
             valid_responses = sum(x$n_valid))))
rownames(fig4) <- NULL
rho <- cor(round(fig4$accuracy_pct, 12), round(fig4$items_changed_pct, 12), method = "spearman")
check("Every RMET model has 36 items in both conditions", all(fig4$item_condition_combinations == 72))
check("RMET cells contain four or five valid responses", all(rm_profiles$n_valid %in% c(4,5)))
write_csv(rm_profiles, "RMET_item_condition_profiles.csv")

# Full source tables are written before rendering so every point is inspectable.
tables <- list(Fig2A_distributions = dist, Fig2B_GPT5_scores = fig2b, Fig3A_GPT55_VSA = fig3a,
               Fig3B_all_combinations = agreement, Fig3B_groups = groups, Fig3C_response_variation = fig3c,
               Fig4_rmet_by_model = fig4, FigS14_model_categories = s14,
               Fig2B_all_models=fig2b_all,Fig3A_all_models_VSA=vsa_grid,Fig3C_scale_heatmap=fig3c_scale)
for (name in names(tables)) write_csv(tables[[name]], paste0(name, ".csv"))

# Compare every published quantity with frozen earlier exports. These references
# never supply values to the plot. A mismatch stops figure export, rather than
# changing data to force agreement. --skip-reference-checks permits new raw data.
ref_candidates <- file.path(ROOT, c("Figures/Archive/Revision_options/source_data",
                                    "Figures/Main_figures/Revision_options/source_data"))
REF <- ref_candidates[dir.exists(ref_candidates)][1]
compare <- function(name, now, old, keys, fields, tol = 1e-9) {
  ka <- key(now, keys); kb <- key(old, keys)
  check(paste(name, "unique keys"), !anyDuplicated(ka) && !anyDuplicated(kb))
  check(paste(name, "same observations"), setequal(ka, kb), paste(nrow(now), "recomputed;", nrow(old), "reference"))
  old <- old[match(ka, kb), , drop = FALSE]
  for (field in fields) {
    a <- now[[field]]; b <- old[[field]]
    same_missing <- identical(is.na(a), is.na(b))
    ok <- !is.na(a) & !is.na(b)
    if (is.numeric(a) && is.numeric(b)) {
      delta <- if (any(ok)) max(abs(a[ok]-b[ok])) else 0
      check(paste(name, field), same_missing && delta <= tol, sprintf("max absolute difference %.12g", delta))
    } else check(paste(name, field), same_missing && all(as.character(a[ok]) == as.character(b[ok])))
  }
}
if (!"--skip-reference-checks" %in% args) {
  check("Frozen reference exports are available", !is.na(REF), "Used for checking only, not plotting")
  get_ref <- function(name) read_csv(file.path(REF, paste0(name, ".csv")))
  compare("Figure 2A", dist, get_ref("Fig2A_distributions"), c("language","response"), c("count","valid_responses","percent"))
  compare("Figure 2B", fig2b, get_ref("Fig2B_GPT5_scores"), "run", c("english","chinese","difference"))
  compare("Figure 3A", fig3a, get_ref("Fig3A_GPT55_VSA"), "item", paste0("run_", 1:5))
  compare("Figure 3B all reliability values", agreement,
          read_csv(file.path(ROOT, "Analysis_output/Reframing_checks/R3_agreement_by_model.csv")),
          c("display","inst_lang"), c("n_items","all_identical","alpha_interval","n_changed","pct_changed"))
  compare("Figure 3B groups", groups, get_ref("Fig3B_groups"), c("alpha_group","items_changed_group"), "count")
  compare("Figure 3C", fig3c, get_ref("Fig3C_response_variation"), "initial_response", c("n","changed","percent","ci_low","ci_high"))
  compare("Figure 4", fig4, get_ref("Fig4_rmet_by_model"), "model", setdiff(names(fig4), "model"))
  compare("Supplementary S14", s14, get_ref("FigS14_model_categories"), "model", c("category",paste0("run_",1:5)))
} else {
  checks[[length(checks)+1L]] <- data.frame(check = "Historical reference comparison", passed = NA,
                                          detail = "Explicitly skipped; raw-derived data are still validated")
}
check("Raw files unchanged during computation", identical(unname(raw_hash), unname(tools::md5sum(raw_paths))))
write_csv(do.call(rbind, checks), "validation_checks.csv")
write_csv(data.frame(raw_file = names(raw_hash), md5 = unname(raw_hash)), "input_file_hashes.csv")
capture.output(sessionInfo(), file = file.path(DIR_DATA, "R_session_info.txt"))

# Drawing helpers: fixed publication layout, with raw-derived values only.
library(grid)
INK <- "#20272D"; SEC <- "#59616B"; GRID <- "#E5E8EC"; EDGE <- "#C6CCD3"
BLUE <- "#0072B2"; ORANGE <- "#D55E00"; PALE <- "#FFF0E7"
EN <- "#42566D"; ZH <- ORANGE
# Okabe-Ito blue/vermillion for groups; cividis for ordered quantities.
# https://jfly.uni-koeln.de/color/ ; https://arxiv.org/abs/1712.01662
if(!requireNamespace("viridisLite",quietly=TRUE))stop('Install the colour palette package with install.packages("viridisLite").')
RAMP <- viridisLite::cividis(7)
SEQ <- viridisLite::cividis(101)
VSA_COL <- viridisLite::cividis(9)
DIV <- grDevices::colorRampPalette(c(BLUE,"#FFFFFF",ORANGE),space="Lab")(201)
FONT <- "Arial"; W <- H <- 0; SVG <- NULL
xml <- function(x){x<-gsub("&","&amp;",x,fixed=TRUE);x<-gsub("<","&lt;",x,fixed=TRUE);gsub(">","&gt;",x,fixed=TRUE)}
emit <- function(x)if(!is.null(SVG))SVG<<-c(SVG,x)
svg_colour<-function(x)if(is.na(x))"none" else x
contrast_ink<-function(col){
  rgb<-grDevices::col2rgb(col)/255;v<-ifelse(rgb<=.04045,rgb/12.92,((rgb+.055)/1.055)^2.4)
  l<-as.numeric(c(.2126,.7152,.0722)%*%v)
  ifelse((1.05/(l+.05))>((l+.05)/.05),"white","#000000")
}
round_label<-function(x){
  v<-floor(abs(x)*100+.5+1e-9)/100
  paste0(ifelse(v==0,"",ifelse(x<0,"-","+")),sprintf("%.2f",v))
}
text_at <- function(x,y,label,size=11,col=INK,bold=FALSE,anchor="start",angle=0) {
  label <- as.character(label)
  chunks <- strsplit(gsub("(SDO)([67])", "\\1|SUB\\2|", label, perl=TRUE), "|", fixed=TRUE)[[1]]
  subscript <- grepl("^SUB[67]$", chunks)
  chunks[subscript] <- substring(chunks[subscript],4)
  sizes <- ifelse(subscript,size*.72,size)
  widths <- vapply(seq_along(chunks),function(i)convertWidth(grobWidth(textGrob(chunks[i],
    gp=gpar(fontfamily=FONT,fontsize=sizes[i],fontface=if(bold)2 else 1))),"bigpts",valueOnly=TRUE),numeric(1))
  offset <- switch(anchor,start=0,middle=sum(widths)/2,end=sum(widths))
  pushViewport(viewport(x=unit(x,"bigpts"),y=unit(H-y,"bigpts"),width=unit(W,"bigpts"),
                        height=unit(H,"bigpts"),just=c(0,0),angle=angle))
  cursor <- -offset
  for(i in seq_along(chunks)) {
    grid.text(chunks[i],x=unit(cursor,"bigpts"),y=unit(if(subscript[i])-size*.22 else 0,"bigpts"),
              just=c(0,.2),gp=gpar(fontfamily=FONT,fontsize=sizes[i],fontface=if(bold)2 else 1,col=col))
    cursor <- cursor+widths[i]
  }
  popViewport()
  dy<-ifelse(subscript,size*.22,0);spans<-character(length(chunks));last<-0
  for(i in seq_along(chunks)){spans[i]<-sprintf('<tspan font-size="%g" dy="%g">%s</tspan>',sizes[i],dy[i]-last,xml(chunks[i]));last<-dy[i]}
  tr<-if(angle)sprintf(' transform="rotate(%g %g %g)"',-angle,x,y)else ""
  emit(sprintf('<text x="%g" y="%g"%s font-family="Arial, Helvetica, sans-serif" font-size="%g" font-weight="%s" fill="%s" text-anchor="%s">%s</text>',
    x,y,tr,size,if(bold)"700" else "400",col,anchor,paste(spans,collapse="")))
}
rect_at <- function(x,y,w,h,fill="white",stroke=NA,sw=.7,radius=0) {
  gp <- gpar(fill=fill,col=stroke,lwd=sw)
  if(radius>0) grid.roundrect(x=unit(x,"bigpts"),y=unit(H-y,"bigpts"),width=unit(w,"bigpts"),height=unit(h,"bigpts"),
    r=unit(radius,"bigpts"),just=c(0,1),gp=gp)
  else grid.rect(x=unit(x,"bigpts"),y=unit(H-y,"bigpts"),width=unit(w,"bigpts"),height=unit(h,"bigpts"),just=c(0,1),gp=gp)
  emit(sprintf('<rect x="%g" y="%g" width="%g" height="%g" rx="%g" fill="%s" stroke="%s" stroke-width="%g"/>',x,y,w,h,radius,fill,svg_colour(stroke),sw))
}
line_at <- function(x1,y1,x2,y2,col=EDGE,sw=1,dash=FALSE){grid.lines(x=unit(c(x1,x2),"bigpts"),y=unit(H-c(y1,y2),"bigpts"),gp=gpar(col=col,lwd=sw,lty=if(dash)2 else 1))
  emit(sprintf('<line x1="%g" y1="%g" x2="%g" y2="%g" stroke="%s" stroke-width="%g"%s/>',x1,y1,x2,y2,col,sw,if(dash)' stroke-dasharray="4,3"' else ""))}
dot_at <- function(x,y,r,fill,stroke="white",sw=1){grid.circle(x=unit(x,"bigpts"),y=unit(H-y,"bigpts"),r=unit(r,"bigpts"),gp=gpar(fill=fill,col=stroke,lwd=sw))
  emit(sprintf('<circle cx="%g" cy="%g" r="%g" fill="%s" stroke="%s" stroke-width="%g"/>',x,y,r,fill,stroke,sw))}
arrow_at <- function(x1,y1,x2,y2,col=EDGE,sw=1.3) {
  a <- atan2(y2-y1,x2-x1); x1<-x1+6*cos(a);y1<-y1+6*sin(a);x2<-x2-6*cos(a);y2<-y2-6*sin(a)
  line_at(x1,y1,x2,y2,col,sw)
  for(side in c(-1,1))line_at(x2,y2,x2-5*cos(a+side*.45),y2-5*sin(a+side*.45),col,sw)
}
heading <- function(letter,label,y,x=18,size=14) {text_at(x,y,letter,19,bold=TRUE);text_at(x+28,y-1,label,size,bold=TRUE)}
save_figure <- function(stem,width,height,draw,folder) {
  dir.create(folder,recursive=TRUE,showWarnings=FALSE)
  W <<- width; H <<- height
  for(ext in c("pdf","png","tiff","svg")) {
    path <- file.path(folder,paste0(stem,".",ext))
    SVG<<-NULL
    measure_file<-if(ext=="svg")tempfile(fileext=".pdf")else path
    if(ext %in% c("png","tiff")) {
      if(requireNamespace("ragg",quietly=TRUE)) {
        if(ext=="png")ragg::agg_png(path,width=width/72,height=height/72,units="in",res=600,background="white")
        else ragg::agg_tiff(path,width=width/72,height=height/72,units="in",res=600,background="white",compression="lzw")
      } else {
        if(ext=="png")grDevices::png(path,width=width/72,height=height/72,units="in",res=600,type=if(capabilities("aqua"))"quartz"else "cairo",bg="white")
        else grDevices::tiff(path,width=width/72,height=height/72,units="in",res=600,type=if(capabilities("aqua"))"quartz"else "cairo",bg="white",compression="lzw")
      }
    } else {
      if(capabilities("aqua"))grDevices::quartz(type="pdf",file=measure_file,width=width/72,height=height/72,family=FONT,bg="white")
      else grDevices::cairo_pdf(measure_file,width=width/72,height=height/72,family=FONT,bg="white")
      if(ext=="svg")SVG<<-c(sprintf('<svg xmlns="http://www.w3.org/2000/svg" width="%gpt" height="%gpt" viewBox="0 0 %g %g">',width,height,width,height),
                            sprintf('<rect width="%g" height="%g" fill="white"/>',width,height))
    }
    tryCatch({grid.newpage();draw()},finally=dev.off())
    if(ext=="svg"){writeLines(c(SVG,"</svg>"),path,useBytes=TRUE);SVG<<-NULL;unlink(measure_file)}
  }
  message("Saved ",stem," (PDF, PNG, TIFF, SVG)")
}

colour_bar <- function(x,y,w,h,palette,lo,hi,ticks,label,size=13) {
  for(i in seq_along(palette))rect_at(x+(i-1)*w/length(palette),y,w/length(palette)+.05,h,palette[i])
  text_at(x,y-10,label,size)
  for(t in ticks){xx<-x+(t-lo)/(hi-lo)*w;line_at(xx,y+h,xx,y+h+4,SEC,.7)
    text_at(xx,y+h+23,format(t,trim=TRUE),size,SEC,anchor="middle")}
}
missing_cell <- function(x,y,w,h) {
  rect_at(x,y,w,h,"#E2E2E2","white",.5)
  line_at(x+2,y+2,x+w-2,y+h-2,"#8A8A8A",.5)
}
row_models <- function()s14$model[match(unlist(FAMILIES,use.names=FALSE),short(s14$model))]

draw_fig2 <- function() {
  heading("A","Pooled social dominance orientation responses",28,size=18)
  text_at(175,54,sprintf("16-item scale; valid responses pooled within each language across %d models",length(unique(s6$model_label))),13,SEC)
  for(i in 1:2){z<-dist[dist$language==c("en","zh")[i],];y<-76+(i-1)*50
    text_at(160,y+22,c("English","Chinese")[i],15,anchor="end");left<-175
    for(k in 1:7){bw<-760*z$percent[k]/100;rect_at(left,y,bw,32,RAMP[k],"white",.7)
      if(z$percent[k]>=3)text_at(left+bw/2,y+23,sprintf("%.0f%%",z$percent[k]),if(z$percent[k]>=10)14 else 11,
        contrast_ink(RAMP[k]),bold=z$percent[k]>=10,anchor="middle");left<-left+bw}
  }
  text_at(405,191,"Most egalitarian",13,SEC,anchor="end")
  for(k in 1:7){rect_at(420+(k-1)*34,175,32,22,RAMP[k]);text_at(436+(k-1)*34,192,k,13,contrast_ink(RAMP[k]),TRUE,"middle")}
  text_at(671,191,"Most hierarchy-endorsing",13,SEC)
  heading("B","Language differences across models and runs",238,size=18)
  models<-row_models();x0<-190;y0<-294;cw<-149;rh<-16
  text_at(25,y0+length(models)*rh/2,"Model",14,anchor="middle",angle=90)
  for(j in 1:5)text_at(x0+(j-.5)*cw,281,paste("Run",j),14,anchor="middle")
  bound<-ceiling(max(abs(fig2b_all$difference))*2)/2
  for(i in seq_along(models)){
    text_at(177,y0+(i-.5)*rh+4,display(models[i]),13,anchor="end")
    for(j in 1:5){z<-fig2b_all[fig2b_all$model==models[i]&fig2b_all$run==j,];v<-z$difference
      cc<-DIV[1+round((v+bound)/(2*bound)*200)];x<-x0+(j-1)*cw;y<-y0+(i-1)*rh
      rect_at(x,y,cw,rh,cc,"white",.5)
      label<-round_label(v)
      if(z$n_english<16 || z$n_chinese<16)label<-paste0(label,"*")
      text_at(x+cw/2,y+rh/2+4,label,13,contrast_ink(cc),anchor="middle")
    }
  }
  text_at(x0+5*cw/2,756,"Repeated run",14,anchor="middle")
  text_at(190,755,"* Incomplete responses",11.5,SEC)
  colour_bar(340,794,500,12,DIV,-bound,bound,c(-bound,0,bound),"Difference in mean score (Chinese - English)",13)
  text_at(340,843,"Higher in English",12,SEC);text_at(840,843,"Higher in Chinese",12,SEC,anchor="end")
}

diamond_at <- function(x,y,r,fill) {
  grid.polygon(x=unit(c(x,x+r,x,x-r),"bigpts"),y=unit(H-c(y-r,y,y+r,y),"bigpts"),gp=gpar(fill=fill,col="white",lwd=.7))
  emit(sprintf('<polygon points="%g,%g %g,%g %g,%g %g,%g" fill="%s" stroke="white" stroke-width=".7"/>',x,y-r,x+r,y,x,y+r,x-r,y,fill))
}

draw_fig3 <- function() {
  heading("A","Very Short Authoritarianism: responses across runs",28,size=18)
  models<-row_models();x0<-190;y0<-100;cw<-745/30;rh<-16
  text_at(935,53,paste(length(models),"models; six items"),13,SEC,anchor="end")
  text_at(25,y0+length(models)*rh/2,"Model",14,anchor="middle",angle=90)
  for(item in 1:6){text_at(x0+(item-.5)*5*cw,69,paste("Item",item),14,bold=TRUE,anchor="middle")
    for(run in 1:5)text_at(x0+((item-1)*5+run-.5)*cw,90,run,13,SEC,anchor="middle")}
  for(i in seq_along(models)){
    text_at(177,y0+(i-.5)*rh+4,display(models[i]),13,anchor="end")
    for(item in 1:6)for(run in 1:5){v<-vsa_grid$response[vsa_grid$model==models[i]&vsa_grid$item==item&vsa_grid$run==run]
      x<-x0+((item-1)*5+run-1)*cw;y<-y0+(i-1)*rh
      if(is.na(v))missing_cell(x,y,cw,rh)else rect_at(x,y,cw,rh,VSA_COL[v+5],"white",.4)
    }
  }
  for(item in 1:5)line_at(x0+item*5*cw,y0,x0+item*5*cw,y0+length(models)*rh,"white",1.6)
  text_at(190,558,"Run numbers 1-5 within each item",13,SEC)
  text_at(445,558,"Rescored item response (full scale)",13)
  for(v in -4:4){xx<-445+(v+4)*43;rect_at(xx,568,43,15,VSA_COL[v+5]);text_at(xx+21.5,606,v,13,SEC,anchor="middle")}
  if(anyNA(vsa_grid$response)){missing_cell(868,570,14,14);text_at(889,582,"Missing",12,SEC)}
  heading("B","Reliability and response changes by scale",632,size=18)
  text_at(25,784,"Items changed across runs (%)",14,anchor="middle",angle=90)
  for(s in seq_along(SCALES)){
    id<-SCALES[s];a<-a_defined[a_defined$inst_lang==id,];left<-95+(s-1)*215;right<-left+190;centre<-(left+right)/2
    name<-if(id=="VSA")"Very Short Authoritarianism"else "Social dominance orientation"
    version<-switch(id,SDO7s="8-item scale (English)",`SDO6-EN`="16-item scale (English)",`SDO6-ZH`="16-item scale (Chinese)",VSA="6-item scale (English)")
    text_at(centre,662,name,12.5,bold=TRUE,anchor="middle");text_at(centre,681,version,12.5,anchor="middle")
    text_at(centre,699,sprintf("n = %d (alpha defined)",nrow(a)),12,SEC,anchor="middle")
    X<-function(v)left+(v+.025)/1.06*190;Y<-function(v)850-v/100*136
    for(t in c(0,50,100)){line_at(left,Y(t),right,Y(t),GRID,.7);if(s==1)text_at(left-10,Y(t)+4,t,13,SEC,anchor="end")}
    line_at(left,850,right,850,EDGE,.7);line_at(X(.8),714,X(.8),850,SEC,.8,TRUE)
    text_at(X(.8)+3,710,".80",10,SEC)
    for(i in seq_len(nrow(a))){p<-a$pct_changed[i];xx<-X(a$alpha_interval[i]);yy<-Y(p)
      if(p==0)diamond_at(xx,yy,4.2,BLUE)else dot_at(xx,yy,3.8,ORANGE,sw=.5)}
    for(t in c(0,.5,1))text_at(X(t),875,sprintf("%.1f",t),13,SEC,anchor="middle")
    diamond_at(left+12,896,4,BLUE);text_at(left+23,900,paste("No changes:",sum(a$pct_changed==0)),12.5,INK)
    dot_at(left+12,914,3.5,ORANGE);text_at(left+23,918,paste("Any change:",sum(a$pct_changed>0)),12.5,INK)
  }
  text_at(510,946,"Response reliability (Krippendorff's interval alpha)",14,anchor="middle")
  heading("C","Response variation by initial response",984,size=18)
  text_at(935,983,paste("All",length(unique(d$model_label)),"models"),13,SEC,anchor="end")
  text_at(615,1010,"Initial rescored response (Run 1)",14,anchor="middle")
  labels<-c("1","2","3","4","5-7");x0<-300;cw<-127;top<-1055;rh<-42
  for(j in 1:5){xx<-x0+(j-.5)*cw;text_at(xx,1032,labels[j],15,bold=TRUE,anchor="middle")
    if(j%in%c(1,4,5))text_at(xx,1048,c("Most egalitarian","Neutral","Hierarchy-endorsing")[match(j,c(1,4,5))],11.5,SEC,anchor="middle")}
  text_at(25,1139,"Scale version",14,anchor="middle",angle=90)
  for(s in 1:4){id<-c(SCALES[1:3],"Pooled")[s];yy<-top+(s-.5)*rh
    if(id=="Pooled")text_at(286,yy+5,"All scales pooled",13.5,bold=TRUE,anchor="end")
    else {text_at(286,yy-4,"Social dominance orientation",12.5,anchor="end")
      text_at(286,yy+13,switch(id,SDO7s="8-item scale, English",`SDO6-EN`="16-item scale, English",`SDO6-ZH`="16-item scale, Chinese"),12.5,anchor="end")}
    for(j in 1:5){z<-fig3c_scale[fig3c_scale$scale==id&fig3c_scale$initial_response==labels[j],];x<-x0+(j-1)*cw;y<-top+(s-1)*rh
      if(is.na(z$percent)){missing_cell(x,y,cw,rh);text_at(x+cw/2,yy,"No items",13,anchor="middle")}
      else {cc<-SEQ[1+round(z$percent)];rect_at(x,y,cw,rh,cc,"white",.8)
        text_at(x+cw/2,yy,sprintf("%.0f%%",z$percent),17,contrast_ink(cc),TRUE,"middle")
        text_at(x+cw/2,yy+16,sprintf("(n = %d)",z$n),12.5,contrast_ink(cc),anchor="middle")}
    }
  }
  colour_bar(460,1253,475,14,SEQ,0,100,c(0,25,50,75,100),"Items varying in Runs 2-5 (%)",13)
}

draw_fig4 <- function() {
  text_at(80,28,"Relationship between accuracy and response variability",16,bold=TRUE)
  text_at(80,51,"Reading the Mind in the Eyes Test",11.5,SEC)
  X<-function(v)86+(v-40)/52*602;Y<-function(v)420-v/40*320
  for(t in seq(0,40,10)){line_at(86,Y(t),688,Y(t),GRID,.7);text_at(73,Y(t)+4,t,11,SEC,anchor="end")}
  for(t in seq(40,90,10)){line_at(X(t),420,X(t),424,EDGE,.8);text_at(X(t),441,t,11,SEC,anchor="middle")}
  line_at(86,100,86,420,EDGE,.8);line_at(86,420,688,420,EDGE,.8)
  text_at(387,470,"Accuracy (% correct)",12.5,anchor="middle")
  text_at(25,260,"Items with response changes across runs (%)",12,anchor="middle",angle=90)
  line_at(X(HUMAN_MEAN),100,X(HUMAN_MEAN),420,SEC,1,TRUE)
  text_at(X(HUMAN_MEAN)-10,73,"Adult reference mean",10.5,SEC,anchor="end")
  text_at(X(HUMAN_MEAN)-10,89,sprintf("%.1f%% (26.2/36)",HUMAN_MEAN),10.5,SEC,anchor="end")
  text_at(87,85,sprintf("Spearman's ρ = %.2f",rho),11.5)
  text_at(688,85,paste(nrow(fig4),"models"),11,SEC,anchor="end")
  for(i in seq_len(nrow(fig4)))dot_at(X(fig4$accuracy_pct[i]),Y(fig4$items_changed_pct[i]),4.7,EN,sw=.9)
}

FAMILIES <- list(
  "OpenAI GPT-4"=c("gpt-4o","gpt-4.1"),
  "OpenAI GPT-5"=c("gpt-5","gpt-5.1","gpt-5.2","gpt-5.4","gpt-5.5","gpt-5.6-sol"),
  "OpenAI GPT-OSS"=c("gpt-oss-20b","gpt-oss-120b"),
  "Anthropic Claude 4"=c("claude-opus-4-1","claude-sonnet-4-5","claude-haiku-4-5","claude-opus-4-5","claude-opus-4-6","claude-sonnet-4-6","claude-opus-4-7","claude-opus-4-8"),
  "Anthropic Claude 5"=c("claude-opus-5","claude-sonnet-5","claude-fable-5"),
  "Google Gemma"="gemma-3-12b", "Meta Llama"=c("meta-llama-3-1-8b-instruct","meta-llama-3-3-70b-instruct","llama-4-maverick"),
  "Alibaba Qwen"=c("qwen3-next-80b-a3b-instruct","qwen35-122b-a10b"))
check("Supplementary layout includes every attitude model exactly once", setequal(short(s14$model),unlist(FAMILIES)) && !anyDuplicated(unlist(FAMILIES)))
draw_s14 <- function() {
  text_at(20,28,"Language-effect direction across repeated runs",17,bold=TRUE)
  face<-c(always=BLUE,mostly="#56B4E9",english="#DADBD7",switched=ORANGE)
  for(i in seq_along(FAMILIES)){y<-53+(i-1)*32;text_at(166,y+16,names(FAMILIES)[i],11,anchor="end")
    for(j in seq_along(FAMILIES[[i]])){id<-FAMILIES[[i]][j];ii<-match(id,short(s14$model));cat<-s14$category[ii];label<-unname(DISPLAY[id])
      if(grepl("^Anthropic",names(FAMILIES)[i]))label<-sub("^Claude ","",label)
      x<-180+(j-1)*95;rect_at(x,y,90,24,face[cat],radius=3)
      text_at(x+45,y+16,label,if(nchar(label)<13)9.3 else 8.4,contrast_ink(face[cat]),cat=="switched","middle")}}
  counts<-table(factor(s14$category,levels=names(face)))
  labs<-c(always=sprintf("Chinese > English in all five runs (%d)",counts["always"]),
          switched=sprintf("Direction reversed across runs (%d)",counts["switched"]),
          mostly=sprintf("Chinese ≥ English; at least one tie (%d)",counts["mostly"]),
          english=sprintf("English ≥ Chinese; at least one tie (%d)",counts["english"]))
  for(i in seq_along(labs)){x<-180+(i-1)%%2*375;y<-329+(i-1)%/%2*26;rect_at(x,y,12,12,face[names(labs)[i]]);text_at(x+20,y+11,labs[i],11)}
}

if (!"--verify-only" %in% args) {
  save_figure("Fig2_language",960,850,draw_fig2,DIR_MAIN)
  save_figure("Fig3_answer_changes",960,1310,draw_fig3,DIR_MAIN)
  save_figure("Fig4_rmet",720,484,draw_fig4,DIR_MAIN)
  save_figure("FigS14_language_direction",960,394,draw_s14,DIR_SUPP)
}
check("Raw files unchanged after figure export", identical(unname(raw_hash),unname(tools::md5sum(raw_paths))))
write_csv(do.call(rbind,checks),"validation_checks.csv")
report <- c("# Final figure verification", "", paste("Generated:",format(Sys.time(),tz="Australia/Sydney",usetz=TRUE)), "",
  if("--verify-only"%in%args)"Mode: verification only; figure files were not rendered in this run."else "Mode: all final figure files rendered in this run.","",
  "All plotted data were recomputed from the two raw Results CSV files. No cached statistic supplies a plot value.", "",
  sprintf("Model exclusion rule: more than %.0f%% of requests are API failures; exactly %.0f%% is retained.",100*MAX_API_FAIL,100*MAX_API_FAIL),
  paste("Excluded attitude endpoints:",paste(ae$audit$model[ae$audit$excluded],collapse=", ")),
  paste("Excluded RMET endpoints:",paste(re$audit$model[re$audit$excluded],collapse=", ")), "",
  sprintf("Checks passed: %d. Historical checks explicitly skipped: %d.",sum(vapply(checks,function(x)isTRUE(x$passed),logical(1))),sum(vapply(checks,function(x)is.na(x$passed),logical(1)))), "",
  sprintf("Figure 2A valid responses: English %d; Chinese %d.",dist$valid_responses[1],dist$valid_responses[8]),
  sprintf("Historical GPT-5.5 illustration cross-check: interval alpha %.12f; changed items %d/%d.",alpha_example,sum(apply(M,1,function(x)length(unique(x))>1)),nrow(M)),
  sprintf("Revised Figure 3A: %d raw VSA item-run cells across all %d models; %d missing values shown explicitly; colour scale -4 to +4.",nrow(vsa_grid),length(unique(vsa_grid$model)),sum(is.na(vsa_grid$response))),
  sprintf("Figure 3B combinations: %d total, %d defined alpha, %d undefined alpha.",nrow(agreement),nrow(a_defined),sum(is.na(agreement$alpha_interval))),
  sprintf("Overlapping unchanged combinations at alpha=1: %d.",sum(abs(a_defined$alpha_interval-1)<1e-12 & a_defined$pct_changed==0)),
  paste("Figure 3C denominators:",paste(fig3c$n,collapse=", ")),
  sprintf("Figure 4: %d models; Spearman rho %.12f; %d four-response cells and %d five-response cells.",nrow(fig4),rho,sum(rm_profiles$n_valid==4),sum(rm_profiles$n_valid==5)),
  "", "The published adult reference (26.2/36) and the .80 alpha benchmark are external reference constants, not estimates from the model data.",
  "Figure 3A shows all raw VSA responses; Figures 3B and 3C use only complete five-response item profiles. Figure 4 retains available responses within each item-condition combination, matching the original aggregation.",
  "Revised Figure 3C is a scale-by-initial-response heatmap with explicit denominators. The pooled percentages match the former figure; historical Wilson intervals remain in the verification table only and are not plotted.",
  "", "Input hashes, exclusion decisions, complete item profiles, model coordinates and the R session are saved alongside this report.")
writeLines(report,file.path(DIR_DATA,"validation_report.md"))
captions<-list(
  Fig2_language=paste0("Fig. 2. Language differences in social dominance orientation responses. ",
    "(A) Distribution of rescored responses to the 16-item scale, pooled within each language across all ",length(unique(s6$model_label)),
    " models and five runs (English: ",dist$valid_responses[1]," valid responses; Chinese: ",dist$valid_responses[8],
    "). These are response-weighted pooled proportions, not an unweighted average of model percentages. Items 9-16 were reverse-scored; 1 indicates the most egalitarian and 7 the most hierarchy-endorsing response. ",
    "(B) Within-run mean-score differences for every model, calculated as Chinese minus English using the valid responses in each language. ",
    "Positive values indicate higher scores in Chinese; negative values indicate higher scores in English. Displayed values are rounded to two decimals (half away from zero); the source tables retain higher-precision language means and differences. ",
    "An asterisk identifies a comparison with fewer than 16 valid responses in at least one language. No missing response was imputed. ",
    "Colour limits are symmetric around zero. The former panel C is Supplementary Fig. S14."),
  Fig3_answer_changes=paste0("Fig. 3. Item responses, reliability and response variation across models. ",
    "(A) Rescored responses to all six Very Short Authoritarianism items for all ",length(unique(vsa_grid$model)),
    " models in five runs. Each model is one row; within each item the columns are Runs 1-5. Items 1, 4 and 5 had their signs reversed. ",
    "The colour scale covers the full theoretical range from -4 to +4, including values not observed in a particular row. Grey crossed cells indicate missing responses. ",
    "(B) Krippendorff's interval alpha and the percentage of items with at least one response change, shown separately for the 8-item Social Dominance Orientation scale in English, ",
    "the 16-item Social Dominance Orientation scale in English and Chinese, and the Very Short Authoritarianism scale in English. ",
    "Only item profiles with all five scored responses contribute to each model's alpha and change percentage. Of ",nrow(agreement)," model-scale combinations, ",
    sum(is.na(agreement$alpha_interval))," have undefined alpha because all responses are invariant; they are omitted from the scatterplots. ",
    "Each point is a model, and exact overlaps are retained. Blue diamonds indicate no item changes; vermillion circles indicate at least one changed item. Counts below each plot include all plotted models, including overlapping points. ",
    "Dashed lines mark the .80 reference benchmark, not a test of item-level identity. ",
    "(C) Percentage of complete model-item profiles with at least two distinct responses among Runs 2-5, grouped by their rescored response in Run 1 and by scale version. ",
    "All models are pooled within each scale row; the final row pools the three scale versions. Values of n give the number of model-item profiles in each cell. ",
    "Run 1 defines the groups and is excluded from the variation calculation. Initial scores 5-7 are combined; crossed cells, if present, indicate no eligible items rather than 0% variation."),
  Fig4_rmet=paste0("Fig. 4. Relationship between accuracy and response variability. Each point represents one of ",nrow(fig4),
    " models on the Reading the Mind in the Eyes Test. Accuracy is calculated within each model-item-condition combination across valid responses and then averaged across the 72 combinations ",
    "(36 items with and without word definitions). Response variability is the percentage of these combinations with more than one distinct answer across runs. ",
    sum(rm_profiles$n_valid==4)," combinations had four valid responses and ",sum(rm_profiles$n_valid==5)," had five; missing responses were not imputed. ",
    sprintf("Spearman's rank correlation is %.2f. ",rho),"The dashed line is the published adult reference mean of 26.2/36 (72.8%; Baron-Cohen et al., 2001), an external comparison rather than a quantity estimated from the model data."),
  FigS14_language_direction=paste0("Supplementary Fig. S14. Language-effect direction across repeated runs (formerly Figure 2C). ",
    "Each tile identifies one model and its sign pattern for Chinese-minus-English mean scores across the five runs. ",
    "Blue: Chinese scores higher in every run; light blue: Chinese scores at least as high in every run with a tie; vermillion: both positive and negative differences occur; grey: English scores at least as high in every run. ",
    "Category counts are calculated from the raw responses, not manually assigned. Means use available valid rescored items in each language-run combination.")
)
for(name in names(captions))writeLines(captions[[name]],file.path(DIR_CAP,paste0(name,".md")))
writeLines(c("# Colour accessibility", "",
  "Group comparisons use the Okabe-Ito blue (#0072B2) and vermillion (#D55E00) palette. Ordered responses and percentages use cividis from viridisLite. ",
  "Figure 2B uses a symmetric blue-white-vermillion scale with numerical signs and cell values. Figure 3B distinguishes groups by both shape and colour. ",
  "Heatmap text uses whichever of black or white gives the higher contrast against the actual cell colour; missing values use a crossed cell, never a zero.","",
  "Sources: https://jfly.uni-koeln.de/color/ ; https://arxiv.org/abs/1712.01662"),file.path(DIR_DATA,"colour_accessibility.md"))
lum<-function(col){rgb<-grDevices::col2rgb(col)/255;v<-ifelse(rgb<=.04045,rgb/12.92,((rgb+.055)/1.055)^2.4);as.numeric(c(.2126,.7152,.0722)%*%v)}
cell_colours<-unique(c(RAMP,SEQ,VSA_COL,DIV,BLUE,ORANGE,"#56B4E9","#DADBD7"))
fg<-contrast_ink(cell_colours);l<-lum(cell_colours)
ratio<-ifelse(fg=="white",1.05/(l+.05),(l+.05)/.05)
check("Heatmap and tile text has at least 4.5:1 contrast",all(ratio>=4.5),sprintf("minimum contrast %.3f:1",min(ratio)))
write_csv(data.frame(background=cell_colours,foreground=fg,contrast_ratio=ratio),"text_colour_contrast.csv")
if(requireNamespace("colorspace",quietly=TRUE)){
  vision<-list(standard=identity,protanopia=colorspace::protan,deuteranopia=colorspace::deutan,tritanopia=colorspace::tritan)
  cvd<-do.call(rbind,lapply(names(vision),function(name){
    f<-vision[[name]];cols<-f(c(BLUE,ORANGE));lab<-grDevices::convertColor(t(grDevices::col2rgb(cols)/255),from="sRGB",to="Lab",scale.in=1)
    clab<-grDevices::convertColor(t(grDevices::col2rgb(f(SEQ))/255),from="sRGB",to="Lab",scale.in=1)
    data.frame(vision=name,blue=cols[1],vermillion=cols[2],blue_vermillion_Lab_distance=sqrt(sum((lab[1,]-lab[2,])^2)),
               cividis_min_lightness_step=min(diff(clab[,1])))
  }))
  write_csv(cvd,"colour_vision_simulations.csv")
}
write_csv(do.call(rbind,checks),"validation_checks.csv")
report[grep("^Checks passed:",report)]<-sprintf("Checks passed: %d. Historical checks explicitly skipped: %d.",
  sum(vapply(checks,function(x)isTRUE(x$passed),logical(1))),sum(vapply(checks,function(x)is.na(x$passed),logical(1))))
capture.output(sessionInfo(),file=file.path(DIR_DATA,"R_session_info.txt"))
writeLines(c("# External reference value", "",
  "The adult comparison line in Figure 4 is an external published value, not a value calculated from the model responses.",
  "Verified against Baron-Cohen et al. (2001), Table 3, Group 2: general-population controls, N=122; mean=26.2 of 36, SD=3.6. ",
  "The plotted percentage is 100*26.2/36 = 72.777...%. The line is descriptive; it is not a condition-matched human-versus-model experiment.", "",
  "Primary source: https://docs.autismresearchcentre.com/papers/2001_BCetal_adulteyes.pdf#page=5"),file.path(DIR_DATA,"external_reference_sources.md"))
if(!"--verify-only"%in%args){
  figure_paths<-c(file.path(DIR_MAIN,paste0(rep(c("Fig2_language","Fig3_answer_changes","Fig4_rmet"),each=4),".",rep(c("pdf","png","tiff","svg"),3))),
                  file.path(DIR_SUPP,paste0("FigS14_language_direction.",c("pdf","png","tiff","svg"))))
  hashes<-tools::md5sum(figure_paths)
  write_csv(data.frame(file=names(hashes),md5=unname(hashes),
                       R_script_md5=unname(tools::md5sum(file.path(ROOT,"Analysis_script/15_final_manuscript_figures.R")))),"figure_file_hashes.csv")
}
writeLines(c(report,"",sprintf("Text contrast check: minimum %.3f:1 across all heatmap and tile backgrounds.",min(ratio)),
  "Colour-vision simulation results are saved when colorspace is installed; group shapes and printed heatmap values provide redundant information."),file.path(DIR_DATA,"validation_report.md"))
message("Finished: raw-derived values verified; raw CSV files unchanged.")
