#!/usr/bin/env bash
# run_all.sh — run the Paper 1 pipeline in dependency order and write every
# script's console output into ../data_summaries as <script>_output.txt.
#
#   cd analysis_code && bash run_all.sh
#
# Notes:
#   - Runs from anywhere: paths are resolved relative to this script.
#   - Each step also gets AAI_DATA_DIR, so every script agrees on the data folder.
#   - Scoring is OFF by default (it overwrites the committed by-model CSVs and
#     depends on the raw-filename question). Turn it on with:  RUN_SCORING=1 bash run_all.sh
#   - The RMET (Paper 2) step runs only if RMET_Results.csv is present.
#   - One failing step does NOT stop the others; a summary prints at the end.

set -uo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DATA="$(cd "$SCRIPT_DIR/.." && pwd)/data_summaries"
mkdir -p "$DATA"
export AAI_DATA_DIR="$DATA"

RS="$(command -v Rscript || true)"
PY="$(command -v python3 || command -v python || true)"
[ -n "$RS" ] || echo "WARNING: Rscript not found on PATH — R steps will be skipped."
[ -n "$PY" ] || echo "WARNING: python not found on PATH — the Python step will be skipped."
echo "data_summaries -> $DATA"
echo

declare -a NAMES=() STATUS=()
run_step () {          # run_step <label> <logfile-stem> <command...>
  local label="$1" stem="$2"; shift 2
  local log="$DATA/${stem}_output.txt"
  echo "==> $label"
  if [ -z "$RS" ] && [[ "$1" == *Rscript* ]]; then echo "    skipped (no Rscript)"; return; fi
  "$@" |& tee "$log"
  local rc=${PIPESTATUS[0]}
  NAMES+=("$label"); STATUS+=("$rc")
  echo "    exit=$rc   log: $log"
  echo
}

# 0. (optional) scoring: raw -> the three by-model CSVs
if [ "${RUN_SCORING:-0}" = "1" ] && [ -n "$RS" ]; then
  run_step "0. scoring (attitude_evolution.R)" "attitude_evolution" \
           "$RS" "$SCRIPT_DIR/attitude_evolution.R"
else
  echo "==> 0. scoring SKIPPED (set RUN_SCORING=1 to regenerate the by-model CSVs)"; echo
fi

# 1. §3 stats -> writes table2_check.csv (must precede §6)
[ -n "$RS" ] && run_step "1. verify_section3_stats.R" "verify_section3_stats" \
           "$RS" "$SCRIPT_DIR/verify_section3_stats.R"

# 2. §6 checks (needs SDO_VSA_Results.csv + table2_check.csv)
[ -n "$RS" ] && run_step "2. verify_section6.R" "verify_section6" \
           "$RS" "$SCRIPT_DIR/verify_section6.R"

# 3. independent checks (needs the coin package)
[ -n "$RS" ] && run_step "3. additional_checks_v3.R" "additional_checks_v3" \
           "$RS" "$SCRIPT_DIR/additional_checks_v3.R"

# 4. Python numbers + figure
[ -n "$PY" ] && run_step "4. attitude_analysis.py" "attitude_analysis" \
           "$PY" "$SCRIPT_DIR/attitude_analysis.py"

# 5. (Paper 2) RMET — only if its data is present
if [ -f "$DATA/RMET_Results.csv" ] && [ -n "$RS" ]; then
  run_step "5. rmet_evolution.R (Paper 2)" "rmet_evolution" \
           "$RS" "$SCRIPT_DIR/rmet_evolution.R"
else
  echo "==> 5. RMET SKIPPED (no RMET_Results.csv in data_summaries)"; echo
fi

echo "======================= summary ======================="
for i in "${!NAMES[@]}"; do
  printf "  [%s] %s\n" "$([ "${STATUS[$i]}" = 0 ] && echo PASS || echo FAIL)" "${NAMES[$i]}"
done
echo "======================================================="
