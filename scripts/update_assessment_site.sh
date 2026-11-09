#!/usr/bin/env bash
# Publish report files from this repository; no model fitting or Word export.
set -euo pipefail
cd "$(dirname "$0")/.."
quarto render index.qmd --to html
quarto render development.qmd --to html
for report in 2026/weight-at-age-bts-cv-sensitivity.qmd 2026/projection-empirical-weight-comparison.qmd 2026/projection-re-weight-comparison.qmd; do
  if [[ -f "$report" ]]; then
    quarto render "$report" --to html
    report_html="${report%.qmd}.html"
    cp "docs/$report_html" "$report_html"
  fi
done
mkdir -p docs/2026
rsync -a --delete 2026/ docs/2026/
python3 scripts/update_site_navigation.py
python3 scripts/check_assessment_site.py
