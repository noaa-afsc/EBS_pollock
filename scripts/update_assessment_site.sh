#!/usr/bin/env bash
# Publish report files from this repository; no model fitting or Word export.
set -euo pipefail
cd "$(dirname "$0")/.."
quarto render index.qmd --to html
quarto render development.qmd --to html
if [[ -f 2026/weight-at-age-bts-cv-sensitivity.qmd ]]; then
  quarto render 2026/weight-at-age-bts-cv-sensitivity.qmd --to html
  cp docs/2026/weight-at-age-bts-cv-sensitivity.html 2026/weight-at-age-bts-cv-sensitivity.html
fi
mkdir -p docs/2026
rsync -a --delete 2026/ docs/2026/
python3 scripts/update_site_navigation.py
python3 scripts/check_assessment_site.py
