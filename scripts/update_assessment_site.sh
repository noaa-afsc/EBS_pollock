#!/usr/bin/env bash
# Publish report files from this repository; no model fitting or Word export.
set -euo pipefail
cd "$(dirname "$0")/.."
quarto render index.qmd --to html
quarto render development.qmd --to html
mkdir -p docs/2026
rsync -a --delete 2026/ docs/2026/
python3 scripts/update_site_navigation.py
python3 scripts/check_assessment_site.py
