#!/usr/bin/env bash
# Publish report files from this repository; no model fitting or Word export.
set -euo pipefail
cd "$(dirname "$0")/.."
case "${1:-}" in
  "") ;;
  --assessment)
    bash safe-report/render.sh
    pdf_python="${SAFE_PYTHON:-$PWD/safe-report/_build/python-env/bin/python}"
    "$pdf_python" safe-report/verify_release.py
    # Source content, image pixels and link targets must pass before promotion.
    "$pdf_python" safe-report/promote.py
    ;;
  *) echo "Usage: bash scripts/update_assessment_site.sh [--assessment]" >&2; exit 2 ;;
esac
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
# Prefer macOS's bundled copy, independent of Homebrew shared-library updates.
if [[ "$(uname -s)" == Darwin && -x /usr/bin/rsync ]]; then
  PATH=/usr/bin:/bin:/usr/sbin:/sbin /usr/bin/rsync -a --delete 2026/ docs/2026/
else
  rsync -a --delete 2026/ docs/2026/
fi
python3 scripts/update_site_navigation.py
python3 scripts/check_assessment_site.py
