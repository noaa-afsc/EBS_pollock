#!/usr/bin/env bash
# Generate both formats from assessment.qmd and its versioned assets.
set -euo pipefail
cd "$(dirname "$0")"
case "${1:-all}" in
  html|all|pdf) target="${1:-all}" ;;
  *) echo "Usage: bash safe-report/render.sh [html|pdf|all]" >&2; exit 2 ;;
esac
build="$PWD/_build"
output="$build/2026"
mkdir -p "$output"
for resource in assets tables table2-sources validation; do
  ln -sfn "$PWD/../2026/$resource" "$output/$resource"
done
ln -sfn "$PWD/fonts" "$output/fonts"
ln -sfn "$PWD/../docs/index.html" "$build/index.html"
quarto pandoc assessment.qmd --from markdown --to html --standalone \
  --template published-layout.html --output "$output/assessment.tmp.html"
mv "$output/assessment.tmp.html" "$output/assessment.html"
if [[ "$target" != html ]]; then
  "${SAFE_PYTHON:-$PWD/_build/pdf-env/bin/python}" render_pdf.py "$output/assessment.html" "$output/assessment.pdf"
fi
printf '%s\n' "$output/assessment.html"
