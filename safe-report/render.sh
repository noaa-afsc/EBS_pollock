#!/usr/bin/env bash
# Render the readable authoring source; model estimation is separate.
set -euo pipefail
cd "$(dirname "$0")"
case "${1:-all}" in
  html|all|pdf) export SAFE_RENDER_TARGET="${1:-all}" ;;
  *) echo "Usage: bash safe-report/render.sh [html|pdf|all]" >&2; exit 2 ;;
esac
mkdir -p _build/2026
ln -sfn ../2026/assets assets
# Quarto copies image resources into the independent build tree.
if [[ -L _build/2026/assets ]]; then unlink _build/2026/assets; fi
quarto render EBSpollock.qmd --to html
printf '%s\n' "$PWD/_build/2026/assessment.html"
