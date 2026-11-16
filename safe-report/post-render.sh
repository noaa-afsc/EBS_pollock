#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"
pdf_python="${SAFE_PYTHON:-$PWD/_build/python-env/bin/python}"
if [[ ! -x "$pdf_python" && -z "${SAFE_PYTHON:-}" ]]; then
  pdf_python="$PWD/_build/pdf-env/bin/python"
fi
"$pdf_python" finish_html.py _build/2026/assessment.html
for resource in tables table2-sources validation; do
  ln -sfn "$PWD/../2026/$resource" "_build/2026/$resource"
done
if [[ "${SAFE_RENDER_TARGET:-all}" != html ]]; then
  "$pdf_python" render_pdf.py _build/2026/assessment.html _build/2026/assessment.pdf
fi
