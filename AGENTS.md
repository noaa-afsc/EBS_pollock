# Assessment website workflow

Report content edits start in `safe-report/assessment.qmd`. The published
comparison baseline is `2026/assessment.html`, `2026/assessment.pdf`, and
`2026/assets/`. The exact-layout migration remains in progress. Follow the
QMD migration commands below to build and compare both formats.

Keep HTML and PDF scientifically consistent. Generate no Word files. Update
`2026/release.json` hashes and the homepage version when revising the report.
Run `bash scripts/update_assessment_site.sh` and inspect changed layouts.
The script checks links and synchronizes the report into `docs/2026/`.
GitHub Pages deploys `docs/` on a push to `main`.

Preserve older scientific content when changing navigation. Scientific inputs,
model runs, and diagnostics remain in their owning repositories; this website
contains published report products rather than a migrated estimation pipeline.
Publication status and scientific adoption are separate. Record accessibility
checks accurately; manual accessibility review remains pending.

## QMD migration

`safe-report/assessment.qmd` is the editable migration source. Its PDF renderer
must generate all content from the QMD and versioned assets. Keep historical
PDFs exclusively as comparison references. Run `bash safe-report/render.sh`
and `safe-report/_build/pdf-env/bin/python safe-report/verify_release.py --inspect`.
The user requires the exact published 280-page layout. The full-source build
currently differs, so migration remains in progress. Use
`bash scripts/update_assessment_site.sh --assessment` for promotion. It enforces
the exact-layout check before changing published report files. Record a failed
comparison accurately. General site updates use the command without that flag.
