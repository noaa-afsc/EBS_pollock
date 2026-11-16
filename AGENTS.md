# Assessment website workflow

Report content edits start in `safe-report/EBSpollock.qmd`. The published
comparison baseline is `2026/assessment.html`, `2026/assessment.pdf`, and
`2026/assets/`. The PDF uses a dedicated print template; it should closely resemble the
published report, while pagination may differ. Follow the source commands below.

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

## QMD source workflow

`safe-report/EBSpollock.qmd` supplies report content to both formats. Its PDF
renderer generates all content from the QMD and versioned assets. Keep earlier
PDFs exclusively as comparison references. Run `bash safe-report/render.sh`
and `safe-report/_build/python-env/bin/python safe-report/verify_release.py`.
The user relaxed exact PDF reproduction on November 16: match the report's
print style closely, with separate HTML/PDF layouts and flexible pagination.
HTML byte identity was also relaxed on November 16. Keep EBSpollock.qmd as
readable Markdown; store table values in CSV and formatting in Lua/CSS helpers.
Run verify_migration.py for content-preserving changes and verify_release.py
for PDF content, images, links and source freshness. Inspect both formats.
Use `bash scripts/update_assessment_site.sh --assessment` to build, validate,
promote and synchronize both formats. General site updates omit that flag.
