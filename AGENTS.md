# Assessment website workflow

The current assessment is maintained here in `2026/assessment.html`,
`2026/assessment.pdf`, and `2026/assets/`. Treat these as the current editorial
baseline. Make subsequent report edits here and propagate them through the
website; avoid a private report or Downloads as the publication source.

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
