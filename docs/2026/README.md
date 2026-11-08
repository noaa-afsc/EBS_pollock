# 2026 assessment publication

This directory owns the current web-ready report: `assessment.html`,
`assessment.pdf`, and the figure assets in `assets/`. Future report revisions
belong here, rather than in Downloads or a private report export. Keep both
formats at the same scientific revision and update `release.json` and the
homepage version together. No Word files are required.

Run `bash scripts/update_assessment_site.sh` from the repository root after
updating the report. It renders the homepage and development page, synchronizes
the report into `docs/2026/`, and checks local links and assets. Commit the
source files and generated site together; the Pages workflow deploys `docs/`
after publication to `main`.

Model inputs, estimation workflows, and prepared scientific products remain in
their owning assessment repositories. This published HTML is the current
editorial baseline; it is a saved report, not a promise that the historical
Quarto/scientific pipeline has been migrated. Revise its corresponding PDF
directly or through a direct PDF renderer. Retain this distinction when
changing scientific results.

The initial release contains 88 tables, 88 images, and a 259-page PDF. Local-only
supporting-file links were removed while preserving their displayed text.
Scientific content and the PDF page layout were retained. Manual accessibility
review is pending; automated link checks establish link integrity only.
