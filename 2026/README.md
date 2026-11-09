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

The November 8 PSC revision adds main Table 54 (89 tables total) and
one PDF page (260 total), retaining the numbers of all earlier tables. Annual aggregated
CSV values and the R aggregation recipe are provided; vessel-level inputs are
excluded.

The subsequent FMP bycatch update adds main Tables 55-56 and matching narrative.
The assessment now contains 91 tables, 88 images, and 262 PDF pages.
Aggregated annual data, category maps, exclusion totals, and R scripts accompany
the report. Vessel- and processor-level input rows remain outside this repository.

The nontarget update adds main Table 57 and narrative, bringing the report to
92 tables, 88 images, and 263 PDF pages. It uses BSAI records and explicitly
maps the selected groups, with preliminary 2026 coverage through October 24.

Final layout validation removed an existing blank appendix section-break page,
redirected its table-section link, and refreshed page numbers (262 pages total).
