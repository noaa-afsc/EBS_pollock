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

Introduction/audit revision: three explanatory paragraphs summarize data and
method changes since 2024. The jellyfish audit traces the twofold historical
change to the November 2024 precomputed summary, validates current filters,
and distinguishes the documented transition from its unresolved upstream cause.
The PDF has 263 pages after adding room for the expanded introduction.

A separate WtAgeRE sensitivity doubles 2025-2026 BTS observation SDs/CVs.
Its report is weight-at-age-bts-cv-sensitivity.qmd/html, with saved summaries
and independent checks under weight-at-age-bts-cv-sensitivity/.
The original fit and assessment products remain the baseline.

The separate Model26.0 spmR comparison uses equal-year 2020-2024 empirical
fishery input means for both catch and SSB weights. See
projection-empirical-weight-comparison.qmd/html and its saved summaries.
The original assessment and projections are retained. Historical gap fills
and the 2024 TMA conversion are documented in the comparison source notes.

November9:projection-re-weight-comparison.qmd/html adds the original and
doubled-BTS-CV WtAgeRE 2027 forecasts to the projection comparisons. Both use
the fishery forecast for catch and SSB at ages3-15+, with baseline young-age
weights retained. The original-RE control isolates the CV effect. Main
assessment advice remains unchanged. AppendixB now uses the author-provided
POL_ESP_HexSticker emblem in both HTML and PDF.

Weight-at-age figure revision: Figure 22a in Section 4.3.2 uses the saved
omit_bts_2026_20261109/attempt-001 fit. Historical and recent-year panels
distinguish observations, 2026 estimates, and 2027–2028 forecasts. Figure data
are supplied as CSV. The PDF insertion uses printed page 18a to retain existing
page numbers and Figure 22a to retain existing figure references.

Fishery structure and terminology revision: Section 4.1 now contains Catch,
Age compositions, and Weight-at-age (4.1.1–4.1.3). The fishery-weight treatment
and former4.3.2 material are consolidated in4.1.3. The recent-year weight
heatmap is Figure23 in the Figures section; the full-history panel was removed.
Subsequent main figures are renumbered through74. The biological-schedule
figure is now34 and distinguishes2026 weights and the selected2027 projection
curve. Report prose, tables, captions and eight graphic assets use tons; numeric
values retain their metric-ton basis. See tables/weight-age-revision-20261109/
for section source, saved curve data, PDF reflow source and validation.

Table 2 link restoration: all36 row-source links and3 note links are active
in both HTML and PDF. Targets are hosted under table2-sources/; archived HTML
sources have self-contained reading copies with original-source provenance.
PDF text and all266 pages are unchanged. Source hashes and verification
evidence accompany the restored files.

Bycatch table revision: the formerTables7,8,6,9 now appear asTables6,7,8,9.
All four estimate and column-definition downloads are part of their small Source
footers in HTML and PDF. Table8 now supplies a PSC column dictionary derived
from the existing aggregation recipe. Numeric content is unchanged.
Pages publishes docs/ from main using branch-based publishing. The redundant
static.yml deployment workflow is disabled after repeated deployment collisions
and synchronization stalls. The rendered report remains owned by2026/.

PDF typography revision: report text and tables use embedded Computer Modern
Unicode Serif (regular, bold, italic and bold italic). Latin Modern Math and
a small Unicode fallback cover specialized symbols. Existing line and table
positions, links, bookmarks, page count and raster figure pixels were verified.
The PDF remains a direct revision of the current published report, rather than
a rerun of the historical scientific/Quarto pipeline.
