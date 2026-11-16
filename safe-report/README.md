# Editable EBS pollock assessment

[EBSpollock.qmd](EBSpollock.qmd) is the authoring source for both the HTML report
and PDF. It contains ordinary Markdown prose, headings, links, captions and
LaTeX equations. The former HTML-heavy `assessment.qmd` and web-shell template
have been retired; their history remains in Git.

## What to edit

- **Prose, authors, captions, equations and links:** edit `EBSpollock.qmd`.
- **Table values:** edit the CSV named in each table's `data-table` attribute.
  [The table index](tables/README.md) identifies all 89 tables. Values remain
  strings so precision, units and missing-value symbols survive rendering.
- **Merged headers and footnotes:** edit the matching table JSON. It specifies
  header/footer row counts, cell spans and alignment; it contains no report prose.
- **Figures:** update the referenced images in `2026/assets/`; edit their captions
  and `fig-alt` descriptions in the QMD.
- **Appearance:** `report.css` controls the website; `print.css` controls the PDF.

A table declaration looks like this:

```markdown
::: {#tbl-example .report-table data-table="tables/example.csv"}
Table 1: A readable caption with [a source link](https://example.org).
:::
```

Figure divisions group images, notes and captions. Their text is ordinary
Markdown. Keep existing anchor IDs stable so published links keep working.
Existing section, table and figure numbers are explicit in the source to
preserve the assessment and appendix numbering during this migration.

## Build both formats

Use Python 3.11 or newer; CI uses Python 3.12. The existing dependency update to
Pillow 13 requires a newer interpreter than Python 3.10. Quarto 1.10.18 is pinned
for this release.

From the repository root:

```sh
python3 -m venv safe-report/_build/python-env
safe-report/_build/python-env/bin/python -m pip install -r safe-report/pdf-requirements.txt
safe-report/_build/python-env/bin/python -m playwright install chromium
bash safe-report/render.sh
```

On Linux, install browser system libraries with
`playwright install --with-deps chromium`. Set `SAFE_PYTHON` to use another
Python environment containing the pinned dependencies.

`quarto render EBSpollock.qmd` from this directory also builds both formats.
Quarto renders native Markdown and native tables to HTML. The post-render step
applies semantic caption/anchor formatting and prints a dedicated view with the
pinned Chromium renderer. It reads no existing PDF and needs no Git history.
Rendering consumes saved results and does not execute assessment models or R.

Outputs are `_build/2026/assessment.html` and `_build/2026/assessment.pdf`. Their
published filenames remain stable. For HTML-only editing:

```sh
bash safe-report/render.sh html
# Or, from safe-report:
SAFE_RENDER_TARGET=html quarto preview EBSpollock.qmd
```

## Verify and publish

```sh
safe-report/_build/python-env/bin/python safe-report/verify_migration.py
safe-report/_build/python-env/bin/python safe-report/verify_release.py
safe-report/_build/python-env/bin/python safe-report/check_editable_source.py
```

The migration comparison checks paragraphs/headings, every table cell and its
spans, 89 images and their alt text, 84 LaTeX equations, links and anchors against
the published HTML. It is appropriate for content-preserving changes; deliberate
future content revisions should be reviewed as differences rather than forced
to match old scientific results.

The release check verifies current source hashes, PDF content, image pixels,
link targets and page bounds. The editing test changes prose and a caption in
the QMD plus a CSV value, then rebuilds both formats in a clean directory with
no Git history or input PDF. CI runs these checks and uploads the candidate PDF.
Exact HTML bytes and exact PDF pagination are outside the current requirement.
Manual accessibility review remains pending; automated checks establish only
the properties explicitly recorded in the validation reports.

After verification and visual review, publish with:

```sh
bash scripts/update_assessment_site.sh --assessment
```

This builds and checks both formats, promotes the HTML/PDF and local web
resources, updates the release manifest and homepage, and synchronizes `docs/`.
Commit the source and publication artifacts together. Website-only updates use
the same command without `--assessment`.

`tools/migrate_readable.py` records the one-time conversion from the November 10
HTML. It is excluded from normal builds. Do not rerun it over subsequent edits.

## Scientific provenance

This is a report-source migration. Assessment fits, assumptions, units,
interpretation and management recommendations are preserved. Scientific inputs,
model diagnostics and estimation remain in the companion `ebswp_rceattle`
workspace. The saved population fit is
`results/catch-csv-20261104/rceattle/26_0/fit.rds`; projections are under
`results/omit_bts2026_adopted_projections_20261109/`.
