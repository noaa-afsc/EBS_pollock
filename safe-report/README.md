# Assessment source for HTML and PDF

[assessment.qmd](assessment.qmd) supplies the report's text, tables, equations,
captions, and figure references to both formats. Edit its raw HTML block for
content changes; that markup preserves the released web report exactly. Keep
cross-reference element IDs stable. Figure assets live in `2026/assets/`.

The formats have separate templates. `published-layout.html` supplies the web
shell and navigation. `prepare_print.py` and `print.css` supply PDF typography,
cover, contents, table pagination, figure pages, and appendix layout. The PDF
build reads the QMD-generated HTML and versioned images/fonts. It reads no
existing PDF and needs no Git history.

## Verified migration

The November 16 source build reproduces the November 10 HTML byte for byte.
The PDF uses a close visual match to the published report; exact pagination
was explicitly relaxed on November 16. The revised local build has 261 pages
versus the published 280. Its main tables and figures occupy 60 and 74 pages,
respectively. The narrative and appendices reflow.

All 89 source images and all 23,542 checked prose/table/caption elements are
present. Link destinations and page boundaries are checked automatically.
See [the comparison](validation/migration-comparison.json) and
[visual checks](validation/print-layout-review.json). Math pages were sampled
visually; full manual accessibility review remains pending.

The existing published PDF remains the comparison reference. A source build
writes to `_build/2026/`; publication is a separate explicit command.

## Build

From the repository root, install the pinned renderer:

```sh
python3 -m venv safe-report/_build/pdf-env
safe-report/_build/pdf-env/bin/python -m pip install -r safe-report/pdf-requirements.txt
safe-report/_build/pdf-env/bin/python -m playwright install chromium
bash safe-report/render.sh
```

Quarto 1.10.18 supplies Pandoc. Linux also requires Playwright's browser system
libraries (`playwright install --with-deps chromium`). CMU and Latin Modern Math
fonts and their licenses are versioned in `fonts/`. PDF rendering is offline.
Set `SAFE_PYTHON` to select another environment with the pinned dependencies.

Outputs are `safe-report/_build/2026/assessment.html` and `assessment.pdf`.
`assessment.print.html` is an intermediate print view. Pass `html` to
`render.sh` to build only HTML.

## Verify and publish

```sh
safe-report/_build/pdf-env/bin/python safe-report/verify_release.py
safe-report/_build/pdf-env/bin/python safe-report/check_editable_source.py
```

The first command checks source text, image pixels (including transparency),
internal/external link structure, and text page bounds. It records comparisons
with the published artifacts. Use `--require-html-match` when checking that the
migration preserves existing HTML bytes. Future intentional edits may change
those bytes. `--require-exact` remains available as an optional archival test;
exact PDF layout is no longer a publication condition.

The second command edits narrative, a table cell and a caption in a temporary
source tree with no input PDF or Git history, rebuilds both formats and checks
that all three edits appear. GitHub Actions runs these checks and uploads the
candidate PDF and comparison report.

After reviewing a source revision, publish it with:

```sh
bash scripts/update_assessment_site.sh --assessment
```

This builds both formats, checks content and links, promotes them into `2026/`,
updates the release manifest, and synchronizes `docs/2026/`. Commit source and
publication artifacts together. The command without `--assessment` performs
website-only updates.

## Scientific sources

Rendering assembles saved assessment results. It does not refit the model.
Model estimation and projections remain in the companion `ebswp_rceattle`
workspace. The population fit is
`results/catch-csv-20261104/rceattle/26_0/fit.rds`, with numerical checks in its
`diagnostics.json`. Projection products are under
`results/omit_bts2026_adopted_projections_20261109/`. New calculations require
updated tables/figures and scientific review before publication.
