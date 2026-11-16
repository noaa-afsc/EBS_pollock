# Editable assessment source — migration in progress

[assessment.qmd](assessment.qmd) contains the report's text, tables, equations,
captions, and figure references. Its raw HTML block preserves the released web
report's markup. Edit that block for content changes. Figures are the versioned
images in `2026/assets/`. Keep the element IDs used by cross-references stable.
The page shell, styling, and contents menu are in `published-layout.html`.
Heading changes also require updating the matching contents entry there.

## Current status

The QMD rebuild reproduces the November 10 published HTML byte for byte.
The full PDF renderer typesets every page from that HTML and the image/font
assets. Its build uses no earlier PDF and needs no Git history. It supports
edits throughout the report.

The user requires the exact published 280-page PDF layout. The new source build
has 293 pages with different pagination and typography. See
[the recorded comparison](validation/migration-comparison.json). Exact PDF migration remains open.
The publication command enforces the layout comparison before replacing the
published files. The earlier 280-page replay is historical code and belongs
outside this source build.

## Build

Install dependencies from the repository root:

```sh
python3 -m venv safe-report/_build/pdf-env
safe-report/_build/pdf-env/bin/python -m pip install -r safe-report/pdf-requirements.txt
safe-report/_build/pdf-env/bin/python -m playwright install chromium
```

Quarto 1.10.18 supplies Pandoc. On Linux, Playwright also needs system browser
libraries, which can be installed with `playwright install --with-deps chromium`.
The versioned Computer Modern Unicode fonts carry their OFL license in `fonts/`.

```sh
bash safe-report/render.sh
```

Outputs are `safe-report/_build/2026/assessment.html` and `assessment.pdf`.
Pass `html` to build just HTML. Set `SAFE_PYTHON` to use another Python environment
containing the pinned dependencies. PDF rendering operates offline.

## Verify and publish

```sh
safe-report/_build/pdf-env/bin/python safe-report/verify_release.py --inspect
```

This writes `safe-report/_build/release-comparison.json` with HTML identity,
PDF pagination, images, text coverage, links, and outstanding visual checks.
The published artifacts are inputs to verification only. Manual equation and
accessibility review remain pending. Paragraph and cell text extraction uses
both PDF reading orders, with remaining differences reported for inspection.

```sh
bash scripts/update_assessment_site.sh --assessment
```

This builds both formats and requires the exact PDF comparison to pass before
promoting files into `2026/`, updating the release manifest, and synchronizing
`docs/2026/`. It currently stops at the layout comparison. Existing website-only
updates continue to use `bash scripts/update_assessment_site.sh`.

The automated source build also runs `python safe-report/check_editable_source.py`.
It changes a paragraph, a table cell, and a caption in an isolated copy with no
Git history or input PDF, then checks both generated formats.

## Scientific sources

This migration uses saved report content and image assets. Model estimation
and projection calculations remain in the companion `ebswp_rceattle` workspace.
The population fit is `results/catch-csv-20261104/rceattle/26_0/fit.rds`, with
numerical checks in its `diagnostics.json`. Projection inputs and results are
under `results/omit_bts2026_adopted_projections_20261109/`. Rendering this report
assembles the published scientific results. New assessment calculations need
updated tables, figures, and scientific review before publication.
