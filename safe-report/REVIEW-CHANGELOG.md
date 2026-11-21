---
title: "EBS pollock SAFE: change log"
subtitle: "Carey’s editorial review and the Council review copy"
date: "November 19, 2026"
fontsize: 11pt
geometry: "paperwidth=6.25in,paperheight=10.25in,margin=0.48in"
colorlinks: true
---

## Where to start

The [November 10 Plan Team PDF](https://drive.google.com/file/d/1SlGpfiS_XBxj3xWljihapB3aJSxArCKn/view) is the posted comparison copy, not the editing base. The current QMD descends from the Nov 10 branch (`da4d3fe`) and includes the Nov 16 QMD conversion (`27ed5f7`), Nov 17 figure and wording edits (`036e54a`) and later local selectivity work.

### Already addressed before this pass

- **Executive Summary and projections:** The draft already identified Model 26.0 as the base case, described Scenario 2 and distinguished earlier specifications from current recommendations. Existing harvest advice and the current selectivity work were preserved.
- **Responses and bycatch:** Much of the SSC and Plan Team response material and the bycatch text were already in place.
- **AVO (2011 and 2013):** Both observations were already included. Table 27 matches the source index values; no separate two-year sensitivity was verified.

### Fixed in this pass

- **Catch (§§3–4; Tables 1, 3–4; Figure 11):** Explained why the 2024–25 assessment catch differs from AKFIN Answers (Bogoslof, area 518). Corrected reversed NW/SE labels in Tables 3–4; numeric rows did not change. Figure 11 covers NW areas 520–529; the tables cover 520–539. Area 530 contributes only 20.30 t in 1991 and 2.23 t in 2002, none in 2026. Table 3 and Figure 11 use a June 1 split on week-ending dates, separate from the June 10 regulatory handoff.
- **Sampling and surveys (§4; Table 12, Table 20, Figures 16–17, Appendix G):** Clarified fishery age sampling and effective weights. The 1988–90 gap is in the stratified sample counts, not in the historical catch-at-age series. Explicitly stated that provisional 2026 ATS ages use length data down to 0.5 m above the seafloor; the historical Figure 19 heatmap retains its 3 m boundary. The full 2026 ATS age dataset is expected in 2027.
- **Models and results (§§5–6):** Added a model-comparison table and clarified the accepted 2024 Model 23.0, the updated-data ADMB fits and the Rceattle alternatives. Clarified what changes across fits, how to read the comparisons and which uncertainty each interval includes.
- **Historical weights and references:** Matched the fixed 1964–1990 schedule to [2001 Table 1.9](https://apps-afsc.fisheries.noaa.gov/refm/docs/2001/BSpollock.pdf#page=31) and cited the [2007 report](https://apps-afsc.fisheries.noaa.gov/refm/docs/2007/EBSpollock.pdf#page=96) for uncertainty in future weights. Also cleaned up fishery and survey text, indicator descriptions, cross-references and citations.


### Carey’s PR #12, folded in November 19

[PR #12](https://github.com/noaa-afsc/EBS_pollock/pull/12) edits Results §§6.1–6.1.1 up to the recruits-per-spawner heading. The local QMD now carries its main changes: the distinction between lower posterior medians and the fitted estimates used for advice, the record 2018 year class, and the contrasting spawning-biomass projections. It keeps the existing uncertainty definitions and separate Scenario 2 catch assumptions. Figure 34’s historical comparison is limited to 2002–2024, the shared fitted years shown. No GitHub merge or push.

## Decisions and checks before release

1. **Which October 17 catch figure to emphasize?** The October 22 Alaska Region report lists **1,297,206 t**; the reporting-area weekly series in Table 3 and Figure 11 gives **1,267,543 t**. The **29,663 t** difference is unreconciled and cannot be explained by area 530. The annual assessment input is separate: **1,312,505 t** rounded. **Recommendation:** keep both observed totals labeled by source and do not change the assessment input without a reconciliation.
2. **Figure 33 artwork:** Kept “Accepted 2024 (Model 23.0).” The text distinguishes it from the updated-data refit. No graphics or tables changed in the PR integration.

### Review copy and source

The 260-page PDF and HTML were rebuilt from the QMD. The release checks passed for text, 90 figures and links. The separate Council-format PDF retains the body, image placements, links and bookmarks. It has December 2026 headers and footers, page numbering and no Plan Team disclaimer. Manual accessibility review remains separate. Nothing was submitted or published, and this pass did not refit models or change numerical inputs or harvest advice.

In your `EBS_pollock-publish-20261108` checkout, the editable log is `safe-report/REVIEW-CHANGELOG.md`.

The Council review copy before the Table 8 correction was `Council/EBSpollock.pdf` in this build folder:

```text
safe-report/_build/council-review-20261119-pr12/
```

For exact line changes against the checked-in version, run:

```sh
git diff --no-ext-diff -- safe-report/EBSpollock.qmd
```

That diff also includes your preexisting local edits. Use this log to distinguish the editorial pass from that earlier work. For the catch-table headers, also compare the two CSVs beginning `008-` and `009-` in `safe-report/tables/`.

### Table 8 midwater-only correction, November 19

Table 8 now uses only trips classified as **Pollock - midwater**, for every year (1991–2026). No gear or vessel-sector filter was added. The Oct 27 AKR export contains 551,030 records; 503,393 meet this trip-target definition. The retained gear codes are PTR (482,246 records), TRW (21,004), NPT (126), and HAL (17). The caption simply identifies the midwater pollock fishery. The surrounding text explains that the earlier 2024 assessment included trips classified as bottom pollock, where pollock was the main catch species, outside the main directed pollock fishery.

The displayed table, linked unrounded annual CSV, aggregation script, coverage metadata and repeated narrative totals were updated together. Independent checks matched all 360 annual species cells and their rounded display values to the original export. The 2020 values are **1,713 snow crab and 10 red king crab**. The 2024/2025/2026 Chinook totals are 8,034/19,702/14,503; non-Chinook totals are 34,888/142,809/145,788; halibut mortality rounds to 22/50/43 t. No model inputs or harvest advice changed.

The fresh 260-page HTML/PDF build passed release checks (90 figures, 887 links, 188 bookmarks). The Council copy preserves body text, image placements, links and bookmarks, with December 2026 headers/footers, a single page number and no Plan Team disclaimer. Table 8 is on **page 50** and was visually checked; all rows and source notes fit on the page. Manual accessibility review remains pending.

Latest local Council proof: open `Council/EBSpollock.pdf` in this build folder:

```text
safe-report/_build/council-review-20261119-midwater/
```

Source and required dependencies were pushed in commit `9b9857e` on branch `Murphy-assessment-qmd-source`. This was not a merge to main or a posted assessment release. The aggregation script, ancillary metadata and this changelog remain local unless separately included in a later commit.


### Approved late PR #12 Results edits, November 20

Applied the approved nine Results-only prose changes to the local QMD on `Murphy-assessment-qmd-source`, against commit `9b9857e`. The exact patch changes 19 lines in and 14 lines out; all content outside the targeted Results paragraphs is unchanged. Existing unrelated local edits are preserved. No data, figures, Table 8, model inputs or harvest advice changed. No commit, push, merge or submission.

The matching Council proof was rendered and checked separately from the exact candidate source. It has 260 pages, 90 images, 893 links, 188 bookmarks and 353 named destinations, with December 2026 headers/footers, page numbers and no disclaimer. Manual accessibility conformance remains uncertified. Source SHA-256: `5e8445cf03d726a20b5f6fa175dae3b4c723eb5176df57bc71829d7024727035`. Approved Council PDF SHA-256: `caec90fb7b8688d74b7ff03baa284928fa37ac37427ed7c1e35376d52b1391bd`. Existing published HTML/PDF outputs are not replaced by this local source integration.

The verified approved PDF was copied without overwriting earlier proofs to `safe-report/_build/council-approved-20261120-late-pr12/Council/EBSpollock.pdf`; its SHA-256 matches the approved Library copy.


### Reconciled merged PR #12 with the local draft, November 20

Fetched the merged PR at `4e7eb76` and compared every Results change against the local draft. Preserved the approved November 20 edits in `0331828` before reconciling the histories. The combined source retains Carey's clearer section introduction, posterior-versus-point-estimate explanation, record 2018 year class, contrasting projections, recent-recruitment data limitations, parameter-table explanation, historical selectivity changes, 1965–1975 mean-age discrepancy, and ATS QQ-plot departure.

Retained local scientific qualifications: the Figure 34 comparison applies to the shared fitted years 2002–2024; Scenario 2 catch assumptions differ between assessments; asymptotic and posterior intervals have distinct meanings; covariance-adjusted BTS residuals use the plotted reference range; narrower QQ plots and structured annual residuals limit interpretation of aggregate fits. Retained the local Table 8 midwater-trip definition, all table values and figure assets, and existing harvest advice. Simplified the biological-schedules sentence and repaired paragraph wrapping.

This reconciliation is editorial. Model compilation, gradient and Hessian checks are Not Applicable because saved model outputs are unchanged. The release build and consistency checks are recorded separately; manual accessibility validation remains Not Tested. The October 17 catch-source discrepancy remains explicitly documented above.


### Google review document checked, November 20

Read [Carey's re-write suggestions](https://docs.google.com/document/d/1OG8IfkO3rnUZEcPbOto62blQZdyF372iTBVL_hbEfdA/edit?tab=t.0) and its comment panel through the signed-in browser after the connector returned 403. Also compared the two commits on `origin/edits_analytic_approach` (`1870858`, `88aec6b`). Their substantive wording requests are already incorporated in the clearer local Section 5; kept current figure numbers and the verified ADMB bridge definition of Model 23.2. The review document's description of 23.2 as Rceattle is superseded by the actual configuration description.

| Review request | Disposition in combined draft |
|---|---|
| Executive summary: changes since 2024, base case, uncertainty method, Scenario 2, and separate adopted specifications | Incorporated in Sections 1.1–1.4 and detailed Sections 2.1.1–2.1.2; sensitivity advice moved from the executive summary. |
| Remove supplied/extract/tracking prose; standardize tons, weight-at-age and base case | Earlier edits incorporated; targeted terminology search found no supplied, tonnes, Natural-mortality, base model, or weight at age occurrences. Necessary retained-catch and likelihood qualifications remain. |
| Introduction, life history, catch-source scope, management/bycatch context | Earlier editorial revisions incorporated. Area 518 explains the small 2024–2025 AKFIN differences. The separate October 17 discrepancy remains unresolved. |
| Bycatch prose and numbers | Concise structure incorporated; older review-document numbers are superseded by the verified November 19 midwater-only Table 8 values. |
| Fishery ages, sampling gap, conversion methods, AVO units/zero intervals, Japanese CPUE, weights and composition N | Incorporated in Section 4. Provisional 2026 ATS ages are distinguished from fishery ages ending in 2025. |
| Model-comparison table, early/modern split, marginal versus conditional fits, ATS age-1 rule | Incorporated in Sections 5.1–5.1.4, including data coverage, fixed SD values, periods and source of uncertainty. |
| Results through OSA plots and accepted-2024 comparison | PR #12 and approved local edits reconciled. Accepted-2024 label retained; shared fitted years are specified. |
| Figure 38: fully define spawning exploitation | Added explicit 100 × (1 − with-fishing / without-fishing next-spawning female biomass) definition to Results and caption. Calculation and values unchanged. |
| OSA implementation note removed in Carey's PR | Replaced version/software details with one sentence explaining that residuals assess fit conditional on fitted parameters. Retained observed residual patterns and dispersion. |
| Figure captions 4–19 | Earlier caption revisions incorporated; retain current numbering because the local figure sequence has changed. Preserve verified week-ending seasonal boundary in Figure 11, rather than the older proposed calendar caption. |
| Figure 44 error bars | Caption and Results explain ±2 SE and observation uncertainty. Carey's November 19 comment says she is still editing this item; further author review remains open. |
| Reference corrections | Fournier, Gann, Honkalehto 2015/DY1407/DOI, Ianelli reports, Kimura, Lang, Lauffenburger and Livingston corrected previously; Francis corrigendum already present. Added missing Honkalehto 2011 original DOI and corrigendum, verified from publisher-deposited Crossref metadata (10.1139/f2011-101; 68(8):1508–1509). |

All comments attached to deleted source text were reviewed as historical context; their open UI status alone does not identify a remaining draft defect. No Google Doc comments were resolved or suggestions accepted in the shared document.

### Reconciled-draft validation

The final build and local site synchronization completed successfully. Source-content verification passed with 260 pages, 90 figures, 890 links, 188 bookmarks and no off-page text. Site validation passed for 90 HTML tables (89 data tables plus the model-comparison table), 39 Table 2 source links, local assets, matching HTML/PDF copies and release hashes. Credential-pattern scan of the edited report source and site-check script found no matches.

The new Council proof is `safe-report/_build/council-reconciled-20261120/Council/EBSpollock.pdf`. Its 260 pages preserve the plain PDF's body text, image pixels and placements, 890 links, 188 bookmarks and 353 named destinations. It has December 2026 headers/footers and one page number, with the first page unnumbered and the disclaimer disabled. Pages 1, 18, 50 and 137 were visually checked. Formatting warned that inherited accessibility tags require review; manual accessibility validation remains Not Tested. The earlier approved PDF remains available unchanged. These are local review products; no push or Council submission was performed.

### Author's final editorial batch, November 20

Simplified Section 1.3 to female spawning biomass and stock/risk status; retained the advice and status tables. Removed Table 2's Action stage column and row-review link, preserving milestone distinctions within the row wording. Merged the roe-production and ESP-context paragraphs with rounded B-season values. Right-aligned all Table 8 columns. Renamed Section 4 to Assessment data. Merged the conditional retrospective paragraphs without bold, condensed historical assessment revisions into one paragraph without a subheading, and replaced the composition-weighting discussion with the author's future-research wording. Applied past tense to reporting narrative while preserving future recommendations, quotations and mathematical notation; removed bold in the specified self-test passage. Renamed Posterior summaries and placed the SI note after the suggested citation.

Source checks verified every requested edit and preserved numeric tokens and display equations during the tense pass. No model fits, input datasets or harvest advice were changed. Model compilation, gradients and Hessian verification are Not Applicable to this editorial release.

The release layout review found Table 2's redundant verification notes on a nearly empty continuation page. Removed the two audit paragraphs from the table footer, retaining its historical-source download links and all milestone/source rows.

## November 21: PR #18 Results follow-up

Kept the merged Results edits and existing local edits. Clarified the model-specific retrospective summaries and distinguished historical age-3+ biomass from within-model peels. Corrected the Figure 54 source to 2024 SAFE Table 23 (including the 2024 estimate of 9,415 thousand t). Described the Francis diagnostic by source and confirmed the assessment retained its input weights. No fit, table values, or advice changed. PR #19 remains separate.

## November 21: Selective prose from PR #19

Reviewed Carey's projections branch at `f921b11`. Kept the current section order, headings, anchors, risk rationales, status tests, prior-year OFL-equivalent F, decision tables, and sensitivity discussion. Adopted or adapted explanations for Tier 3 reference points, scenario assumptions, spmR, and reading the decision table. Corrected the existing general Tier 3 rule description: the FMP's default zero-F threshold is 0.05 × B40%, not B20%. That correction describes the rule; no model code, outputs or advice changed. Kept the 1.375 Mt Scenario 2 catches supported by Tables 48 and 50, the distinction between deterministic tables and simulated figures, and the distinction between equilibrium and historical biomass. Left out the duplicate sections, unfinished risk language, inconsistent scenario/status text, SIS blank and executable fragments. No merge, push, or submission.

The 259-page Council review proof is also in Downloads as `EBSpollock-2026-Council-review-20261121.pdf`. Review §§6.1.5–6.1.6 on page 21, §7 on page 24, Table 8 on page 49, and Figure 54 on page 152. The QA comparison found identical cell text across all 90 report tables versus merged PR #18. The Council copy preserves body text, figures, links, bookmarks, and named destinations, with the Council header/footer, a single page number, and no disclaimer. Manual accessibility conformance was not tested. This is a review proof, not a submitted or published release.

### Author-approved catch and ecosystem QA corrections, November 21

Section 3.1 now distinguishes the two observed October 17 catch-to-date series (a 29,663 t difference) from the estimated full-year 2026 model catch. Table 4 identifies the 2026 row as the assessment estimate. Section 8.2 and Table 55 distinguish the small observed adult diet share from modeled predation mortality of age-1 pollock. Sections 11.1 and 11.2 retain the supported year-over-year decline in overall age-1 predation mortality but omit conflicting historical cannibalism benchmarks. Table 8, catch data, model inputs, stock status, risk scores and harvest advice are unchanged.

### November 21: Remove empty Appendix J

Removed the unused Appendix J headings and contents entries. Appendices A–I, assessment content, tables, figures, and advice are unchanged.
