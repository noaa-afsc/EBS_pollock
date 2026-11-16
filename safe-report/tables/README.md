# Editable table data

Each CSV stores the displayed cells in row order. The first CSV line is an editing header (`cell_1`, `cell_2`, ...), not a report row. Short rows correspond to merged cells. The JSON file with the same stem records header/footer counts, spans and alignment. Captions and explanatory notes stay in EBSpollock.qmd.

Cells support inline Markdown for emphasis, links, subscripts and superscripts. Keep the supplied numerical precision and units.

| CSV | First displayed heading |
| --- | --- |
| [001-advice-model260.csv](001-advice-model260.csv) | Model 26.0 advice from revised fits |
| [002-status-model260.csv](002-status-model260.csv) | Model 26.0 stock-status summary. The 2025--2026 columns report status for 2025 and 2026; historical  |
| [003-table-003.csv](003-table-003.csv) | Source / Type / Years |
| [004-tbl-risk-summary.csv](004-tbl-risk-summary.csv) | Considerations |
| [005-status-model-options.csv](005-status-model-options.csv) | Comparison of Model 26.0 and the sensitivity analysis |
| [006-tbl-catch-management-history.csv](006-tbl-catch-management-history.csv) | EBS catch and adopted harvest limits |
| [007-tbl-management-measures.csv](007-tbl-management-measures.csv) | Historical pollock management and monitoring measures |
| [008-tbl-catch-area-season.csv](008-tbl-catch-area-season.csv) | Year / NW A / NW B / SE A / SE B / NW / SE / A / B / Total |
| [009-tbl-catch-retained-discarded.csv](009-tbl-catch-retained-discarded.csv) | Year / NW Discarded / SE Discarded / NW Retained / SE Retained / Total / Discarded % |
| [010-tbl-fishery-catch-at-age.csv](010-tbl-fishery-catch-at-age.csv) | Year / 1 / 2 / 3 / 4 / 5 / 6 / 7 / 8 / 9 / 10 / 11 / 12 / 13 / 14 / 15+ / All ages |
| [011-tbl-fmp-species-in-pollock.csv](011-tbl-fmp-species-in-pollock.csv) | Year / P.cod / Rock.Sole / Flathead / Arrowtooth / POP / Yellowfin / Sablefish / Shark / Atka.Mkrl / |
| [012-tbl-pollock-in-other-fisheries.csv](012-tbl-pollock-in-other-fisheries.csv) | Year / Rock.sole / Other.flats / Other.spp / P.cod / Yellowfin.sole / Total |
| [013-tbl-prohibited-species-catch.csv](013-tbl-prohibited-species-catch.csv) | Year / Bairdi / Chinook / Halib / Halib.M / Herring / NonChin / Snow Crab / Red King / Blue King / G |
| [014-tbl-nontarget-species-bycatch.csv](014-tbl-nontarget-species-bycatch.csv) | Year / Jellies / MiscFish / SeaStar / Osmerid / Grenadier / Eelpouts / Sea.pen / Anemone / Snails /  |
| [015-tbl-fishery-length-samples.csv](015-tbl-fishery-length-samples.csv) | Year / A / B southeast / B northwest / Total |
| [016-tbl-fishery-weight-length-samples.csv](016-tbl-fishery-weight-length-samples.csv) | Year / A / B southeast / B northwest / Total |
| [017-tbl-fishery-age-samples.csv](017-tbl-fishery-age-samples.csv) | Year / A / B southeast / B northwest / Total |
| [018-tbl-bts-sampling-effort.csv](018-tbl-bts-sampling-effort.csv) | Year / Hauls / Lengths / Aged / age assigned |
| [019-tbl-bts-biomass.csv](019-tbl-bts-biomass.csv) | Year / Biomass (million t) / SE (thousand t) / CV (%) |
| [020-tbl-bts-abundance-at-age.csv](020-tbl-bts-abundance-at-age.csv) | Year / 1 / 2 / 3 / 4 / 5 / 6 / 7 / 8 / 9 / 10 / 11 / 12 / 13 / 14 / 15+ / Total |
| [021-tbl-bts-historical-weights.csv](021-tbl-bts-historical-weights.csv) | Year / 1 / 2 / 3 / 4 / 5 / 6 / 7 / 8 / 9 / 10 / 11 / 12 / 13 / 14 / 15+ |
| [022-tbl-ats-sampling-effort.csv](022-tbl-ats-sampling-effort.csv) | Year / Hauls / Length records / Otoliths / Number aged |
| [023-tbl-ats-biomass.csv](023-tbl-ats-biomass.csv) | Year / Date / Area (nmi^2^) / Biomass (million t) / Rel. error / Assessment CV / Assessment SE (kt) |
| [024-tbl-ats-regions.csv](024-tbl-ats-regions.csv) | Region / EDSU records / Biomass (t) / Abundance (million fish) |
| [025-tbl-ats-age-history.csv](025-tbl-ats-age-history.csv) | Year / Age / Age 2+ / Total at age |
| [026-tbl-ats-core-estimates.csv](026-tbl-ats-core-estimates.csv) | Recent core-area ATS estimates |
| [027-tbl-ats-derived-abundance.csv](027-tbl-ats-derived-abundance.csv) | Age (FT-NIRS / TMA) / FT-NIRS abundance / TMA abundance |
| [028-tbl-ats-tma-weights.csv](028-tbl-ats-tma-weights.csv) | TMA age / Biomass (t) / Abundance (million fish) / Mean weight (kg/fish) |
| [029-tbl-model23-age-sample-sizes.csv](029-tbl-model23-age-sample-sizes.csv) | Year / Fishery / BTS / ATS |
| [030-tbl-avo-index.csv](030-tbl-avo-index.csv) | Year / Index (million m^2^) / Relative error (%) / SE (million m^2^) / Relative index / SE (relative |
| [031-tbl-avo-coverage.csv](031-tbl-avo-coverage.csv) | Year / Survey intervals / Positive intervals / Zeros |
| [032-tbl-avo-assessment-series.csv](032-tbl-avo-assessment-series.csv) | Year / ATS biomass (million t) / AVO index (million m^2^) / Sampling RE / Assessment CV / Assessment |
| [033-tbl-fishery-weights-projections.csv](033-tbl-fishery-weights-projections.csv) | Year / basis / Age (years) |
| [034-tbl-model23-data-impacts.csv](034-tbl-model23-data-impacts.csv) | Input updates within Model 23.0 |
| [035-tbl-updated-configuration-effects.csv](035-tbl-updated-configuration-effects.csv) | 2026 estimates after the catch revision |
| [036-tbl-model262-current-terminal-estimates.csv](036-tbl-model262-current-terminal-estimates.csv) | Estimated 2026 population |
| [037-tbl-model262-current-annual-estimates.csv](037-tbl-model262-current-annual-estimates.csv) | Annual population estimates and 95% intervals |
| [038-tbl-model260-regularized-posterior.csv](038-tbl-model260-regularized-posterior.csv) | Posterior population uncertainty |
| [039-tbl-model262-current-numbers-at-age.csv](039-tbl-model262-current-numbers-at-age.csv) | Numbers at age (billion fish) |
| [040-tbl-model262-current-terminal-numbers.csv](040-tbl-model262-current-terminal-numbers.csv) | 2026 numbers at age and uncertainty |
| [041-tbl-model262-current-biological-schedules.csv](041-tbl-model262-current-biological-schedules.csv) | Biological schedules and recent weights |
| [042-tbl-model262-current-selectivity.csv](042-tbl-model262-current-selectivity.csv) | 2026 selectivity and uncertainty |
| [043-tbl-model262-current-parameter-families.csv](043-tbl-model262-current-parameter-families.csv) | Estimated parameter groups |
| [044-tbl-model262-current-likelihood-summary.csv](044-tbl-model262-current-likelihood-summary.csv) | Contributions to the fit |
| [045-tbl-model262-index-residual-summary.csv](045-tbl-model262-index-residual-summary.csv) | Model 26.0 index residuals |
| [046-tbl-model260-ats-q-ssb2027.csv](046-tbl-model260-ats-q-ssb2027.csv) | 2027 female spawning biomass at fixed ATS catchability |
| [047-tbl-catch-revised-mohn-rho.csv](047-tbl-catch-revised-mohn-rho.csv) | Model 26.0 retrospective Mohn\'s rho |
| [048-tbl-model260-current-retrospective-revisions.csv](048-tbl-model260-current-retrospective-revisions.csv) | Retrospective revisions in Model 26.0 |
| [049-tbl-model261-nll-comparison.csv](049-tbl-model261-nll-comparison.csv) | model / objective / method |
| [050-tbl-coordinated-model-abc.csv](050-tbl-coordinated-model-abc.csv) | Maximum ABC and OFL under matched Scenario 2 catches |
| [051-tbl-catch-revised-posterior-check.csv](051-tbl-catch-revised-posterior-check.csv) | Posterior stock and selectivity summaries |
| [052-tbl-proposed-tier3-reference-points.csv](052-tbl-proposed-tier3-reference-points.csv) | Quantity / Model 26.0 / 26.1.2 Early-late fishery split / Units |
| [053-tbl-proposed-tier3-scenarios.csv](053-tbl-proposed-tier3-scenarios.csv) | Scenario / Rule |
| [054-tbl-proposed-tier3-short-term.csv](054-tbl-proposed-tier3-short-term.csv) | Year / Assumed catch / Spawning biomass / Maximum ABC / OFL / F |
| [055-tbl-proposed-tier3-catch.csv](055-tbl-proposed-tier3-catch.csv) | Year / Catch (kt) by harvest scenario |
| [056-tbl-proposed-tier3-ssb.csv](056-tbl-proposed-tier3-ssb.csv) | Year / SSB (kt) by harvest scenario |
| [057-tbl-proposed-tier3-f.csv](057-tbl-proposed-tier3-f.csv) | Year / F (yr⁻¹) by harvest scenario |
| [058-tbl-current-decision-definitions.csv](058-tbl-current-decision-definitions.csv) | Probability event / Definition / Purpose |
| [059-tbl-current-decision-model26.csv](059-tbl-current-decision-model26.csv) | Probability event / 2027 catch (kt) |
| [060-tbl-plan-team.csv](060-tbl-plan-team.csv) | SSC and Plan Team requests |
| [061-table-061.csv](061-table-061.csv) | SSC comment / Response |
| [062-table-062.csv](062-table-062.csv) | Review topic / Contribution in this draft / Completion needed |
| [063-table-063.csv](063-table-063.csv) | Data group / Source and definition / Status in this contribution |
| [064-table-064.csv](064-table-064.csv) | Stage and timing / Processes and evidence / Priority observations |
| [065-table-065.csv](065-table-065.csv) | Driver and mechanism / Expected relation and timing / Evidence and candidate measurement |
| [066-table-066.csv](066-table-066.csv) | Indicator family / Proposed measure / Management question |
| [067-table-067.csv](067-table-067.csv) | Measure / Unit / 2020 / 2021 / 2022 / 2023 / 2024 |
| [068-table-068.csv](068-table-068.csv) | Measure / Unit / 2020 / 2021 / 2022 / 2023 / 2024 |
| [069-esp2026-trade-global-panel-a.csv](069-esp2026-trade-global-panel-a.csv) | Metric / Unit / 2020 / 2021 / 2022 / 2023 / 2024 |
| [070-esp2026-trade-global-panel-b.csv](070-esp2026-trade-global-panel-b.csv) | Metric / Unit / 2020 / 2021 / 2022 / 2023 / 2024 / 2025 |
| [071-table-071.csv](071-table-071.csv) | Year / NBS (kJ/g dry mass) / SEBS (kJ/g dry mass) |
| [072-table-072.csv](072-table-072.csv) | Specification / Delta AIC / Residual variance reduction / Maximum gradient / Gradient below 1e-4 |
| [073-table-073.csv](073-table-073.csv) | Specification / Pathway / Estimate / 95 percent interval |
| [074-app-table-b1-panel-1.csv](074-app-table-b1-panel-1.csv) | Collection (agency) / 1991 / 1994 / 1997 / 1999 / 2001 / 2003 / 2005 / 2007 / 2009 |
| [075-app-table-b1-panel-2.csv](075-app-table-b1-panel-2.csv) | Collection (agency) / 2010 / 2011 / 2012 / 2013 / 2014 / 2015 / 2016 / 2017 |
| [076-app-table-b1-panel-3.csv](076-app-table-b1-panel-3.csv) | Collection (agency) / 2018 / 2019 / 2020 / 2021 / 2022 / 2023 / 2024 / 2025 |
| [077-app-table-b2-data.csv](077-app-table-b2-data.csv) | Collection year / Reported BS total (t) / EFP (t) / Explicitly Bogoslof-named (t) / Other reported B |
| [078-tbl-model262-current-parameters-complete.csv](078-tbl-model262-current-parameters-complete.csv) | Complete parameter estimates and uncertainty |
| [079-tbl-model262-current-parameter-correlations.csv](079-tbl-model262-current-parameter-correlations.csv) | Strong parameter correlations |
| [080-tbl-ftnir-treatment.csv](080-tbl-ftnir-treatment.csv) | Gear or survey / Collection year / Input or model version / Age basis and year-specific key / Applic |
| [081-tbl-ftnir-conversion-status.csv](081-tbl-ftnir-conversion-status.csv) | Stream / Conversion key / Current action |
| [082-tbl-ftnir-fishery-2024-weights.csv](082-tbl-ftnir-fishery-2024-weights.csv) | Age / Prepared mean / Prepared SE / Partial TMA mean / Partial TMA SD |
| [083-tbl-ftnir-fishery-weights.csv](083-tbl-ftnir-fishery-weights.csv) | Year / Age / Original mean / Original SE / TMA mean / TMA SE / SD |
| [084-tbl-ftnir-bts-weights.csv](084-tbl-ftnir-bts-weights.csv) | Year / Age / Original mean / Original SE / TMA mean / TMA SE |
| [085-tbl-ftnir-checks.csv](085-tbl-ftnir-checks.csv) | Check / Result |
| [086-tbl-bts-spatial-settings.csv](086-tbl-bts-spatial-settings.csv) | Setting / Biomass-index fit / Abundance-at-age fit |
| [087-advice-model2612.csv](087-advice-model2612.csv) | Model 26.1.2 Early-late fishery split comparison |
| [088-status-model2612.csv](088-status-model2612.csv) | Model 26.1.2 Early-late fishery split stock-status summary. The 2025--2026 columns report status for |
| [089-tbl-data-updates.csv](089-tbl-data-updates.csv) | Annual data and weight-at-age register |
