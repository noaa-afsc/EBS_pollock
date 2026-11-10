# Assessment editorial revision

The canonical publication baseline is commit `1620917`. The three revision scripts run in order:

1. `scripts/revise_fishery_narrative_20261109.py` applies the requested narrative edits, merges and subsection removals.
2. `scripts/remove_ats_biomass_allocation_table.py` removes original Tables 23 and 41 and original Figure 42, moves the original Figure 68 after recruitment Figure 32, and updates references and PDF page numbers.
3. `scripts/check_fishery_narrative_revision.py` checks the final artifacts.

The PDF uses Computer Modern Unicode. Original scientific inputs, fitted results, retained table values and retained raster-figure pixels are preserved. Footnote markers associated with removed prose are removed. The model-compilation and convergence checks are not applicable because this is an editorial revision using saved results.

The Quarto site workflow and local resource checks run separately. Manual accessibility review remains untested.
