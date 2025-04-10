# Readme

## Folder organizaton

ebswp_ss.R is the script for making plots

The folders represent a step-wise evolution to attempt to get SS3 runs to be like the EBS pollock model ones, hence modifications/refinements
may show up in the "base" folder while others were steps in the process that might be worth saving: 
  *  "base" is a model with the age1 indices split off as separate from main data, multinomial only
  *  "a45" is like "base" but instead of age 3-8 in mean fish selex, set to age 4-5
  *  "a39" is like "base" but instead of age 3-8 in mean fish selex, set to age 3-9
  *  "flex" is like "base" but SE on penalty set to 0.7 (from 0.2)
  *  "DM_option" is like "base" but can optionally turn on D-Multinomial 
  *  "no_age1_ind" is like base model with the age1 indices included as part ofeage-comps for Acoustics and bottom trawl
