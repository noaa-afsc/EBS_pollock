#V3.30.xx.yy;_safe;_compile_date:_Apr  7 2025;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_13.2
#_Stock_Synthesis_is_a_work_of_the_U.S._Government_and_is_not_subject_to_copyright_protection_in_the_United_States.
#_Foreign_copyrights_may_apply._See_copyright.txt_for_more_information.
#_User_support_available_at:_https://groups.google.com/g/ss3-forum_and_NMFS.Stock.Synthesis@noaa.gov
#_User_info_available_at:_https://nmfs-ost.github.io/ss3-website/
#_Source_code_at:_https://github.com/nmfs-ost/ss3-source-code

#C 2022 ebspollock control file
#_data_and_control_files: data.ss // control.ss_new
1  # 0 means do not read wtatage.ss; 1 means read and use wtatage.ss and also read and use growth parameters
1  #_N_Growth_Patterns (Growth Patterns, Morphs, Bio Patterns, GP are terms used interchangeably in SS3)
1 #_N_platoons_Within_GrowthPattern 
#_Cond 1 #_Platoon_within/between_stdev_ratio (no read if N_platoons=1)
#_Cond sd_ratio_rd < 0: platoon_sd_ratio parameter required after movement params.
#_Cond  1 #vector_platoon_dist_(-1_in_first_val_gives_normal_approx)
#
4 # recr_dist_method for parameters:  2=main effects for GP, Area, Settle timing; 3=each Settle entity; 4=none (only when N_GP*Nsettle*pop==1)
1 # not yet implemented; Future usage: Spawner-Recruitment: 1=global; 2=by area
1 #  number of recruitment settlement assignments 
0 # unused option
#GPattern month  area  age (for each settlement assignment)
 1 1 1 0
#
#_Cond 0 # N_movement_definitions goes here if Nareas > 1
#_Cond 1.0 # first age that moves (real age at begin of season, not integer) also cond on do_migration>0
#_Cond 1 1 1 2 4 10 # example move definition for seas=1, morph=1, source=1 dest=2, age1=4, age2=10
#
0 #_Nblock_Patterns
#_Cond 0 #_blocks_per_pattern 
# begin and end years of blocks
#
# controls for all timevary parameters 
1 #_time-vary parm bound check (1=warn relative to base parm bounds; 3=no bound check); Also see env (3) and dev (5) options to constrain with base bounds
#
# AUTOGEN
 1 1 1 1 1 # autogen: 1st element for biology, 2nd for SR, 3rd for Q, 4th reserved, 5th for selex
# where: 0 = autogen time-varying parms of this category; 1 = read each time-varying parm line; 2 = read then autogen if parm min==-12345
#
#_Available timevary codes
#_Block types: 0: P_block=P_base*exp(TVP); 1: P_block=P_base+TVP; 2: P_block=TVP; 3: P_block=P_block(-1) + TVP
#_Block_trends: -1: trend bounded by base parm min-max and parms in transformed units (beware); -2: endtrend and infl_year direct values; -3: end and infl as fraction of base range
#_EnvLinks:  1: P(y)=P_base*exp(TVP*env(y));  2: P(y)=P_base+TVP*env(y);  3: P(y)=f(TVP,env_Zscore) w/ logit to stay in min-max;  4: P(y)=2.0/(1.0+exp(-TVP1*env(y) - TVP2))
#_DevLinks:  1: P(y)*=exp(dev(y)*dev_se;  2: P(y)+=dev(y)*dev_se;  3: random walk;  4: zero-reverting random walk with rho;  5: like 4 with logit transform to stay in base min-max
#_DevLinks(more):  21-25 keep last dev for rest of years
#
#_Prior_codes:  0=none; 6=normal; 1=symmetric beta; 2=CASAL's beta; 3=lognormal; 4=lognormal with biascorr; 5=gamma
#
# setup for M, growth, wt-len, maturity, fecundity, (hermaphro), recr_distr, cohort_grow, (movement), (age error), (catch_mult), sex ratio 
#_NATMORT
3 #_natM_type:_0=1Parm; 1=N_breakpoints;_2=Lorenzen;_3=agespecific;_4=agespec_withseasinterpolate;_5=BETA:_Maunder_link_to_maturity;_6=Lorenzen_range
 #_Age_natmort_by sex x growthpattern (nest GP in sex)
 0.9 0.6 0.45 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3
#
1 # GrowthModel: 1=vonBert with L1&L2; 2=Richards with L1&L2; 3=age_specific_K_incr; 4=age_specific_K_decr; 5=age_specific_K_each; 6=NA; 7=NA; 8=growth cessation
1 #_Age(post-settlement) for L1 (aka Amin); first growth parameter is size at this age; linear growth below this
15 #_Age(post-settlement) for L2 (aka Amax); 999 to treat as Linf
-999 #_exponential decay for growth above maxage (value should approx initial Z; -999 replicates 3.24; -998 to not allow growth above maxage)
0  #_placeholder for future growth feature
#
0 #_SD_add_to_LAA (set to 0.1 for SS2 V1.x compatibility)
0 #_CV_Growth_Pattern:  0 CV=f(LAA); 1 CV=F(A); 2 SD=F(LAA); 3 SD=F(A); 4 logSD=F(A)
#
5 #_maturity_option:  1=length logistic; 2=age logistic; 3=read age-maturity matrix by growth_pattern; 4=read age-fecundity; 5=disabled; 6=read length-maturity
#_Age_Fecundity by growth pattern from wt-at-age.ss now invoked by read bodywt flag
2 #_First_Mature_Age
# NOTE: maturity options 4 and 5 cause fecundity_at_length to be ignored, but parameters still needed 
1 #_fecundity_at_length option:(1)eggs=Wt*(a+b*Wt);(2)eggs=a*L^b;(3)eggs=a*Wt^b; (4)eggs=a+b*L; (5)eggs=a+b*W
0 #_hermaphroditism option:  0=none; 1=female-to-male age-specific fxn; -1=male-to-female age-specific fxn
1 #_parameter_offset_approach for M, G, CV_G:  1- direct, no offset**; 2- male=fem_parm*exp(male_parm); 3: male=female*exp(parm) then old=young*exp(parm)
#_** in option 1, any male parameter with value = 0.0 and phase <0 is set equal to female parameter
#
#_growth_parms
#_ LO HI INIT PRIOR PR_SD PR_type PHASE env_var&link dev_link dev_minyr dev_maxyr dev_PH Block Block_Fxn
# Sex: 1  BioPattern: 1  NatMort
# Sex: 1  BioPattern: 1  Growth
 2 15 5 32 99 0 -5 0 0 0 0 0 0 0 # L_at_Amin_Fem_GP_1
 45 60 53.2 50 99 0 -3 0 0 0 0 0 0 0 # L_at_Amax_Fem_GP_1
 0.2 0.4 0.3 0.3 99 0 -3 0 0 0 0 0 0 0 # VonBert_K_Fem_GP_1
 0.03 0.16 0.066 0.1 99 0 -5 0 0 0 0 0 0 0 # CV_young_Fem_GP_1
 0.03 0.16 0.062 0.1 99 0 -5 0 0 0 0 0 0 0 # CV_old_Fem_GP_1
# Sex: 1  BioPattern: 1  WtLen
 -3 3 7e-06 7e-06 99 0 -50 0 0 0 0 0 0 0 # Wtlen_1_Fem_GP_1
 -3 3 2.9624 2.9624 99 0 -50 0 0 0 0 0 0 0 # Wtlen_2_Fem_GP_1
# Sex: 1  BioPattern: 1  Maturity&Fecundity
 -3 43 36.89 36.89 99 0 -50 0 0 0 0 0 0 0 # Mat50%_Fem_GP_1
 -3 3 -0.48 -0.48 99 0 -50 0 0 0 0 0 0 0 # Mat_slope_Fem_GP_1
 -3 3 1 1 99 0 -50 0 0 0 0 0 0 0 # Eggs/kg_inter_Fem_GP_1
 -3 3 0 0 99 0 -50 0 0 0 0 0 0 0 # Eggs/kg_slope_wt_Fem_GP_1
# Hermaphroditism
#  Recruitment Distribution 
#  Cohort growth dev base
 0.1 10 1 1 1 0 -1 0 0 0 0 0 0 0 # CohortGrowDev
#  Movement
#  Platoon StDev Ratio 
#  Age Error from parameters
#  catch multiplier
#  fraction female, by GP
 1e-05 0.99999 0.5 0.5 0.5 0 -99 0 0 0 0 0 0 0 # FracFemale_GP_1
#  M2 parameter for each predator fleet
#
#_no timevary MG parameters
#
#_seasonal_effects_on_biology_parms
 0 0 0 0 0 0 0 0 0 0 #_femwtlen1,femwtlen2,mat1,mat2,fec1,fec2,Malewtlen1,malewtlen2,L1,K
#_ LO HI INIT PRIOR PR_SD PR_type PHASE
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no seasonal MG parameters
#
3 #_Spawner-Recruitment; Options: 1=NA; 2=Ricker; 3=std_B-H; 4=SCAA; 5=Hockey; 6=B-H_flattop; 7=survival_3Parm; 8=Shepherd_3Parm; 9=RickerPower_3parm
0  # 0/1 to use steepness in initial equ recruitment calculation
0  #  future feature:  0/1 to make realized sigmaR a function of SR curvature
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type      PHASE    env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn #  parm_name
             8            17        12.187            12            99             0          1          0          0          0          0          0          0          0 # SR_LN(R0)
           0.2             1      0.877275         0.777         0.113             2         -4          0          0          0          0          0          0          0 # SR_BH_steep
           0.1           1.6           1.0           1.1            99             0         -6          0          0          0          0          0          0          0 # SR_sigmaR
            -5             5             0             0            99             0        -50          0          0          0          0          0          0          0 # SR_regime
             0             2             0             1            99             0        -50          0          0          0          0          0          0          0 # SR_autocorr
#_no timevary SR parameters
2 #do_recdev:  0=none; 1=devvector (R=F(SSB)+dev); 2=deviations (R=F(SSB)+dev); 3=deviations (R=R0*dev; dev2=R-f(SSB)); 4=like 3 with sum(dev2) adding penalty
1970 # first year of main recr_devs; early devs can precede this era
2024 # last year of main recr_devs; forecast devs start in following year
1 #_recdev phase 
1 # (0/1) to read 13 advanced options
 1949 #_recdev_early_start (0=none; neg value makes relative to recdev_start)
 3 #_recdev_early_phase
 5 #_forecast_recruitment phase (incl. late recr) (0 value resets to maxphase+1)
 1 #_lambda for Fcast_recr_like occurring before endyr+1
 1951.3 #_last_yr_nobias_adj_in_MPD; begin of ramp
 1984 #_first_yr_fullbias_adj_in_MPD; begin of plateau
 2018.6 #_last_yr_fullbias_adj_in_MPD
 2021.4 #_end_yr_for_ramp_in_MPD (can be in forecast to shape ramp, but SS3 sets bias_adj to 0.0 for fcast yrs)
 0.8588 #_max_bias_adj_in_MPD (typical ~0.8; -3 sets all years to 0.0; -2 sets all non-forecast yrs w/ estimated recdevs to 1.0; -1 sets biasadj=1.0 for all yrs w/ recdevs)
 0 #_period of cycles in recruitment (N parms read below)
 -6 #min rec_dev
 6 #max rec_dev
 0 #_read_recdevs
#_end of advanced SR options
#
#_placeholder for full parameter lines for recruitment cycles
# read specified recr devs
#_year Input_value
#
# all recruitment deviations
#  1949E 1950E 1951E 1952E 1953E 1954E 1955E 1956E 1957E 1958E 1959E 1960E 1961E 1962E 1963E 1964E 1965E 1966E 1967E 1968E 1969E 1970R 1971R 1972R 1973R 1974R 1975R 1976R 1977R 1978R 1979R 1980R 1981R 1982R 1983R 1984R 1985R 1986R 1987R 1988R 1989R 1990R 1991R 1992R 1993R 1994R 1995R 1996R 1997R 1998R 1999R 2000R 2001R 2002R 2003R 2004R 2005R 2006R 2007R 2008R 2009R 2010R 2011R 2012R 2013R 2014R 2015R 2016R 2017R 2018R 2019R 2020R 2021R 2022R 2023R 2024R 2025F 2026F 2027F 2028F
#  -0.510471 -0.234858 -0.297074 -0.370157 -0.524536 -0.709334 -0.872217 -1.0225 -1.09112 -1.15513 -1.32143 -1.09851 -0.464115 -0.873014 -0.670151 -0.031998 -0.251038 0.43431 0.569677 0.952331 0.571399 -0.0294984 0.568856 0.0739449 -0.173497 0.176021 0.177887 0.362396 0.16508 2.17104 0.364501 1.30564 -0.486303 1.85806 -0.74979 1.51589 -0.276164 -0.74048 -1.21662 -1.64615 1.95237 0.0438564 -0.945728 1.74333 -0.102012 -0.893372 0.778187 1.4714 -0.548985 0.328806 0.811713 1.46638 0.288169 0.699975 -1.04164 -1.42561 -0.116076 1.31768 -2.25775 1.87439 0.695323 -0.555115 -0.680508 1.80254 1.55141 -0.152658 -0.712714 -0.941332 -1.18755 2.37787 -1.46315 -0.404055 -0.107206 -0.00977807 0.522617 0 0 0 0 0
#
#Fishing Mortality info 
0.1 # F ballpark value in units of annual_F
-1999 # F ballpark year (neg value to disable)
3 # F_Method:  1=Pope midseason rate; 2=F as parameter; 3=F as hybrid; 4=fleet-specific parm/hybrid (#4 is superset of #2 and #3 and is recommended)
1.5 # max F (methods 2-4) or harvest fraction (method 1)
5  # N iterations for tuning in hybrid mode; recommend 3 (faster) to 5 (more precise if many fleets)
#
#_initial_F_parms; for each fleet x season that has init_catch; nest season in fleet; count = 0
#_for unconstrained init_F, use an arbitrary initial catch and set lambda=0 for its logL
#_ LO HI INIT PRIOR PR_SD  PR_type  PHASE
#
# F rates by fleet x season
#_year:  1964 1965 1966 1967 1968 1969 1970 1971 1972 1973 1974 1975 1976 1977 1978 1979 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 2023 2024 2025 2026 2027 2028
# seas:  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
# Fishery 0.108379 0.155139 0.177252 0.394355 0.547299 0.678364 0.97201 1.26286 1.25249 1.17338 1.30676 1.36104 1.28663 1.25818 1.35448 1.17427 0.9832 0.586113 0.273746 0.158275 0.14715 0.152196 0.153007 0.106576 0.149526 0.162853 0.244952 0.303223 0.448109 0.314403 0.223567 0.207284 0.21545 0.195127 0.218816 0.219102 0.236888 0.252972 0.294601 0.328863 0.305717 0.283711 0.312024 0.326478 0.343362 0.320904 0.274327 0.31934 0.269042 0.213672 0.214874 0.282835 0.279436 0.208778 0.194342 0.222762 0.348034 0.434717 0.239128 0.212372 0.189983 1.5 1.5 1.5 1.5
#
#_Q_setup for fleets with cpue or survey or deviation data
#_1:  fleet number
#_2:  link type: 1=simple q; 2=mirror; 3=power (+1 parm); 4=mirror with scale (+1p); 5=offset (+1p); 6=offset & power (+2p)
#_     where power is applied as y = q * x ^ (1 + power); so a power value of 0 has null effect
#_     and with the offset included it is y = q * (x + offset) ^ (1 + power)
#_3:  extra input for link, i.e. mirror fleet# or dev index number
#_4:  0/1 to select extra sd parameter
#_5:  0/1 for biasadj or not
#_6:  0/1 to float
#_   fleet      link link_info  extra_se   biasadj     float  #  fleetname
         2         1         0         0         0         1  #  Acoustic_Survey
         3         1         0         0         0         1  #  bottom_Survey
         4         1         0         0         0         1  #  AVO
         5         1         0         1         0         1  #  Age1_ATS
         6         1         0         1         0         1  #  Age1_BTS
-9999 0 0 0 0 0
#
#_Q_parameters
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type      PHASE    env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn  #  parm_name
           -15            15     -0.814964             0             1             0         -1          0          0          0          0          0          0          0  #  LnQ_base_Acoustic_Survey(2)
           -15            15       0.46492             0             1             0         -1          0          0          0          0          0          0          0  #  LnQ_base_bottom_Survey(3)
           -15            15      -8.14154             0             1             0         -1          0          0          0          0          0          0          0  #  LnQ_base_AVO(4)
           -15            15      -2.23486             0             1             0         -1          0          0          0          0          0          0          0  #  LnQ_base_Age1_ATS(5)
          0.05           1.8      0.095273        0.0755           0.1             0          8          0          0          0          0          0          0          0  #  Q_extraSD_Age1_ATS(5)
           -15            15       12.8485             0             1             0         -1          0          0          0          0          0          0          0  #  LnQ_base_Age1_BTS(6)
          0.05           1.8      0.078761        0.0755           0.1             0          8          0          0          0          0          0          0          0  #  Q_extraSD_Age1_BTS(6)
#_no timevary Q parameters
#
#_size_selex_patterns
#Pattern:_0;  parm=0; selex=1.0 for all sizes
#Pattern:_1;  parm=2; logistic; with 95% width specification
#Pattern:_5;  parm=2; mirror another size selex; PARMS pick the min-max bin to mirror
#Pattern:_11; parm=2; selex=1.0  for specified min-max population length bin range
#Pattern:_15; parm=0; mirror another age or length selex
#Pattern:_6;  parm=2+special; non-parm len selex
#Pattern:_43; parm=2+special+2;  like 6, with 2 additional param for scaling (mean over bin range)
#Pattern:_8;  parm=8; double_logistic with smooth transitions and constant above Linf option
#Pattern:_9;  parm=6; simple 4-parm double logistic with starting length; parm 5 is first length; parm 6=1 does desc as offset
#Pattern:_21; parm=2*special; non-parm len selex, read as N break points, then N selex parameters
#Pattern:_22; parm=4; double_normal as in CASAL
#Pattern:_23; parm=6; double_normal where final value is directly equal to sp(6) so can be >1.0
#Pattern:_24; parm=6; double_normal with sel(minL) and sel(maxL), using joiners
#Pattern:_2;  parm=6; double_normal with sel(minL) and sel(maxL), using joiners, back compatibile version of 24 with 3.30.18 and older
#Pattern:_25; parm=3; exponential-logistic in length
#Pattern:_27; parm=special+3; cubic spline in length; parm1==1 resets knots; parm1==2 resets all 
#Pattern:_42; parm=special+3+2; cubic spline; like 27, with 2 additional param for scaling (mean over bin range)
#_discard_options:_0=none;_1=define_retention;_2=retention&mortality;_3=all_discarded_dead;_4=define_dome-shaped_retention
#_Pattern Discard Male Special
 0 0 0 0 # 1 Fishery
 0 0 0 0 # 2 Acoustic_Survey
 0 0 0 0 # 3 bottom_Survey
 0 0 0 0 # 4 AVO
 0 0 0 0 # 5 Age1_ATS
 0 0 0 0 # 6 Age1_BTS
#
#_age_selex_patterns
#Pattern:_0; parm=0; selex=1.0 for ages 0 to maxage
#Pattern:_10; parm=0; selex=1.0 for ages 1 to maxage
#Pattern:_11; parm=2; selex=1.0  for specified min-max age
#Pattern:_12; parm=2; age logistic
#Pattern:_13; parm=8; age double logistic. Recommend using pattern 18 instead.
#Pattern:_14; parm=nages+1; age empirical
#Pattern:_15; parm=0; mirror another age or length selex
#Pattern:_16; parm=2; Coleraine - Gaussian
#Pattern:_17; parm=nages+1; empirical as random walk  N parameters to read can be overridden by setting special to non-zero
#Pattern:_41; parm=2+nages+1; // like 17, with 2 additional param for scaling (mean over bin range)
#Pattern:_18; parm=8; double logistic - smooth transition
#Pattern:_19; parm=6; simple 4-parm double logistic with starting age
#Pattern:_20; parm=6; double_normal,using joiners
#Pattern:_26; parm=3; exponential-logistic in age
#Pattern:_27; parm=3+special; cubic spline in age; parm1==1 resets knots; parm1==2 resets all 
#Pattern:_42; parm=2+special+3; // cubic spline; with 2 additional param for scaling (mean over bin range)
#Age patterns entered with value >100 create Min_selage from first digit and pattern from remainder
#_Pattern Discard Male Special
 41 0 0 15 # 1 Fishery
 17 0 0 15 # 2 Acoustic_Survey
 17 0 0 15 # 3 bottom_Survey
 15 0 0 2 # 4 AVO
 11 0 0 0 # 5 Age1_ATS
 11 0 0 0 # 6 Age1_BTS
#
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type      PHASE    env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn  #  parm_name
# 1   Fishery LenSelex
# 2   Acoustic_Survey LenSelex
# 3   bottom_Survey LenSelex
# 4   AVO LenSelex
# 5   Age1_ATS LenSelex
# 6   Age1_BTS LenSelex
# 1   Fishery AgeSelex
             0            10             3            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_ScaleAgeLo
             0            18             9            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_ScaleAgeHi
         -1002             3         -1000            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P1_Acoustic_Survey(2)
         -1002             3         -1000            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P2_Acoustic_Survey(2)
#            -5            19      -4.88888            -1          0.01             0          3          0          3       1992       2023          4          0          0  #  AgeSel_P3_Fishery(1)
            -5            19      -4.88888            -1          0.01             0         -3          0          0          0          0          0          0          0  #  AgeSel_P3_Fishery(1)
            -5             9       8.88888            -1          0.01             0          2          0          3       1964       2023          4          0          0  #  AgeSel_P4_Fishery(1)
            -5             9       8.88888            -1          0.01             0          2          0          3       1964       2023          4          0          0  #  AgeSel_P5_Fishery(1)
            -5             9      0.838644            -1          0.01             0          3          0          3       1964       2023          4          0          0  #  AgeSel_P6_Fishery(1)
            -5             9      0.272751            -1          0.01             0          3          0          3       1964       2023          4          0          0  #  AgeSel_P7_Fishery(1)
            -5             9    -0.0571268            -1          0.01             0          3          0          3       1964       2023          4          0          0  #  AgeSel_P8_Fishery(1)
            -5             9     -0.172666            -1          0.01             0          3          0          3       1964       2023          4          0          0  #  AgeSel_P9_Fishery(1)
            -5             9     -0.253324            -1          0.01             0          3          0          3       1964       2023          4          0          0  #  AgeSel_P10_Fishery(1)
            -5             9     -0.253324            -1          0.01             0          3          0          3       1964       2023          4          0          0  #  AgeSel_P10_Fishery(1)
            -5             9     -0.253324            -1          0.01             0          3          0          3       1964       2023          4          0          0  #  AgeSel_P10_Fishery(1)
            -5             9             0            -1          0.01             0         -3          0          0          0          0          0          0          0  #  AgeSel_P13_Fishery(1)
            -5             9             0            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P14_Fishery(1)
            -5             9             0            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P15_Fishery(1)
            -5             9             0            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P15_Fishery(1)
# 2   Acoustic_Survey AgeSelex
         -1002             3         -1000            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P1_Acoustic_Survey(2)
         -1002             3         -1000            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P2_Acoustic_Survey(2)
            -1             1             0            -1          0.01             0         -3          0          0          0          0          0          0          0  #  AgeSel_P3_Acoustic_Survey(2)
            -5             9      0.504171            -1          0.01             0          3          0          0          0          0          0          0          0  #  AgeSel_P4_Acoustic_Survey(2)
            -5             9     -0.218817            -1          0.01             0          3          0          0          0          0          0          0          0  #  AgeSel_P5_Acoustic_Survey(2)
            -5             9      0.181692            -1          0.01             0          3          0          0          0          0          0          0          0  #  AgeSel_P6_Acoustic_Survey(2)
            -5             9    -0.0300505            -1          0.01             0          3          0          0          0          0          0          0          0  #  AgeSel_P7_Acoustic_Survey(2)
            -5             9     0.0318457            -1          0.01             0          3          0          0          0          0          0          0          0  #  AgeSel_P8_Acoustic_Survey(2)
            -5             9             0            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P9_Acoustic_Survey(2)
            -5             9             0            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P10_Acoustic_Survey(2)
            -5             9             0            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P11_Acoustic_Survey(2)
            -5             9             0            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P12_Acoustic_Survey(2)
            -5             9             0            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P13_Acoustic_Survey(2)
            -5             9             0            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P14_Acoustic_Survey(2)
            -5             9             0            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P15_Acoustic_Survey(2)
            -5             9             0            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P16_Acoustic_Survey(2)
# 3   bottom_Survey AgeSelex
         -1002             3         -1000            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P1_bottom_Survey(3)
         -1002             3         -1000            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P2_bottom_Survey(3)
            -1             1             0            -1          0.01             0          3          0          0          0          0          0          0          0  #  AgeSel_P3_bottom_Survey(3)
            -5             9       1.03542            -1          0.01             0          3          0          0          0          0          0          0          0  #  AgeSel_P4_bottom_Survey(3)
            -5             9       1.04948            -1          0.01             0          3          0          0          0          0          0          0          0  #  AgeSel_P5_bottom_Survey(3)
            -5             9      0.702233            -1          0.01             0          3          0          0          0          0          0          0          0  #  AgeSel_P6_bottom_Survey(3)
            -5             9      0.543097            -1          0.01             0          3          0          0          0          0          0          0          0  #  AgeSel_P7_bottom_Survey(3)
            -5             9      0.160803            -1          0.01             0          3          0          0          0          0          0          0          0  #  AgeSel_P8_bottom_Survey(3)
            -5             9      0.147589            -1          0.01             0          3          0          0          0          0          0          0          0  #  AgeSel_P9_bottom_Survey(3)
            -5             9     -0.104979            -1          0.01             0          3          0          0          0          0          0          0          0  #  AgeSel_P10_bottom_Survey(3)
            -5             9             0            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P11_bottom_Survey(3)
            -5             9             0            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P12_bottom_Survey(3)
            -5             9             0            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P13_bottom_Survey(3)
            -5             9             0            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P14_bottom_Survey(3)
            -5             9             0            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P15_bottom_Survey(3)
            -5             9             0            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P16_bottom_Survey(3)
# 4   AVO AgeSelex
# 5   Age1_ATS AgeSelex
             1             1             1            -1          0.01             0        -99          0          0          0          0          0          0          0  #  minage@sel=1_Age1_ATS(5)
             1             1             1            -1          0.01             0        -99          0          0          0          0          0          0          0  #  maxage@sel=1_Age1_ATS(5)
# 6   Age1_BTS AgeSelex
             1             1             1            -1          0.01             0        -99          0          0          0          0          0          0          0  #  minage@sel=1_Age1_BTS(6)
             1             1             1            -1          0.01             0        -99          0          0          0          0          0          0          0  #  maxage@sel=1_Age1_BTS(6)
#_No_Dirichlet parameters
# timevary selex parameters 
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type    PHASE  #  parm_name
        0.0001             2          0.20           0.5           0.5            -1      -5  # AgeSel_P3_Fishery(1)_dev_se
         -0.99          0.99             0             0           0.5            -1      -6  # AgeSel_P3_Fishery(1)_dev_autocorr
        0.0001             2          0.20           0.5           0.5            -1      -5  # AgeSel_P4_Fishery(1)_dev_se
         -0.99          0.99             0             0           0.5            -1      -6  # AgeSel_P4_Fishery(1)_dev_autocorr
        0.0001             2          0.20           0.5           0.5            -1      -5  # AgeSel_P5_Fishery(1)_dev_se
         -0.99          0.99             0             0           0.5            -1      -6  # AgeSel_P5_Fishery(1)_dev_autocorr
        0.0001             2          0.20           0.5           0.5            -1      -5  # AgeSel_P6_Fishery(1)_dev_se
         -0.99          0.99             0             0           0.5            -1      -6  # AgeSel_P6_Fishery(1)_dev_autocorr
        0.0001             2          0.20           0.5           0.5            -1      -5  # AgeSel_P7_Fishery(1)_dev_se
         -0.99          0.99             0             0           0.5            -1      -6  # AgeSel_P7_Fishery(1)_dev_autocorr
        0.0001             2          0.20           0.5           0.5            -1      -5  # AgeSel_P8_Fishery(1)_dev_se
         -0.99          0.99             0             0           0.5            -1      -6  # AgeSel_P8_Fishery(1)_dev_autocorr
        0.0001             2          0.20           0.5           0.5            -1      -5  # AgeSel_P9_Fishery(1)_dev_se
         -0.99          0.99             0             0           0.5            -1      -6  # AgeSel_P9_Fishery(1)_dev_autocorr
        0.0001             2          0.30           0.5           0.5            -1      -5  # AgeSel_P10_Fishery(1)_dev_se
         -0.99          0.99             0             0           0.5            -1      -6  # AgeSel_P10_Fishery(1)_dev_autocorr
        0.0001             2          0.30           0.5           0.5            -1      -5  # AgeSel_P11_Fishery(1)_dev_se
         -0.99          0.99             0             0           0.5            -1      -6  # AgeSel_P11_Fishery(1)_dev_autocorr
# info on dev vectors created for selex parms are reported with other devs after tag parameter section 
#
0   #  use 2D_AR1 selectivity? (0/1)
#_no 2D_AR1 selex offset used
#_specs:  fleet, ymin, ymax, amin, amax, sigma_amax, use_rho, len1/age2, devphase, before_range, after_range
#_sigma_amax>amin means create sigma parm for each bin from min to sigma_amax; sigma_amax<0 means just one sigma parm is read and used for all bins
#_needed parameters follow each fleet's specifications
# -9999  0 0 0 0 0 0 0 0 0 0 # terminator
#
# Tag loss and Tag reporting parameters go next
0  # TG_custom:  0=no read and autogen if tag data exist; 1=read
#_Cond -6 6 1 1 2 0.01 -4 0 0 0 0 0 0 0  #_placeholder if no parameters
#
# deviation vectors for timevary parameters
#  base   base first block   block  env  env   dev   dev   dev   dev   dev
#  type  index  parm trend pattern link  var  vectr link _mnyr  mxyr phase  dev_vector
#      5     2     1     0     0     0     0     1     2  1964  2023     5 -5.32907e-15 -1.06581e-14 3.90799e-14 -1.77636e-15 2.66454e-14 -4.26326e-14 2.66454e-14 4.61853e-14 2.13163e-14 -4.26326e-14 1.77636e-15 3.19744e-14 1.75859e-13 -5.68434e-14 6.39488e-14 1.06581e-14 1.95399e-14      0 1.95399e-14 -5.86198e-14 7.28306e-14 -6.39488e-14 2.84217e-14 -8.17124e-14 -9.9476e-14 -3.55271e-15 -3.37508e-14 -1.08358e-13 -3.55271e-14 -1.13687e-13 1.42109e-14 -1.95399e-14 5.15143e-14 6.75016e-14 1.77636e-14 1.77636e-14 3.90799e-14 -1.95399e-14 -5.50671e-14 -1.77636e-14 8.17124e-14 -4.44089e-14 1.77636e-15 -7.81597e-14 7.10543e-15 1.06581e-14 -1.36779e-13 8.88178e-15 8.88178e-15 1.95399e-14 -3.90799e-14 -9.23706e-14      0 3.55271e-15 2.66454e-14 -1.06581e-14 9.59233e-14 -6.39488e-14 -1.77636e-15 1.61648e-13
#      5     3     3     0     0     0     0     2     2  1964  2023     5 -4.69964e-09 -3.59256e-09 -5.77311e-09 -2.85061e-09 -9.31195e-09 -2.1426e-09 -2.55563e-08 -1.58468e-08 -8.29169e-09 -1.17807e-09 -4.77098e-09 -1.85319e-09 6.58247e-10 -9.38746e-09 -5.41342e-09 -2.11009e-07 4.42316e-09 1.08431e-08 5.02226e-10 -6.28088e-09 2.07969e-10 1.22978e-09 1.06368e-11 2.719e-10 9.43334e-11 3.24167e-11 -1.23425e-08 8.5404e-09 5.20015e-09 7.04845e-08 6.91348e-09 3.50454e-09 6.20713e-09 -3.07062e-08 5.7564e-09 2.46096e-08 2.845e-08 4.73564e-08 2.17074e-08 3.76121e-08 5.06092e-09 3.14858e-09 7.27183e-09 9.08139e-09 3.8595e-09 2.40316e-07 6.49781e-08 1.87788e-08 8.33502e-09 5.28868e-09 7.10408e-08 3.24038e-08 1.19832e-08 3.88947e-09 2.54876e-09 -1.73128e-08 2.47504e-09 3.46272e-08 7.066e-09 3.6849e-09
#      5     4     5     0     0     0     0     3     2  1964  2023     5 -0.00487199 -0.00143115 -0.0120344 -0.00555672 -0.012519 -0.0036621 -0.0310391 -0.0157699 -0.00555554 -0.0044268 -0.0175698 -0.00671401 -0.00484703 -0.0129367 -0.0302817 -0.0380646 -0.0436014 0.00475491 0.00686062 -0.00190255 0.00116541 -0.00017262 -0.00606664 0.000313583 -0.000226147 1.6892e-05 -0.00165142 -0.0978445 0.0118497 0.0093211 0.0407378 0.0152865 0.00152753 -0.116646 0.0151452 0.0122979 0.023719 0.0440338 0.0649627 0.0319568 0.0583866 0.0103831 0.00313861 -0.0140755 0.130119 0.00583616 0.299915 0.107956 0.0163855 0.0128824 0.031517 0.160615 0.0433784 0.0136507 0.00975182 -0.00221594 -0.688913 0.00882287 -0.00705899 -0.00898489
#      5     5     7     0     0     0     0     4     2  1964  2023     5 -0.0292668 -0.013807 -0.0296391 -0.0631671 -0.0298096 -0.039845 -0.0680914 -0.0288024 -0.034954 -0.0109226 -0.0939165 -0.07157 -0.0348094 -0.0518785 -0.148525 -0.143942 -0.102051 -0.0404278 0.0179326 -0.0334847 0.00175149 -0.0472612 -0.00804776 0.00174004 -0.0140785 -0.000123549 -0.0138638 -0.11075 -0.21094 0.0745155 0.0566488 0.469152 0.0553949 -0.167355 0.00619158 -0.207182 0.0752701 0.183158 0.172316 0.0176267 0.212049 0.328581 0.0280538 -0.0428237 0.165349 0.322605 0.32003 1.59739 0.244408 0.0966135 0.0535479 -1.39039 0.847398 0.166298 0.106222 0.052762 -0.77214 -1.58619 -0.00482858 -0.0273207
#      5     6     9     0     0     0     0     5     2  1964  2023     5 -0.0430409 -0.071071 -0.0525478 -0.0862328 -0.0574412 -0.0592715 -0.0550182 -0.0282429 -0.0479664 -0.012736 -0.0937079 -0.193207 -0.0611408 -0.0584382 -0.23677 -0.115093 -0.119482 -0.0692676 -0.0225333 -0.0344778 -0.00937057 -0.0461015 -0.0636081 0.00438667 -0.133828 -0.000178159 -0.039691 -0.120603 -0.208597 -0.143231 0.113659 0.49355 1.19161 -0.0514264 0.0433509 -0.371276 0.350161 0.279326 0.232464 -0.0330967 0.0874244 0.494141 0.178572 -0.0351094 0.155088 0.152404 -0.172489 1.62573 0.0310504 0.309783 0.100416 -1.56961 -0.951222 0.943194 0.356925 0.391399 -0.798335 -1.51056 0.261477 -0.0169448
#      5     7    11     0     0     0     0     6     2  1964  2023     5 -0.0491711 -0.0806652 -0.0671361 -0.0769611 -0.0533997 -0.0403513 -0.0318129 -0.0191022 -0.0277823 -0.0025339 -0.0628769 -0.223146 -0.0336627 -0.0423073 -0.234946 -0.0381072 -0.0465263 -0.0587957 -0.0277611 -0.0143838 -0.014483 0.00903097 -0.0644486 0.0687125 -0.137031 0.0094999 -0.0624471 -0.0645204 -0.145455 -0.111658 -0.0513666 0.465761 1.23102 0.921986 0.218236 -0.28674 0.536076 0.428352 0.259686 -0.0699432 0.0336144 0.0126392 0.11254 0.0625835 0.174371 0.104494 -0.148961 0.350432 0.0362213 -0.0356518 0.163294 -1.50388 -0.959841 0.384046 -0.110529 0.930984 -0.708291 -1.19099 0.318366 0.0695777
#      5     8    13     0     0     0     0     7     2  1964  2023     5 -0.0576202 -0.0678102 -0.0533063 -0.0486447 -0.0407019 -0.0258921 -0.0134365 -0.00705945 -0.00768008 0.000794656 -0.00299551 0.00163666 -0.0110956 -0.0141642 -0.0171554 -0.0118247 -0.00776194 -0.0186769 -0.0136708 -0.00632533 0.0044271 0.0168396 -0.0526497 0.074662 -0.0955623 0.00580909 -0.0690279 0.0393803 -0.0482171 -0.0833783 -0.0476508 0.036033 1.09899 0.88273 0.467912 -0.153727 0.575811 0.29585 0.134909 -0.0836534 0.0798753 -0.107299 -0.0579391 -0.0504101 0.112598 0.0795069 -0.0784959 0.205894 0.0160736 -0.0255732 -0.0980526 -1.15581 -0.834868 0.325374 -0.100083 0.38941 -0.75013 -0.935392 0.303618 0.106585
#      5     9    15     0     0     0     0     8     2  1964  2023     5 -0.0555081 -0.0525913 -0.0421039 -0.0358637 -0.025936 -0.0189692 -0.00871102 -0.00304765 -0.00346073 0.000584193 0.000354502 0.000822742 -0.00346485 -0.00714704 -0.00278402 -0.00158952 8.53669e-06 -0.00344276 -0.00239841 -0.000456313 0.00523207 0.0118146 -0.0473016 0.0884576 -0.0903893 -0.028964 -0.0708758 0.327891 0.0165745 -0.0833323 -0.0433119 0.0343021 0.0965686 0.695903 0.448129 -0.175963 0.489737 0.270859 0.121248 -0.126648 0.0754695 -0.0721723 -0.0368042 -0.0609721 0.0784554 0.0909013 -0.0357792 0.202952 0.0358591 0.146777 -0.0962708 -0.334689 -0.593772 0.251012 -0.0776061 -0.00986448 -0.680574 -0.852689 0.176701 0.119926
#      5    10    17     0     0     0     0     9     2  1964  2023     5 -0.045102 -0.0401869 -0.0322923 -0.0286439 -0.0200696 -0.0124446 -0.00672803 -0.00206938 -0.00122694 9.88406e-05 0.000421412 0.000732801 -0.000560706 -0.00245676 -0.000309197 0.000937238 0.00151358 -0.000895909 -0.000460486 0.000709126 0.00252672 0.00920987 -0.00631672 0.0783802 -0.0543904 -0.0275818 -0.0573493 0.342839 0.110514 -0.0867982 -0.0356733 0.0344398 0.0811654 0.0655565 0.359703 -0.174026 -0.104546 0.223144 0.111481 -0.0987038 0.0174401 -0.0625732 -0.0221845 0.00361298 0.0944403 0.101913 -0.00736353 0.187469 0.0419093 0.14227 0.0127236 -0.321572 -0.187274 0.0988589 -0.0899292 -0.0118707 -0.0838111 -0.609278 0.0178146 0.0932466
     #
# Input variance adjustments factors: 
 #_1=add_to_survey_CV
 #_2=add_to_discard_stddev
 #_3=add_to_bodywt_CV
 #_4=mult_by_lencomp_N
 #_5=mult_by_agecomp_N
 #_6=mult_by_size-at-age_N
 #_7=mult_by_generalized_sizecomp
#_factor  fleet  value
 -9999   1    0  # terminator
#
1 #_maxlambdaphase
1 #_sd_offset; must be 1 if any growthCV, sigmaR, or survey extraSD is an estimated parameter
# read 0 changes to default Lambdas (default value is 1.0)
# Like_comp codes:  1=surv; 2=disc; 3=mnwt; 4=length; 5=age; 6=SizeFreq; 7=sizeage; 8=catch; 9=init_equ_catch; 
# 10=recrdev; 11=parm_prior; 12=parm_dev; 13=CrashPen; 14=Morphcomp; 15=Tag-comp; 16=Tag-negbin; 17=F_ballpark; 18=initEQregime
#like_comp fleet  phase  value  sizefreq_method
-9999  1  1  1  1  #  terminator
#
# lambdas (for info only; columns are phases)
#  0 #_CPUE/survey:_1
#  1 #_CPUE/survey:_2
#  1 #_CPUE/survey:_3
#  1 #_CPUE/survey:_4
#  1 #_CPUE/survey:_5
#  1 #_CPUE/survey:_6
#  1 #_agecomp:_1
#  1 #_agecomp:_2
#  1 #_agecomp:_3
#  0 #_agecomp:_4
#  0 #_agecomp:_5
#  0 #_agecomp:_6
#  1 #_init_equ_catch1
#  1 #_init_equ_catch2
#  1 #_init_equ_catch3
#  1 #_init_equ_catch4
#  1 #_init_equ_catch5
#  1 #_init_equ_catch6
#  1 #_recruitments
#  1 #_parameter-priors
#  1 #_parameter-dev-vectors
#  1 #_crashPenLambda
#  0 # F_ballpark_lambda
2 # (0/1/2) read specs for more stddev reporting: 0 = skip, 1 = read specs for reporting stdev for selectivity, size, and numbers, 2 = add options for M,Dyn. Bzero, SmryBio
 2 2 -1 3 # Selectivity: (1) 0 to skip or fleet, (2) 1=len/2=age/3=combined, (3) year, (4) N selex bins; NOTE: combined reports in age bins
 0 0 # Growth: (1) 0 to skip or growth pattern, (2) growth ages; NOTE: does each sex
 1 -1 1 # Numbers-at-age: (1) 0 or area(-1 for all), (2) year, (3) N ages;  NOTE: sums across morphs
 0 0 # Mortality: (1) 0 to skip or growth pattern, (2) N ages for mortality; NOTE: does each sex
2 # Dyn Bzero: 0 to skip, 1 to include, or 2 to add recr
0 # SmryBio: 0 to skip, 1 to include
 3 4 5 # vector with selex std bins (-1 in first bin to self-generate)
 # -1 # list of ages for growth std (-1 in first bin to self-generate)
 15 # vector with NatAge std ages (-1 in first bin to self-generate)
 # -1 # list of ages for NatM std (-1 in first bin to self-generate)
999

