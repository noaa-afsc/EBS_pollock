#V3.30.xx.yy;_safe;_compile_date:_Nov  2 2023;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_13.1
#_Stock_Synthesis_is_a_work_of_the_U.S._Government_and_is_not_subject_to_copyright_protection_in_the_United_States.
#_Foreign_copyrights_may_apply._See_copyright.txt_for_more_information.
#_User_support_available_at:NMFS.Stock.Synthesis@noaa.gov
#_User_info_available_at:https://vlab.noaa.gov/group/stock-synthesis
#_Source_code_at:_https://github.com/nmfs-stock-synthesis/stock-synthesis

#C 2019 Hake control file
#_data_and_control_files: hake_data.ss // hake_control.ss
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
0 #_natM_type:_0=1Parm; 1=N_breakpoints;_2=Lorenzen;_3=agespecific;_4=agespec_withseasinterpolate;_5=BETA:_Maunder_link_to_maturity;_6=Lorenzen_range
  #_no additional input for selected M option; read 1P per morph
#
1 # GrowthModel: 1=vonBert with L1&L2; 2=Richards with L1&L2; 3=age_specific_K_incr; 4=age_specific_K_decr; 5=age_specific_K_each; 6=NA; 7=NA; 8=growth cessation
1 #_Age(post-settlement) for L1 (aka Amin); first growth parameter is size at this age; linear growth below this
20 #_Age(post-settlement) for L2 (aka Amax); 999 to treat as Linf
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
 0.05 0.4 0.199013 -1.60944 0.1 3 4 0 0 0 0 0 0 0 # NatM_uniform_Fem_GP_1
# Sex: 1  BioPattern: 1  Growth
 2 15 10 32 99 0 -5 0 0 0 0 0 0 0 # L_at_Amin_Fem_GP_1
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
            13            17       14.5848            15            99             0          1          0          0          0          0          0          0          0 # SR_LN(R0)
           0.2             1      0.865216         0.777         0.113             2          4          0          0          0          0          0          0          0 # SR_BH_steep
             1           1.6           1.4           1.1            99             0         -6          0          0          0          0          0          0          0 # SR_sigmaR
            -5             5             0             0            99             0        -50          0          0          0          0          0          0          0 # SR_regime
             0             2             0             1            99             0        -50          0          0          0          0          0          0          0 # SR_autocorr
#_no timevary SR parameters
2 #do_recdev:  0=none; 1=devvector (R=F(SSB)+dev); 2=deviations (R=F(SSB)+dev); 3=deviations (R=R0*dev; dev2=R-f(SSB)); 4=like 3 with sum(dev2) adding penalty
1970 # first year of main recr_devs; early devs can precede this era
2022 # last year of main recr_devs; forecast devs start in following year
1 #_recdev phase 
1 # (0/1) to read 13 advanced options
 1946 #_recdev_early_start (0=none; neg value makes relative to recdev_start)
 3 #_recdev_early_phase
 5 #_forecast_recruitment phase (incl. late recr) (0 value resets to maxphase+1)
 1 #_lambda for Fcast_recr_like occurring before endyr+1
 1965 #_last_yr_nobias_adj_in_MPD; begin of ramp
 1971 #_first_yr_fullbias_adj_in_MPD; begin of plateau
 2019 #_last_yr_fullbias_adj_in_MPD
 2021 #_end_yr_for_ramp_in_MPD (can be in forecast to shape ramp, but SS3 sets bias_adj to 0.0 for fcast yrs)
 -1 #_max_bias_adj_in_MPD (typical ~0.8; -3 sets all years to 0.0; -2 sets all non-forecast yrs w/ estimated recdevs to 1.0; -1 sets biasadj=1.0 for all yrs w/ recdevs)
 0 #_period of cycles in recruitment (N parms read below)
 -6 #min rec_dev
 6 #max rec_dev
 0 #_read_recdevs
#_end of advanced SR options
#
#_placeholder for full parameter lines for recruitment cycles
# read specified recr devs
#_Yr Input_value
#
# all recruitment deviations
#  1946E 1947E 1948E 1949E 1950E 1951E 1952E 1953E 1954E 1955E 1956E 1957E 1958E 1959E 1960E 1961E 1962E 1963E 1964E 1965E 1966E 1967E 1968E 1969E 1970R 1971R 1972R 1973R 1974R 1975R 1976R 1977R 1978R 1979R 1980R 1981R 1982R 1983R 1984R 1985R 1986R 1987R 1988R 1989R 1990R 1991R 1992R 1993R 1994R 1995R 1996R 1997R 1998R 1999R 2000R 2001R 2002R 2003R 2004R 2005R 2006R 2007R 2008R 2009R 2010R 2011R 2012R 2013R 2014R 2015R 2016R 2017R 2018R 2019R 2020R 2021R 2022R 2023F 2024F 2025F 2026F 2027F
#  -0.30638 -0.0842452 -0.10108 -0.120915 -0.144151 -0.171199 -0.200605 -0.23305 -0.270482 -0.311562 -0.354901 -0.399496 -0.440402 -0.466537 -0.458108 -0.374484 -0.176953 0.0462059 0.303306 0.761842 0.417741 1.40299 0.923877 -0.200072 1.90617 -0.238677 -0.758393 1.47529 -1.05657 0.30881 -1.5827 1.64343 -1.98697 0.123949 2.68975 -1.37203 -1.34778 -0.770718 2.49053 -2.01555 -1.67691 1.74704 0.697539 -1.90878 1.35377 0.192922 -1.8685 1.11944 1.15696 0.275784 0.59495 -0.0304116 0.654935 2.5325 -0.953306 0.1495 -3.10058 0.454618 -3.09334 0.977308 0.729547 -3.53521 1.71368 0.318233 2.70529 -0.847241 0.349782 -0.982123 1.9944 -3.24806 1.62556 0.274458 -0.947069 -1.31558 1.38513 2.08257 0.700675 0 0 0 0 0
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
# Yr:  1966 1967 1968 1969 1970 1971 1972 1973 1974 1975 1976 1977 1978 1979 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 2023 2024 2025 2026 2027
# seas:  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
# Fishery 0.160916 0.277768 0.163979 0.228548 0.273933 0.166264 0.112007 0.12649 0.158683 0.130021 0.11177 0.0689651 0.0634545 0.0733908 0.0592862 0.0941376 0.0747827 0.0606299 0.0676457 0.048643 0.0796392 0.0996826 0.104421 0.139977 0.111292 0.183502 0.159813 0.132953 0.268137 0.212311 0.277732 0.276392 0.351895 0.377452 0.280618 0.233671 0.161986 0.132433 0.299829 0.327217 0.321048 0.375247 0.468799 0.23612 0.440148 0.374909 0.215677 0.229591 0.181033 0.106563 0.183932 0.249649 0.229668 0.279068 0.267186 0.285891 0.31547 0.265295 0.380731 0.380961 0.401765 0.444124
#
#_Q_setup for fleets with cpue or survey data
#_1:  fleet number
#_2:  link type: (1=simple q, 1 parm; 2=mirror simple q, 1 mirrored parm; 3=q and power, 2 parm; 4=mirror with offset, 2 parm)
#_3:  extra input for link, i.e. mirror fleet# or dev index number
#_4:  0/1 to select extra sd parameter
#_5:  0/1 for biasadj or not
#_6:  0/1 to float
#_   fleet      link link_info  extra_se   biasadj     float  #  fleetname
         2         1         0         1         0         1  #  Acoustic_Survey
         3         1         0         1         0         1  #  Age1_Survey
-9999 0 0 0 0 0
#
#_Q_parms(if_any);Qunits_are_ln(q)
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type      PHASE    env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn  #  parm_name
           -15            15     0.0767414             0             1             0         -1          0          0          0          0          0          0          0  #  LnQ_base_Acoustic_Survey(2)
          0.05           1.2      0.265396        0.0755           0.1             0          5          0          0          0          0          0          0          0  #  Q_extraSD_Acoustic_Survey(2)
           -15            15     -0.479903             0             1             0         -1          0          0          0          0          0          0          0  #  LnQ_base_Age1_Survey(3)
          0.05           1.2      0.264275        0.0755           0.1             0          5          0          0          0          0          0          0          0  #  Q_extraSD_Age1_Survey(3)
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
#Pattern:_21; parm=2+special; non-parm len selex, read as pairs of size, then selex
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
 0 0 0 0 # 3 Age1_Survey
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
 17 0 0 20 # 1 Fishery
 17 0 0 20 # 2 Acoustic_Survey
 11 0 0 0 # 3 Age1_Survey
#
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type      PHASE    env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn  #  parm_name
# 1   Fishery LenSelex
# 2   Acoustic_Survey LenSelex
# 3   Age1_Survey LenSelex
# 1   Fishery AgeSelex
         -1002             3         -1000            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P1_Fishery(1)
            -1             1             0            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P2_Fishery(1)
            -5             9       2.70125            -1          0.01             0          2          0          2       1981       2023          5          0          0  #  AgeSel_P3_Fishery(1)
            -5             9      0.862877            -1          0.01             0          2          0          2       1981       2023          5          0          0  #  AgeSel_P4_Fishery(1)
            -5             9      0.398521            -1          0.01             0          2          0          2       1981       2023          5          0          0  #  AgeSel_P5_Fishery(1)
            -5             9      0.210481            -1          0.01             0          2          0          2       1981       2023          5          0          0  #  AgeSel_P6_Fishery(1)
            -5             9      0.475195            -1          0.01             0          2          0          2       1981       2023          5          0          0  #  AgeSel_P7_Fishery(1)
            -5             9      0.475195            -1          0.01             0          2          0          2       1981       2023          5          0          0  #  AgeSel_P7_Fishery(1)
            -5             9      0.475195            -1          0.01             0          2          0          2       1981       2023          5          0          0  #  AgeSel_P7_Fishery(1)
            -5             9      0.475195            -1          0.01             0          2          0          2       1981       2023          5          0          0  #  AgeSel_P7_Fishery(1)
            -5             9      0.475195            -1          0.01             0          2          0          2       1981       2023          5          0          0  #  AgeSel_P7_Fishery(1)
            -5             9      0.475195            -1          0.01             0          2          0          2       1981       2023          5          0          0  #  AgeSel_P7_Fishery(1)
            -5             9             0            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P13_Fishery(1)
            -5             9             0            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P14_Fishery(1)
            -5             9             0            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P15_Fishery(1)
            -5             9             0            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P16_Fishery(1)
            -5             9             0            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P17_Fishery(1)
            -5             9             0            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P18_Fishery(1)
            -5             9             0            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P19_Fishery(1)
            -5             9             0            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P20_Fishery(1)
            -5             9             0            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P21_Fishery(1)
# 2   Acoustic_Survey AgeSelex
         -1002             3         -1000            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P1_Acoustic_Survey(2)
         -1002             3         -1000            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P2_Acoustic_Survey(2)
            -1             1             0            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P3_Acoustic_Survey(2)
            -5             9      0.478726            -1          0.01             0          2          0          0          0          0          0          0          0  #  AgeSel_P4_Acoustic_Survey(2)
            -5             9     -0.150167            -1          0.01             0          2          0          0          0          0          0          0          0  #  AgeSel_P5_Acoustic_Survey(2)
            -5             9      0.292865            -1          0.01             0          2          0          0          0          0          0          0          0  #  AgeSel_P6_Acoustic_Survey(2)
            -5             9      0.314357            -1          0.01             0          2          0          0          0          0          0          0          0  #  AgeSel_P7_Acoustic_Survey(2)
            -5             9             0            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P8_Acoustic_Survey(2)
            -5             9             0            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P9_Acoustic_Survey(2)
            -5             9             0            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P10_Acoustic_Survey(2)
            -5             9             0            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P11_Acoustic_Survey(2)
            -5             9             0            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P12_Acoustic_Survey(2)
            -5             9             0            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P13_Acoustic_Survey(2)
            -5             9             0            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P14_Acoustic_Survey(2)
            -5             9             0            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P15_Acoustic_Survey(2)
            -5             9             0            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P16_Acoustic_Survey(2)
            -5             9             0            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P17_Acoustic_Survey(2)
            -5             9             0            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P18_Acoustic_Survey(2)
            -5             9             0            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P19_Acoustic_Survey(2)
            -5             9             0            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P20_Acoustic_Survey(2)
            -5             9             0            -1          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P21_Acoustic_Survey(2)
# 3   Age1_Survey AgeSelex
             1             1             1            -1          0.01             0        -99          0          0          0          0          0          0          0  #  minage@sel=1_Age1_Survey(3)
             1             1             1            -1          0.01             0        -99          0          0          0          0          0          0          0  #  maxage@sel=1_Age1_Survey(3)
#_Dirichlet and/or MV Tweedie parameters for composition error
#_multiple_fleets_can_refer_to_same_parm;_but_list_cannot_have_gaps
            -5            20     -0.296221             0         1.813             6          5          0          0          0          0          0          0          0  #  ln(DM_theta)_Age_P1
            -5            20       2.73498             0         1.813             6          5          0          0          0          0          0          0          0  #  ln(DM_theta)_Age_P2
# timevary selex parameters 
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type    PHASE  #  parm_name
        0.0001             2           1.4           0.5           0.5            -1      -5  # AgeSel_P3_Fishery(1)_dev_se
         -0.99          0.99             0             0           0.5            -1      -6  # AgeSel_P3_Fishery(1)_dev_autocorr
        0.0001             2           1.4           0.5           0.5            -1      -5  # AgeSel_P4_Fishery(1)_dev_se
         -0.99          0.99             0             0           0.5            -1      -6  # AgeSel_P4_Fishery(1)_dev_autocorr
        0.0001             2           1.4           0.5           0.5            -1      -5  # AgeSel_P5_Fishery(1)_dev_se
         -0.99          0.99             0             0           0.5            -1      -6  # AgeSel_P5_Fishery(1)_dev_autocorr
        0.0001             2           1.4           0.5           0.5            -1      -5  # AgeSel_P6_Fishery(1)_dev_se
         -0.99          0.99             0             0           0.5            -1      -6  # AgeSel_P6_Fishery(1)_dev_autocorr
        0.0001             2           1.4           0.5           0.5            -1      -5  # AgeSel_P7_Fishery(1)_dev_se
         -0.99          0.99             0             0           0.5            -1      -6  # AgeSel_P7_Fishery(1)_dev_autocorr
        0.0001             2           1.4           0.5           0.5            -1      -5  # AgeSel_P7_Fishery(1)_dev_se
         -0.99          0.99             0             0           0.5            -1      -6  # AgeSel_P7_Fishery(1)_dev_autocorr
        0.0001             2           1.4           0.5           0.5            -1      -5  # AgeSel_P7_Fishery(1)_dev_se
         -0.99          0.99             0             0           0.5            -1      -6  # AgeSel_P7_Fishery(1)_dev_autocorr
        0.0001             2           1.4           0.5           0.5            -1      -5  # AgeSel_P7_Fishery(1)_dev_se
         -0.99          0.99             0             0           0.5            -1      -6  # AgeSel_P7_Fishery(1)_dev_autocorr
        0.0001             2           1.4           0.5           0.5            -1      -5  # AgeSel_P7_Fishery(1)_dev_se
         -0.99          0.99             0             0           0.5            -1      -6  # AgeSel_P7_Fishery(1)_dev_autocorr
        0.0001             2           1.4           0.5           0.5            -1      -5  # AgeSel_P7_Fishery(1)_dev_se
         -0.99          0.99             0             0           0.5            -1      -6  # AgeSel_P7_Fishery(1)_dev_autocorr
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
#      5     3     1     0     0     0     0     1     2  1991  2023     5 0.552909 -0.0387276 -0.0176655 0.0279187 -0.315957 0.428044 0.023391 0.164606 1.01773 0.487855 0.012163 -0.012153 -0.00444109 0.206725 -0.00309113 0.612506 0.541318 -0.022986 0.399309 0.902948 -0.00727384 0.0120296 0.113003 0.283186 -0.676733 -0.0935275 -0.205282 -1.48031 0.788825 -0.0273577 -0.55387 1.83639 0.78215
#      5     4     3     0     0     0     0     2     2  1991  2023     5 0.247634 0.790429 0.780227 0.0689494 0.210635 -0.381995 1.26396 0.913754 -0.169483 0.810408 0.951042 0.675858 0.646384 0.349012 0.591804 -0.250856 0.37404 0.480021 0.704503 0.153211 1.02791 0.26312 0.732034 0.432432 0.238802 -0.916732 -0.578567 -0.499389 -0.855486 0.884586 -0.00566608 -1.36885 -0.151249
#      5     5     5     0     0     0     0     3     2  1991  2023     5 -0.831257 -0.152325 0.0636783 0.811471 0.236733 -0.266526 -0.147649 -0.595266 0.0870851 -0.224635 0.350407 0.533031 0.604638 0.640659 0.649617 0.0130047 -0.216871 -0.319867 -0.259302 0.494402 -0.715681 0.160633 -0.174732 -0.484676 -0.00225012 0.00775764 -0.135087 -0.210152 -0.0545984 0.509605 0.690135 0.182423 -0.863804
#      5     6     7     0     0     0     0     4     2  1991  2023     5 -0.0241622 -0.501586 -0.0547246 -0.126321 0.690759 -0.0917383 -0.378651 0.349718 -0.37172 0.176976 -0.219713 0.21354 0.273585 -0.62194 0.385118 0.150998 -0.195461 0.238144 -0.330347 -0.345974 -0.2043 -0.443892 -0.0622986 0.071913 -0.106418 0.0790779 -0.185985 -0.31826 0.105101 -0.325763 0.0986877 0.619393 0.242484
#      5     7     9     0     0     0     0     5     2  1991  2023     5 -0.173297 0.0818674 -0.445854 0.0968988 -0.133273 0.348925 0.160025 -0.479252 -0.241834 -0.0652344 -0.234352 -0.492266 -0.261686 -0.129909 -0.532769 -0.326568 0.0140084 -0.1128 -0.0212155 -0.746239 -0.46419 -0.313657 0.142592 -0.101377 -0.427838 -0.511343 -0.0516003 0.209892 -0.17957 -0.0533288 -0.274427 0.0196191 0.579345
     #
# Input variance adjustments factors: 
 #_1=add_to_survey_CV
 #_2=add_to_discard_stddev
 #_3=add_to_bodywt_CV
 #_4=mult_by_lencomp_N
 #_5=mult_by_agecomp_N
 #_6=mult_by_size-at-age_N
 #_7=mult_by_generalized_sizecomp
#_Factor  Fleet  Value
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
#  1 #_agecomp:_1
#  1 #_agecomp:_2
#  0 #_agecomp:_3
#  1 #_init_equ_catch1
#  1 #_init_equ_catch2
#  1 #_init_equ_catch3
#  1 #_recruitments
#  1 #_parameter-priors
#  1 #_parameter-dev-vectors
#  1 #_crashPenLambda
#  0 # F_ballpark_lambda
2 # (0/1/2) read specs for more stddev reporting: 0 = skip, 1 = read specs for reporting stdev for selectivity, size, and numbers, 2 = add options for M,Dyn. Bzero, SmryBio
 2 2 -1 4 # Selectivity: (1) 0 to skip or fleet, (2) 1=len/2=age/3=combined, (3) year, (4) N selex bins; NOTE: combined reports in age bins
 0 0 # Growth: (1) 0 to skip or growth pattern, (2) growth ages; NOTE: does each sex
 1 -1 1 # Numbers-at-age: (1) 0 or area(-1 for all), (2) year, (3) N ages;  NOTE: sums across morphs
 0 0 # Mortality: (1) 0 to skip or growth pattern, (2) N ages for mortality; NOTE: does each sex
2 # Dyn Bzero: 0 to skip, 1 to include, or 2 to add recr
0 # SmryBio: 0 to skip, 1 to include
 2 3 4 5 # vector with selex std bins (-1 in first bin to self-generate)
 # -1 # list of ages for growth std (-1 in first bin to self-generate)
 20 # vector with NatAge std ages (-1 in first bin to self-generate)
 # -1 # list of ages for NatM std (-1 in first bin to self-generate)
999

