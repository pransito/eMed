## PREAMBLE ###################################################################
# script to get psychosocial data from SPSS data
# SYNTAX NGFN13 (using the one of Bonn)
# for Berlin
# for Mannheim
# for Bonn

## LIBRARIES ==================================================================
library(haven)
library(pracma)
library(stringr)
library(foreign)

## TABLE OF CONTENTS ==========================================================
# 1.) FAGERSTROEM
# 2.) BDI-I##############################################
# 3.) STAI-T############################################
# 4.) BIS-11############################################
# 5.) TCI ###############################################
# 6.) SSSV#############################################
# 7.) NEO-FFI##########################################
##########8.) AUDIT ############################################
##########9.) SOC-13###########################################
#########10.) PANAS###########################################
#########11.) ADS##############################################
#########12.) OCDS############################################
#########13.) URICA############################################

## GET DATA ===================================================================
# get the data
cur_file_mnm = 'S:/AG/AG-Emotional-Neuroscience-Backup/NGFN/SPSS-Datenmasken/Datenmasken_FINALs/Mannheim/Screen_Base_Neuro_SUMMENWERTE_ALL_FINAL_2013_Multicentre_gelöscht_23.01.2013_copy.sav'
cur_file_bnn = ''
cur_file_bln = 'S:/AG/AG-Emotional-Neuroscience/Restricted/NGFN/NGFN plus/Datenmasken/NGFNplus_Datenmaske_Fragebogen_Berlin_05.03.13_KCH.sav'
spss_mnm     = as.data.frame(read.spss(cur_file_mnm,encoding='UTF-8',use.value.labels = F))

## MANNHEIM ===================================================================
## FAGERSTRÖM =================================================================
spss_mnm$TUP6_ftnd_sum = with(spss_mnm,{(ftnd_1+ftnd_2+ftnd_3+ftnd_4+ftnd_5+ftnd_6)})

## BDI-I ======================================================================
spss_mnm$TUP6_BDI_I_sum                       = NA
spss_mnm$bdi_19[which(spss_mnm$bdi_19a == 1)] = 0
bdi_df                                        = spss_mnm[names(spss_mnm)[grep('bdi_[0-9].*',names(spss_mnm))]]
bdi_df$bdi_19a                                = NULL
cur_cmd                                       = paste(names(bdi_df),collapse='+')
spss_mnm$TUP6_BDI_I_sum                       = with(bdi_df,eval(parse(text=cur_cmd)))

## STAI-T =====================================================================
spss_mnm$TUP6_STAI_TRAIT_sum                  = NA
stai_df                                       = spss_mnm[names(spss_mnm)[grep('stai2_[0-9].*',names(spss_mnm))]]
cur_cmd                                       = paste(names(stai_df),collapse='+')
spss_mnm$TUP6_STAI_TRAIT_sum                  = with(stai_df,eval(parse(text=cur_cmd)))

## BIS-11 =====================================================================
# Aufmerksamkeitsbezogene Impulsivität (AI)
# N Items: 8
spss_mnm$TUP6_BIS_AI                            = with(spss_mnm, {bis_5+bis_6+bis_9+bis_11+bis_20+bis_24+bis_26+bis_28})
attr(spss_mnm$TUP6_BIS_AI,'Variable_Name_Long') = 'Barratt-Impulsiveness-Test-11_Attentional_Impulsivity'
attr(spss_mnm$TUP6_BIS_AI,'value.labels')       = NULL

# Motorische Impulsivität (MI)
spss_mnm$TUP6_BIS_MI                            = with(spss_mnm, {bis_2+bis_3+bis_4+bis_16+bis_17+bis_19+bis_21+bis_22+bis_23+bis_25+bis_30})
attr(spss_mnm$TUP6_BIS_MI,'Variable_Name_Long') = 'Barratt-Impulsiveness-Test-11_Motor_Impulsivity'
attr(spss_mnm$TUP6_BIS_MI,'value.labels')       = NULL

# Nichtplanende Impulsivität (NI)
spss_mnm$TUP6_BIS_NI                            = with(spss_mnm, {bis_1+bis_7+bis_8+bis_10+bis_12+bis_13+bis_14+bis_15+bis_18+bis_27+bis_29})
attr(spss_mnm$TUP6_BIS_NI,'Variable_Name_Long') = 'Barratt-Impulsiveness-Test-11_Nonplanning_Impulsivity'
attr(spss_mnm$TUP6_BIS_NI,'value.labels')       = NULL
  
# BIS Total Score (TS)#/
spss_mnm$TUP6_BIS_TS                            = with(spss_mnm, {TUP6_BIS_AI+TUP6_BIS_MI+TUP6_BIS_NI})
attr(spss_mnm$TUP6_BIS_TS,'Variable_Name_Long') = 'Barratt-Impulsiveness-Test-11_TotalScore_Impulsivity'
attr(spss_mnm$TUP6_BIS_TS,'value.labels')       = NULL
  
## TCI ========================================================================
# Novelty Seeking (NS)
# NS1
# N Items: 11
spss_mnm$TUP6_TCI_NS1                            = with(spss_mnm, {tci_1+tci_14+tci_22+tci_32+tci_41+tci_49+tci_60+tci_71+tci_80+tci_89+tci_99})
attr(spss_mnm$TUP6_TCI_NS1,'Variable_Name_Long') = 'TCI Explorative Excitability vs. Stoic Rigidity NS1'
attr(spss_mnm$TUP6_TCI_NS1,'value.labels')       = NULL
  
# NS2
# N Items: 10
spss_mnm$TUP6_TCI_NS2                            = with(spss_mnm, {tci_5+tci_16+tci_26+tci_37+tci_44+tci_54+tci_62+tci_77+tci_85+tci_98})
attr(spss_mnm$TUP6_TCI_NS2,'Variable_Name_Long') = 'TCI Impulsivity vs. Rumination NS2'
attr(spss_mnm$TUP6_TCI_NS2,'value.labels')       = NULL

# NS3
# N Items: 9
spss_mnm$TUP6_TCI_NS3                            = with(spss_mnm, {tci_7+tci_17+tci_29+tci_45+tci_56+tci_65+tci_72+tci_81+tci_92})
attr(spss_mnm$TUP6_TCI_NS3,'Variable_Name_Long') = 'TCI Extravagance vs. Restraint NS3'
attr(spss_mnm$TUP6_TCI_NS3,'value.labels')       = NULL

  
# NS4
# N Items: 10
spss_mnm$TUP6_TCI_NS4                            = with(spss_mnm, {tci_15+tci_23+tci_34+tci_39+tci_46+tci_57+tci_70+tci_76+tci_86+tci_90})
attr(spss_mnm$TUP6_TCI_NS4,'Variable_Name_Long') = 'TCI Disorderliness vs. Orderliness NS4'
attr(spss_mnm$TUP6_TCI_NS4,'value.labels')       = NULL

# Novelty Seeking (NS)
spss_mnm$TUP6_TCI_NS                             = with(spss_mnm, {TUP6_TCI_NS1+TUP6_TCI_NS2+TUP6_TCI_NS3+TUP6_TCI_NS4})
attr(spss_mnm$TUP6_TCI_NS,'Variable_Name_Long')  = 'TCI Novelty Seeking (NS) Total Score'
attr(spss_mnm$TUP6_TCI_NS,'value.labels')        = NULL

# Harm Avoidance (HA)
# HA1
# N Items: 11
spss_mnm$TUP6_TCI_HA1                             = with(spss_mnm, {tci_2+tci_8+tci_18+tci_28+tci_36+tci_47+tci_51+tci_63+tci_69+tci_78+tci_94})
attr(spss_mnm$TUP6_TCI_HA1,'Variable_Name_Long')  = 'TCI Anticipatory worry vs. Extreme Optimism HA1'
attr(spss_mnm$TUP6_TCI_HA1,'value.labels')        = NULL

# HA2
# N Items: 7
spss_mnm$TUP6_TCI_HA2                             = with(spss_mnm, {tci_4+tci_11+tci_30+tci_53+tci_64+tci_79+tci_91})
attr(spss_mnm$TUP6_TCI_HA2,'Variable_Name_Long')  = 'TCI Fear of Uncertainty vs. Confidence HA2'
attr(spss_mnm$TUP6_TCI_HA2,'value.labels')        = NULL

# HA3
# N Items: 8
spss_mnm$TUP6_TCI_HA3                             = with(spss_mnm, {tci_12+tci_24+tci_35+tci_42+tci_58+tci_67+tci_87+tci_96})
attr(spss_mnm$TUP6_TCI_HA3,'Variable_Name_Long')  = 'TCI Shyness/Shyness with Strangers vs. Sociability HA3'
attr(spss_mnm$TUP6_TCI_HA3,'value.labels')        = NULL
  
# HA4
# N Items: 9
spss_mnm$TUP6_TCI_HA4                             = with(spss_mnm, {tci_10+tci_19+tci_27+tci_40+tci_48+tci_61+tci_75+tci_84+tci_97})
attr(spss_mnm$TUP6_TCI_HA4,'Variable_Name_Long')  = 'TCI Fatigability/Fatigability and asthenia (weakness) vs. Vitality HA4'
attr(spss_mnm$TUP6_TCI_HA4,'value.labels')        = NULL
  
# Harm Avoidance (HA)
spss_mnm$TUP6_TCI_HA                              = with(spss_mnm, {TUP6_TCI_HA1+TUP6_TCI_HA2+TUP6_TCI_HA3+TUP6_TCI_HA4})
attr(spss_mnm$TUP6_TCI_HA,'Variable_Name_Long')   = 'TCI Harm Avoidance Total Sum(HA)'
attr(spss_mnm$TUP6_TCI_HA,'value.labels')         = NULL
  
# Reward Dependence
# RD1
# N Items: 10
spss_mnm$TUP6_TCI_RD1                             = with(spss_mnm, {tci_3+tci_13+tci_25+tci_38+tci_43+tci_52+tci_68+tci_74+tci_88+tci_93})
attr(spss_mnm$TUP6_TCI_RD1,'Variable_Name_Long')  = 'TCI Sentimentality vs. Insensitivity RD1'
attr(spss_mnm$TUP6_TCI_RD1,'value.labels')        = NULL
  
warning('RD2 syntax is missing!!! Not needed?')

# RD3
# N Items: 8
spss_mnm$TUP6_TCI_RD3                             = with(spss_mnm, {tci_9+tci_20+tci_31+tci_50+tci_59+tci_73+tci_83+tci_95})
attr(spss_mnm$TUP6_TCI_RD3,'Variable_Name_Long')  = 'TCI Attachment vs. Detachment RD3'
attr(spss_mnm$TUP6_TCI_RD3,'value.labels')        = NULL
  
# RD4
# N Items:6##/
spss_mnm$TUP6_TCI_RD4                             = with(spss_mnm, {tci_6+tci_21+tci_33+tci_55+tci_66+tci_82})
attr(spss_mnm$TUP6_TCI_RD4,'Variable_Name_Long')  = 'TCI Dependence on Approval by Others RD4'
attr(spss_mnm$TUP6_TCI_RD4,'value.labels')        = NULL
  
# Reward Dependence (RD) Total Score
spss_mnm$TUP6_TCI_RD                              = with(spss_mnm, {TUP6_TCI_RD1+TUP6_TCI_RD3+TUP6_TCI_RD4})
attr(spss_mnm$TUP6_TCI_RD,'Variable_Name_Long')   = 'TCI Reward Dependence Total Score (RD)'
attr(spss_mnm$TUP6_TCI_RD,'value.labels')         = NULL

## SSSV (TAS/ DIS/ ES) ========================================================
# Thrill & Adventure Seeking (TAS)
# N Items: 10
warning('SSSV Mannheim Data sss_38 and probably other items are missing!')
spss_mnm$TUP6_SSSV_TAS                            = with(spss_mnm,{sss_3+sss_11+sss_16+sss_17+sss_20+sss_21+sss_23+sss_28+sss_38+sss_40})
attr(spss_mnm$TUP6_SSSV_TAS,'Variable_Name_Long') = 'SSSV Thrill & Adventure Seeking'
attr(spss_mnm$TUP6_SSSV_TAS,'value.labels')       = NULL

# Disinhibition (DIS)
# N Items: 10
warning('SSSV Mannheim Data sss_32 and probably other items are missing!')
spss_mnm$TUP6_SSSV_DIS                            = with(spss_mnm,{sss_1+sss_12+sss_13+sss_25+sss_29+sss_30+sss_32+sss_33+sss_35+sss_36})
attr(spss_mnm$TUP6_SSSV_DIS,'Variable_Name_Long') = 'SSSV Disinhibition'
attr(spss_mnm$TUP6_SSSV_DIS,'value.labels')       = NULL
  
# Experience Seeking (ES)
# N Items: 10
spss_mnm$TUP6_SSSV_ES                            = with(spss_mnm,{sss_4+sss_6+sss_9+sss_10+sss_14+sss_18+sss_19+sss_22+sss_26+sss_37})
attr(spss_mnm$TUP6_SSSV_ES,'Variable_Name_Long') = 'SSSV Experience Seeking'
attr(spss_mnm$TUP6_SSSV_ES,'value.labels')       = NULL
  
# SSSV Total Score (TS)
spss_mnm$TUP6_SSSV_TS                            = with(spss_mnm,{TUP6_SSSV_TAS+TUP6_SSSV_DIS+TUP6_SSSV_ES})
attr(spss_mnm$TUP6_SSSV_TS,'Variable_Name_Long') = 'SSSV Total Score'
attr(spss_mnm$TUP6_SSSV_TS,'value.labels')       = NULL

## NEO-FFI (60 Items) =========================================================
# SCORE-BERECHNUNG
# Neurotizismus (N Items: 12)
spss_mnm$TUP6_NEO_N                            = with(spss_mnm,{neo_1+neo_6+neo_11+neo_16+neo_21+neo_26+neo_31+neo_36+neo_41+neo_46+neo_51+neo_56})
attr(spss_mnm$TUP6_NEO_N,'Variable_Name_Long') = 'NEO-FFI Neuroticism'
attr(spss_mnm$TUP6_NEO_N,'value.labels')       = NULL
  
# Extraversion (N Items: 12)
spss_mnm$TUP6_NEO_E                            = with(spss_mnm,{neo_2+neo_7+neo_12+neo_17+neo_22+neo_27+neo_32+neo_37+neo_42+neo_47+neo_52+neo_57})
attr(spss_mnm$TUP6_NEO_E,'Variable_Name_Long') = 'NEO-FFI Extraversion'
attr(spss_mnm$TUP6_NEO_E,'value.labels')       = NULL
  
# Openess to Experiences (N Items: 12)
spss_mnm$TUP6_NEO_O                            = with(spss_mnm,{neo_3+neo_8+neo_13+neo_18+neo_23+neo_28+neo_33+neo_38+neo_43+neo_48+neo_53+neo_58})
attr(spss_mnm$TUP6_NEO_O,'Variable_Name_Long') = 'NEO-FFI Openness to New Experiences'
attr(spss_mnm$TUP6_NEO_O,'value.labels')       = NULL
  
# Agreeableness (N Items: 12)
spss_mnm$TUP6_NEO_A                            = with(spss_mnm,{neo_4+neo_9+neo_14+neo_19+neo_24+neo_29+neo_34+neo_39+neo_44+neo_49+neo_54+neo_59})
attr(spss_mnm$TUP6_NEO_A,'Variable_Name_Long') = 'NEO-FFI Agreeableness'
attr(spss_mnm$TUP6_NEO_A,'value.labels')       = NULL
  
# Gewissenhaftigkeit/ Conscientiousness (N Items: 12)
spss_mnm$TUP6_NEO_C                            = with(spss_mnm,{neo_5+neo_10+neo_15+neo_20+neo_25+neo_30+neo_35+neo_40+neo_45+neo_50+neo_55+neo_60})
attr(spss_mnm$TUP6_NEO_C,'Variable_Name_Long') = 'NEO-FFI Conscientiousness'
attr(spss_mnm$TUP6_NEO_C,'value.labels')       = NULL



## AUDIT ======================================================================
# AUDIT Total Score (TS)
spss_mnm$TUP6_AUDIT_TS                            = with(spss_mnm, {aud_1+aud_2+aud_3+aud_4+aud_5+aud_6+aud_7+aud_8+aud_9+aud_10})
attr(spss_mnm$TUP6_AUDIT_TS,'Variable_Name_Long') = 'AUDIT Total Score'
attr(spss_mnm$TUP6_AUDIT_TS,'value.labels')       = NULL


## CONTINUE HERE ##
# Recoding
RECODE Audit_TS (0 thru 7=0) (8 thru 15=1) (16 thru 19=2) (20 thru Highest=3) (ELSE=SYSMIS) INTO audit_RiskCat. 
VARIABLE LABELS  Audit_RiskCat 'AUDIT Risiko Kategorien'. 
VALUE LABELS Audit_RiskCat 0 'Low risk' 1 'Risky or hazardous level' 2 'High risk or harmful level' 3 'High risk' .
EXECUTE.

# AUDIT Consumption Score (CS)
# N Items: 3
spss_mnm$TUP6_AUDIT_CS                            = with(spss_mnm, {aud_1+aud_2+aud_3})
attr(spss_mnm$TUP6_AUDIT_CS,'Variable_Name_Long') = 'AUDIT Consumption Score (CS)'
attr(spss_mnm$TUP6_AUDIT_CS,'value.labels')       = NULL

# AUDIT Dependence Score (DS)
# N Items: 3
spss_mnm$TUP6_AUDIT_DS                            = with(spss_mnm, {aud_4+aud_5+aud_6})
attr(spss_mnm$TUP6_AUDIT_DS,'Variable_Name_Long') = 'AUDIT Dependence Score (DS)'
attr(spss_mnm$TUP6_AUDIT_DS,'value.labels')       = NULL

# AUDIT Alcohol-related problems Score (AS)
# N Items: 4
spss_mnm$TUP6_AUDIT_AS                            = with(spss_mnm, {aud_7+aud_8+aud_9+aud_10})
attr(spss_mnm$TUP6_AUDIT_AS,'Variable_Name_Long') = 'AUDIT Alcohol-related problems Score (AS)'
attr(spss_mnm$TUP6_AUDIT_AS,'value.labels')       = NULL


## Sense of Coherence (SOC-13) =================================================
# SOC-13 Total Score (TS)
# N Items: 13
spss_mnm$TUP6_SOC_TS                            = with(spss_mnm, {soc_1+soc_2+soc_3+soc_4+soc_5+soc_6+soc_7+soc_8+soc_9+soc_10+soc_11+soc_12+soc_13})
attr(spss_mnm$TUP6_SOC_TS,'Variable_Name_Long') = 'Sense Of Coherence (SOC-13) Total Score'
attr(spss_mnm$TUP6_SOC_TS,'value.labels')       = NULL



## PANAS =======================================================================
# Positive Affect (PA)
# N Items: 10
spss_mnm$TUP6_PANAS_PA                            = with(spss_mnm, {pan_1+pan_3+pan_4+pan_6+pan_10+pan_11+pan_13+pan_15+pan_17+pan_18})
attr(spss_mnm$TUP6_PANAS_PA,'Variable_Name_Long') = 'PANAS Positive Affect (PA)'
attr(spss_mnm$TUP6_PANAS_PA,'value.labels')       = NULL

# Negative Affect (NA)
# N Items: 10
spss_mnm$TUP6_PANAS_NA                            = with(spss_mnm, {pan_2+pan_5+pan_7+pan_8+pan_9+pan_12+pan_14+pan_16+pan_19+pan_20})
attr(spss_mnm$TUP6_PANAS_NA,'Variable_Name_Long') = 'PANAS Negative Affect (NA)'
attr(spss_mnm$TUP6_PANAS_NA,'value.labels')       = NULL


## ADS =========================================================================
# ADS Total Score (TS)
spss_mnm$TUP6_ADS_TS                            = with(spss_mnm, {ads_1+ads_2+ ads_3+ads_4+ads_5+ads_6+ads_7+ads_8+ads_9+ads_10+ads_11+ads_12+ads_13+ads_14+ads_15+ads_16+ads_17+ads_18+ads_19+ads_20+ads_21+ads_22+ads_23+ads_24+ads_25})
attr(spss_mnm$TUP6_ADS_TS,'Variable_Name_Long') = 'ADS Total Score (TS)'
attr(spss_mnm$TUP6_ADS_TS,'value.labels')       = NULL

## OCDS ========================================================================




/######################################################/
  /################OCDS###############################/
  /######################################################/
  /########SCORE-BERECHNUNG##############/
  /##################################################/
  
  /##############Nach Mann/Ackermann#######################/
  /#Berechnung der Scores mit Item 7 und 8#/
  /#Vier Hilfsvariablen: Jeweils das Maximum#/
  
  COMPUTE OCDm_12 = MAX(ocd_1, ocd_2).
EXECUTE.
COMPUTE OCDm_78 = MAX(ocd_7, ocd_8).
EXECUTE.
COMPUTE OCDm_910 = MAX(ocd_9, ocd_10).
EXECUTE.
COMPUTE OCDm_1314 = MAX(ocd_13, ocd_14).
EXECUTE.

COMPUTE OCDS_TS_Mann = SUM(ocdm_12, ocd_3, ocd_4, ocd_5, ocd_6, ocdm_78,
                           ocdm_910, ocd_11, ocd_12, ocdm_1314) .
VARIABLE LABELS OCDS_TS_Mann 'OCDS Total Score nach Mann/Ackermann'.
EXECUTE .

COMPUTE OCDS_Kog_Mann = SUM(ocdm_12, ocd_3, ocd_4, ocd_5, ocd_6 ).
VARIABLE LABELS OCDS_Kog_Mann 'OCDS Gedanken nach Mann/Ackermann'.
EXECUTE .

COMPUTE OCDS_Han_Mann = SUM(ocdm_78,ocdm_910, ocd_11, ocd_12, ocdm_1314) .
VARIABLE LABELS OCDS_Han_Mann 'OCDS Handlungen nach Mann/Ackermann'.
EXECUTE .

/###########################################################################/
  
  /##############Nach Nakovics et al#######################/
  /#Berechnung der Scores ohne Item 7 und 8#/
  
  /#Anmerkung Helmut:#/
  /#Gesamtscore kann zum Zwecke der Vergleichbarkeit beibehalten werden.#/ 
  /#Die 2-Faktorlösung hat jedoch u.a. eine höhere Stabilität und Varianzaufklärung und sollte daher präferiert werden#/
  
  COMPUTE OCDS_TS_Nak = SUM(ocd_1 TO ocd_6, ocd_9 TO ocd_14) .
VARIABLE LABELS OCDS_TS_Nak 'OCDS Total Score ohne Item7/8 nach Nakovics et al'.
EXECUTE .

COMPUTE OCDS_Kog_Nak = SUM(ocd_1 TO ocd_6) .
VARIABLE LABELS OCDS_Kog_Nak 'OCDS Gedanken ohne Item7/8 nach Nakovics et al'.
EXECUTE .

COMPUTE OCDS_Han_Nak = SUM(ocd_9 TO ocd_14) .
VARIABLE LABELS OCDS_Han_Nak 'OCDS Handlungen ohne Item7/8 nach Nakovics et al'.
EXECUTE .

/###Löschen der Hilfsvariablen###/
  
  DELETE VARIABLES OCDm_12 OCDm_78 OCDm_910 OCDm_1314.
EXECUTE.


## URICA =======================================================================
# Precontemplation (PC)
# N Items: 7 (Item 31 is omitted)
# In Mannheim data, URICA Item 1 is named "uri" instead of "uri_1" 
spss_mnm$TUP6_URICA_PC                            = with(spss_mnm, {(uri_1+uri_5+uri_11+uri_13+uri_23+uri_26+uri_29)/7})
attr(spss_mnm$TUP6_URICA_PC,'Variable_Name_Long') = 'URICA Precontemplation (PC)'
attr(spss_mnm$TUP6_URICA_PC,'value.labels')       = NULL


# Contemplation (C)
# N Items: 7 (Item 4 is omitted)
spss_mnm$TUP6_URICA_C                            = with(spss_mnm, {(uri_2+uri_8+uri_12+uri_15+uri_19+uri_21+uri_24)/7})
attr(spss_mnm$TUP6_URICA_C,'Variable_Name_Long') = 'URICA Contemplation (C)'
attr(spss_mnm$TUP6_URICA_C,'value.labels')       = NULL


# Action (A)
# N Items: 7 (Item 20 is omitted)
spss_mnm$TUP6_URICA_A                            = with(spss_mnm, {(uri_3+uri_7+uri_10+uri_14+uri_17+uri_25+uri_30)/7})
attr(spss_mnm$TUP6_URICA_A,'Variable_Name_Long') = 'URICA Action (A)'
attr(spss_mnm$TUP6_URICA_A,'value.labels')       = NULL


# Maintenance (M)
# N Items: 7 (Item 9 is omitted)
spss_mnm$TUP6_URICA_M                            = with(spss_mnm, {(uri_6+uri_16+uri_18+uri_22+uri_27+uri_28+uri_32)/7})
attr(spss_mnm$TUP6_URICA_M,'Variable_Name_Long') = 'URICA Maintenance (M)'
attr(spss_mnm$TUP6_URICA_M,'value.labels')       = NULL


# Readiness to Change (RC)
# RC = c + A + M - PC
spss_mnm$TUP6_URICA_RC                            = with(spss_mnm, {TUP6_URICA_C + TUP6_URICA_A + TUP6_URICA_M - TUP6_URICA_PC})
attr(spss_mnm$TUP6_URICA_RC,'Variable_Name_Long') = 'URICA Readiness to Change (RC)'
attr(spss_mnm$TUP6_URICA_RC,'value.labels')       = NULL




/################################################/
  /############URICA###########################/
  /#################################################/
  /########SCORE-BERECHNUNG##############/
  /##################################################/
  
  /#Score: Precontemplation (Skala 1)#/
  
  #item 31 wird weggelassen#
  
  #COMPUTE URICA_PC = SUM(uri_1,uri_5,uri_11,uri_13,uri_23,uri_26,uri_29) .
  #VARIABLE LABELS URICA_PC 'URICA-Precontemplation'.
  #EXECUTE .
  
  
  COMPUTE URICA_PC_Mean = MEAN(uri_1,uri_5,uri_11,uri_13,uri_23,uri_26,uri_29,uri_31) .
VARIABLE LABELS URICA_PC_Mean 'URICA-Precontemplation_Mean'.
EXECUTE .

/#############################################/
  
  /#Score: Contemplation (Skala 2)#/
  
  #item 4 wird weggelassen#
  
  #COMPUTE URICA_C = SUM(uri_2,uri_8,uri_12,uri_15,uri_19,uri_21,uri_24) .
  #VARIABLE LABELS URICA_C 'URICA-Contemplation'.
  #EXECUTE .
  
  COMPUTE URICA_C_Mean = MEAN(uri_2,uri_8,uri_12,uri_15,uri_19,uri_21,uri_24) .
VARIABLE LABELS URICA_C_Mean 'URICA-Contemplation_Mean'.
EXECUTE .

/#############################################/
  
  /#Score: Action (Skala 3)#/
  
  #item 20 wird weggelassen#
  
  #COMPUTE URICA_A = SUM(uri_3,uri_7,uri_10,uri_14,uri_17,uri_25,uri_30) .
  #VARIABLE LABELS URICA_A 'URICA-Action'.
  #EXECUTE .
  
  COMPUTE URICA_A_Mean = MEAN(uri_3,uri_7,uri_10,uri_14,uri_17,uri_25,uri_30) .
VARIABLE LABELS URICA_A_Mean 'URICA-Action_Mean'.
EXECUTE .

/#############################################/
  
  /#Score: MAINTENANCE (Skala 4)#/
  
  #item 9 wird weggelassen#
  
  #COMPUTE URICA_M = SUM(uri_6,uri_16,uri_18,uri_22,uri_27,uri_28,uri_32) .
  #VARIABLE LABELS URICA_M 'URICA-Maintenance'.
  #EXECUTE .
  
  COMPUTE URICA_M_Mean = MEAN(uri_6,uri_16,uri_18,uri_22,uri_27,uri_28,uri_32) .
VARIABLE LABELS URICA_M_Mean 'URICA-Maintenance_Mean'.
EXECUTE .

/#############################################/
  
  /#Summenscore Readiness for Change#/
  
  #Readiness for Change = Mean Skala 2 + Mean Skala 3 + Mean Skala 4 - Mean Skala 1#
  
  COMPUTE URICA_Readiness_for_Change = SUM(URICA_C_Mean,URICA_A_Mean,URICA_M_Mean)-URICA_PC_Mean.
VARIABLE LABELS URICA_Readiness_for_Change 'URICA-Readiness for Change'.
EXECUTE.


## Alcohol Usage Questionnaire (AUQ) ===========================================
# AUQ before MRI session (AUQ_T1)
# N Items: 8
spss_mnm$TUP6_AUQ_T1                                  = with(spss_mnm, {auq_1v+auq_2v+auq_3v+auq_4v+auq_5v+auq_6v+auq_7v+auq_8v})
attr(spss_mnm$TUP6_URICA_AUQ_T1,'Variable_Name_Long') = 'AUQ before MRI session (T1)'
attr(spss_mnm$TUP6_URICA_AUQ_T1,'value.labels')       = NULL


# AUQ after MRI session (AUQ_T2)
# N Items: 8
spss_mnm$TUP6_AUQ_T2                                  = with(spss_mnm, {auq_1n+auq_2n+auq_3n+auq_4n+auq_5n+auq_6n+auq_7n+auq_8n})
attr(spss_mnm$TUP6_URICA_AUQ_T2,'Variable_Name_Long') = 'AUQ after MRI session (T2)'
attr(spss_mnm$TUP6_URICA_AUQ_T2,'value.labels')       = NULL


# AUQ Total Score (AUQ_TS)
# N Items: 16
spss_mnm$TUP6_AUQ_TS                                  = with(spss_mnm, {TUP6_AUQ_T1+TUP6_AUQ_T2})
attr(spss_mnm$TUP6_URICA_AUQ_TS,'Variable_Name_Long') = 'AUQ TOtal Score (TS)'
attr(spss_mnm$TUP6_URICA_AUQ_TS,'value.labels')       = NULL






## BONN =======================================================================
## FAGERSTRÖM =================================================================
/###################################################/
  /##############FAGERSTROEM##################/
  /########SCORE-BERECHNUNG###############/
  /###################################################/
  
  /# CORRECT VARIABLE IS FTND SUM      #/
  
  /# RECODING; WE DROP THIS FOR MH    #/
  
  # Recode
  ftnd_1 (0=3) (1=2) (2=1) (3=0) INTO ftnd_1_korr.
# VARIABLE LABELS ftnd_1_korr 'ftnd_1_korrigiert'.
# execute.

# Recode
ftnd_2  (0=1) (1=0) INTO ftnd_2_korr.
# VARIABLE LABELS ftnd_2_korr 'ftnd_2_korrigiert'.
# execute.

# Recode
ftnd_3 (0=1) (1=0) INTO ftnd_3_korr.
# VARIABLE LABELS ftnd_3_korr 'ftnd_3_korrigiert'.
# execute.

# Recode
ftnd_5 (0=1) (1=0) INTO ftnd_5_korr.
# VARIABLE LABELS ftnd_5_korr 'ftnd_5_korrigiert'.
# execute.

# Recode
ftnd_6 (0=1) (1=0) INTO ftnd_6_korr.
# VARIABLE LABELS ftnd_6_korr 'ftnd_6_korrigiert'.
# execute.

COMPUTE 
Fagerstroem_TS_MA = sum(ftnd_1, ftnd_2, ftnd_3, ftnd_4, ftnd_5, ftnd_6).
VARIABLE LABELS Fagerstroem_TS_MA 'Fagerstroem Total Score Marcus Alex'.
execute.

/###Löschen der Hilfsvariablen###/
  
  DELETE VARIABLES ftnd_1_korr ftnd_2_korr ftnd_3_korr ftnd_5_korr ftnd_6_korr.
EXECUTE.

# # LDH
# # ldh berlin or mannheim syntax usable
# Sorry, also in Mannheim gibt es den Einschub zum UmKodieren von ein paar Items ganz am Anfang von der Syntax, nicht in Berlin. 
# 
# Von: Beck, Anne 
# Gesendet: Donnerstag, 19. Juli 2018 15:33
# An: Genauck, Alexander <alexander.genauck@charite.de>
#   Cc: Rothkirch, Marcus <marcus.rothkirch@charite.de>
#   Betreff: LDH
# 
# Hier liegt die LDH-Syntax von Mannheim:
#   
#   S:\AG\AG-Emotional-Neuroscience\Restricted\NGFN\NGFN plus\Datenmasken\LDH\LDH-Syntax Mannheim
# 
# Ich würde die NGFN13_SYNTAX_LDH_ANNE_14062011_neu nehmen, das war damals die Doktorandin in Mannheim.
# 
# Und hier liegt Berlin:
#   
#   S:\AG\AG-Emotional-Neuroscience\Restricted\NGFN\NGFN plus\Datenmasken\LDH
# 
# Hier scheint NGFN13_SYNTAX_LDH_Berlin_23.09.2011 identisch mit der Mannheimer, außer dass am Anfang wohl falsch kodierte Items umkodiert werden.
# 
# Ich würde also für Bonn einfach die Mannheimer Maske nehmen, weil da wahrscheinlich nichts umkodiert werden muss.
# 
# LG!
#   Anne
