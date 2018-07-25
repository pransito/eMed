## PREAMBLE ###################################################################
# script to get psychosocial data from SPSS data
# SYNTAX NGFN13 (using the one of Bonn)
# for Berlin
# for Mannheim
# not for Bonn, here we take the finished sum scores

## LIBRARIES ==================================================================
library(haven)
library(pracma)
library(stringr)
library(foreign)
library(compiler)
library(psych)

## FUNCTIONS ==================================================================
agk.recode <- function(x,y,z) {
  # a recode function
  # function to recode x, given a source vector y
  # and a translated vector z
  x     = as.character(x)
  y     = as.character(y)
  z     = as.character(z)
  x_rec = x
  for (ii in 1:length(x)) {
    done <- 0
    for (jj in 1:length(y)) {
      # NA in x will be NA
      if(is.na(x[ii])) {
        x_rec[ii] <- NA
        break
      }
      if (x[ii] == y[jj]) {
        x_rec[ii] <- z[jj]
        done <- 1
      }
      if (done == 1) {break}
    }
  }
  return(x_rec)
}
agk.recode.c = cmpfun(agk.recode)

letter2num <- function(x) {
  # function to convert a letter into a number (1-26)
  utf8ToInt(tolower(x)) - utf8ToInt("a") + 1L
}

## TABLE OF CONTENTS ==========================================================
# 1.) FAGERSTROEM
# 2.) BDI-I##############################################
# 3.) STAI-T############################################
# 4.) BIS-11############################################
# 5.) TCI ###############################################
# 6.) SSSV#############################################
# 7.) NEO-FFI##########################################
# 8.) AUDIT ############################################
# 9.) SOC-13###########################################
# 10.) PANAS###########################################
# 11.) ADS##############################################
# 12.) OCDS############################################
# 13.) URICA############################################

## PATHS TO SPSS FILES ==========================================================
# get the data
cur_file_mnm = 'S:/AG/AG-Emotional-Neuroscience-Backup/NGFN/SPSS-Datenmasken/Datenmasken_FINALs/Mannheim/Screen_Base_Neuro_SUMMENWERTE_ALL_FINAL_2013_Multicentre_gelöscht_23.01.2013_copy.sav'
cur_file_bnn = 'S:/AG/AG-Emotional-Neuroscience-Backup/NGFN/SPSS-Datenmasken/Datenmasken_FINALs/Bonn/NGFN13_Bonn_alle_Daten.sav'
cur_file_bln = 'S:/AG/AG-Emotional-Neuroscience/Restricted/NGFN/NGFN plus/Datenmasken/NGFNplus_Datenmaske_Fragebogen_Berlin_05.03.13_KCH.sav'
auq_file_bln = 'S:/AG/AG-Emotional-Neuroscience/Restricted/NGFN/NGFN plus/Datenmasken/NGFNplus_Datenmaske_Berlin_19 02 14_KCH_AUQs.sav'

spss_files      = list()
cur_f           = list()
cur_f$file      = cur_file_mnm
cur_f$site      = 'Mannheim'
spss_files[[1]] = cur_f
cur_f$file      = cur_file_bln 
cur_f$site      = 'Berlin'
spss_files[[2]] = cur_f
cur_f$file      = cur_file_bnn 
cur_f$site      = 'Bonn'
spss_files[[3]] = cur_f

# adding the AUQ for Berlin (sum scores only)
auq_bln = as.data.frame(read.spss(auq_file_bln,encoding='UTF-8',use.value.labels = F))

for (ss in 1:length(spss_files)) {
  ## GET DATA ===================================================================
  spss_mnm     = as.data.frame(read.spss(spss_files[[ss]]$file,encoding='UTF-8',use.value.labels = F))
  
  ## FOR EACH SITE ==============================================================
  ## FAGERSTRÖM =================================================================
  # recoding for Berlin
  names(spss_mnm)        = gsub('Fagerstroem_','ftnd_',names(spss_mnm))
  
  # computing
  if (spss_files[[ss]]$site != 'Bonn') {
    spss_mnm$TUP6_ftnd_sum = with(spss_mnm,{(ftnd_1+ftnd_2+ftnd_3+ftnd_4+ftnd_5+ftnd_6)})
  } else {
    spss_mnm$TUP6_ftnd_sum = spss_mnm$FAG_ges
  }
  
  ## BDI-I ======================================================================
  # some recoding work
  names(spss_mnm) = gsub('BDI_','bdi_',names(spss_mnm))
  names(spss_mnm) = gsub('bdi_Essen','bdi_19a',names(spss_mnm))
  cur_names       = names(spss_mnm)[grep('bdi_[A-Z]$',names(spss_mnm))]
  cur_names       = strsplit(cur_names,'_')
  get_second      = function(x) {return(x[2])}
  cur_names       = unlist(lapply(cur_names,get_second))
  cur_names       = unlist(lapply(cur_names,FUN = letter2num))
  cur_names       = paste0('bdi_',cur_names)
  names(spss_mnm)[grep('bdi_[A-Z]$',names(spss_mnm))] = cur_names
  
  if (spss_files[[ss]]$site != 'Bonn') {
    # computation
    spss_mnm$TUP6_BDI_I_sum                       = NA
    spss_mnm$bdi_19[which(spss_mnm$bdi_19a == 1)] = 0
    bdi_df                                        = spss_mnm[names(spss_mnm)[grep('bdi_[0-9].*',names(spss_mnm))]]
    bdi_df$bdi_19a                                = NULL
    cur_cmd                                       = paste(names(bdi_df),collapse='+')
    spss_mnm$TUP6_BDI_I_sum                       = with(bdi_df,eval(parse(text=cur_cmd)))
  } else {
    spss_mnm$TUP6_BDI_I_sum = spss_mnm$BDI_ges
  }
  
  ## STAI-T =====================================================================
  # recoding for Berlin
  names(spss_mnm)                               = gsub('STAI_T_','stai2_',names(spss_mnm))
  
  if (spss_files[[ss]]$site != 'Bonn') {
    # computing
    spss_mnm$TUP6_STAI_TRAIT_sum                  = NA
    stai_df                                       = spss_mnm[names(spss_mnm)[grep('stai2_[0-9].*',names(spss_mnm))]]
    cur_cmd                                       = paste(names(stai_df),collapse='+')
    spss_mnm$TUP6_STAI_TRAIT_sum                  = with(stai_df,eval(parse(text=cur_cmd)))
  } else {
    spss_mnm$TUP6_STAI_TRAIT_sum                  = spss_mnm$STAI_ges
  }
  
  ## BIS-11 =====================================================================
  if (spss_files[[ss]]$site != 'Bonn') {
    spss_mnm$TUP6_BIS_AI                            = with(spss_mnm, {bis_5+bis_6+bis_9+bis_11+bis_20+bis_24+bis_26+bis_28})
    spss_mnm$TUP6_BIS_MI                            = with(spss_mnm, {bis_2+bis_3+bis_4+bis_16+bis_17+bis_19+bis_21+bis_22+bis_23+bis_25+bis_30})
    spss_mnm$TUP6_BIS_NI                            = with(spss_mnm, {bis_1+bis_7+bis_8+bis_10+bis_12+bis_13+bis_14+bis_15+bis_18+bis_27+bis_29})
    spss_mnm$TUP6_BIS_TS                            = with(spss_mnm, {TUP6_BIS_AI+TUP6_BIS_MI+TUP6_BIS_NI})
  } else {
    spss_mnm$TUP6_BIS_AI                            = spss_mnm$BIS_AI
    spss_mnm$TUP6_BIS_MI                            = spss_mnm$BIS_MI
    spss_mnm$TUP6_BIS_NI                            = spss_mnm$BIS_NI
    spss_mnm$TUP6_BIS_TS                            = spss_mnm$BIS_ges
  }
  
  # Aufmerksamkeitsbezogene Impulsivität (AI)
  attr(spss_mnm$TUP6_BIS_AI,'Variable_Name_Long') = 'Barratt-Impulsiveness-Test-11_Attentional_Impulsivity'
  attr(spss_mnm$TUP6_BIS_AI,'value.labels')       = NULL
  
  # Motorische Impulsivität (MI)
  attr(spss_mnm$TUP6_BIS_MI,'Variable_Name_Long') = 'Barratt-Impulsiveness-Test-11_Motor_Impulsivity'
  attr(spss_mnm$TUP6_BIS_MI,'value.labels')       = NULL
  
  # Nichtplanende Impulsivität (NI)
  attr(spss_mnm$TUP6_BIS_NI,'Variable_Name_Long') = 'Barratt-Impulsiveness-Test-11_Nonplanning_Impulsivity'
  attr(spss_mnm$TUP6_BIS_NI,'value.labels')       = NULL
  
  # BIS Total Score (TS)
  attr(spss_mnm$TUP6_BIS_TS,'Variable_Name_Long') = 'Barratt-Impulsiveness-Test-11_TotalScore_Impulsivity'
  attr(spss_mnm$TUP6_BIS_TS,'value.labels')       = NULL
  
  ## TCI ========================================================================
  if (spss_files[[ss]]$site != 'Bonn') {
    # Novelty Seeking (NS)
    # NS1
    # N Items: 11
    spss_mnm$TUP6_TCI_NS1                            = with(spss_mnm, {tci_1+tci_14+tci_22+tci_32+tci_41+tci_49+tci_60+tci_71+tci_80+tci_89+tci_99})
    spss_mnm$TUP6_TCI_NS2                            = with(spss_mnm, {tci_5+tci_16+tci_26+tci_37+tci_44+tci_54+tci_62+tci_77+tci_85+tci_98})
    spss_mnm$TUP6_TCI_NS3                            = with(spss_mnm, {tci_7+tci_17+tci_29+tci_45+tci_56+tci_65+tci_72+tci_81+tci_92})
    spss_mnm$TUP6_TCI_NS4                            = with(spss_mnm, {tci_15+tci_23+tci_34+tci_39+tci_46+tci_57+tci_70+tci_76+tci_86+tci_90})
    spss_mnm$TUP6_TCI_NS                             = with(spss_mnm, {TUP6_TCI_NS1+TUP6_TCI_NS2+TUP6_TCI_NS3+TUP6_TCI_NS4})
    
    spss_mnm$TUP6_TCI_HA1                            = with(spss_mnm, {tci_2+tci_8+tci_18+tci_28+tci_36+tci_47+tci_51+tci_63+tci_69+tci_78+tci_94})
    spss_mnm$TUP6_TCI_HA2                            = with(spss_mnm, {tci_4+tci_11+tci_30+tci_53+tci_64+tci_79+tci_91})
    spss_mnm$TUP6_TCI_HA3                            = with(spss_mnm, {tci_12+tci_24+tci_35+tci_42+tci_58+tci_67+tci_87+tci_96})
    spss_mnm$TUP6_TCI_HA4                            = with(spss_mnm, {tci_10+tci_19+tci_27+tci_40+tci_48+tci_61+tci_75+tci_84+tci_97})
    spss_mnm$TUP6_TCI_HA                             = with(spss_mnm, {TUP6_TCI_HA1+TUP6_TCI_HA2+TUP6_TCI_HA3+TUP6_TCI_HA4})
    
    spss_mnm$TUP6_TCI_RD1                            = with(spss_mnm, {tci_3+tci_13+tci_25+tci_38+tci_43+tci_52+tci_68+tci_74+tci_88+tci_93})
    spss_mnm$TUP6_TCI_RD3                            = with(spss_mnm, {tci_9+tci_20+tci_31+tci_50+tci_59+tci_73+tci_83+tci_95})
    spss_mnm$TUP6_TCI_RD4                            = with(spss_mnm, {tci_6+tci_21+tci_33+tci_55+tci_66+tci_82})
    spss_mnm$TUP6_TCI_RD                             = with(spss_mnm, {TUP6_TCI_RD1+TUP6_TCI_RD3+TUP6_TCI_RD4})
  } else {
    spss_mnm$TUP6_TCI_NS1                            = NA
    spss_mnm$TUP6_TCI_NS2                            = NA
    spss_mnm$TUP6_TCI_NS3                            = NA
    spss_mnm$TUP6_TCI_NS4                            = NA
    spss_mnm$TUP6_TCI_NS                             = spss_mnm$TCI_NS
    
    spss_mnm$TUP6_TCI_HA1                            = NA
    spss_mnm$TUP6_TCI_HA2                            = NA
    spss_mnm$TUP6_TCI_HA3                            = NA 
    spss_mnm$TUP6_TCI_HA4                            = NA
    spss_mnm$TUP6_TCI_HA                             = spss_mnm$TCI_HA
    
    spss_mnm$TUP6_TCI_RD1                            = NA
    spss_mnm$TUP6_TCI_RD3                            = NA
    spss_mnm$TUP6_TCI_RD4                            = NA
    spss_mnm$TUP6_TCI_RD                             = spss_mnm$TCI_RD
  }
  
  # NS1
  attr(spss_mnm$TUP6_TCI_NS1,'Variable_Name_Long') = 'TCI Explorative Excitability vs. Stoic Rigidity NS1'
  attr(spss_mnm$TUP6_TCI_NS1,'value.labels')       = NULL
  
  # NS2
  attr(spss_mnm$TUP6_TCI_NS2,'Variable_Name_Long') = 'TCI Impulsivity vs. Rumination NS2'
  attr(spss_mnm$TUP6_TCI_NS2,'value.labels')       = NULL
  
  # NS3
  attr(spss_mnm$TUP6_TCI_NS3,'Variable_Name_Long') = 'TCI Extravagance vs. Restraint NS3'
  attr(spss_mnm$TUP6_TCI_NS3,'value.labels')       = NULL
  
  
  # NS4
  attr(spss_mnm$TUP6_TCI_NS4,'Variable_Name_Long') = 'TCI Disorderliness vs. Orderliness NS4'
  attr(spss_mnm$TUP6_TCI_NS4,'value.labels')       = NULL
  
  # Novelty Seeking (NS)
  attr(spss_mnm$TUP6_TCI_NS,'Variable_Name_Long')  = 'TCI Novelty Seeking (NS) Total Score'
  attr(spss_mnm$TUP6_TCI_NS,'value.labels')        = NULL
  
  # Harm Avoidance (HA)
  # HA1
  # N Items: 11
  
  attr(spss_mnm$TUP6_TCI_HA1,'Variable_Name_Long')  = 'TCI Anticipatory worry vs. Extreme Optimism HA1'
  attr(spss_mnm$TUP6_TCI_HA1,'value.labels')        = NULL
  
  # HA2
  # N Items: 7
  
  attr(spss_mnm$TUP6_TCI_HA2,'Variable_Name_Long')  = 'TCI Fear of Uncertainty vs. Confidence HA2'
  attr(spss_mnm$TUP6_TCI_HA2,'value.labels')        = NULL
  
  # HA3
  # N Items: 8
  
  attr(spss_mnm$TUP6_TCI_HA3,'Variable_Name_Long')  = 'TCI Shyness/Shyness with Strangers vs. Sociability HA3'
  attr(spss_mnm$TUP6_TCI_HA3,'value.labels')        = NULL
  
  # HA4
  # N Items: 9
  
  attr(spss_mnm$TUP6_TCI_HA4,'Variable_Name_Long')  = 'TCI Fatigability/Fatigability and asthenia (weakness) vs. Vitality HA4'
  attr(spss_mnm$TUP6_TCI_HA4,'value.labels')        = NULL
  
  # Harm Avoidance (HA)
  
  attr(spss_mnm$TUP6_TCI_HA,'Variable_Name_Long')   = 'TCI Harm Avoidance Total Sum(HA)'
  attr(spss_mnm$TUP6_TCI_HA,'value.labels')         = NULL
  
  # Reward Dependence
  # RD1
  # N Items: 10
  
  attr(spss_mnm$TUP6_TCI_RD1,'Variable_Name_Long')  = 'TCI Sentimentality vs. Insensitivity RD1'
  attr(spss_mnm$TUP6_TCI_RD1,'value.labels')        = NULL
  
  warning('RD2 syntax is missing!!! Not needed?')
  
  # RD3
  # N Items: 8
  
  attr(spss_mnm$TUP6_TCI_RD3,'Variable_Name_Long')  = 'TCI Attachment vs. Detachment RD3'
  attr(spss_mnm$TUP6_TCI_RD3,'value.labels')        = NULL
  
  # RD4
  # N Items:6##/
  
  attr(spss_mnm$TUP6_TCI_RD4,'Variable_Name_Long')  = 'TCI Dependence on Approval by Others RD4'
  attr(spss_mnm$TUP6_TCI_RD4,'value.labels')        = NULL
  
  # Reward Dependence (RD) Total Score
  
  attr(spss_mnm$TUP6_TCI_RD,'Variable_Name_Long')   = 'TCI Reward Dependence Total Score (RD)'
  attr(spss_mnm$TUP6_TCI_RD,'value.labels')         = NULL
  
  ## SSSV (TAS/ DIS/ ES) ========================================================
  if (spss_files[[ss]]$site != 'Bonn') {
    # Mannheim probably wrote down item codes wrongly; one subscale was taken out so
    # only 30 instead of 40 items; however, somebody wrote down the items 1:30, so 
    # that ommitted items were ignored in counting item numbers
    if (spss_files[[ss]]$site == 'Mannheim') {
      source                                        = paste0('sss_',c(1:30))
      transl                                        = paste0('sss_',c(1,3,4,6,9,10:14,16:23,25,26,28:30,32,33,35:38,40))
      names(spss_mnm)[grep('sss_',names(spss_mnm))] = agk.recode(names(spss_mnm)[grep('sss_',names(spss_mnm))], source,transl)
    }
    
    # N Items: 10
    spss_mnm$TUP6_SSSV_TAS                           = with(spss_mnm,{sss_3+sss_11+sss_16+sss_17+sss_20+sss_21+sss_23+sss_28+sss_38+sss_40})
    spss_mnm$TUP6_SSSV_DIS                           = with(spss_mnm,{sss_1+sss_12+sss_13+sss_25+sss_29+sss_30+sss_32+sss_33+sss_35+sss_36})
    spss_mnm$TUP6_SSSV_ES                            = with(spss_mnm,{sss_4+sss_6+sss_9+sss_10+sss_14+sss_18+sss_19+sss_22+sss_26+sss_37})
    spss_mnm$TUP6_SSSV_TS                            = with(spss_mnm,{TUP6_SSSV_TAS+TUP6_SSSV_DIS+TUP6_SSSV_ES})
  } else {
    spss_mnm$TUP6_SSSV_TAS                           = spss_mnm$SSSV_TA
    spss_mnm$TUP6_SSSV_DIS                           = spss_mnm$SSSV_Dis
    spss_mnm$TUP6_SSSV_ES                            = spss_mnm$SSSV_E
    spss_mnm$TUP6_SSSV_TS                            = with(spss_mnm,{TUP6_SSSV_TAS+TUP6_SSSV_DIS+TUP6_SSSV_ES})
  }
  
  # Thrill & Adventure Seeking (TAS)
  attr(spss_mnm$TUP6_SSSV_TAS,'Variable_Name_Long') = 'SSSV Thrill & Adventure Seeking'
  attr(spss_mnm$TUP6_SSSV_TAS,'value.labels')       = NULL
  
  # Disinhibition (DIS)
  # N Items: 10
  attr(spss_mnm$TUP6_SSSV_DIS,'Variable_Name_Long') = 'SSSV Disinhibition'
  attr(spss_mnm$TUP6_SSSV_DIS,'value.labels')       = NULL
  
  # Experience Seeking (ES)
  # N Items: 10
  attr(spss_mnm$TUP6_SSSV_ES,'Variable_Name_Long') = 'SSSV Experience Seeking'
  attr(spss_mnm$TUP6_SSSV_ES,'value.labels')       = NULL
  
  # SSSV Total Score (TS)
  attr(spss_mnm$TUP6_SSSV_TS,'Variable_Name_Long') = 'SSSV Total Score'
  attr(spss_mnm$TUP6_SSSV_TS,'value.labels')       = NULL
  
  ## CONTINUE HERE FOR BONN ##
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
  
  # AUDIT Risk Score (RS)
  spss_mnm$TUP6_AUDIT_RS                            = NA
  # make a list of recoding rules
  recode_brackets      = list()
  recode_brackets[[1]] = c(0:7)
  recode_brackets[[2]] = c(8:15)
  recode_brackets[[3]] = c(16:19)
  recode_brackets[[4]] = c(20:100)
  
  recode_translations  = list()
  for (rr in 1:length(recode_brackets)) {
    recode_translations[[rr]] = rep(rr-1,length(recode_brackets[[rr]]))
  }
  rec_brack  = unlist(recode_brackets)
  rec_transl = unlist(recode_translations)
  # Recoding
  spss_mnm$TUP6_AUDIT_RS                            = with(spss_mnm, {as.numeric(agk.recode.c(TUP6_AUDIT_TS,rec_brack,rec_transl))})
  attr(spss_mnm$TUP6_AUDIT_RS,'Variable_Name_Long') = 'AUDIT Risk Score (RS)'
  attr(spss_mnm$TUP6_AUDIT_RS,'value.labels')       = NULL
  spss_mnm$TUP6_AUDIT_RS                            = factor(spss_mnm$TUP6_AUDIT_RS,levels=c(0,1,2,3),
                                                             labels=c('Low risk','Risky or hazardous level','High risk or harmful level','High risk' ))
  
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
  # recoding names
  names(spss_mnm) = gsub('PANAS_','pan_',names(spss_mnm))
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
  # Computation according to Mann/Ackermann
  spss_mnm$ocdm_12                                = with(spss_mnm, {apply(cbind(ocd_1, ocd_2),FUN = max,MARGIN = 1)})
  spss_mnm$ocdm_78                                = with(spss_mnm, {apply(cbind(ocd_7, ocd_8),FUN = max,MARGIN = 1)})
  spss_mnm$ocdm_910                               = with(spss_mnm, {apply(cbind(ocd_9, ocd_10),FUN = max,MARGIN = 1)})
  spss_mnm$ocdm_1314                              = with(spss_mnm, {apply(cbind(ocd_13, ocd_14),FUN = max,MARGIN = 1)})
  
  spss_mnm$TUP6_OCDS_TS_Mann                            = with(spss_mnm, {ocdm_12+ocd_3+ocd_4+ocd_5+ocd_6+ocdm_78+ocdm_910+ocd_11+ocd_12+ocdm_1314})
  attr(spss_mnm$TUP6_OCDS_TS_Mann,'Variable_Name_Long') = 'OCDS Total Score according to Mann/Ackermann'
  attr(spss_mnm$TUP6_OCDS_TS_Mann,'value.labels')       = NULL
  
  spss_mnm$TUP6_OCDS_Cog_Mann                            = with(spss_mnm, {ocdm_12+ocd_3+ocd_4+ocd_5+ocd_6})
  attr(spss_mnm$TUP6_OCDS_Cog_Mann,'Variable_Name_Long') = 'OCDS Cognitions according to Mann/Ackermann'
  attr(spss_mnm$TUP6_OCDS_Cog_Mann,'value.labels')       = NULL
  
  spss_mnm$TUP6_OCDS_Act_Mann                            = with(spss_mnm, {ocdm_78+ocdm_910+ocd_11+ocd_12+ocdm_1314})
  attr(spss_mnm$TUP6_OCDS_Act_Mann,'Variable_Name_Long') = 'OCDS Actions according to Mann/Ackermann'
  attr(spss_mnm$TUP6_OCDS_Act_Mann,'value.labels')       = NULL
  
  
  # Nach Nakovics et al#######################/
  # Berechnung der Scores ohne Item 7 und 8
  # Anmerkung Helmut: [We do not know who this is]
  # Gesamtscore kann zum Zwecke der Vergleichbarkeit beibehalten werden.
  # Die 2-Faktorlösung hat jedoch u.a. eine höhere Stabilität und Varianzaufklärung und sollte daher präferiert werden
  # maybe it is this paper: https://www.sciencedirect.com/science/article/pii/S0306460308001597 
  spss_mnm$TUP6_OCDS_TS_Nak                            = with(spss_mnm, {ocd_1+ocd_2+ocd_3+ocd_4+ocd_5+ocd_6+ocd_9+ocd_10+ocd_11+ocd_12+ocd_13+ocd_14})
  attr(spss_mnm$TUP6_OCDS_TS_Nak,'Variable_Name_Long') = 'OCDS Total Score without Items 7/8 according to Nakovics et al'
  attr(spss_mnm$TUP6_OCDS_TS_Nak,'value.labels')       = NULL
  
  spss_mnm$TUP6_OCDS_Cog_Nak                            = with(spss_mnm, {ocd_1+ocd_2+ocd_3+ocd_4+ocd_5+ocd_6})
  attr(spss_mnm$TUP6_OCDS_Cog_Nak,'Variable_Name_Long') = 'OCDS Cognition Score without Items 7/8 according to Nakovics et al'
  attr(spss_mnm$TUP6_OCDS_Cog_Nak,'value.labels')       = NULL
  
  spss_mnm$TUP6_OCDS_Act_Nak                            = with(spss_mnm, {ocd_9+ocd_10+ocd_11+ocd_12+ocd_13+ocd_14})
  attr(spss_mnm$TUP6_OCDS_Act_Nak,'Variable_Name_Long') = 'OCDS Action Score without Items 7/8 according to Nakovics et al'
  attr(spss_mnm$TUP6_OCDS_Act_Nak,'value.labels')       = NULL
  
  # Löschen der Hilfsvariablen 
  spss_mnm[c('ocdm_12','ocdm_78','ocdm_910','ocdm_1314')] = NULL
  
  
  ## URICA =======================================================================
  # recoding for Mannheim
  names(spss_mnm) = gsub('uri$','uri_1',names(spss_mnm))
  
  # checking if items are already coded into right direction; it seems yes
  # with(spss_mnm, {corr.test(cbind(uri_1,uri_5,uri_11,uri_13,uri_23,uri_26,uri_29))})
  
  # Precontemplation (PC)
  # N Items: 7 (Item 31 is omitted)
  spss_mnm$TUP6_URICA_PC                            = with(spss_mnm, {apply(cbind(uri_1,uri_5,uri_11,uri_13,uri_23,uri_26,uri_29),MARGIN = 1,FUN = mean)})
  attr(spss_mnm$TUP6_URICA_PC,'Variable_Name_Long') = 'URICA Precontemplation (PC)'
  attr(spss_mnm$TUP6_URICA_PC,'value.labels')       = NULL
  
  
  # Contemplation (C)
  # N Items: 7 (Item 4 is omitted)
  spss_mnm$TUP6_URICA_C                            = with(spss_mnm, {apply(cbind(uri_2,uri_8,uri_12,uri_15,uri_19,uri_21,uri_24),MARGIN = 1,FUN = mean)})
  attr(spss_mnm$TUP6_URICA_C,'Variable_Name_Long') = 'URICA Contemplation (C)'
  attr(spss_mnm$TUP6_URICA_C,'value.labels')       = NULL
  
  
  # Action (A)
  # N Items: 7 (Item 20 is omitted)
  spss_mnm$TUP6_URICA_A                            = with(spss_mnm, {apply(cbind(uri_3,uri_7,uri_10,uri_14,uri_17,uri_25,uri_30),MARGIN = 1, FUN = mean)})
  attr(spss_mnm$TUP6_URICA_A,'Variable_Name_Long') = 'URICA Action (A)'
  attr(spss_mnm$TUP6_URICA_A,'value.labels')       = NULL
  
  
  # Maintenance (M)
  # N Items: 7 (Item 9 is omitted)
  spss_mnm$TUP6_URICA_M                            = with(spss_mnm, {apply(cbind(uri_6+uri_16+uri_18+uri_22+uri_27+uri_28+uri_32),MARGIN = 1, FUN = mean)})
  attr(spss_mnm$TUP6_URICA_M,'Variable_Name_Long') = 'URICA Maintenance (M)'
  attr(spss_mnm$TUP6_URICA_M,'value.labels')       = NULL
  
  
  # Readiness to Change (RC)
  # RC = c + A + M - PC
  spss_mnm$TUP6_URICA_RC                            = with(spss_mnm, {TUP6_URICA_C + TUP6_URICA_A + TUP6_URICA_M - TUP6_URICA_PC})
  attr(spss_mnm$TUP6_URICA_RC,'Variable_Name_Long') = 'URICA Readiness to Change (RC)'
  attr(spss_mnm$TUP6_URICA_RC,'value.labels')       = NULL
  
  ## Alcohol Usage Questionnaire (AUQ) ===========================================
  if (spss_files[[ss]]$site == 'Mannheim' | spss_files[[ss]]$site == 'Bonn') {
    # AUQ before MRI session (AUQ_T1)
    # N Items: 8
    spss_mnm$TUP6_AUQ_T1                            = with(spss_mnm, {auq_1v+auq_2v+auq_3v+auq_4v+auq_5v+auq_6v+auq_7v+auq_8v})
    attr(spss_mnm$TUP6_AUQ_T1,'Variable_Name_Long') = 'AUQ before MRI session (T1)'
    attr(spss_mnm$TUP6_AUQ_T1,'value.labels')       = NULL
    
    
    # AUQ after MRI session (AUQ_T2)
    # N Items: 8
    spss_mnm$TUP6_AUQ_T2                            = with(spss_mnm, {auq_1n+auq_2n+auq_3n+auq_4n+auq_5n+auq_6n+auq_7n+auq_8n})
    attr(spss_mnm$TUP6_AUQ_T2,'Variable_Name_Long') = 'AUQ after MRI session (T2)'
    attr(spss_mnm$TUP6_AUQ_T2,'value.labels')       = NULL
    
    
    # AUQ Total Score (AUQ_TS)
    # N Items: 16
    spss_mnm$TUP6_AUQ_TS                            = with(spss_mnm, {TUP6_AUQ_T1+TUP6_AUQ_T2})
    attr(spss_mnm$TUP6_AUQ_TS,'Variable_Name_Long') = 'AUQ TOtal Score (TS)'
    attr(spss_mnm$TUP6_AUQ_TS,'value.labels')       = NULL
  } else {
    message('Berlin AUQ is still missing!!! Will probably need to take the sum score AUQ.')
    
    # AUQ before MRI session (AUQ_T1)
    # N Items: 8
    spss_mnm$TUP6_AUQ_T1                            = NA
    attr(spss_mnm$TUP6_AUQ_T1,'Variable_Name_Long') = 'AUQ before MRI session (T1)'
    attr(spss_mnm$TUP6_AUQ_T1,'value.labels')       = NULL
    
    
    # AUQ after MRI session (AUQ_T2)
    # N Items: 8
    spss_mnm$TUP6_AUQ_T2                            = NA
    attr(spss_mnm$TUP6_AUQ_T2,'Variable_Name_Long') = 'AUQ after MRI session (T2)'
    attr(spss_mnm$TUP6_AUQ_T2,'value.labels')       = NULL
    
    
    # AUQ Total Score (AUQ_TS)
    # N Items: 16
    spss_mnm$TUP6_AUQ_TS                            = NA
    attr(spss_mnm$TUP6_AUQ_TS,'Variable_Name_Long') = 'AUQ TOtal Score (TS)'
    attr(spss_mnm$TUP6_AUQ_TS,'value.labels')       = NULL
  }
  
  # packing the data
  spss_files[[ss]]$data = spss_mnm
  if (spss_files[[ss]]$site == 'Mannheim') {
    spss_files[[ss]]$data$TUP6_ID = spss_files[[ss]]$data$ID_initials_ngfn
  } else if (spss_files[[ss]]$site == 'Berlin') {
    spss_files[[ss]]$data$TUP6_ID = spss_files[[ss]]$data$ID
  } else if (spss_files[[ss]]$site == 'Bonn') {
    ## implement later
  } else {
    stop('Unknown site.')
  }
}

# packing the data
TUP6_psychosocial_data = spss_files[[1]]$data
TUP6_psychosocial_data = TUP6_psychosocial_data[grep('TUP6',names(TUP6_psychosocial_data))]
for (ss in 2:length(spss_files)) {
  cur_dat                = spss_files[[ss]]$data
  cur_dat                = cur_dat[grep('TUP6',names(cur_dat))]
  TUP6_psychosocial_data = rbind(TUP6_psychosocial_data,cur_dat)
}

View(TUP6_psychosocial_data)





# # # LDH
# # # ldh berlin or mannheim syntax usable
# # Sorry, also in Mannheim gibt es den Einschub zum UmKodieren von ein paar Items ganz am Anfang von der Syntax, nicht in Berlin. 
# # 
# # Von: Beck, Anne 
# # Gesendet: Donnerstag, 19. Juli 2018 15:33
# # An: Genauck, Alexander <alexander.genauck@charite.de>
# #   Cc: Rothkirch, Marcus <marcus.rothkirch@charite.de>
# #   Betreff: LDH
# # 
# # Hier liegt die LDH-Syntax von Mannheim:
# #   
# #   S:\AG\AG-Emotional-Neuroscience\Restricted\NGFN\NGFN plus\Datenmasken\LDH\LDH-Syntax Mannheim
# # 
# # Ich würde die NGFN13_SYNTAX_LDH_ANNE_14062011_neu nehmen, das war damals die Doktorandin in Mannheim.
# # 
# # Und hier liegt Berlin:
# #   
# #   S:\AG\AG-Emotional-Neuroscience\Restricted\NGFN\NGFN plus\Datenmasken\LDH
# # 
# # Hier scheint NGFN13_SYNTAX_LDH_Berlin_23.09.2011 identisch mit der Mannheimer, außer dass am Anfang wohl falsch kodierte Items umkodiert werden.
# # 
# # Ich würde also für Bonn einfach die Mannheimer Maske nehmen, weil da wahrscheinlich nichts umkodiert werden muss.
# # 
# # LG!
# #   Anne
