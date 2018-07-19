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
cur_file_bln = 'S:\AG\AG-Emotional-Neuroscience\Restricted\NGFN\NGFN plus\Datenmasken/NGFNplus_Datenmaske_Fragebogen_Berlin_05.03.13_KCH.sav'
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


/###################################################/
  /#################BIS-11############################/
  /###################################################/
  /########SCORE-BERECHNUNG###############/
  /###################################################/
  
  /#Aufmerksamkeitsbezogene Impulsivität (AI)#/
  /##AI##/
  /##N Items: 8##/
  
  COMPUTE BIS_AI = sum(bis_5, bis_6, bis_9, bis_11, bis_20, bis_24, bis_26, bis_28) .
VARIABLE LABELS BIS_AI 'BIS-11 Aufmerksamkeitsbezogene Impulsivität'.
EXECUTE.


/#Motorische Impulsivität (MI)#/
  /##MI##/
  /##N Items: 11##/
  
  COMPUTE BIS_MI = sum(bis_2, bis_3, bis_4, bis_16, bis_17, bis_19, bis_21, bis_22, bis_23, bis_25, bis_30) .
VARIABLE LABELS BIS_MI 'BIS-11 Motorische Impulsivität'.
EXECUTE.


/#Nichtplanende Impulsivität (NI)#/
  /##NI##/
  /##N Items: 11##/
  
  COMPUTE BIS_NI = sum(bis_1, bis_7, bis_8, bis_10, bis_12, bis_13, bis_14, bis_15, bis_18, bis_27, bis_29) .
VARIABLE LABELS BIS_NI 'BIS-11 Nichtplanende Impulsivität'.
EXECUTE.


/#BIS Total Score (TS)#/
  
  COMPUTE BIS_TS = sum(BIS_AI, BIS_MI, BIS_NI).
VARIABLE LABELS BIS_TS 'BIS-11 Total Score'.
EXECUTE.





/###################################################/
  /##################TCI#############################/
  /##################################################/
  /########SCORE-BERECHNUNG##############/
  /##################################################/
  
  /#Novelty Seeking (NS)#/
  
  /##NS1##/
  /##N Items: 11##/
  
  
  COMPUTE TCI_NS1 = sum(tci_1, tci_14, tci_22, tci_32, tci_41, tci_49, tci_60, tci_71, tci_80, tci_89, tci_99) .
VARIABLE LABELS TCI_NS1 'TCI Explorative Erregbarkeit vs. stoische Rigidität NS1'.
EXECUTE.


/##NS2##/
  /## N Items: 10##/
  
  COMPUTE TCI_NS2 = sum(tci_5, tci_16, tci_26, tci_37, tci_44, tci_54, tci_62, tci_77, tci_85, tci_98) .
VARIABLE LABELS TCI_NS2 'TCI Impulsivität vs. Nachdenklichkeit NS2'.
EXECUTE.


/##NS3##/
  /## N Items: 9##/
  
  COMPUTE TCI_NS3 = sum(tci_7, tci_17, tci_29, tci_45, tci_56, tci_65, tci_72, tci_81, tci_92) .
VARIABLE LABELS TCI_NS3 'TCI Überspanntheit vs. Zurückhaltung NS3'.
EXECUTE.


/##NS4##/
  /## N Items:10##/
  
  COMPUTE TCI_NS4 = sum(tci_15, tci_23, tci_34, tci_39, tci_46, tci_57, tci_70, tci_76, tci_86, tci_90) .
VARIABLE LABELS TCI_NS4 'TCI Unordentlichkeit vs. Organisiertheit NS4'.
EXECUTE.


/#Novelty Seeking (NS)#/
  
  COMPUTE TCI_NS = sum(TCI_NS1, TCI_NS2, TCI_NS3, TCI_NS4).
VARIABLE LABELS TCI_NS 'TCI Novelty Seeking (NS)'.
EXECUTE.

/##################################################/
  
  /#Harm Avoidance (HA)#/
  
  /##HA1##/
  /## N Items:11##/
  
  COMPUTE TCI_HA1 = sum(tci_2, tci_8, tci_18, tci_28, tci_36, tci_47, tci_51, tci_63, tci_69, tci_78, tci_94) .
VARIABLE LABELS TCI_HA1 'TCI Antizipatorische Sorgen und Pessimismus vs. ungehemmter Optimismus HA1'.
EXECUTE.


/##HA2##/
  /## N Items:7##/
  
  COMPUTE TCI_HA2 = sum(tci_4, tci_11, tci_30, tci_53, tci_64, tci_79, tci_91).
VARIABLE LABELS TCI_HA2 'TCI Angst vor dem Ungewissen vs. Zuversicht HA2'.
EXECUTE.


/##HA3##/
  /## N Items:8##/
  
  COMPUTE TCI_HA3 = sum(tci_12, tci_24, tci_35, tci_42, tci_58, tci_67, tci_87, tci_96).
VARIABLE LABELS TCI_HA3 'TCI Schüchternheit gegenüber Fremden vs. Geselligkeit HA3'.
EXECUTE.


/##HA4##/
  /## N Items:9##/
  
  COMPUTE TCI_HA4 = sum(tci_10, tci_19, tci_27, tci_40, tci_48, tci_61, tci_75, tci_84, tci_97).
VARIABLE LABELS TCI_HA4 'TCI Ermüdbarkeit vs. Vitalität HA4'.
EXECUTE.


/#Harm Avoidance (HA)#/
  
  COMPUTE TCI_HA = sum(TCI_HA1, TCI_HA2, TCI_HA3, TCI_HA4).
VARIABLE LABELS TCI_HA 'TCI Harm Avoidance (HA)'.
EXECUTE.

/##################################################/
  
  /#Reward Dependence (RD)#/
  
  /##RD1##/
  /## N Items:10##/
  
  COMPUTE TCI_RD1 = sum(tci_3, tci_13, tci_25, tci_38, tci_43, tci_52, tci_68, tci_74, tci_88, tci_93) .
VARIABLE LABELS TCI_RD1 'TCI Empfindsamkeit vs. Unempfindlichkeit RD1'.
EXECUTE.


/##RD3##/
  /## N Items:8##/
  
  COMPUTE TCI_RD3 = sum(tci_9, tci_20, tci_31, tci_50, tci_59, tci_73, tci_83, tci_95) .
VARIABLE LABELS TCI_RD3 'TCI Attachment vs. Detachment RD3'.
EXECUTE.


/##RD4##/
  /## N Items:6##/
  
  COMPUTE TCI_RD4 = sum(tci_6, tci_21, tci_33, tci_55, tci_66, tci_82) .
VARIABLE LABELS TCI_RD4 'TCI Abhängigkei vs. Unabhängigkeit RD4'.
EXECUTE.


/#Reward Dependence (RD)#/
  
  COMPUTE TCI_RD = sum(TCI_RD1, TCI_RD3, TCI_RD4).
VARIABLE LABELS TCI_RD 'TCI Reward Dependence (RD)'.
EXECUTE.





/###################################################/
  /#########SSSV (TAS/ DIS/ ES)#################/
  /##################################################/
  /########SCORE-BERECHNUNG##############/
  /##################################################/
  
  /#Thrill & Adventure Seeking (TAS)#/
  /##N Items: 10##/
  
  COMPUTE SSSV_TAS = sum(sss_3,sss_11,sss_16,sss_17,sss_20,sss_21,sss_23,sss_28,sss_38,sss_40) .
VARIABLE LABELS SSSV_TAS 'SSSV Thrill & Adventure Seeking'.
EXECUTE.


/#Disinhibition (DIS)#/
  /##N Items: 10##/
  
  COMPUTE SSSV_DIS = sum(sss_1,sss_12,sss_13,sss_25,sss_29,sss_30,sss_32,sss_33,sss_35,sss_36) .
VARIABLE LABELS SSSV_DIS 'SSSV Disinhibition'.
EXECUTE.


/#Experience Seeking (ES)#/
  /##N Items: 10##/
  
  COMPUTE SSSV_ES = sum(sss_4,sss_6,sss_9,sss_10,sss_14,sss_18,sss_19,sss_22,sss_26,sss_37) .
VARIABLE LABELS SSSV_ES 'SSSV Experience Seeking'.
EXECUTE.


/#SSSV Total Score (TS)#/
  
  COMPUTE SSSV_TS = sum(SSSV_TAS, SSSV_DIS, SSSV_ES).
VARIABLE LABELS SSSV_TS 'SSSV Total Score'.
EXECUTE.





/###################################################/
  /#########NEO-FFI (60 Items)#################/
  /##################################################/
  /########SCORE-BERECHNUNG##############/
  /##################################################/
  
  /#Neurotizismus (N Items: 12)#/
  
  COMPUTE NEO_N = sum(neo_1, neo_6, neo_11, neo_16, neo_21, neo_26, neo_31, neo_36, neo_41, neo_46, neo_51, neo_56) .
VARIABLE LABELS NEO_N 'NEO-FFI Neurotizismus'.
EXECUTE.


/#Extraversion (N Items: 12)#/
  
  COMPUTE NEO_E = sum(neo_2, neo_7, neo_12, neo_17, neo_22, neo_27, neo_32, neo_37, neo_42, neo_47, neo_52, neo_57) .
VARIABLE LABELS NEO_E 'NEO-FFI Extraversion'.
EXECUTE.


/#Offenheit/ Openess to Experiences (N Items: 12)#/
  
  COMPUTE NEO_O = sum(neo_3, neo_8, neo_13, neo_18, neo_23, neo_28, neo_33, neo_38, neo_43, neo_48, neo_53, neo_58) .
VARIABLE LABELS NEO_O 'NEO-FFI Offenheit'.
EXECUTE.


/#Verträglichkeit/ Agreeableness (N Items: 12)#/
  
  COMPUTE NEO_V = sum(neo_4, neo_9, neo_14, neo_19, neo_24, neo_29, neo_34, neo_39, neo_44, neo_49, neo_54, neo_59) .
VARIABLE LABELS NEO_V 'NEO-FFI Verträglichkeit'.
EXECUTE.


/#Gewissenhaftigkeit/ Conscientiousness (N Items: 12)#/
  
  COMPUTE NEO_G = sum(neo_5, neo_10, neo_15, neo_20, neo_25, neo_30, neo_35, neo_40, neo_45, neo_50, neo_55, neo_60) .
VARIABLE LABELS NEO_G 'NEO-FFI Gewissenhaftigkeit'.
EXECUTE.





/###################################################/
  /#########AUDIT############################/
  /##################################################/
  /########SCORE-BERECHNUNG##############/
  /##################################################/
  
  COMPUTE Audit_TS = sum(aud_1, aud_2, aud_3, aud_4, aud_5, aud_6, aud_7, aud_8, aud_9, aud_10).
VARIABLE LABELS Audit_TS 'AUDIT Total Score'.
EXECUTE .


RECODE Audit_TS (0 thru 7=0) (8 thru 15=1) (16 thru 19=2) (20 thru Highest=3) (ELSE=SYSMIS) INTO audit_RiskCat. 
VARIABLE LABELS  Audit_RiskCat 'AUDIT Risiko Kategorien'. 
VALUE LABELS Audit_RiskCat 0 'Low risk' 1 'Risky or hazardous level' 2 'High risk or harmful level' 3 'High risk' .
EXECUTE.

/#Consumption Score (N Items: 3)#/
  
  COMPUTE Audit_CS = sum(aud_1, aud_2, aud_3) .
VARIABLE LABELS Audit_CS 'AUDIT-Consumption Score'.
EXECUTE.

/#Dependence Score (N Items: 3)#/
  
  COMPUTE Audit_DS = sum(aud_4,aud_5, aud_6).
VARIABLE LABELS Audit_DS 'AUDIT-Dependence Score'.
EXECUTE.

/#Alcohol-related Problems Score (N Items: 4)#/
  
  COMPUTE Audit_AS = sum(aud_7,aud_8, aud_9, aud_10).
VARIABLE LABELS Audit_AS 'AUDIT-Alcohol-related Problems Score'.
EXECUTE.





/###################################################/
  /#########Sense of Coherence (SOC-13)##########/
  /##################################################/
  /########SCORE-BERECHNUNG##############/
  /##################################################/
  
  /#SOC Total Score (TS)#/
  /##N Items: 13##/
  
  COMPUTE SOC_TS = sum(soc_1, soc_2, soc_3, soc_4, soc_5, soc_6, soc_7, soc_8, soc_9, soc_10, soc_11, soc_12, soc_13).
VARIABLE LABELS SOC_TS 'SOC-13 Total Score'.
EXECUTE.





/###################################################/
  /##################PANAS#########################/
  /##################################################/
  /########SCORE-BERECHNUNG##############/
  /##################################################/
  
  /#Positiver Affekt (PA)#/
  /##N Items: 10##/
  
  COMPUTE PANAS_PA = sum(PANAS_1, PANAS_3, PANAS_4, PANAS_6, PANAS_10, PANAS_11, PANAS_13, PANAS_15, PANAS_17, PANAS_18).
VARIABLE LABELS PANAS_PA 'PANAS_Positiver Affekt'.
EXECUTE.


/#Negativer Affekt (NA)#/
  /##N Items: 10##/
  
  COMPUTE PANAS_NA = sum(PANAS_2, PANAS_5, PANAS_7, PANAS_8, PANAS_9, PANAS_12, PANAS_14, PANAS_16, PANAS_19, PANAS_20).
VARIABLE LABELS PANAS_NA 'PANAS_Negativer Affekt'.
EXECUTE.





/##################################################/
  /####################ADS#########################/
  /##################################################/
  /########SCORE-BERECHNUNG##############/
  /##################################################/
  
  COMPUTE ADS_TS = sum(ads_1, ads_2,  ads_3, ads_4, ads_5, ads_6,  ads_7,  ads_8, ads_9, ads_10,  ads_11, ads_12, ads_13,  ads_14,  ads_15, ads_16, ads_17, ads_18,  ads_19,  ads_20,  ads_21, ads_22,  ads_23, ads_24,  ads_25).
VARIABLE LABELS ADS_TS 'ADS Total Score'.
EXECUTE.





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


/#############################################/
  
  /#Summenscore AUQ #/
  
  COMPUTE AUQ_A_Score=SUM(auqa_1,auqa_2,auqa_3,auqa_4,auqa_5,auqa_6,auqa_7,auqa_8).
EXECUTE.

COMPUTE AUQ_B_Score=SUM(auqb_1,auqb_2,auqb_3,auqb_4,auqb_5,auqb_6,auqb_7,auqb_8).
EXECUTE.

COMPUTE AUQ_Gesamt_Score=SUM(auqa_1,auqa_2,auqa_3,auqa_4,auqa_5,auqa_6,auqa_7,auqa_8,auqb_1,auqb_2,auqb_3,auqb_4,auqb_5,auqb_6,auqb_7,auqb_8).
EXECUTE.




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