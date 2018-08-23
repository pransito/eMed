## PREAMBLE ###################################################################
# Script to read in LDH data from SPSS table and compute LDH total scores
# Scoring of LDH according to Harvey A. Skinner's LDH: Administration & Scoring Guidelines (1979)

## LIBRARIES
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


## PATHS TO SPSS FILES ==========================================================
# get the data
# NB: There are several SPSS data files for Mannheim. It is unclear in what respect they differ.
# NB: For now taking the most recent document.
# syntax used: 'S:/AG/AG-Emotional-Neuroscience/Restricted/NGFN/NGFN plus/Datenmasken/LDH/LDH-Syntax Mannheim/NGFN13_SYNTAX_LDH_ANNE_14062011_neu.sps'

cur_file_mnm = 'S:/AG/AG-Emotional-Neuroscience/Restricted/NGFN/NGFN plus/Datenmasken/LDH/LDH-Syntax Mannheim/ldh_gesamt_01-55+2001-2005_korrigiert_15.06.2011.sav'
cur_file_bnn = ''
cur_file_bln = ''

spss_files      = list()
cur_f           = list()
cur_f$file      = cur_file_mnm
cur_f$site      = 'Mannheim'
spss_files[[1]] = cur_f
cur_f$file      = cur_file_bln 
cur_f$site      = 'Berlin'
spss_files[[2]] = cur_f

for (ss in 1:length(spss_files)) {
  ## GET DATA ===================================================================
  spss_mnm = as.data.frame(read.spss(spss_files[[ss]]$file,use.value.labels = F))
  
  # recode all variable names to lower case
  names(spss_mnm) = tolower(names(spss_mnm))
  
  ## RECODING OF VARIABLES FOR MANNHEIM DUE TO INCORRECTLY NAMED VARIABLES ('ldg' -> 'ldh')
  source          = 'ldg_'
  transl          = 'ldh_'
  names(spss_mnm) = gsub(source,transl,names(spss_mnm))
  
  ## GET PHASE LENGTHS FOR ALL DRINKING PHASES ==================================
  # get all phase starts
  phase_starts = names(spss_mnm)[grep('ldh_za',names(spss_mnm))]
  phase_ends   = names(spss_mnm)[grep('ldh_ze',names(spss_mnm))]
  stopifnot(length(phase_starts) == length(phase_ends))
  
  for (pp in 1:length(phase_starts)) {
    # put day information on phase start variable
    spss_mnm[phase_starts[pp]] = paste0('01/',spss_mnm[[phase_starts[pp]]])
    
    # recode date of phase start into machine-readible date format
    spss_mnm[phase_starts[pp]] = as.Date(spss_mnm[[phase_starts[pp]]],format = '%d/%m/%Y')
    
    # find the according phase_end
    cur_ps  = phase_starts[pp]
    cur_ps  = strsplit(cur_ps,'ldh_za')
    cur_key = paste0('ldh_ze',cur_ps[[1]][2])
    
    # put day information on phase end variable
    spss_mnm[cur_key] = paste0('01/',spss_mnm[[cur_key]])
    
    # recode date of phase end into machine-readible date format
    spss_mnm[cur_key] = as.Date(spss_mnm[[cur_key]],format = '%d/%m/%Y')
    
    # compute phase length in months
    cur_phase_name             = paste0('Ph',cur_ps[[1]][2],'_length')
    spss_mnm[[cur_phase_name]] = (spss_mnm[[cur_key]]-spss_mnm[[phase_starts[pp]]])/30.41667
    spss_mnm[[cur_phase_name]] = round(as.numeric(spss_mnm[[cur_phase_name]]))
    
    # Variable Lable
    attr(spss_mnm[[cur_phase_name]],'Variable_Name_Long') = paste('Phase Length',pp)
    
    print(spss_mnm[cur_phase_name])
    
  }
  
  ## During phases of abstinence  number of mean and maximum consumption days need to be set to 0
  trs          = names(spss_mnm)[grep('ldh_tr',names(spss_mnm))]
  dtgs         = names(spss_mnm)[grep('ldh_dtg',names(spss_mnm))]
  mtgs         = names(spss_mnm)[grep('ldh_mtg',names(spss_mnm))]
  stopifnot(length(trs) == length(dtgs) & length(dtgs) == length(mtgs))
  
  for (tt in 1:length(trs)) {
    spss_mnm[[dtgs[tt]]][spss_mnm[[trs[tt]]] == 5] = 0
    spss_mnm[[mtgs[tt]]][spss_mnm[[trs[tt]]] == 5] = 0
  }
  
  
  ## BERECHNUNG DES "MONTHLY DRINKING TOTAL"***
  ## Hilfsvariable: Gesamtalkoholmenge pro Tag (g) pro Phase getrennt nach Durchschnitt (d) & Maximalkonsum (m)*****
  dbgs         = names(spss_mnm)[grep('ldh_dbg',names(spss_mnm))]
  dwgs         = names(spss_mnm)[grep('ldh_dwg',names(spss_mnm))]
  dsgs         = names(spss_mnm)[grep('ldh_dsg',names(spss_mnm))]
  drgs         = names(spss_mnm)[grep('ldh_drg',names(spss_mnm))]
  
  mbgs         = names(spss_mnm)[grep('ldh_mbg',names(spss_mnm))]
  mwgs         = names(spss_mnm)[grep('ldh_mwg',names(spss_mnm))]
  msgs         = names(spss_mnm)[grep('ldh_msg',names(spss_mnm))]
  mrgs         = names(spss_mnm)[grep('ldh_mrg',names(spss_mnm))]
  
  d            = spss_mnm
  
  for (tt in 1:length(dbgs)) {
    new_var_name                                          = paste0('Ph',tt,'_alkamount_d')
    spss_mnm[[new_var_name]]                              = d[dbgs[[tt]]]+d[dwgs[[tt]]]+d[dsgs[[tt]]]+d[drgs[[tt]]]
    attr(spss_mnm[[new_var_name]],'Variable_Name_Long')   = paste('Alcohol intake on an average day',pp)
    
    new_var_name                                          = paste0('Ph',tt,'_alkamount_m')
    spss_mnm[[new_var_name]]                              = d[mbgs[[tt]]]+d[mwgs[[tt]]]+d[msgs[[tt]]]+d[mrgs[[tt]]]
    attr(spss_mnm[[new_var_name]],'Variable_Name_Long')   = paste('Alcohol intake on a maximum day',pp)
  }
  
  # continue at line 1336 ***BEREINIGUNG MÖGLICHER ANGABEFEHLER: Wenn Maximalkonsummenge kleiner als Durchschnittskonsummenge, ersetze Maximalkonsummenge mit Null*** 
  IF(Ph1_alkamount_m<Ph1_alkamount_d) Ph1_alkamount_m=0.
  


}