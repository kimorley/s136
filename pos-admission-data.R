# processing PoS data - information collected at ADMISSION ONLY
# KIMorley | last update: 14/12/2017
#---------------------------------------------------------------


# setup #####

rm(list=ls())
setwd("T:/Katherine Morley/S136") 

library(data.table)
library(ggplot2)

today <- substr(Sys.time(),1,10)

#---------------------------
# create core data set #####
#---------------------------

uds <- read.csv(file='T:/Katherine Morley/S136/SQL-direct-output/urine_screen_136episode_131117.csv',
                           header=TRUE)
uds$Discharge_Date <- as.Date(uds$Discharge_Date, format='%d/%m/%Y')
uds$Accepted_Date <- as.Date(uds$Accepted_Date, format='%d/%m/%Y')

corepos1 <- read.csv(file='T:/Katherine Morley/S136/SQL-direct-output/pos-dump-22Nov17.csv',
                     header=TRUE)
corepos1$exit_date <- as.Date(substr(corepos1$ViewText,1,10), format="%d/%m/%Y")

#corepos2 <- read.csv(file='T:/Katherine Morley/S136/SQL-direct-output/episodes_from010117.csv',
#                     header=TRUE)
corepos <- merge(subset(uds, select=c('BrcId','Accepted_Date','Discharge_Date')), 
                 subset(corepos1, select=c('BrcId','Discharge_Destination_ID','exit_date','Spell_Number',
                                           'Movement_Type_ID','Referral_Admin_Status_ID','Rejection_Reason_ID','Rejection_Detail')), 
                        by.x=c('BrcId','Discharge_Date'), by.y=c('BrcId','exit_date'), all=TRUE)

corepos <- data.table(corepos[!is.na(corepos$BrcId),])
setkey(corepos, BrcId)

# first entry date for each patient

pos_entry <- corepos[, .SD[which.min(Accepted_Date)], by = BrcId]
pos_entry <- subset(pos_entry, select=c('BrcId','Accepted_Date','Discharge_Date'))
names(pos_entry) <- c('BrcId','index_date','exit_date')

# identify second admission date

temp <- merge(corepos, pos_entry, by='BrcId', all=TRUE)
temp <- temp[ (Referral_Admin_Status_ID!='Rejected') & (Accepted_Date > exit_date) ]
pos_readmit <- temp[, .SD[which.min(Accepted_Date)], by = BrcId]
pos_readmit <- subset(pos_readmit, select=c('BrcId','Accepted_Date'))
names(pos_readmit)[2] <- 'readmit_date'

# combine data

posInfo <- data.table(merge(pos_entry, pos_readmit, by='BrcId', all=TRUE))
posInfo[ , posTime:=exit_date-index_date]
posInfo[ , readmitTime:=readmit_date-exit_date]

#------------------------
# sociodemographics #####
#------------------------

load("T:/Katherine Morley/S136/Data/sociodemo-S136.RData")

# [1] core patient info (no entry date or duplicates as this is centralised and updated with each patient contact)

posad <- data.table(merge(posInfo, sociodemo, by='BrcId', all=TRUE))

# ETHNICITY - ONS categories

posad[ ethnicitycleaned %in% c('British (A)', 'Irish (B)',
                                   'Any other white background (C)'), ethnicity:= 'White']
posad[ ethnicitycleaned %in% c('White and black Caribbean (D)', 
                                   'White and Black African (E)','White and Asian (F)',
                                   'Any other mixed background (G)'), ethnicity:= 'Mixed']
posad[ ethnicitycleaned %in% c('Indian (H)','Pakistani (J)', 'Bangladeshi (K)',
                                   'Chinese (R)', 'Any other Asian background (L)'), ethnicity:= 'Asian']
posad[ ethnicitycleaned %in% c('Caribbean (M)','African (N)',
                                   'Any other black background (P)'), ethnicity:= 'Black']
posad[ ethnicitycleaned %in% c('Any other ethnic group (S)'), ethnicity:= 'Other']
posad[ is.na(ethnicity), ethnicity:= 'Unknown']
posad[ , ethnicity:=factor(ethnicity)]

# DOB

posad[ , dob:=as.Date(cleaneddateofbirth, format='%Y-%m-%d')]

# clean-up

posad[ , c('entry_date', 'cleaneddateofbirth','ethnicitycleaned'):=NULL]
rm('sociodemo')

# [2] housing situation from brief risk screen

names(briefRS)[3:4] <- c('brsDate','brsHousing')

temp <- data.table(merge(subset(posInfo, select=c('BrcId','index_date','exit_date')), 
                         subset(briefRS, select=c('BrcId','brsDate','brsHousing')), by='BrcId', all=TRUE))
temp[ , brsDate:= as.Date(brsDate, format='%Y-%m-%d')]
temp[ brsDate >= index_date & brsDate <= (exit_date+1), posBrs:=TRUE]
posad <- data.table(merge(posad, temp[posBrs=='TRUE', c('BrcId','brsHousing')], by='BrcId', all=T))

rm('briefRS')
rm('riskass')
rm('TOP')
rm('NDTMS')
rm('temp')

#------------------------
# substance use data ####
#------------------------

# combining data from multiple tables here as I'm not sure which ones
# are routinely used by PoS staff
# NOTE: only taking data recording within PoS admission window
#       but there are lots more data for these guys outside this window

load("T:/Katherine Morley/S136/Data/substance-S136.RData")

# [1] Current drug and alcohol table

temp <- data.table(merge(subset(posInfo, select=c('BrcId','index_date','exit_date')), 
                         Current, by='BrcId', all=TRUE))
temp[ , cdaDate:= as.Date(Todays_Date, format='%Y-%m-%d')]
temp[ cdaDate >= index_date & cdaDate <= (exit_date+1), posCda:=TRUE]

# there are 355 instances where the CDA table was used during a PoS
# admission - selecting these for further processing (312 patients)
# remove those with no info apart from date ( patients)

temp <- temp[posCda==TRUE & !is.na(Substance_1_ID)]

# data collection is not by substance so need to convert

drugList <- c()
for (i in 4:13){
  drugList <- c(drugList,names(table(temp[,i, with=F])))
}
drugList <- unique(drugList)

#[1] "Alcohol"                   "Alochol"                   "Amphetamines"             
#[4] "Benzodiazepines"           "Buprenorphine (Subutex)"   "Cannabis"                 
#[7] "Cannabis Herbal (Skunk)"   "Cocaine"                   "Crack Cocaine"            
#[10] "Diazepam"                  "Ecstasy"                   "Heroin"                   
#[13] "Khat"                      "Mephedrone"                "Methadone"                
#[16] "Methamphetamine"           "Minor Analgesics"          "Other"                    
#[19] "Other Opiates"             "Solvents"                  "Stimulants Unspecified"   
#[22] "Substance Free"            "Substances Free"           "Cocaine Hydrochloride"    
#[25] "GHB/GBH"                   "Hallucinogens Unspecified" "Ketamine"                 
#[28] "MDMA"                      "Methadone Mixture"         "Other Hallucinogens"      
#[31] "Codeine"                   "Hemineverin"               "Opium"                    
#[34] "Other Hallucinogen" 

dataCols <- grep('Substance', names(temp), value=T)

temp[apply(subset(temp, select=c(dataCols)), 1, 
              function(i) !identical(grep('Alcohol',i, ignore.case=T),integer(0))), 
        cdaAlcohol:=TRUE]
temp[apply(subset(temp, select=c(dataCols)), 1, 
           function(i) !identical(grep('Alochol',i, ignore.case=T),integer(0))), 
     cdaAlcohol:=TRUE]

temp[apply(subset(temp, select=c(dataCols)), 1, 
           function(i) !identical(grep('Amphetamines',i, ignore.case=T),integer(0))), 
     cdaAmph:=TRUE]

temp[apply(subset(temp, select=c(dataCols)), 1, 
           function(i) !identical(grep('Benzo',i, ignore.case=T),integer(0))), 
     cdaBenzo:=TRUE]

temp[apply(subset(temp, select=c(dataCols)), 1, 
           function(i) !identical(grep('Cannabis',i, ignore.case=T),integer(0))), 
     cdaCannabis:=TRUE]

temp[apply(subset(temp, select=c(dataCols)), 1, 
           function(i) !identical(grep('Cocaine',i, ignore.case=T),integer(0))), 
     cdaCocaine:=TRUE]

temp[apply(subset(temp, select=c(dataCols)), 1, 
           function(i) !identical(grep('Halluc',i, ignore.case=T),integer(0))), 
     cdaHalluc:=TRUE]

temp[apply(subset(temp, select=c(dataCols)), 1, 
           function(i) !identical(grep('Free',i, ignore.case=T),integer(0))), 
     cdaNone:=TRUE]

temp[ is.na(cdaAlcohol) & is.na(cdaAmph) & is.na(cdaBenzo) & is.na(cdaCannabis) 
      & is.na(cdaCocaine) & is.na(cdaNone), cdaOther:=TRUE]

temp <- subset(temp, select=c('BrcId', 'cdaDate','cdaAlcohol',
                      'cdaAmph','cdaBenzo','cdaCannabis',
                      'cdaCocaine','cdaHalluc','cdaNone', 'cdaOther'))

posad <- data.table(merge(posad, temp, by='BrcId', all=TRUE))
rm(list=c('temp','dataCols','drugList'))

# [2] parse diagnosis table for substance use and MH conditions at same time

temp <- data.table(merge(subset(posInfo, select=c('BrcId','index_date','exit_date')), 
                         icd10, by='BrcId', all=TRUE))
temp[ , diagDate:= as.Date(Diagnosis_Date, format='%Y-%m-%d')]
temp[ diagDate >= index_date & diagDate <= (exit_date+1), posDiag:=TRUE]

# there are 190 instances where the diagnosis table was used during a PoS
# admission - selecting these for further processing (144 patients)

temp <- temp[posDiag==TRUE]

# data collection is not by substance so need to convert


dataCols <- c('Primary_Diag',grep('Secondary', names(temp), value=T))
temp[ , (dataCols) := lapply(.SD, function(x){substr(x,1,3)}), .SDcols=(dataCols)]

codeList <- unique(c(names(table(temp$Primary_Diag)), names(table(temp$Secondary_Diag_1)),
                   names(table(temp$Secondary_Diag_2)), names(table(temp$Secondary_Diag_3)),
                   names(table(temp$Secondary_Diag_4)), names(table(temp$Secondary_Diag_5)), 
                   names(table(temp$Secondary_Diag_6))))
codeList <- codeList[codeList!='#4.'] # remove 'inadequate parental supervision'                   

for (j in codeList){
  temp[apply(subset(temp, select=c(dataCols)), 1, 
             function(i) !identical(grep(j,i),integer(0))), (j):=TRUE]
}

temp <- subset(temp, select=c('BrcId','diagDate', codeList))
posad <- data.table(merge(posad, temp, by='BrcId', all=TRUE))
rm(list=c('temp','dataCols'))

# AUDIT

temp <- data.table(merge(subset(posInfo, select=c('BrcId','index_date','exit_date')), 
                         AUDIT, by='BrcId', all=TRUE))
temp[ ,auditDate:= as.Date(A_Audit_Assessment_Date, format='%Y-%m-%d')]
temp[ auditDate >= index_date & auditDate <= exit_date, posAudit:=TRUE]

# there are 580 instances where the CDA table was used during a PoS
# admission - selecting these for further processing (503 patients)
# remove those with no info apart from date (now 344 patients)

temp <- temp[posAudit==TRUE & !is.na(AAudit_Total_Score) & !is.na(AAudit_Risk_Cat)]
temp <- subset(temp, select=c('BrcId','auditDate','AAudit_Total_Score',
                                        'AAudit_Risk_Cat'))
levels(temp$AAudit_Risk_Cat)[6:8] <- 'Lower risk'
levels(temp$AAudit_Risk_Cat)[1] <- 'Lower risk'
temp[ , AAudit_Risk_Cat:=droplevels(AAudit_Risk_Cat)]

posad <- data.table(merge(posad, temp, by='BrcId', all=TRUE))
rm(list=c('temp','AUDIT','con','NDTMS','icd10','TOP'))

# [3] UDS

uds <- data.table(read.csv(file='T:/Katherine Morley/S136/SQL-direct-output/urine_screen_136episode_131117.csv',
                           header=TRUE))
uds <- uds[, c('BrcId','Urine_Screen_Assessment_Date','Urine_Test_Type',
               'Opiates','Methadone','Benzos','Barbiturates','Cannabis','Cocaine','Amphetamines','Buprenorphine')]

temp <- merge(posInfo, uds, by='BrcId', all.y=TRUE)
temp <- temp[!is.na(BrcId)]
temp[ , udsDate:= as.Date(Urine_Screen_Assessment_Date, format='%d/%m/%Y')]
temp <- unique(temp[ (udsDate >= index_date) & (udsDate <= exit_date)])
temp[ , c('index_date','exit_date','readmit_date','posTime','readmitTime','Urine_Screen_Assessment_Date','Urine_Test_Type'):=NULL]
temp <- temp[, .SD[which.min(udsDate)], by = BrcId]
posad <- data.table(merge(posad, temp, by='BrcId', all=TRUE))
rm(list=c('temp','uds'))

#------------------------
# mental health data ####
#------------------------

# cross-checking with diagnosis table

load("T:/Katherine Morley/S136/Data/mentalhealth-S136.RData")

temp <- data.table(merge(subset(posInfo, select=c('BrcId','index_date','exit_date')), 
                         psychosis, by='BrcId', all=TRUE))
temp[ , psychosisDate:= as.Date(Diagnosis_Date, format='%Y-%m-%d')]
temp[ psychosisDate >= index_date & psychosisDate <= (exit_date+1), posPsyc:=TRUE]
temp <- subset(temp, posPsyc==TRUE, select=c('BrcId','psychosisDate'))
posad <- data.table(merge(posad, temp, by='BrcId', all=TRUE))

temp <- data.table(merge(subset(posInfo, select=c('BrcId','index_date','exit_date')), 
                         mania, by='BrcId', all=TRUE))
temp[ , maniaDate:= as.Date(Diagnosis_Date, format='%Y-%m-%d')]
temp[ maniaDate >= index_date & maniaDate <= (exit_date+1), posPsyc:=TRUE]
temp <- subset(temp, posPsyc==TRUE, select=c('BrcId','maniaDate'))
posad <- data.table(merge(posad, temp, by='BrcId', all=TRUE))

temp <- data.table(merge(subset(posInfo, select=c('BrcId','index_date','exit_date')), 
                         depression, by='BrcId', all=TRUE))
temp[ , depressionDate:= as.Date(Diagnosis_Date, format='%Y-%m-%d')]
temp[ depressionDate >= index_date & depressionDate <= (exit_date+1), posPsyc:=TRUE]
temp <- subset(temp, posPsyc==TRUE, select=c('BrcId','depressionDate'))
posad <- data.table(merge(posad, temp, by='BrcId', all=TRUE))

#------------------------
# consolidate data ####
#------------------------

# consolidate mental health

posad[ !is.na(psychosisDate) | (F23), diagPsychosis:=TRUE] # includes schizophrenia and brief episode
posad[ !is.na(maniaDate) | (F31) , diagMania:=TRUE] # includes BPD
posad[ !is.na(depressionDate) | (F32) | (F33) | (F41), diagAnxDep:=TRUE]
posad[ (F43) | (F60) | (F61) | (Z71) | (F06) | (F70) | (F84) , diagOther:=TRUE] # includes PTSD, Borderline personality disorder, ID, autism and aspergers

posad[ (F10) | (F11) | (F12) | (F14) | (F15) | (F16) | (F19), diagSUD:=TRUE]

posad[ is.na(diagSUD) & is.na(diagPsychosis) & is.na(diagMania) & is.na(diagAnxDep) & is.na(diagOther), noMHdiag:=TRUE]

# consolidate substance use

posad[ (cdaAlcohol) | (F10) | 
         AAudit_Risk_Cat %in% c('Harmful/higher risk','Hazardous/increasing risk','Possible/dependence'),
       suAlcohol:=TRUE]
posad[ Cannabis=='Negative', suCannabis:=FALSE]
posad[ (cdaCannabis) | (F12) | Cannabis=='Positive', suCannabis:=TRUE]
posad[ Cocaine=='Negative', suCocaine:=FALSE]
posad[ (cdaCocaine) | (F14) | Cocaine=='Positive', suCocaine:=TRUE]
posad[ Opiates=='Negative', suOpiates:=FALSE]
posad[ (F11) | Opiates=='Positive', suOpiates:=TRUE]
posad[ Benzos=='Negative', suBenzos:=FALSE]
posad[ (cdaBenzo) | Benzos=='Positive', suBenzos:=TRUE]
posad[ Amphetamines=='Negative', suAmphetamines:=FALSE]
posad[ (cdaAmph) | Amphetamines=='Positive', suAmphetamines:=TRUE]


#----------------------------
# SAVE ####
#----------------------------

write.csv(posad, file=paste0('T:/Katherine Morley/S136/Output/',today,'-pos-first-admission-data.csv'))
save(posad, file=paste0('T:/Katherine Morley/S136/Output/',today,'-pos-first-admission-data.RData'))

#----------------------------
# SUMMARISE ####
#----------------------------

.libPaths(c("T:/Katherine Morley/R", .libPaths()))
require(car)
require(ggplot2)
require(xlsx)

source('T:/Katherine Morley/resources/R/catSummary.R')

tabSocioDemo <- catSummary(posad, vars=c('Gender_ID','ethnicity','Housing_Status','Occupation_ID'), 
                           varNames=c('Gender','Ethnicity', 'Housing status','Employment'),
                           returnTable=T, latex=F, excel=F)

write.xlsx(tabSocioDemo, file=paste('PoS-admission-data-summary-',today,'.xlsx',sep=''), 
           sheetName='Sociodemographics',
           append=TRUE, row.names=FALSE)

tabSUD <- catSummary(posad, vars=c('AAudit_Risk_Cat', grep('su', names(posad), value=TRUE), 'cdaNone'), 
                           varNames=c('AUDIT risk category','Alcohol use','Cannabis use','Cocaine use',
                                      'Opiate use','Benzodiazepine use','Amphetamine use','No substances'),
                           returnTable=T, latex=F, excel=F)

write.xlsx(tabSUD, file=paste('PoS-admission-data-summary-',today,'.xlsx',sep=''), 
           sheetName='Substance use',
           append=TRUE, row.names=FALSE)

tabMH <- catSummary(posad, vars=c(grep('diag', names(posad), value=TRUE)[-1]), 
                     varNames=c('Psychosis','Mania','Anxiety/Depression','Other',
                                'Substance dependence','No mental health diagnosis'),
                     returnTable=T, latex=F, excel=F)

write.xlsx(tabMH, file=paste('PoS-admission-data-summary-',today,'.xlsx',sep=''), 
           sheetName='Mental health',
           append=TRUE, row.names=FALSE)

