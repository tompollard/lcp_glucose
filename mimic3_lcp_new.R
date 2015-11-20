# Copyright © 2015 by Wei-Hung Weng
# All rights reserved. This document or any portion thereof
# may not be reproduced or used in any manner whatsoever
# without the express written permission of the author.
#
# Title : glucose classifier, using MIMIC3, relative hypoglycemia
# Author : Wei-Hung Weng
# Created : 11/19/2015
# Comment : 
# Reference : 
# 1. MIMIC3 database
# 2. Rinaldo

# List of packages for session
.packages <- c("RPostgreSQL", "ggplot2", "data.table", "dplyr", "plyr", "sqldf", "foreign", "e1071", "caTools", "ROCR")
# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
# Load packages into session 
lapply(.packages, library, character.only=TRUE)
# Set path
setwd("h:/GitHub/lcp_glucose/")

# db connection
con = dbConnect(dbDriver("PostgreSQL"), dbname = "mimic",
                host = "127.0.0.1", port = 5432,
                user = "postgres", password = "0367")
dbListTables(con)
dbExistsTable(con, c("mimiciii", "admissions"))
dbExistsTable(con, c("mimiciii", "diagnoses_icd"))
dbExistsTable(con, c("mimiciii", "d_labitems"))
dbExistsTable(con, c("mimiciii", "labevents"))

###### processing ######

# get admission id (hadm_id), expire or not, length of stay (in hours) in table admissions
dbGetQuery(con, "select * from mimiciii.admissions limit 2")
admission = dbGetQuery(con, "SELECT hadm_id, hospital_expire_flag as expire, 
                       DATE_PART('day', dischtime - admittime) * 24 + DATE_PART('hour', dischtime - admittime) as los 
                       from mimiciii.admissions")

# get diagnosis with/without DM (ICD-9 = 250.xx -> mimic3: 250xx) - use admission
# admDM: all admissions with diagnosis of DM (ICD-9 250.*)
withDM = dbGetQuery(con, "SELECT hadm_id FROM mimiciii.diagnoses_icd WHERE icd9_code LIKE '250%' GROUP BY hadm_id")
withDM$dm = 1
admission = left_join(admission, withDM, by="hadm_id")
admission$dm[which(is.na(admission$dm))] = 0
rm(withDM)

# find the item ID of blood glucose exam (#50809 in ABG, case sensitive)
labABGglucose = dbGetQuery(con, "SELECT * FROM mimiciii.d_labitems WHERE fluid LIKE 'BLOOD' and label LIKE '%GLUCOSE%'")

# find RH
# find all ABG glucose level
glu = dbGetQuery(con, "SELECT hadm_id, subject_id, 
                     to_char(charttime, 'YYYY-MM-DD HH24:MI:SS') as charttime, valuenum 
                     FROM mimiciii.labevents 
                     WHERE hadm_id IS NOT NULL AND itemid = '50809'
                     ORDER BY hadm_id, charttime")

# unify the datetime format, for R calculation
glu$charttime = as.POSIXct(glu$charttime, format="%Y-%m-%d %H:%M:%S")

# tag those hypoglycemic events
glu$hypo = 0
for (i in 1:nrow(glu)) {
  if (glu$valuenum[i] < 70.2) {
    glu$hypo[i] = 1
  }
}

# find RH events
for (i in 1:nrow(glu)) {
  
  # if the next row is in the same admission then keep processing, otherwise skip
  if (glu[i+1, 1] == glu[i, 1]) {
    
    # calculate time difference
    glu$difftime[i+1] = difftime(glu[i+1, 3], glu[i, 3], units="mins")
    
    # calculate ABG glucose value difference
    glu$diffval[i+1] = glu[i+1, 4] - glu[i, 4]
    
    # calculate value difference (in percantage) (compare to the previous value)
    glu$dropPercent[i+1] = (glu[i+1, 4] - glu[i, 4]) / glu[i, 4]
    
    # percentage difference in 4hr (240 min) scale
    glu$dropPercent4h[i+1] = glu$dropPercent[i+1] / glu$difftime[i+1] * 240
  }
}

glu = na.omit(glu)

# find RH and rapid RH in ABG glucose
glu$rh = 0
for (i in 1:nrow(glu)) {
  if (glu$dropPercent[i] <= - 0.3) {
    glu$rh[i] = 1
  } 
}

glu$rrh = 0
for (i in 1:nrow(glu)) {
  if (glu$dropPercent4h[i] <= - 0.3) {
    glu$rrh[i] = 1
  } 
}

# find hypoglycemia after previous RH/RRH
glu$hypoAfterRH = 0

for (i in 2:nrow(glu)) {
  if (glu$hadm_id[i] == glu$hadm_id[i-1] & glu$hypo[i] == 1 & glu$rh[i] == 1) {
    glu$hypoAfterRH[i] = 1
  }
}

glu$hypoAfterRRH = 0

for (i in 2:nrow(glu)) {
  if (glu$hadm_id[i] == glu$hadm_id[i-1] & glu$hypo[i] == 1 & glu$rrh[i] == 1) {
    glu$hypoAfterRRH[i] = 1
  }
}

# find hypoglycemia after hyperglycemia (> 10 mmol/L (>180 mg/dl))
glu$rhAfterHyper = 0

for (i in 2:nrow(glu)) {
  if (glu$hadm_id[i] == glu$hadm_id[i-1] & glu$rh[i] == 1 & glu$valuenum[i-1] > 180) {
    glu$rhAfterHyper[i] = 1
  }
}


# group by admission
gluAdm = sqldf("SELECT hadm_id, count(rh) as cntRH, max(rh) as rh, count(rrh) as cntRRH, max(rrh) as rrh
               FROM glu GROUP BY hadm_id", drv='SQLite')  

admission = left_join(admission, gluAdm, by="hadm_id")
admission[is.na(admission)] = 0
rm(gluAdm)

write.table(admission, "admission.csv", sep=";", eol="\n", row.names=F, col.names=T)
write.table(glu, "glucose.csv", sep=";", eol="\n", row.names=F, col.names=T)

###### analysis ######

admission = read.csv("admission.csv", sep=";")
glu = read.csv("glucose.csv", sep=";")

# hypothesis 1: RH is more common in DM than in NDM? Y
count(admission$rh == 1)[2, 2] / nrow(admission) # 0.138
count(admission$rh == 1 & admission$dm == 1)[2, 2] / count(admission$dm == 1)[2, 2] # 0.204
count(admission$rh == 1 & admission$dm == 0)[2, 2] / count(admission$dm == 0)[2, 2] # 0.117

# hypothesis 3: RH independently (how?) associated with greater risk of death in DM/NDM? [adjust?]
# association with death is stronger in DM? Y
count(admission$rh == 1 & admission$expire == 1)[2, 2] / count(admission$expire == 1)[2, 2] # 0.188
count(admission$rh == 1 & admission$expire == 0)[2, 2] / count(admission$expire == 0)[2, 2] # 0.133

# hypothesis 2: RRH is more common in DM than in NDM? Y
count(admission$rrh == 1 & admission$dm == 1)[2, 2] / count(admission$dm == 1)[2, 2] # 0.240
count(admission$rrh == 1 & admission$dm == 0)[2, 2] / count(admission$dm == 0)[2, 2] # 0.157

# hypothesis 4: RRH independently (how?) associated with greater risk of death in DM/NDM? [adjust?]
# association with death is stronger in DM? Y
count(admission$rrh == 1 & admission$expire == 1)[2, 2] / count(admission$expire == 1)[2, 2] # 0.156
count(admission$rrh == 1 & admission$expire == 0)[2, 2] / count(admission$expire == 0)[2, 2] # 0.179

# hypothesis 5: RH independently (how?) associated with subsequent true hypoglycemia? NO, 51 vs. 49
count(glu$hypo == 1 & glu$hypoAfterRH == 1) / count(glu$hypo == 1) # 0.51

# hypothesis 6: RRH independently (how?) associated with subsequent true hypoglycemia? ? 43 vs. 57
count(glu$hypo == 1 & glu$hypoAfterRRH == 1) / count(glu$hypo == 1) # 0.43

# hypothesis 7: RH independently associated with LOS? [yes/no, count]
# association with death is stronger in DM -> same as hypothesis 3???

# length of stay vs. RH, all/diabetes/non-diabetes (linear regression)
lm.all = lm(los ~ rh, admission)
summary(lm.all) # R-sq = 0.034
lm.dm = lm(los ~ rh, subset(admission[which(admission$dm == 1), ]))
summary(lm.dm) # R-sq = 0.067
lm.ndm = lm(los ~ rh, subset(admission[which(admission$dm == 0), ]))
summary(lm.ndm) # R-sq = 0.030

# length of stay vs. number of RH events, all/diabetes/non-diabetes (linear regression)
lm.all = lm(los ~ cntRH, admission)
summary(lm.all) # R-sq = 0.082
lm.dm = lm(los ~ cntRH, subset(admission[which(admission$dm == 1), ]))
summary(lm.dm) # R-sq = 0.124
lm.ndm = lm(los ~ cntRH, subset(admission[which(admission$dm == 0), ]))
summary(lm.ndm) # R-sq = 0.076

# hypothesis 8: RRH independently associated with LOS? [yes/no, count]
# association with death is stronger in DM -> same as hypothesis 3???

# length of stay vs. RRH, all/diabetes/non-diabetes (linear regression)
lm.all = lm(los ~ rrh, admission)
summary(lm.all) # R-sq = 0.0108
lm.dm = lm(los ~ rrh, subset(admission[which(admission$dm == 1), ]))
summary(lm.dm) # R-sq = 0.022
lm.ndm = lm(los ~ rrh, subset(admission[which(admission$dm == 0), ]))
summary(lm.ndm) # R-sq = 0.009

# length of stay vs. number of RRH events, all/diabetes/non-diabetes (linear regression)
lm.all = lm(los ~ cntRRH, admission)
summary(lm.all) # R-sq = 0.082
lm.dm = lm(los ~ cntRRH, subset(admission[which(admission$dm == 1), ]))
summary(lm.dm) # R-sq = 0.124
lm.ndm = lm(los ~ cntRRH, subset(admission[which(admission$dm == 0), ]))
summary(lm.ndm) # R-sq = 0.076

# question 12: how many RH followed by hypers? 34%
count(glu$rh == 1 & glu$rhAfterHyper == 1) / count(glu$rh == 1) # 0.34

# question 13: how many RH followed by insulin? ???

# question 13: 