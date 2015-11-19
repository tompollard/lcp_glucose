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
setwd("h:/db/")

# db connection
con = dbConnect(dbDriver("PostgreSQL"), dbname = "mimic",
                host = "127.0.0.1", port = 5432,
                user = "postgres", password = "0367")
dbListTables(con)
dbExistsTable(con, c("mimiciii", "admissions"))
dbExistsTable(con, c("mimiciii", "diagnoses_icd"))
dbExistsTable(con, c("mimiciii", "d_labitems"))
dbExistsTable(con, c("mimiciii", "labevents"))

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

# group by admission
gluAdm = sqldf("SELECT hadm_id, count(rh) as cntRH, max(rh) as rh, count(rrh) as cntRRH, max(rrh) as rrh
               FROM glu GROUP BY hadm_id", drv='SQLite')  

admission = left_join(admission, gluAdm, by="hadm_id")
admission[is.na(admission)] = 0
rm(gluAdm)

write.table(admission, "admission.csv", sep=";", eol="\n", row.names=F, col.names=T)
write.table(glu, "glucose.csv", sep=";", eol="\n", row.names=F, col.names=T)

# prevalence
count(admission$rh == 1)[2, 2] / nrow(admission) # 0.138
count(admission$rh == 1 & admission$dm == 1)[2, 2] / count(admission$dm == 1)[2, 2] # 0.204
count(admission$rh == 1 & admission$dm == 0)[2, 2] / count(admission$dm == 0)[2, 2] # 0.117

count(admission$rh == 1 & admission$expire == 1)[2, 2] / count(admission$expire == 1)[2, 2] # 0.188
count(admission$rh == 1 & admission$expire == 0)[2, 2] / count(admission$expire == 0)[2, 2] # 0.133

count(admission$rrh == 1 & admission$dm == 1)[2, 2] / count(admission$dm == 1)[2, 2] # 0.240
count(admission$rrh == 1 & admission$dm == 0)[2, 2] / count(admission$dm == 0)[2, 2] # 0.157

count(admission$rrh == 1 & admission$expire == 1)[2, 2] / count(admission$expire == 1)[2, 2] # 0.156
count(admission$rrh == 1 & admission$expire == 0)[2, 2] / count(admission$expire == 0)[2, 2] # 0.179
