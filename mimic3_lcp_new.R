# Title : glucose!, using MIMIC3, relative hypoglycemia
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

# find the item ID of blood glucose exam (#50809 in gluLac, case sensitive)
labgluLacglucose = dbGetQuery(con, "SELECT * FROM mimiciii.d_labitems WHERE fluid LIKE 'BLOOD' and label LIKE '%GLUCOSE%'")

# find RH
# find all gluLac glucose level
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
    
    # calculate gluLac glucose value difference
    glu$diffval[i+1] = glu[i+1, 4] - glu[i, 4]
    
    # calculate value difference (in percantage) (compare to the previous value)
    glu$dropPercent[i+1] = (glu[i+1, 4] - glu[i, 4]) / glu[i, 4]
    
    # percentage difference in 4hr (240 min) scale
    glu$dropPercent4h[i+1] = glu$dropPercent[i+1] / glu$difftime[i+1] * 240
  }
}

glu = na.omit(glu)

# find RH and rapid RH in gluLac glucose
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
gluAdm = sqldf("SELECT hadm_id, sum(rh = 1) as cntRH, max(rh = 1) as rh, sum(rrh = 1) as cntRRH, max(rrh = 1) as rrh
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
summary(lm.all) # R-sq = 0.059
lm.dm = lm(los ~ cntRH, subset(admission[which(admission$dm == 1), ]))
summary(lm.dm) # R-sq = 0.106
lm.ndm = lm(los ~ cntRH, subset(admission[which(admission$dm == 0), ]))
summary(lm.ndm) # R-sq = 0.053

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
summary(lm.all) # R-sq = 0.019
lm.dm = lm(los ~ cntRRH, subset(admission[which(admission$dm == 1), ]))
summary(lm.dm) # R-sq = 0.029
lm.ndm = lm(los ~ cntRRH, subset(admission[which(admission$dm == 0), ]))
summary(lm.ndm) # R-sq = 0.018

# question 12: how many RH followed by hypers? 34%
count(glu$rh == 1 & glu$rhAfterHyper == 1) / count(glu$rh == 1) # 0.34

# question 13: how many RH followed by insulin? ???

# question 13: 

# lab chem item id
# find the item ID of blood lactate exam (#50813 in abg / #50954 in vein, case sensitive)
labLact = dbGetQuery(con, "SELECT * FROM mimiciii.d_labitems WHERE fluid LIKE 'BLOOD' and label LIKE '%LACTAT%'")
# find the item ID of blood base excess exam (#50802 in abg, case sensitive)
labBase = dbGetQuery(con, "SELECT * FROM mimiciii.d_labitems WHERE fluid LIKE 'BLOOD' and label LIKE '%BAS%'")
# find the item ID of blood CO2 exam (#50804 in abg, case sensitive)
labCO2 = dbGetQuery(con, "SELECT * FROM mimiciii.d_labitems WHERE fluid LIKE 'BLOOD' and label LIKE '%CO2%'")
# find the item ID of blood pH exam (#50820 in abg, case sensitive)
labPH = dbGetQuery(con, "SELECT * FROM mimiciii.d_labitems WHERE fluid LIKE 'BLOOD' and label LIKE '%PH%'")
# find the item ID of blood K+ exam (#50822 in abg / # 50971 in vein, case sensitive)
labK = dbGetQuery(con, "SELECT * FROM mimiciii.d_labitems WHERE fluid LIKE 'BLOOD' and label LIKE '%POTA%'")
# find the item ID of blood Cl- exam (#50806 in abg / # 50902 in vein, case sensitive)
labCl = dbGetQuery(con, "SELECT * FROM mimiciii.d_labitems WHERE fluid LIKE 'BLOOD' and label LIKE '%CHL%'")

###### lactate
lac = dbGetQuery(con, "SELECT hadm_id, subject_id, 
                     to_char(charttime, 'YYYY-MM-DD HH24:MI:SS') as charttime, valuenum 
                 FROM mimiciii.labevents 
                 WHERE hadm_id IS NOT NULL AND itemid = '50813'
                 ORDER BY hadm_id, charttime")

# unify the datetime format, for R calculation
lac$charttime = as.POSIXct(lac$charttime, format="%Y-%m-%d %H:%M:%S")

# gluLac data study
gluLac = inner_join(glu, lac, by = c("hadm_id", "subject_id", "charttime"))
colnames(gluLac)[4] = "glu"
colnames(gluLac)[15] = "lac"

gluLac$lacPrior1 = NA
gluLac$lacPrior2 = NA
gluLac$lacAfter = NA
gluLac$lacDiff = NA

# repeated the first row 3 times, for further loop
gluLac = rbind(gluLac[1, ], gluLac[1, ], gluLac)

for (i in 3:nrow(gluLac)) {
  
  # find the previous lac
  if (gluLac$hadm_id[i] == gluLac$hadm_id[i-1]) {
    gluLac$lacPrior1[i] = gluLac$lac[i-1]
    gluLac$lacDiff[i] = gluLac$lac[i] - gluLac$lac[i-1]
  }

  if (gluLac$hadm_id[i] == gluLac$hadm_id[i-2]) {
    gluLac$lacPrior2[i] = gluLac$lac[i-2]
  }
  
  if (gluLac$hadm_id[i] == gluLac$hadm_id[i+1]) {
    gluLac$lacAfter[i] = gluLac$lac[i+1]
  }

}

gluLac = gluLac[-c(2, 3), ]

###### base
bas = dbGetQuery(con, "SELECT hadm_id, subject_id, 
                 to_char(charttime, 'YYYY-MM-DD HH24:MI:SS') as charttime, valuenum 
                 FROM mimiciii.labevents 
                 WHERE hadm_id IS NOT NULL AND itemid = '50802'
                 ORDER BY hadm_id, charttime")

# unify the datetime format, for R calculation
bas$charttime = as.POSIXct(bas$charttime, format="%Y-%m-%d %H:%M:%S")

# gluBas data study
gluBas = inner_join(glu, bas, by = c("hadm_id", "subject_id", "charttime"))
colnames(gluBas)[4] = "glu"
colnames(gluBas)[15] = "bas"

gluBas$basPrior1 = NA
gluBas$basPrior2 = NA
gluBas$basAfter = NA
gluBas$basDiff = NA

# repeated the first row 3 times, for further loop
gluBas = rbind(gluBas[1, ], gluBas[1, ], gluBas)

for (i in 3:nrow(gluBas)) {
  
  # find the previous lac
  if (gluBas$hadm_id[i] == gluBas$hadm_id[i-1]) {
    gluBas$basPrior1[i] = gluBas$bas[i-1]
    gluBas$basDiff[i] = gluBas$bas[i] - gluBas$bas[i-1]
  }
  
  if (gluBas$hadm_id[i] == gluBas$hadm_id[i-2]) {
    gluBas$basPrior2[i] = gluBas$bas[i-2]
  }
  
  if (gluBas$hadm_id[i] == gluBas$hadm_id[i+1]) {
    gluBas$basAfter[i] = gluBas$bas[i+1]
  }
  
}

gluBas = gluBas[-c(2, 3), ]

###### CO2
CO2 = dbGetQuery(con, "SELECT hadm_id, subject_id, 
                 to_char(charttime, 'YYYY-MM-DD HH24:MI:SS') as charttime, valuenum 
                 FROM mimiciii.labevents 
                 WHERE hadm_id IS NOT NULL AND itemid = '50804'
                 ORDER BY hadm_id, charttime")

# unify the datetime format, for R calculation
CO2$charttime = as.POSIXct(CO2$charttime, format="%Y-%m-%d %H:%M:%S")

# gluBas data study
gluCO2 = inner_join(glu, CO2, by = c("hadm_id", "subject_id", "charttime"))
colnames(gluCO2)[4] = "glu"
colnames(gluCO2)[15] = "CO2"

gluCO2$CO2Prior1 = NA
gluCO2$CO2Prior2 = NA
gluCO2$CO2After = NA
gluCO2$CO2Diff = NA

# repeated the first row 3 times, for further loop
gluCO2 = rbind(gluCO2[1, ], gluCO2[1, ], gluCO2)

for (i in 3:nrow(gluCO2)) {
  
  # find the previous lac
  if (gluCO2$hadm_id[i] == gluCO2$hadm_id[i-1]) {
    gluCO2$CO2Prior1[i] = gluCO2$CO2[i-1]
    gluCO2$CO2Diff[i] = gluCO2$CO2[i] - gluCO2$CO2[i-1]
  }
  
  if (gluCO2$hadm_id[i] == gluCO2$hadm_id[i-2]) {
    gluCO2$CO2Prior2[i] = gluCO2$CO2[i-2]
  }
  
  if (gluCO2$hadm_id[i] == gluCO2$hadm_id[i+1]) {
    gluCO2$CO2After[i] = gluCO2$CO2[i+1]
  }
  
}

gluCO2 = gluCO2[-c(2, 3), ]

###### ph
ph = dbGetQuery(con, "SELECT hadm_id, subject_id, 
                 to_char(charttime, 'YYYY-MM-DD HH24:MI:SS') as charttime, valuenum 
                 FROM mimiciii.labevents 
                 WHERE hadm_id IS NOT NULL AND itemid = '50820'
                 ORDER BY hadm_id, charttime")

# unify the datetime format, for R calculation
ph$charttime = as.POSIXct(ph$charttime, format="%Y-%m-%d %H:%M:%S")

# gluBas data study
gluph = inner_join(glu, ph, by = c("hadm_id", "subject_id", "charttime"))
colnames(gluph)[4] = "glu"
colnames(gluph)[15] = "ph"

gluph$phPrior1 = NA
gluph$phPrior2 = NA
gluph$phAfter = NA
gluph$phDiff = NA

# repeated the first row 3 times, for further loop
gluph = rbind(gluph[1, ], gluph[1, ], gluph)

for (i in 3:nrow(gluph)) {
  
  # find the previous lac
  if (gluph$hadm_id[i] == gluph$hadm_id[i-1]) {
    gluph$phPrior1[i] = gluph$ph[i-1]
    gluph$phDiff[i] = gluph$ph[i] - gluph$ph[i-1]
  }
  
  if (gluph$hadm_id[i] == gluph$hadm_id[i-2]) {
    gluph$phPrior2[i] = gluph$ph[i-2]
  }
  
  if (gluph$hadm_id[i] == gluph$hadm_id[i+1]) {
    gluph$phAfter[i] = gluph$ph[i+1]
  }
  
}

gluph = gluph[-c(2, 3), ]

###### potassium
k = dbGetQuery(con, "SELECT hadm_id, subject_id, 
                to_char(charttime, 'YYYY-MM-DD HH24:MI:SS') as charttime, valuenum 
                FROM mimiciii.labevents 
                WHERE hadm_id IS NOT NULL AND itemid = '50822'
                ORDER BY hadm_id, charttime")

# unify the datetime format, for R calculation
k$charttime = as.POSIXct(k$charttime, format="%Y-%m-%d %H:%M:%S")

# gluBas data study
gluK = inner_join(glu, k, by = c("hadm_id", "subject_id", "charttime"))
colnames(gluK)[4] = "glu"
colnames(gluK)[15] = "k"

gluK$kPrior1 = NA
gluK$kPrior2 = NA
gluK$kAfter = NA
gluK$kDiff = NA

# repeated the first row 3 times, for further loop
gluK = rbind(gluK[1, ], gluK[1, ], gluK)

for (i in 3:nrow(gluK)) {
  
  # find the previous lac
  if (gluK$hadm_id[i] == gluK$hadm_id[i-1]) {
    gluK$kPrior1[i] = gluK$k[i-1]
    gluK$kDiff[i] = gluK$k[i] - gluK$k[i-1]
  }
  
  if (gluK$hadm_id[i] == gluK$hadm_id[i-2]) {
    gluK$kPrior2[i] = gluK$k[i-2]
  }
  
  if (gluK$hadm_id[i] == gluK$hadm_id[i+1]) {
    gluK$kAfter[i] = gluK$k[i+1]
  }
  
}

gluK = gluK[-c(2, 3), ]


###### potassium
cl = dbGetQuery(con, "SELECT hadm_id, subject_id, 
               to_char(charttime, 'YYYY-MM-DD HH24:MI:SS') as charttime, valuenum 
               FROM mimiciii.labevents 
               WHERE hadm_id IS NOT NULL AND itemid = '50806'
               ORDER BY hadm_id, charttime")

# unify the datetime format, for R calculation
cl$charttime = as.POSIXct(cl$charttime, format="%Y-%m-%d %H:%M:%S")

# gluBas data study
gluCl = inner_join(glu, cl, by = c("hadm_id", "subject_id", "charttime"))
colnames(gluCl)[4] = "glu"
colnames(gluCl)[15] = "cl"

gluCl$clPrior1 = NA
gluCl$clPrior2 = NA
gluCl$clAfter = NA
gluCl$clDiff = NA

# repeated the first row 3 times, for further loop
gluCl = rbind(gluCl[1, ], gluCl[1, ], gluCl)

for (i in 3:nrow(gluCl)) {
  
  # find the previous lac
  if (gluCl$hadm_id[i] == gluCl$hadm_id[i-1]) {
    gluCl$clPrior1[i] = gluCl$cl[i-1]
    gluCl$clDiff[i] = gluCl$cl[i] - gluCl$cl[i-1]
  }
  
  if (gluCl$hadm_id[i] == gluCl$hadm_id[i-2]) {
    gluCl$clPrior2[i] = gluCl$cl[i-2]
  }
  
  if (gluCl$hadm_id[i] == gluCl$hadm_id[i+1]) {
    gluCl$clAfter[i] = gluCl$cl[i+1]
  }
  
}

gluCl = gluCl[-c(2, 3), ]