# Copyright Â© 2015 by Wei-Hung Weng
# All rights reserved. This document or any portion thereof
# may not be reproduced or used in any manner whatsoever
# without the express written permission of the author.
#
# Title : glucose classifier, using MIMIC3, relative hypoglycemia
# Author : Wei-Hung Weng
# Created : 11/19/2015
# Comment : currently 0115
# Reference : 
# 1. MIMIC3 database
# 2. Rinaldo

# List of packages for session
.packages <- c("RPostgreSQL", "ggplot2", "data.table", "dplyr", "plyr", 
               "sqldf", "foreign", "e1071", "caTools", "ROCR", "arules", "arulesViz")
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
dbExistsTable(con, c("mimiciii", "prescriptions"))

###### processing ######

# get admission id (hadm_id), expire or not, length of stay (in hours) in table admissions
dbGetQuery(con, "select * from mimiciii.admissions limit 2")
admission = dbGetQuery(con, "SELECT hadm_id, hospital_expire_flag as expire, 
                       DATE_PART('day', dischtime - admittime) * 24 + DATE_PART('hour', dischtime - admittime) as los 
                       from mimiciii.admissions")

# get diagnosis with/without DM (ICD-9 = 250.xx -> mimic3: 250xx) - use admission
# admDM: all admissions with diagnosis of DM (ICD-9 250.*)
withDM = dbGetQuery(con, "SELECT hadm_id FROM mimiciii.diagnoses_icd WHERE icd9_code LIKE '250%' GROUP BY hadm_id")

# medication
meds = dbGetQuery(con, "SELECT prescriptions.hadm_id, string_agg(prescriptions.drug, ',') 
                  FROM mimiciii.prescriptions 
                  GROUP BY prescriptions.hadm_id")

# DM mortality pts meds
dmMeds = left_join(withDM, meds, by="hadm_id")
dmMedsMort = left_join(dmMeds, admission, by="hadm_id")
dmMedsMort = dmMedsMort[which(dmMedsMort$expire == 1), 2]

# input for apriori
bow = data.frame()
for (i in 1:length(dmMedsMort)) {
  tmp = data.frame(as.list(strsplit(dmMedsMort, ",")[[i]]))
  for (j in 1:ncol(tmp)) {
    colnames(tmp)[j] = paste0("V", j)
  }
  bow = rbind.fill(bow, tmp)
} 

write.csv(bow, "bagOfWords.csv", sep=";", eol="\n", row.names=F, col.names=F)

# load the data
trans = read.transactions("bagOfWords.csv", format = "basket", sep = ",", rm.duplicates = T)

# apriori algorithm
rules = apriori(trans, parameter = list(support = 0.3, confidence = 0.95, minlen = 3, maxlen = 5))
inspect(sort(rules, by = "lift")[1:50])

# add more features
quality(rules) = cbind(quality(rules), interestMeasure(rules, measure=c("chiSquare", "conviction", "cosine", "coverage", "doc", "gini", "hyperLift", "hyperConfidence", "fishersExactTest", "improvement", "leverage", "lift", "oddsRatio", "phi", "RLD"), rules))

# evaluation
summary(rules)

plot(rules, method="grouped", control=list(k=10))

subrules = head(sort(rules, by="lift"), 30)
plot(subrules, method="graph", control=list(type="itemsets"))
