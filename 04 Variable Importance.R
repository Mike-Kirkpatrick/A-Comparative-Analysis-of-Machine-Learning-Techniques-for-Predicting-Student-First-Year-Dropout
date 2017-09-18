
####################################################################################
#     CALCULATE VARIABLE IMPORTANCE FOR ALL 36 MODELS ~ STANDARDIZE AND COMBINE    #
####################################################################################

# The purpose of this code is to evaluate variable imortance across all 36 models
# Variable importance is calculated for all 76 variables across each of the 36 models
# The metrics are aggregated slightly differently since variable importance is calculated slightly differently for some algorithms




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Initial Loading of Data and Preparing R Environment #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# IMPORT ALL MODELS
setwd("Z:/Advanced Analytics/One Year Retention Risk Indicator/Analyses/Saved Objects/02 Model Building/")
files = list.files(pattern="*.rda")
for (i in 1:length(files)) load(file=files[i])
rm(files,i)

library(caret)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# DATA SET 1 ~  COMPLETE UNBALANCED #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
vi.dt.cu <- varImp(dt.cu)
vi.dt.cu <- vi.dt.cu$importance
vi.dt.cu$Model <- "dt.cu"
vi.dt.cu$rn <- gsub("`", "",rownames(vi.dt.cu))

vi.knn.cu <- varImp(knn.cu)
vi.knn.cu <- vi.knn.cu$importance
t <- data.frame(Overall = vi.knn.cu[,"Y"])
t$Model <- "knn.cu"
t$rn <- rownames(vi.knn.cu)
vi.knn.cu <- t 

vi.lr.cu <- varImp(lr.cu)
vi.lr.cu <- vi.lr.cu$importance
vi.lr.cu$Model <- "lr.cu"
vi.lr.cu$rn <- gsub("`", "",rownames(vi.lr.cu))

vi.nb.cu <- varImp(nb.cu)
vi.nb.cu <- vi.nb.cu$importance
t <- data.frame(Overall = vi.nb.cu[,"Y"])
t$Model <- "nb.cu"
t$rn <- rownames(vi.nb.cu)
vi.nb.cu <- t 

vi.nn.cu <- varImp(nn.cu)
vi.nn.cu <- vi.nn.cu$importance
vi.nn.cu$Model <- "nn.cu"
vi.nn.cu$rn <- gsub("`", "",rownames(vi.nn.cu))

vi.rf.cu <- varImp(rf.cu)
vi.rf.cu <- vi.rf.cu$importance
vi.rf.cu$Model <- "rf.cu"
vi.rf.cu$rn <- gsub("`", "",rownames(vi.rf.cu))

vi.svml.cu <- varImp(svml.cu)
vi.svml.cu <- vi.svml.cu$importance
t <- data.frame(Overall = vi.svml.cu[,"Y"])
t$Model <- "svml.cu"
t$rn <- rownames(vi.svml.cu)
vi.svml.cu <- t 

vi.svmp.cu <- varImp(svmp.cu)
vi.svmp.cu <- vi.svmp.cu$importance
t <- data.frame(Overall = vi.svmp.cu[,"Y"])
t$Model <- "svmp.cu"
t$rn <- rownames(vi.svmp.cu)
vi.svmp.cu <- t 

vi.xgb.cu <- varImp(xgb.cu)
vi.xgb.cu <- vi.xgb.cu$importance
vi.xgb.cu$Model <- "xgb.cu"
vi.xgb.cu$rn <- gsub("`", "",rownames(vi.xgb.cu))




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# DATA SET 2 ~  REDUCED UNBALANCED #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
vi.dt.ru <- varImp(dt.ru)
vi.dt.ru <- vi.dt.ru$importance
vi.dt.ru$Model <- "dt.ru"
vi.dt.ru$rn <- gsub("`", "",rownames(vi.dt.ru))

vi.knn.ru <- varImp(knn.ru)
vi.knn.ru <- vi.knn.ru$importance
t <- data.frame(Overall = vi.knn.ru[,"Y"])
t$Model <- "knn.ru"
t$rn <- rownames(vi.knn.ru)
vi.knn.ru <- t 

vi.lr.ru <- varImp(lr.ru)
vi.lr.ru <- vi.lr.ru$importance
vi.lr.ru$Model <- "lr.ru"
vi.lr.ru$rn <- gsub("`", "",rownames(vi.lr.ru))

vi.nb.ru <- varImp(nb.ru)
vi.nb.ru <- vi.nb.ru$importance
t <- data.frame(Overall = vi.nb.ru[,"Y"])
t$Model <- "nb.ru"
t$rn <- rownames(vi.nb.ru)
vi.nb.ru <- t 

vi.nn.ru <- varImp(nn.ru)
vi.nn.ru <- vi.nn.ru$importance
vi.nn.ru$Model <- "nn.ru"
vi.nn.ru$rn <- gsub("`", "",rownames(vi.nn.ru))

vi.rf.ru <- varImp(rf.ru)
vi.rf.ru <- vi.rf.ru$importance
vi.rf.ru$Model <- "rf.ru"
vi.rf.ru$rn <- gsub("`", "",rownames(vi.rf.ru))

vi.svml.ru <- varImp(svml.ru)
vi.svml.ru <- vi.svml.ru$importance
t <- data.frame(Overall = vi.svml.ru[,"Y"])
t$Model <- "svml.ru"
t$rn <- rownames(vi.svml.ru)
vi.svml.ru <- t 

vi.svmp.ru <- varImp(svmp.ru)
vi.svmp.ru <- vi.svmp.ru$importance
t <- data.frame(Overall = vi.svmp.ru[,"Y"])
t$Model <- "svmp.ru"
t$rn <- rownames(vi.svmp.ru)
vi.svmp.ru <- t 

vi.xgb.ru <- varImp(xgb.ru)
vi.xgb.ru <- vi.xgb.ru$importance
vi.xgb.ru$Model <- "xgb.ru"
vi.xgb.ru$rn <- gsub("`", "",rownames(vi.xgb.ru))




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# DATA SET 3 ~  COMPLETE BALANCED #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
vi.dt.cb <- varImp(dt.cb)
vi.dt.cb <- vi.dt.cb$importance
vi.dt.cb$Model <- "dt.cb"
vi.dt.cb$rn <- gsub("`", "",rownames(vi.dt.cb))

vi.knn.cb <- varImp(knn.cb)
vi.knn.cb <- vi.knn.cb$importance
t <- data.frame(Overall = vi.knn.cb[,"Y"])
t$Model <- "knn.cb"
t$rn <- rownames(vi.knn.cb)
vi.knn.cb <- t 

vi.lr.cb <- varImp(lr.cb)
vi.lr.cb <- vi.lr.cb$importance
vi.lr.cb$Model <- "lr.cb"
vi.lr.cb$rn <- gsub("`", "",rownames(vi.lr.cb))

vi.nb.cb <- varImp(nb.cb)
vi.nb.cb <- vi.nb.cb$importance
t <- data.frame(Overall = vi.nb.cb[,"Y"])
t$Model <- "nb.cb"
t$rn <- rownames(vi.nb.cb)
vi.nb.cb <- t 

vi.nn.cb <- varImp(nn.cb)
vi.nn.cb <- vi.nn.cb$importance
vi.nn.cb$Model <- "nn.cb"
vi.nn.cb$rn <- gsub("`", "",rownames(vi.nn.cb))

vi.rf.cb <- varImp(rf.cb)
vi.rf.cb <- vi.rf.cb$importance
vi.rf.cb$Model <- "rf.cb"
vi.rf.cb$rn <- gsub("`", "",rownames(vi.rf.cb))

vi.svml.cb <- varImp(svml.cb)
vi.svml.cb <- vi.svml.cb$importance
t <- data.frame(Overall = vi.svml.cb[,"Y"])
t$Model <- "svml.cb"
t$rn <- rownames(vi.svml.cb)
vi.svml.cb <- t 

vi.svmp.cb <- varImp(svmp.cb)
vi.svmp.cb <- vi.svmp.cb$importance
t <- data.frame(Overall = vi.svmp.cb[,"Y"])
t$Model <- "svmp.cb"
t$rn <- rownames(vi.svmp.cb)
vi.svmp.cb <- t 

vi.xgb.cb <- varImp(xgb.cb)
vi.xgb.cb <- vi.xgb.cb$importance
vi.xgb.cb$Model <- "xgb.cb"
vi.xgb.cb$rn <- gsub("`", "",rownames(vi.xgb.cb))




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# DATA SET 4 ~  REDUCED BALANCED #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
vi.dt.rb <- varImp(dt.rb)
vi.dt.rb <- vi.dt.rb$importance
vi.dt.rb$Model <- "dt.rb"
vi.dt.rb$rn <- gsub("`", "",rownames(vi.dt.rb))

vi.knn.rb <- varImp(knn.rb)
vi.knn.rb <- vi.knn.rb$importance
t <- data.frame(Overall = vi.knn.rb[,"Y"])
t$Model <- "knn.rb"
t$rn <- rownames(vi.knn.rb)
vi.knn.rb <- t 

vi.lr.rb <- varImp(lr.rb)
vi.lr.rb <- vi.lr.rb$importance
vi.lr.rb$Model <- "lr.rb"
vi.lr.rb$rn <- gsub("`", "",rownames(vi.lr.rb))

vi.nb.rb <- varImp(nb.rb)
vi.nb.rb <- vi.nb.rb$importance
t <- data.frame(Overall = vi.nb.rb[,"Y"])
t$Model <- "nb.rb"
t$rn <- rownames(vi.nb.rb)
vi.nb.rb <- t 

vi.nn.rb <- varImp(nn.rb)
vi.nn.rb <- vi.nn.rb$importance
vi.nn.rb$Model <- "nn.rb"
vi.nn.rb$rn <- gsub("`", "",rownames(vi.nn.rb))

vi.rf.rb <- varImp(rf.rb)
vi.rf.rb <- vi.rf.rb$importance
vi.rf.rb$Model <- "rf.rb"
vi.rf.rb$rn <- gsub("`", "",rownames(vi.rf.rb))

vi.svml.rb <- varImp(svml.rb)
vi.svml.rb <- vi.svml.rb$importance
t <- data.frame(Overall = vi.svml.rb[,"Y"])
t$Model <- "svml.rb"
t$rn <- rownames(vi.svml.rb)
vi.svml.rb <- t 

vi.svmp.rb <- varImp(svmp.rb)
vi.svmp.rb <- vi.svmp.rb$importance
t <- data.frame(Overall = vi.svmp.rb[,"Y"])
t$Model <- "svmp.rb"
t$rn <- rownames(vi.svmp.rb)
vi.svmp.rb <- t 

vi.xgb.rb <- varImp(xgb.rb)
vi.xgb.rb <- vi.xgb.rb$importance
vi.xgb.rb$Model <- "xgb.rb"
vi.xgb.rb$rn <- gsub("`", "",rownames(vi.xgb.rb))




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# COMBINE LOGISTIC REGRESSION & ARTIFICIAL NEURAL NETWORK VARIABLE IMPORTANCE #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
vil <- rbind(vi.lr.cu, vi.nn.cu, vi.lr.ru, vi.nn.ru, vi.lr.cb, vi.nn.cb, vi.lr.rb, vi.nn.rb)

# CORRECT VARIABLE NAMES
vil$Feature <- sapply(vil$rn, function(i)
  if (grepl("^Units",i)) {"Units"}
  else if (grepl("^TransUnits",i)) {"TransUnits"}
  else if (grepl("^TeachingGrp",i)) {"TeachingGrp"}
  else if (grepl("^SIC_avg",i)) {"SIC_avg"}
  else if (grepl("^SchldUnits",i)) {"SchldUnits"}
  else if (grepl("^SchldToNxtFY",i)) {"SchldToNxtFY"}
  else if (grepl("^SchldMaxDays",i)) {"SchldMaxDays"}
  else if (grepl("^RemedialCrs",i)) {"RemedialCrs"}
  else if (grepl("^RelativePerf",i)) {"RelativePerf"}
  else if (grepl("^Probation",i)) {"Probation"}
  else if (grepl("^PrevGPA",i)) {"PrevGPA"}
  else if (grepl("^PrevDegLvl",i)) {"PrevDegLvl"}
  else if (grepl("^PrevAtt4Yr",i)) {"PrevAtt4Yr"}
  else if (grepl("^PrevAtt2Yr",i)) {"PrevAtt2Yr"}
  else if (grepl("^ParentHiEd",i)) {"ParentHiEd"}
  else if (grepl("^P_VaTuit",i)) {"P_VaTuit"}
  else if (grepl("^P_SOC",i)) {"P_SOC"}
  else if (grepl("^P_ROR",i)) {"P_ROR"}
  else if (grepl("^P_RFA",i)) {"P_RFA"}
  else if (grepl("^P_REV",i)) {"P_REV"}
  else if (grepl("^P_OffsiteDisc",i)) {"P_OffsiteDisc"}
  else if (grepl("^P_NOS",i)) {"P_NOS"}
  else if (grepl("^P_MiscSchlr",i)) {"P_MiscSchlr"}
  else if (grepl("^P_HIC",i)) {"P_HIC"}
  else if (grepl("^P_FRD",i)) {"P_FRD"}
  else if (grepl("^P_FBA",i)) {"P_FBA"}
  else if (grepl("^P_FAS",i)) {"P_FAS"}
  else if (grepl("^P_FAO",i)) {"P_FAO"}
  else if (grepl("^P_FAC",i)) {"P_FAC"}
  else if (grepl("^P_EPS",i)) {"P_EPS"}
  else if (grepl("^P_ECS",i)) {"P_ECS"}
  else if (grepl("^P_ECP",i)) {"P_ECP"}
  else if (grepl("^P_BVO",i)) {"P_BVO"}
  else if (grepl("^P_BVA",i)) {"P_BVA"}
  else if (grepl("^P_BV3",i)) {"P_BV3"}
  else if (grepl("^P_BSS",i)) {"P_BSS"}
  else if (grepl("^P_BSP",i)) {"P_BSP"}
  else if (grepl("^P_BRR",i)) {"P_BRR"}
  else if (grepl("^P_BOM",i)) {"P_BOM"}
  else if (grepl("^P_BCR",i)) {"P_BCR"}
  else if (grepl("^P_BCB",i)) {"P_BCB"}
  else if (grepl("^P_B2B",i)) {"P_B2B"}
  else if (grepl("^OnlineOnly",i)) {"OnlineOnly"}
  else if (grepl("^N_WriteOff",i)) {"N_WriteOff"}
  else if (grepl("^N_REC",i)) {"N_REC"}
  else if (grepl("^N_IntlStu",i)) {"N_IntlStu"}
  else if (grepl("^N_Hold",i)) {"N_Hold"}
  else if (grepl("^N_FPW",i)) {"N_FPW"}
  else if (grepl("^N_ECD",i)) {"N_ECD"}
  else if (grepl("^N_CRD",i)) {"N_CRD"}
  else if (grepl("^N_BPP",i)) {"N_BPP"}
  else if (grepl("^N_BLK",i)) {"N_BLK"}
  else if (grepl("^N_BKA",i)) {"N_BKA"}
  else if (grepl("^N_BCW",i)) {"N_BCW"}
  else if (grepl("^MilitaryYN",i)) {"MilitaryYN"}
  else if (grepl("^MaritalStat",i)) {"MaritalStat"}
  else if (grepl("^LearningGrp",i)) {"LearningGrp"}
  else if (grepl("^GovtPrgmY",i)) {"GovtPrgmY"}
  else if (grepl("^Gender",i)) {"Gender"}
  else if (grepl("^FCD_FYQ",i)) {"FCD_FYQ"}
  else if (grepl("^FAFSAby",i)) {"FAFSAby"}
  else if (grepl("^Ethnicity",i)) {"Ethnicity"}
  else if (grepl("^EFC",i)) {"EFC"}
  else if (grepl("^DFUWI",i)) {"DFUWI"}
  else if (grepl("^DependentsY",i)) {"DependentsY"}
  else if (grepl("^DegreeAwardType",i)) {"DegreeAwardType"}
  else if (grepl("^DaysToFYClose",i)) {"DaysToFYClose"}
  else if (grepl("^DaysToFC",i)) {"DaysToFC"}
  else if (grepl("^DaysSinceLastAtt",i)) {"DaysSinceLastAtt"}
  else if (grepl("^ClassLength_avg",i)) {"ClassLength_avg"}
  else if (grepl("^Classes",i)) {"Classes"}
  else if (grepl("^ClassCPE",i)) {"ClassCPE"}
  else if (grepl("^CIP2D",i)) {"CIP2D"}
  else if (grepl("^ChildrenY",i)) {"ChildrenY"}
  else if (grepl("^AGI_PerCapita",i)) {"AGI_PerCapita"}
  else if (grepl("^AGI",i)) {"AGI"}
  else if (grepl("^Age",i)) {"Age"}
  else if (grepl("^AdjunctOnly",i)) {"AdjunctOnly"}
  else if (grepl("^ActiveDuty",i)) {"ActiveDuty"}
  else {NA}
)

vil[is.na(vil)] <- 0
vil <- aggregate(vil[,"Overall"], by=list(Model=vil$Model,Feature=vil$Feature), FUN=max)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# COMBINE TREE BASED MODEL VARIABLE IMPORTANCE #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
rm(vit)
vit <- rbind(vi.dt.cu, vi.rf.cu, vi.xgb.cu,
             vi.dt.ru, vi.rf.ru, vi.xgb.ru,
             vi.dt.cb, vi.rf.cb, vi.xgb.cb,
             vi.dt.rb, vi.rf.rb, vi.xgb.rb)

# CORRECT VARIABLE NAMES
vit$Feature <- sapply(vit$rn, function(i)
  if (grepl("^Units",i)) {"Units"}
  else if (grepl("^TransUnits",i)) {"TransUnits"}
  else if (grepl("^TeachingGrp",i)) {"TeachingGrp"}
  else if (grepl("^SIC_avg",i)) {"SIC_avg"}
  else if (grepl("^SchldUnits",i)) {"SchldUnits"}
  else if (grepl("^SchldToNxtFY",i)) {"SchldToNxtFY"}
  else if (grepl("^SchldMaxDays",i)) {"SchldMaxDays"}
  else if (grepl("^RemedialCrs",i)) {"RemedialCrs"}
  else if (grepl("^RelativePerf",i)) {"RelativePerf"}
  else if (grepl("^Probation",i)) {"Probation"}
  else if (grepl("^PrevGPA",i)) {"PrevGPA"}
  else if (grepl("^PrevDegLvl",i)) {"PrevDegLvl"}
  else if (grepl("^PrevAtt4Yr",i)) {"PrevAtt4Yr"}
  else if (grepl("^PrevAtt2Yr",i)) {"PrevAtt2Yr"}
  else if (grepl("^ParentHiEd",i)) {"ParentHiEd"}
  else if (grepl("^P_VaTuit",i)) {"P_VaTuit"}
  else if (grepl("^P_SOC",i)) {"P_SOC"}
  else if (grepl("^P_ROR",i)) {"P_ROR"}
  else if (grepl("^P_RFA",i)) {"P_RFA"}
  else if (grepl("^P_REV",i)) {"P_REV"}
  else if (grepl("^P_OffsiteDisc",i)) {"P_OffsiteDisc"}
  else if (grepl("^P_NOS",i)) {"P_NOS"}
  else if (grepl("^P_MiscSchlr",i)) {"P_MiscSchlr"}
  else if (grepl("^P_HIC",i)) {"P_HIC"}
  else if (grepl("^P_FRD",i)) {"P_FRD"}
  else if (grepl("^P_FBA",i)) {"P_FBA"}
  else if (grepl("^P_FAS",i)) {"P_FAS"}
  else if (grepl("^P_FAO",i)) {"P_FAO"}
  else if (grepl("^P_FAC",i)) {"P_FAC"}
  else if (grepl("^P_EPS",i)) {"P_EPS"}
  else if (grepl("^P_ECS",i)) {"P_ECS"}
  else if (grepl("^P_ECP",i)) {"P_ECP"}
  else if (grepl("^P_BVO",i)) {"P_BVO"}
  else if (grepl("^P_BVA",i)) {"P_BVA"}
  else if (grepl("^P_BV3",i)) {"P_BV3"}
  else if (grepl("^P_BSS",i)) {"P_BSS"}
  else if (grepl("^P_BSP",i)) {"P_BSP"}
  else if (grepl("^P_BRR",i)) {"P_BRR"}
  else if (grepl("^P_BOM",i)) {"P_BOM"}
  else if (grepl("^P_BCR",i)) {"P_BCR"}
  else if (grepl("^P_BCB",i)) {"P_BCB"}
  else if (grepl("^P_B2B",i)) {"P_B2B"}
  else if (grepl("^OnlineOnly",i)) {"OnlineOnly"}
  else if (grepl("^N_WriteOff",i)) {"N_WriteOff"}
  else if (grepl("^N_REC",i)) {"N_REC"}
  else if (grepl("^N_IntlStu",i)) {"N_IntlStu"}
  else if (grepl("^N_Hold",i)) {"N_Hold"}
  else if (grepl("^N_FPW",i)) {"N_FPW"}
  else if (grepl("^N_ECD",i)) {"N_ECD"}
  else if (grepl("^N_CRD",i)) {"N_CRD"}
  else if (grepl("^N_BPP",i)) {"N_BPP"}
  else if (grepl("^N_BLK",i)) {"N_BLK"}
  else if (grepl("^N_BKA",i)) {"N_BKA"}
  else if (grepl("^N_BCW",i)) {"N_BCW"}
  else if (grepl("^MilitaryYN",i)) {"MilitaryYN"}
  else if (grepl("^MaritalStat",i)) {"MaritalStat"}
  else if (grepl("^LearningGrp",i)) {"LearningGrp"}
  else if (grepl("^GovtPrgmY",i)) {"GovtPrgmY"}
  else if (grepl("^Gender",i)) {"Gender"}
  else if (grepl("^FCD_FYQ",i)) {"FCD_FYQ"}
  else if (grepl("^FAFSAby",i)) {"FAFSAby"}
  else if (grepl("^Ethnicity",i)) {"Ethnicity"}
  else if (grepl("^EFC",i)) {"EFC"}
  else if (grepl("^DFUWI",i)) {"DFUWI"}
  else if (grepl("^DependentsY",i)) {"DependentsY"}
  else if (grepl("^DegreeAwardType",i)) {"DegreeAwardType"}
  else if (grepl("^DaysToFYClose",i)) {"DaysToFYClose"}
  else if (grepl("^DaysToFC",i)) {"DaysToFC"}
  else if (grepl("^DaysSinceLastAtt",i)) {"DaysSinceLastAtt"}
  else if (grepl("^ClassLength_avg",i)) {"ClassLength_avg"}
  else if (grepl("^Classes",i)) {"Classes"}
  else if (grepl("^ClassCPE",i)) {"ClassCPE"}
  else if (grepl("^CIP2D",i)) {"CIP2D"}
  else if (grepl("^ChildrenY",i)) {"ChildrenY"}
  else if (grepl("^AGI_PerCapita",i)) {"AGI_PerCapita"}
  else if (grepl("^AGI",i)) {"AGI"}
  else if (grepl("^Age",i)) {"Age"}
  else if (grepl("^AdjunctOnly",i)) {"AdjunctOnly"}
  else if (grepl("^ActiveDuty",i)) {"ActiveDuty"}
  else {NA}
)

vit[is.na(vit)] <- 0
vit <- aggregate(vit[,"Overall"], by=list(Model=vit$Model,Feature=vit$Feature), FUN=sum)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# COMBINE REMAINING MODELS VARIABLE IMPORTANCE #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
vio <- rbind(vi.knn.cu, vi.knn.ru, vi.knn.cb, vi.knn.rb,
           vi.nb.cu, vi.nb.ru, vi.nb.cb, vi.nb.rb,
           vi.svml.cu, vi.svml.ru,vi.svml.cb, vi.svml.rb,
           vi.svmp.cu, vi.svmp.ru,vi.svmp.cb,vi.svmp.rb)

vio <- vio[c(2,3,1)]
colnames(vio)[2] <- "Feature"
colnames(vio)[3] <- "x"




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# COMBINE VARIABLE IMPORTANCE FOR ALL MODELS #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
v <- rbind(vil,vit,vio)

vi <- cbind(Algorithm=sapply(strsplit(v$Model,"[.]"), `[`, 1)
            ,Data=sapply(strsplit(v$Model,"[.]"), `[`, 2)
            ,v)

save(vi,file = "Z:/Advanced Analytics/One Year Retention Risk Indicator/Analyses/Saved Objects/04 Variable Importance/vi.rda")











############################ EXAMINE FEATURE IMPORTANCE ################################
load("Z:/Advanced Analytics/One Year Retention Risk Indicator/Analyses/Saved Objects/04 Variable Importance/vi.rda")
library(psych)

##### Across All Models
means.feature <- describeBy(vi$x,group = vi$Feature,mat = T)
colnames(means.feature)[2] <- "Feature"
colnames(means.feature)[5] <- "Importance"
means.feature$LB <- means.feature$Importance - 1.96*means.feature$se
means.feature$UB <- means.feature$Importance + 1.96*means.feature$se
means.feature <- means.feature[order(-means.feature$Importance),]

setwd("Z:/Advanced Analytics/One Year Retention Risk Indicator/Analyses/Saved Objects/05 Variable Importance/")
save(means.feature, file = "means.feature.rda")
write.csv(means.feature, file = "Feature Results.csv", row.names = F)




load(file = "Z:/Advanced Analytics/One Year Retention Risk Indicator/Analyses/Saved Objects/05 Variable Importance/means.feature.rda")
feature.plot <- means.feature[order(-means.feature$Importance),]
feature.plot <- head(feature.plot,n=25)
dotplot( with(feature.plot,reorder(Feature,Importance)) ~ LB + Importance + UB, feature.plot,
         ylab="Feature", xlab = "Importance (95% C.I.)",
         main="Feature Importance Across All Models",
         col = c("lightgreen","blue","lightgreen"))




### Feature Importance for XGB and RF
fi.xf <- rbind(vi[vi$Algorithm=="xgb",],vi[vi$Algorithm=="rf",])
fi.xf.means <- describeBy(fi.xf$x,group = fi.xf$Feature,mat = T)
colnames(fi.xf.means)[2] <- "Feature"
colnames(fi.xf.means)[5] <- "Importance"
fi.xf.means$LB <- fi.xf.means$Importance - 1.96*fi.xf.means$se
fi.xf.means$UB <- fi.xf.means$Importance + 1.96*fi.xf.means$se
fi.xf.means <- fi.xf.means[order(-fi.xf.means$Importance),]

setwd("Z:/Advanced Analytics/One Year Retention Risk Indicator/Analyses/Saved Objects/05 Variable Importance/")
save(fi.xf.means, file = "fi.xf.means.rda")
write.csv(fi.xf.means, file = "XGB and RF Feature Results.csv", row.names = F)

load(file = "Z:/Advanced Analytics/One Year Retention Risk Indicator/Analyses/Saved Objects/05 Variable Importance/fi.xf.means.rda")
feature.plot <- fi.xf.means[order(-fi.xf.means$Importance),]
feature.plot <- head(feature.plot,n=25)
dotplot( with(feature.plot,reorder(Feature,Importance)) ~ LB + Importance + UB, feature.plot,
         ylab="Feature", xlab = "Importance (95% C.I.)",
         main="Feature Importance for XGB and RF Models",
        # col = c("blue","red","blue"),
         col = c("lightgreen","blue","lightgreen"))

dotplot( with(feature.plot,reorder(Feature,Importance)) ~ Importance, feature.plot, ylab="Feature",main="Feature Importance for XGB and RF Models")








library(ggplot2)
ggplot(means.feature, aes(x=with(means.feature,reorder(Feature,Importance)),y=Importance)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  coord_flip() +
  geom_errorbar(aes(ymin=LB, ymax=UB),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  ggtitle("Average Variable Importance")


feature.plot <- means.feature[order(-means.feature$Importance),]
feature.plot <- head(feature.plot,n=25)
vi.plot <- vi[!vi$Feature %in% feature.plot$Feature,]

vi.plot$order <- with(vi.plot,reorder(Feature,x,mean))
boxplot(x ~ order,
        data = vi.plot,
        # horizontal = T,
        main = "Feature Importance")


############################ INDIVIDUAL FEATURES ################################
raw <- read.csv("Z:/Advanced Analytics/One Year Retention Risk Indicator/Analyses/Data.csv"
                ,na.strings = "NULL")


raw$Outcome <- as.factor(sapply(raw$Dropout, function(i)
  if (i == "Y") {"Dropout"}
  else if (i == "N") {"Persist"}
  else {NA}))


color <- c("coral","aquamarine3")
yrange <- function(y) c(0,max(y)*1.25) #function to make Y axis 25% bigger


# Export Plots
setwd("Z:/Advanced Analytics/One Year Retention Risk Indicator/Analyses/Saved Objects/05 Variable Importance/")
setwd("C:/Users/MKirkpatrick/Desktop/R Files/05 Variable Importance/")
pdf("Plots.pdf")

dotplot( with(feature.plot,reorder(Feature,Importance)) ~ LB + Importance + UB, 
         feature.plot, 
         ylab="Feature", xlab = "Importance",
         col = c("blue","red","blue"),
         bg = "lightgreen")


