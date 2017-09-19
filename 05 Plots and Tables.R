
###########################################
#     CREATE VARIOUS PLOTS AND TABLES     #
###########################################

# Plots and tables are created from objects that were created in the preceding code

library(lattice)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# PLOT ~ RECURSIVE FEATURE ELIMINATION #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
library(caret)
setwd("Z:/Advanced Analytics/One Year Retention Risk Indicator/Analyses/Saved Objects/01 Data Prep/")
load("rfe.ru.rda")
load("rfe.rb.rda")

png("RFE ru plot.png", width = 528, height = 288)
plot(rfe.ru)
dev.off()

png("RFE rb plot.png", width = 528, height = 288)
plot(rfe.rb)
dev.off()




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# PREP ~ ALGORITHM EVLAUATION METRICS #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
setwd("Z:/Advanced Analytics/One Year Retention Risk Indicator/Analyses/Saved Objects/04 Model Testing/")
load("results.rda")

# Capitalize names
results$Algorithm <- toupper(results$Algorithm)
results$Model <- with(results,paste(Algorithm,"_",Data, sep = ""))




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# PLOT ~ ALGORITHM EVLAUATION METRICS BY MODEL #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
png("Model ROC Area.png", width = 528, height = 600)
results$order <- with(results,reorder(Model,ROC.Area))
dotplot( order ~ ROC.Area, results, xlab = NULL #,main="ROC Area by Model"  
         ,scales=list(x=list(cex=1.02),y=list(cex=1.02)))
dev.off()

png("Model Accuracy.png", width = 528, height = 600)
results$order <- with(results,reorder(Model,Accuracy))
dotplot( order ~ Accuracy, results,xlab = NULL#, main="Accuracy by Model"  
         ,scales=list(x=list(cex=1.02),y=list(cex=1.02)))
dev.off()

png("Model Sensitivity.png", width = 528, height = 600)
results$order <- with(results,reorder(Model,Sensitivity))
dotplot( order ~ Sensitivity, results, xlab = NULL #,main="Sensitivity by Model" 
         ,scales=list(x=list(cex=1.02),y=list(cex=1.02)))
dev.off()

png("Model Specificity.png", width = 528, height = 600)
results$order <- with(results,reorder(Model,Specificity))
dotplot( order ~ Specificity, results, xlab = NULL#,main="Specificity by Model"  
         ,scales=list(x=list(cex=1.02),y=list(cex=1.02)))
dev.off()




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# PLOT ~ ALGORITHM EVLAUATION METRICS BY ALGORITHM #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# AVERAGE METRICS FOR EACH ALGORITHM
results.algo <- aggregate(results[,4:7], by=list(results$Algorithm), FUN=mean)

png("Algorithm Metrics.png", width = 528, height = 600)
par(mfrow=c(2,2))

results$order <- with(results,reorder(Algorithm,ROC.Area,mean))
boxplot(ROC.Area ~ order,las=2,
        data = results,
        main = "ROC Area",par(cex.axis=1.2))
results$order <- with(results,reorder(Algorithm,Accuracy,mean))
boxplot(Accuracy ~ order,las=2,
        data = results,
        main = "Accuracy",par(cex.axis=1.2))
results$order <- with(results,reorder(Algorithm,Sensitivity,mean))
boxplot(Sensitivity ~ order,las=2,
        data = results,
        main = "Sensitivity",par(cex.axis=1.2))
results$order <- with(results,reorder(Algorithm,Specificity,mean))
boxplot(Specificity ~ order,las=2,
        data = results,
        main = "Specificity",par(cex.axis=1.2))

par(mfrow=c(1,1))
dev.off()




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# PLOT ~ ALGORITHM EVLAUATION METRICS BY DATA SET #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# AVERAGE METRICS FOR EACH DATA SET
results.data <- aggregate(results[,4:7], by=list(results$Data), FUN=mean)

png("Dataset Metrics.png", width = 528, height = 528)
par(mfrow=c(2,2))

results$order <- with(results,reorder(Data,ROC.Area,mean))
boxplot(ROC.Area ~ order,
        data = results,
        main = "ROC Area",par(cex.axis=1.2))
results$order <- with(results,reorder(Data,Accuracy,mean))
boxplot(Accuracy ~ order,
        data = results,
        main = "Accuracy",par(cex.axis=1.2))
results$order <- with(results,reorder(Data,Sensitivity,mean))
boxplot(Sensitivity ~ order,
        data = results,
        main = "Sensitivity",par(cex.axis=1.2))
results$order <- with(results,reorder(Data,Specificity,mean))
boxplot(Specificity ~ order,
        data = results,
        main = "Specificity",par(cex.axis=1.2))

par(mfrow=c(1,1))
dev.off()




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# PREP ~ VARIABLE IMPORTANCE OVERALL #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
load("Z:/Advanced Analytics/One Year Retention Risk Indicator/Analyses/Saved Objects/05 Variable Importance/vi.rda")
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




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# PLOT ~ VARIABLE IMPORTANCE OVERALL #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
setwd("Z:/Advanced Analytics/One Year Retention Risk Indicator/Analyses/Saved Objects/05 Variable Importance/")
load(file = "means.feature.rda")

feature.plot <- means.feature[order(-means.feature$Importance),]
feature.plot <- head(feature.plot,n=25)

png("Feature Importance All Models.png", width = 528, height = 600)
dotplot( with(feature.plot,reorder(Feature,Importance)) ~ LB + Importance + UB, feature.plot,
         xlab = "Mean Importance (95% C.I.)",
         #main="Feature Importance Across All Models",
         col = c("lightgreen","blue","lightgreen"),
         scales=list(x=list(cex=1.02),y=list(cex=1.02)))
dev.off()




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# PREP ~ VARIABLE IMPORTANCE XGB and RF #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
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




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# PLOT ~ VARIABLE IMPORTANCE XGB and RF #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
setwd("Z:/Advanced Analytics/One Year Retention Risk Indicator/Analyses/Saved Objects/05 Variable Importance/")
load(file = "fi.xf.means.rda")
feature.plot <- fi.xf.means[order(-fi.xf.means$Importance),]
feature.plot <- head(feature.plot,n=25)

png("Feature Importance XBG and RF.png", width = 528, height = 600)
dotplot( with(feature.plot,reorder(Feature,Importance)) ~ LB + Importance + UB, feature.plot,
         xlab = "Importance (95% C.I.)",
         #main="Feature Importance for XGB and RF Models",
         col = c("lightgreen","blue","lightgreen"),
         scales=list(x=list(cex=1.02),y=list(cex=1.02)))
dev.off()




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# PREP ~ INDIVIDUAL FEATURES #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
raw <- read.csv("Z:/Advanced Analytics/One Year Retention Risk Indicator/Analyses/Data.csv"
                ,na.strings = "NULL")

raw$Outcome <- as.factor(sapply(raw$Dropout, function(i)
  if (i == "Y") {"Dropout"}
  else if (i == "N") {"Persist"}
  else {NA}))

raw$DFUWI <- as.factor(sapply(raw$DFUWI, function(i)
  if (i == "Y") {"DFUWI"}
  else if (i == "N") {"Non-DFUWI"}
  else {NA}))

raw$DegreeAwardType <- as.factor(sapply(raw$DegreeAwardType, function(i)
  if (i == "Associate Degree") {"AA"}
  else if (i == "Bachelor Degree") {"BA"}
  else if (i == "Master's Degree") {"MA"}
  else {NA}))

raw$RelativePerf <- as.factor(sapply(raw$RelativePerf, function(i)
  if (i == "Above Median") {"Above"}
  else if (i == "Below Median") {"Below"}
  else {NA}))

raw$RFA <- as.factor(sapply(raw$P_RFA, function(i)
  if (i == "Y") {"Pending"}
  else if (i == "N") {"Approved"}
  else {NA}))

raw$Learning <- as.factor(sapply(raw$LearningGrp, function(i)
  if (i == "Negative") {"Neg"}
  else if (i == "No Response") {"None"}
  else if (i == "Positive") {"Pos"}
  else {NA}))

raw$Teaching <- as.factor(sapply(raw$TeachingGrp, function(i)
  if (i == "Negative") {"Neg"}
  else if (i == "No Response") {"None"}
  else if (i == "Positive") {"Pos"}
  else {NA}))

raw$PrevDeg <- as.factor(sapply(raw$PrevDegLvl, function(i)
  if (i == "Lower") {"Lower/Unkwn"}
  else if (i == "Equal or Higher") {"Higher/Equal"}
  else {NA}))

raw$CIP <- as.factor(sapply(raw$CIP2D, function(i)
  if (i == "09 - COMMUNICATION, JOURNALISM, AND RELATED PROGRAMS.") {"09 Com"}
  else if (i == "11 - COMPUTER AND INFORMATION SCIENCES AND SUPPORT SERVICES.") {"11 Comp"}
  else if (i == "13 - EDUCATION.") {"13 Edu"}
  else if (i == "14 - ENGINEERING.") {"14 Eng"}
  else if (i == "15 - ENGINEERING TECHNOLOGIES AND ENGINEERING-RELATED FIELDS.") {"15 EngT"}
  else if (i == "16 - FOREIGN LANGUAGES, LITERATURES, AND LINGUISTICS.") {"16 FLng"}
  else if (i == "22 - LEGAL PROFESSIONS AND STUDIES.") {"22 Legl"}
  else if (i == "23 - ENGLISH LANGUAGE AND LITERATURE/LETTERS.") {"23 Engl"}
  else if (i == "24 - LIBERAL ARTS AND SCIENCES, GENERAL STUDIES AND HUMANITIES.") {"24 LibA"}
  else if (i == "26 - BIOLOGICAL AND BIOMEDICAL SCIENCES.") {"26 Bio"}
  else if (i == "27 - MATHEMATICS AND STATISTICS.") {"27 Math"}
  else if (i == "30 - MULTI/INTERDISCIPLINARY STUDIES.") {"30 IntSt"}
  else if (i == "31 - PARKS, RECREATION, LEISURE, AND FITNESS STUDIES.") {"31 Fit"}
  else if (i == "42 - PSYCHOLOGY.") {"42 Psy"}
  else if (i == "43 - HOMELAND SECURITY, LAW ENFORCEMENT, FIREFIGHTING AND RELATED PROTECTIVE SERVICES") {"43 Home"}
  else if (i == "44 - PUBLIC ADMINISTRATION AND SOCIAL SERVICE PROFESSIONS.") {"44 Pub"}
  else if (i == "45 - SOCIAL SCIENCES.") {"45 SS"}
  else if (i == "50 - VISUAL AND PERFORMING ARTS.") {"50 Arts"}
  else if (i == "51 - HEALTH PROFESSIONS AND RELATED PROGRAMS.") {"51 Hlth"}
  else if (i == "52 - BUSINESS, MANAGEMENT, MARKETING, AND RELATED SUPPORT SERVICES.") {"52 Bus"}
  else if (i == "54 - HISTORY.") {"54 Hist"}
  else {NA}))


#defaults for plots
color <- c("coral","aquamarine3")
yrange <- function(y) c(0,max(y)*1.25) #function to make Y axis 25% bigger
setwd("Z:/Advanced Analytics/One Year Retention Risk Indicator/Analyses/Saved Objects/05 Variable Importance/")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# PLOT ~ INDIVIDUAL FEATURES #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# DFUWI
png("DFUWI.png", width = 480, height = 480)
par(mfrow=c(1,2)) 
t <- with(raw, table(Outcome,DFUWI))
barplot(t, beside=T, col=color, ylim = yrange(t),
        main = "Count")
legend("topleft", levels(raw$Outcome),pch=15,
       col=color, bty="n")
p <- prop.table(t,2) #if want a propotion table
barplot(p, col=color,main = "100%")
par(mfrow=c(1,1))
dev.off()

# DegreeAwardType
png("DegreeAwardType.png", width = 480, height = 480)
par(mfrow=c(1,2)) 
t <- with(raw, table(Outcome,DegreeAwardType))
barplot(t, beside=T, col=color, ylim = yrange(t),
        main = "Count")
legend("topleft", levels(raw$Outcome),pch=15,
       col=color, bty="n")
p <- prop.table(t,2) #if want a propotion table
barplot(p, col=color,main = "100%")
par(mfrow=c(1,1))
dev.off()

# RelativePerf
png("RelativePerf.png", width = 480, height = 480)
par(mfrow=c(1,2)) 
t <- with(raw, table(Outcome,RelativePerf))
barplot(t, beside=T, col=color, ylim = yrange(t),
        main = "Count")
legend("topleft", levels(raw$Outcome),pch=15,
       col=color, bty="n")
p <- prop.table(t,2) #if want a propotion table
barplot(p, col=color,main = "100%")
par(mfrow=c(1,1))
dev.off()

# RFA
png("RFA.png", width = 480, height = 480)
par(mfrow=c(1,2)) 
t <- with(raw, table(Outcome,RFA))
barplot(t, beside=T, col=color, ylim = yrange(t),
        main = "Count")
legend("topleft", levels(raw$Outcome),pch=15,
       col=color, bty="n")
p <- prop.table(t,2) #if want a propotion table
barplot(p, col=color,main = "100%")
par(mfrow=c(1,1))
dev.off()

# CIP2D
png("CIP2D.png", width = 480, height = 800)
par(mfrow=c(2,1)) 
t <- with(raw, table(Outcome,CIP))
barplot(t, beside=T, col=color, ylim = yrange(t),
        main = "Count",las=2)
legend("topleft", levels(raw$Outcome),pch=15,
       col=color, bty="n")
p <- prop.table(t,2) #if want a propotion table
barplot(p, col=color,main = "100%",las=2)
par(mfrow=c(1,1))
dev.off()

# Perception of Learning
png("PercOfLearning.png", width = 480, height = 480)
par(mfrow=c(1,2)) 
t <- with(raw, table(Outcome,Learning))
barplot(t, beside=T, col=color, ylim = yrange(t),
        main = "Count")
legend("topleft", levels(raw$Outcome),pch=15,
       col=color, bty="n")
p <- prop.table(t,2) #if want a propotion table
barplot(p, col=color,main = "100%")
par(mfrow=c(1,1))
dev.off()

# Perception of Teaching
png("PercOfTeaching.png", width = 480, height = 480)
par(mfrow=c(1,2)) 
t <- with(raw, table(Outcome,Teaching))
barplot(t, beside=T, col=color, ylim = yrange(t),
        main = "Count")
legend("topleft", levels(raw$Outcome),pch=15,
       col=color, bty="n")
p <- prop.table(t,2) #if want a propotion table
barplot(p, col=color,main = "100%")
par(mfrow=c(1,1))
dev.off()

# PrevDegLvl
png("PrevDegLvl.png", width = 528, height = 480)
par(mfrow=c(1,2)) 
t <- with(raw, table(Outcome,PrevDeg))
barplot(t, beside=T, col=color, ylim = yrange(t),
        main = "Count")
legend("topleft", levels(raw$Outcome),pch=15,
       col=color, bty="n")
p <- prop.table(t,2) #if want a propotion table
barplot(p, col=color,main = "100%")
par(mfrow=c(1,1))
dev.off()

# FCD_FYQ
png("FCD_FYQ.png", width = 480, height = 480)
par(mfrow=c(1,2)) 
t <- with(raw, table(Outcome,FCD_FYQ))
barplot(t, beside=T, col=color, ylim = yrange(t),
        main = "Count")
legend("topleft", levels(raw$Outcome),pch=15,
       col=color, bty="n")
p <- prop.table(t,2) #if want a propotion table
barplot(p, col=color,main = "100%")
par(mfrow=c(1,1))
dev.off()


png("DaysToFYClose.png", width = 480, height = 480)
with(raw, boxplot(DaysToFYClose ~ Outcome, col = color))
dev.off()

png("TransUnits.png", width = 480, height = 480)
with(raw, boxplot(TransUnits ~ Outcome, col = color))
dev.off()

png("PrevGPA.png", width = 480, height = 480)
with(raw, boxplot(PrevGPA ~ Outcome, col = color))
dev.off()

png("AGI_PerCapita.png", width = 480, height = 480)
with(raw, boxplot(AGI_PerCapita ~ Outcome, col = color))
dev.off()




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# VARIABLE IMPORTANCE CHI SQUARE TABLE #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
raw <- read.csv("Z:/Advanced Analytics/One Year Retention Risk Indicator/Analyses/Data.csv"
                ,na.strings = "NULL")
summary(raw)

dropout <- with(raw,data.frame(Dropout))
features <- raw[,4:79]

# Get list of variable types
l <- lapply(features,is.factor)
# create categorical data set
cat <- features[,l==T]
# create continuous data set
con <- features[,l==F]


# Recode Values
dropout$Dropout <- gsub("Y","Dropout", dropout$Dropout)
dropout$Dropout <- gsub("N","Persist", dropout$Dropout)

cat$DFUWI <- gsub("Y","DFUWI", cat$DFUWI)
cat$DFUWI <- gsub("N","Non-DFUWI", cat$DFUWI)
cat$P_RFA <- gsub("Y","Admission Pending", cat$P_RFA)
cat$P_RFA <- gsub("N","Admission Approved", cat$P_RFA)


#DFUWI, DegreeAwardType, RelativePerf, P_RFA, CIP2D, LearningGrp, TeachingGrp, PrevDegLvl
as.matrix(table(cat$DFUWI))
t.count <- rbind(
  as.matrix(table(cat$DFUWI)),
  as.matrix(table(cat$DegreeAwardType)),
  as.matrix(table(cat$RelativePerf)),
  as.matrix(table(cat$P_RFA)),
  as.matrix(table(cat$CIP2D)),
  as.matrix(table(cat$LearningGrp)),
  as.matrix(table(cat$TeachingGrp)),
  as.matrix(table(cat$PrevDegLvl)))

t.perc <- rbind(
  as.matrix(prop.table(table(cat$DFUWI))),
  as.matrix(prop.table(table(cat$DegreeAwardType))),
  as.matrix(prop.table(table(cat$RelativePerf))),
  as.matrix(prop.table(table(cat$P_RFA))),
  as.matrix(prop.table(table(cat$CIP2D))),
  as.matrix(prop.table(table(cat$LearningGrp))),
  as.matrix(prop.table(table(cat$TeachingGrp))),
  as.matrix(prop.table(table(cat$PrevDegLvl))))

t.perc <- round(t.perc*100,1)


d.count <- rbind(
                table(cat$DFUWI,dropout$Dropout),
                table(cat$DegreeAwardType,dropout$Dropout),
                table(cat$RelativePerf,dropout$Dropout),
                table(cat$P_RFA,dropout$Dropout),
                table(cat$CIP2D,dropout$Dropout),
                table(cat$LearningGrp,dropout$Dropout),
                table(cat$TeachingGrp,dropout$Dropout),
                table(cat$PrevDegLvl,dropout$Dropout))


d.perc <- rbind(
              prop.table(table(cat$DFUWI,dropout$Dropout),2),
              prop.table(table(cat$DegreeAwardType,dropout$Dropout),2),
              prop.table(table(cat$RelativePerf,dropout$Dropout),2),
              prop.table(table(cat$P_RFA,dropout$Dropout),2),
              prop.table(table(cat$CIP2D,dropout$Dropout),2),
              prop.table(table(cat$LearningGrp,dropout$Dropout),2),
              prop.table(table(cat$TeachingGrp,dropout$Dropout),2),
              prop.table(table(cat$PrevDegLvl,dropout$Dropout),2))

d.perc <- round(d.perc*100,1)

d <- cbind(t.count,t.perc,d.count,d.perc)


setwd("Z:/Advanced Analytics/One Year Retention Risk Indicator/Analyses/Saved Objects/05 Variable Importance/")
write.csv(d, file = "Feature Frequencies.csv", row.names = T)

#DegreeAwardType, RelativePerf, P_RFA, CIP2D, LearningGrp, TeachingGrp
with(raw,chisq.test(Dropout,DFUWI, correct=F))
with(raw,chisq.test(Dropout,DegreeAwardType, correct=F))
with(raw,chisq.test(Dropout,RelativePerf, correct=F))
with(raw,chisq.test(Dropout,P_RFA, correct=F))
with(raw,chisq.test(Dropout,CIP2D, correct=F))
with(raw,chisq.test(Dropout,LearningGrp, correct=F))
with(raw,chisq.test(Dropout,TeachingGrp, correct=F))
with(raw,chisq.test(Dropout,PrevDegLvl, correct=F))




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# VARIABLE IMPORTANCE Continuous Variables TABLE #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
raw <- read.csv("Z:/Advanced Analytics/One Year Retention Risk Indicator/Analyses/Data.csv"
                ,na.strings = "NULL")



raw$Dropout <- gsub("Y","Dropout", raw$Dropout)
raw$Dropout <- gsub("N","Persist", raw$Dropout)


# DaysToFYClose TransUnits PrevGPA AGI_PerCapita 

library(psych)
t <- raw[,c("Dropout","DaysToFYClose","TransUnits","PrevGPA","AGI_PerCapita")]
describeBy(t[2:5],group = t$Dropout,mat = T, digits = 1)


with(raw, t.test(DaysToFYClose ~ Dropout,var.equal = T))
with(raw, t.test(TransUnits ~ Dropout,var.equal = T))
with(raw, t.test(PrevGPA ~ Dropout,var.equal = T))
with(raw, t.test(AGI_PerCapita ~ Dropout,var.equal = T))




#~~~~~~~~~~~~~~~~~~~~~~~~~#
# FEATURES USED IN MODELS #
#~~~~~~~~~~~~~~~~~~~~~~~~~#
setwd("Z:/Advanced Analytics/One Year Retention Risk Indicator/Analyses/Saved Objects/01 Data Prep/")
load("cu.rda")
load("ru.rda")
load("cb.rda")
load("rb.rda")



n.cu <- data.frame(colnames(cu))
n.ru <- data.frame(colnames(ru))
n.cb <- data.frame(colnames(cb))
n.rb <- data.frame(colnames(rb))

n.cu$j <- colnames(cu)
n.ru$j <- colnames(ru)
n.cb$j <- colnames(cb)
n.rb$j <- colnames(rb)

features <- merge(n.cu,n.ru, by="j", all=T)
features <- merge(features,n.cb, by="j", all=T)
features <- merge(features,n.rb, by="j", all=T)

setwd("Z:/Advanced Analytics/One Year Retention Risk Indicator/Analyses/Saved Objects/04 Model Testing/")
write.csv(features, file = "Features Included.csv", row.names = T, na = "")




