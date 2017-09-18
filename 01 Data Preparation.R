
################################################################
#     CREATE 4 DATA SETS ~ SPLIT INTO TRAINING AND TESTING     #
################################################################

# The first data set is the raw data set which contains an unbalanced class variable (dropout) and all features (i.e., complete)
# The second data set contains an unbalanced class variable (dropout) and a feature set that is reduced via Random Forest Recursive Feature Elimination
# The third data set contains a balanced class variable (dropout) and a complete feature set.
# The fourth data set contains a balanced class variable (dropout) and a feature set that is reduced via Random Forest Recursive Feature Elimination
# Each data set is split into a training (70%) and testing (30%) data set




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Initial Loading of Data and Preparing R Environment #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
raw <- read.csv("Z:/Advanced Analytics/One Year Retention Risk Indicator/Analyses/Data.csv",na.strings = "NULL")

# REPLACE MISSING VALUES WITH MEAN
rawm <- raw 
set.seed(951)
for(i in 1:ncol(rawm)){
  rawm[is.na(rawm[,i]), i] <- mean(rawm[,i], na.rm = TRUE)}

# LOAD NECESSARY PACKAGES
library(caret)
library(parallel)
library(doParallel)

# SET WORKING DIRECTORY FOR SAVING AND LOADING OBJECTS
setwd("Z:/Advanced Analytics/One Year Retention Risk Indicator/Analyses/Saved Objects/01 Data Prep/")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# DATA SET 1 ~  COMPLETE UNBALANCED #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
cu <- rawm[,3:79]

# CREATE TRAINING AND TESTING DATA SETS
set.seed(1)
inTraining <- createDataPartition(cu$Dropout, p = .70, list = FALSE)
cutr <- cu[ inTraining,]
cuts  <- cu[-inTraining,]

save(cu, file = "cu.rda")
save(cutr, file = "cutr.rda")
save(cuts, file = "cuts.rda")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# DATA SET 2 ~  REDUCED UNBALANCED #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# INITIATE PARALLEL PROCESSING
cluster <- makeCluster(detectCores() - 1) # leave 1 core for OS
registerDoParallel(cluster)

# RANDOM FOREST RECURSIVE FEATURE ELIMINATION
rfFuncs$summary <- twoClassSummary   #change the summary function to ROC
rfe_ctrl <- rfeControl(functions=rfFuncs, method="cv", number=10,allowParallel = TRUE)
set.seed(951)
rfe.ru <- rfe(cu[,2:77], cu$Dropout, sizes=c(1:76), rfeControl=rfe_ctrl, metric = "ROC", preProc = c("center","scale"))

# END PARALLEL PROCESSING
stopCluster(cluster)
registerDoSEQ()

# SAVE THE MODEL
save(rfe.ru, file = "rfe.ru.rda")

# SUBSET THE FEATURES TO ONLY INCLUDE PREDICTIVE ONES
ru <- data.frame(cbind("Dropout"=cu[,"Dropout"], cu[,predictors(rfe.ru)]))

# CREATE TRAINING AND TESTING DATA SETS
set.seed(1)
inTraining <- createDataPartition(ru$Dropout, p = .70, list = FALSE)
rutr <- ru[ inTraining,]
ruts  <- ru[-inTraining,]

save(ru, file = "ru.rda")
save(rutr, file = "rutr.rda")
save(ruts, file = "ruts.rda")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# DATA SET 3 ~  COMPLETE BALANCED #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# GET STRATIFIED SAMPLE
cb_n <- cu[cu$Dropout=="N",] 
cb_y <- cu[cu$Dropout=="Y",] 
set.seed(1)
cb_n <- cb_n[sample(1:nrow(cb_n),(table(cu$Dropout))[2], replace = F),]
cb <- rbind(cb_n,cb_y)
set.seed(1)
cb <- cb[sample(nrow(cb)),] #shuffle rows

# CREATE TRAINING AND TESTING DATA SETS
set.seed(1)
inTraining <- createDataPartition(cb$Dropout, p = .70, list = FALSE)
cbtr <- cb[ inTraining,]
cbts  <- cb[-inTraining,]

save(cb, file = "cb.rda")
save(cbtr, file = "cbtr.rda")
save(cbts, file = "cbts.rda")

rm(inTraining,cb_n,cb_y)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# DATA SET 4 ~  REDUCED BALANCED #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# INITIATE PARALLEL PROCESSING
cluster <- makeCluster(detectCores() - 1) # leave 1 core for OS
registerDoParallel(cluster)

# RANDOM FOREST RECURSIVE FEATURE ELIMINATION
rfFuncs$summary <- twoClassSummary   #change the summary function to ROC
rfe_ctrl <- rfeControl(functions=rfFuncs, method="cv", number=10,allowParallel = TRUE)
set.seed(951)
rfe.rb <- rfe(cb[,2:77],cb$Dropout, sizes=c(1:76), rfeControl=rfe_ctrl 
              ,metric = "ROC",preProc = c("center","scale"))
# END PARALLEL PROCESSING
stopCluster(cluster)
registerDoSEQ()

# SAVE THE MODEL
save(rfe.rb, file = "rfe.rb.rda")

# SUBSET THE FEATURES TO ONLY INCLUDE PREDICTIVE ONES
rb <- data.frame(cbind("Dropout"=cb[,"Dropout"], cb[,predictors(rfe.rb)]))

# CREATE TRAINING AND TESTING DATA SETS
set.seed(1)
inTraining <- createDataPartition(rb$Dropout, p = .70, list = FALSE)
rbtr <- rb[ inTraining,]
rbts  <- rb[-inTraining,]

save(rb, file = "rb.rda")
save(rbtr, file = "rbtr.rda")
save(rbts, file = "rbts.rda")
