
#########################################################
#     TRAIN 9 ALGORITHMS ON EACH OF THE 4 DATA SETS     #
#########################################################

# The 9 algorithms are:
#     Decision Tree
#     k-Nearest Neighbors
#     Logistic Regression
#     Naive Bayes
#     Artificial Neural Networks
#     Random Forest
#     Support Vector Machine with a linear kernel
#     Support Vector Machine with a polynomial kernel
#     XGBoost
# Each algorithm is trained on each data set, thus resulting in 36 models




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Initial Loading of Data and Preparing R Environment #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
library(caret)
library(parallel)
library(doParallel)

# LOAD EACH TRAINING DATA SET
load("Z:/Advanced Analytics/One Year Retention Risk Indicator/Analyses/Saved Objects/01 Data Prep/cutr.rda")
load("Z:/Advanced Analytics/One Year Retention Risk Indicator/Analyses/Saved Objects/01 Data Prep/rutr.rda")
load("Z:/Advanced Analytics/One Year Retention Risk Indicator/Analyses/Saved Objects/01 Data Prep/cbtr.rda")
load("Z:/Advanced Analytics/One Year Retention Risk Indicator/Analyses/Saved Objects/01 Data Prep/rbtr.rda")

# SET WORKING DIRECTORY FOR SAVING MODELS
setwd("Z:/Advanced Analytics/One Year Retention Risk Indicator/Analyses/Saved Objects/02 Model Building/")

# INITIATE PARALLEL PROCESSING
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)


# DEFINE TRAIN CONTROL
# use twoClassSummary so we evaluate the model wit ROC instead of Accuracy
tc <- trainControl(method="cv", number=10,savePredictions = "all", classProbs=T, summaryFunction=twoClassSummary, allowParallel = T)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# DATA SET 1 ~  COMPLETE UNBALANCED #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
dt.cu <- train(Dropout~., data=cutr, method="rpart", trControl=tc, preProc = c("center","scale"))
save(dt.cu,file = "dt.cu.rda")

knn.cu <- train(Dropout~., data=cutr, method="knn", trControl=tc, preProc = c("center","scale"))
save(knn.cu,file = "knn.cu.rda")

lr.cu <- train(Dropout~., data=cutr, method="glm", family="binomial", trControl=tc, preProc = c("center","scale"))
save(lr.cu,file = "lr.cu.rda")

nb.cu <- train(Dropout~., data=cutr, method="nb", trControl=tc, preProc = c("center","scale"))
save(nb.cu,file = "nb.cu.rda")

nn.cu <- train(Dropout~., data=cutr, method="nnet", trControl=tc, preProc = c("center","scale")) 
save(nn.cu,file = "nn.cu.rda")

rf.cu <- train(Dropout~., data=cutr, method="rf", trControl=tc, preProc = c("center","scale")) 
save(rf.cu,file = "rf.cu.rda")

svml.cu <- train(Dropout~., data=cutr, method="svmLinear", trControl=tc, preProc = c("center","scale"))
save(svml.cu,file = "svml.cu.rda")

svmp.cu <- train(Dropout~., data=cutr, method="svmPoly", trControl=tc, preProc = c("center","scale"))
save(svmp.cu,file = "svmp.cu.rda")

xgb.cu <- train(Dropout~., data=cutr, method="xgbTree", trControl=tc, preProc = c("center","scale"))
save(xgb.cu,file = "xgb.cu.rda")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# DATA SET 2 ~  REDUCED UNBALANCED #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
dt.ru <- train(Dropout~., data=rutr, method="rpart", trControl=tc, preProc = c("center","scale"))
save(dt.ru,file = "dt.ru.rda")

knn.ru <- train(Dropout~., data=rutr, method="knn", trControl=tc, preProc = c("center","scale"))
save(knn.ru,file = "knn.ru.rda")

lr.ru <- train(Dropout~., data=rutr, method="glm", family="binomial", trControl=tc, preProc = c("center","scale"))
save(lr.ru,file = "lr.ru.rda")

nb.ru <- train(Dropout~., data=rutr, method="nb", trControl=tc, preProc = c("center","scale"))
save(nb.ru,file = "nb.ru.rda")

nn.ru <- train(Dropout~., data=rutr, method="nnet", trControl=tc, preProc = c("center","scale")) 
save(nn.ru,file = "nn.ru.rda")

rf.ru <- train(Dropout~., data=rutr, method="rf", trControl=tc, preProc = c("center","scale")) 
save(rf.ru,file = "rf.ru.rda")

svml.ru <- train(Dropout~., data=rutr, method="svmLinear", trControl=tc, preProc = c("center","scale"))
save(svml.ru,file = "svml.ru.rda")

svmp.ru <- train(Dropout~., data=rutr, method="svmPoly", trControl=tc, preProc = c("center","scale"))
save(svmp.ru,file = "svmp.ru.rda")

xgb.ru <- train(Dropout~., data=rutr, method="xgbTree", trControl=tc, preProc = c("center","scale"))
save(xgb.ru,file = "xgb.ru.rda")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# DATA SET 3 ~  COMPLETE BALANCED #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
dt.cb <- train(Dropout~., data=cbtr, method="rpart", trControl=tc, preProc = c("center","scale"))
save(dt.cb,file = "dt.cb.rda")

knn.cb <- train(Dropout~., data=cbtr, method="knn", trControl=tc, preProc = c("center","scale"))
save(knn.cb,file = "knn.cb.rda")

lr.cb <- train(Dropout~., data=cbtr, method="glm", family="binomial", trControl=tc, preProc = c("center","scale"))
save(lr.cb,file = "lr.cb.rda")

nb.cb <- train(Dropout~., data=cbtr, method="nb", trControl=tc, preProc = c("center","scale"))
save(nb.cb,file = "nb.cb.rda")

nn.cb <- train(Dropout~., data=cbtr, method="nnet", trControl=tc, preProc = c("center","scale")) 
save(nn.cb,file = "nn.cb.rda")

rf.cb <- train(Dropout~., data=cbtr, method="rf", trControl=tc, preProc = c("center","scale")) 
save(rf.cb,file = "rf.cb.rda")

svml.cb <- train(Dropout~., data=cbtr, method="svmLinear", trControl=tc, preProc = c("center","scale"))
save(svml.cb,file = "svml.cb.rda")

svmp.cb <- train(Dropout~., data=cbtr, method="svmPoly", trControl=tc, preProc = c("center","scale"))
save(svmp.cb,file = "svmp.cb.rda")

xgb.cb <- train(Dropout~., data=cbtr, method="xgbTree", trControl=tc, preProc = c("center","scale"))
save(xgb.cb,file = "xgb.cb.rda")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# DATA SET 4 ~  REDUCED BALANCED #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
dt.rb <- train(Dropout~., data=rbtr, method="rpart", trControl=tc, preProc = c("center","scale"))
save(dt.rb,file = "dt.rb.rda")

knn.rb <- train(Dropout~., data=rbtr, method="knn", trControl=tc, preProc = c("center","scale"))
save(knn.rb,file = "knn.rb.rda")

lr.rb <- train(Dropout~., data=rbtr, method="glm", family="binomial", trControl=tc, preProc = c("center","scale"))
save(lr.rb,file = "lr.rb.rda")

nb.rb <- train(Dropout~., data=rbtr, method="nb", trControl=tc, preProc = c("center","scale"))
save(nb.rb,file = "nb.rb.rda")

nn.rb <- train(Dropout~., data=rbtr, method="nnet", trControl=tc, preProc = c("center","scale")) 
save(nn.rb,file = "nn.rb.rda")

rf.rb <- train(Dropout~., data=rbtr, method="rf", trControl=tc, preProc = c("center","scale")) 
save(rf.rb,file = "rf.rb.rda")

svml.rb <- train(Dropout~., data=rbtr, method="svmLinear", trControl=tc, preProc = c("center","scale"))
save(svml.rb,file = "svml.rb.rda")

svmp.rb <- train(Dropout~., data=rbtr, method="svmPoly", trControl=tc, preProc = c("center","scale"))
save(svmp.rb,file = "svmp.rb.rda")

xgb.rb <- train(Dropout~., data=rbtr, method="xgbTree", trControl=tc, preProc = c("center","scale"))
save(xgb.rb,file = "xgb.rb.rda")




# END PARALLEL PROCESSING
stopCluster(cluster)
registerDoSEQ()
