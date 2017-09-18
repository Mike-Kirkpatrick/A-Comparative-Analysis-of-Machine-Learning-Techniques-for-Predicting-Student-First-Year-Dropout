
#################################################################################
#     TEST ALL 36 MODELS & COMBINE MULTIPLE EVALUATION METRICS INTO 1 TABLE     #
#################################################################################

# The 36 models are evaluated on their corresponding testing data set
# The following model evaluation metrics are calculated for each model and then compiled into one table:
#     AUC (ROC Area)
#     Accuracy 
#     Sensitivity
#     Specificity
#     TP (True-Positive rate)
#     FP (False-Positive rate)
#     TN (True-Negative rate)
#     FN (False-Negative rate)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Initial Loading of Data and Preparing R Environment #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# IMPORT ALL MODELS
setwd("Z:/Advanced Analytics/One Year Retention Risk Indicator/Analyses/Saved Objects/02 Model Building/")
files = list.files(pattern="*.rda")
for (i in 1:length(files)) load(file=files[i])

# IMPORT ALL TEST DATA
setwd("Z:/Advanced Analytics/One Year Retention Risk Indicator/Analyses/Saved Objects/01 Data Prep/")
files = list.files(pattern="*ts.rda")
for (i in 1:length(files)) load(file=files[i])

rm(files,i)

library(caret)
library(pROC)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# DATA SET 1 ~  COMPLETE UNBALANCED #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
pred.dt.cu <- cbind(P=predict(dt.cu, cuts),predict(dt.cu, cuts, type="prob"))
cm.dt.cu <- confusionMatrix(pred.dt.cu$P,cuts$Dropout,positive = "Y")
roc.dt.cu <- roc(cuts$Dropout, pred.dt.cu$Y)

pred.knn.cu <- cbind(P=predict(knn.cu, cuts),predict(knn.cu, cuts, type="prob"))
cm.knn.cu <- confusionMatrix(pred.knn.cu$P,cuts$Dropout,positive = "Y")
roc.knn.cu <- roc(cuts$Dropout, pred.knn.cu$Y)

pred.lr.cu <- cbind(P=predict(lr.cu, cuts),predict(lr.cu, cuts, type="prob"))
cm.lr.cu <- confusionMatrix(pred.lr.cu$P,cuts$Dropout,positive = "Y")
roc.lr.cu <- roc(cuts$Dropout, pred.lr.cu$Y)

pred.nb.cu <- cbind(P=predict(nb.cu, cuts),predict(nb.cu, cuts, type="prob"))
cm.nb.cu <- confusionMatrix(pred.nb.cu$P,cuts$Dropout,positive = "Y")
roc.nb.cu <- roc(cuts$Dropout, pred.nb.cu$Y)

pred.nn.cu <- cbind(P=predict(nn.cu, cuts),predict(nn.cu, cuts, type="prob"))
cm.nn.cu <- confusionMatrix(pred.nn.cu$P,cuts$Dropout,positive = "Y")
roc.nn.cu <- roc(cuts$Dropout, pred.nn.cu$Y)

pred.rf.cu <- cbind(P=predict(rf.cu, cuts),predict(rf.cu, cuts, type="prob"))
cm.rf.cu <- confusionMatrix(pred.rf.cu$P,cuts$Dropout,positive = "Y")
roc.rf.cu <- roc(cuts$Dropout, pred.rf.cu$Y)

pred.svml.cu <- cbind(P=predict(svml.cu, cuts),predict(svml.cu, cuts, type="prob"))
cm.svml.cu <- confusionMatrix(pred.svml.cu$P,cuts$Dropout,positive = "Y")
roc.svml.cu <- roc(cuts$Dropout, pred.svml.cu$Y)

pred.svmp.cu <- cbind(P=predict(svmp.cu, cuts),predict(svmp.cu, cuts, type="prob"))
cm.svmp.cu <- confusionMatrix(pred.svmp.cu$P,cuts$Dropout,positive = "Y")
roc.svmp.cu <- roc(cuts$Dropout, pred.svmp.cu$Y)

pred.xgb.cu <- cbind(P=predict(xgb.cu, cuts),predict(xgb.cu, cuts, type="prob"))
cm.xgb.cu <- confusionMatrix(pred.xgb.cu$P,cuts$Dropout,positive = "Y")
roc.xgb.cu <- roc(cuts$Dropout, pred.xgb.cu$Y)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# DATA SET 2 ~  REDUCED UNBALANCED #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
pred.dt.ru <- cbind(P=predict(dt.ru, ruts),predict(dt.ru, ruts, type="prob"))
cm.dt.ru <- confusionMatrix(pred.dt.ru$P,ruts$Dropout,positive = "Y")
roc.dt.ru <- roc(ruts$Dropout, pred.dt.ru$Y)

pred.knn.ru <- cbind(P=predict(knn.ru, ruts),predict(knn.ru, ruts, type="prob"))
cm.knn.ru <- confusionMatrix(pred.knn.ru$P,ruts$Dropout,positive = "Y")
roc.knn.ru <- roc(ruts$Dropout, pred.knn.ru$Y)

pred.lr.ru <- cbind(P=predict(lr.ru, ruts),predict(lr.ru, ruts, type="prob"))
cm.lr.ru <- confusionMatrix(pred.lr.ru$P,ruts$Dropout,positive = "Y")
roc.lr.ru <- roc(ruts$Dropout, pred.lr.ru$Y)

pred.nb.ru <- cbind(P=predict(nb.ru, ruts),predict(nb.ru, ruts, type="prob"))
cm.nb.ru <- confusionMatrix(pred.nb.ru$P,ruts$Dropout,positive = "Y")
roc.nb.ru <- roc(ruts$Dropout, pred.nb.ru$Y)

pred.nn.ru <- cbind(P=predict(nn.ru, ruts),predict(nn.ru, ruts, type="prob"))
cm.nn.ru <- confusionMatrix(pred.nn.ru$P,ruts$Dropout,positive = "Y")
roc.nn.ru <- roc(ruts$Dropout, pred.nn.ru$Y)

pred.rf.ru <- cbind(P=predict(rf.ru, ruts),predict(rf.ru, ruts, type="prob"))
cm.rf.ru <- confusionMatrix(pred.rf.ru$P,ruts$Dropout,positive = "Y")
roc.rf.ru <- roc(ruts$Dropout, pred.rf.ru$Y)

pred.svml.ru <- cbind(P=predict(svml.ru, ruts),predict(svml.ru, ruts, type="prob"))
cm.svml.ru <- confusionMatrix(pred.svml.ru$P,ruts$Dropout,positive = "Y")
roc.svml.ru <- roc(ruts$Dropout, pred.svml.ru$Y)

pred.svmp.ru <- cbind(P=predict(svmp.ru, ruts),predict(svmp.ru, ruts, type="prob"))
cm.svmp.ru <- confusionMatrix(pred.svmp.ru$P,ruts$Dropout,positive = "Y")
roc.svmp.ru <- roc(ruts$Dropout, pred.svmp.ru$Y)

pred.xgb.ru <- cbind(P=predict(xgb.ru, ruts),predict(xgb.ru, ruts, type="prob"))
cm.xgb.ru <- confusionMatrix(pred.xgb.ru$P,ruts$Dropout,positive = "Y")
roc.xgb.ru <- roc(ruts$Dropout, pred.xgb.ru$Y)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# DATA SET 3 ~  COMPLETE BALANCED #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
pred.dt.cb <- cbind(P=predict(dt.cb, cbts),predict(dt.cb, cbts, type="prob"))
cm.dt.cb <- confusionMatrix(pred.dt.cb$P,cbts$Dropout,positive = "Y")
roc.dt.cb <- roc(cbts$Dropout, pred.dt.cb$Y)

pred.knn.cb <- cbind(P=predict(knn.cb, cbts),predict(knn.cb, cbts, type="prob"))
cm.knn.cb <- confusionMatrix(pred.knn.cb$P,cbts$Dropout,positive = "Y")
roc.knn.cb <- roc(cbts$Dropout, pred.knn.cb$Y)

pred.lr.cb <- cbind(P=predict(lr.cb, cbts),predict(lr.cb, cbts, type="prob"))
cm.lr.cb <- confusionMatrix(pred.lr.cb$P,cbts$Dropout,positive = "Y")
roc.lr.cb <- roc(cbts$Dropout, pred.lr.cb$Y)

pred.nb.cb <- cbind(P=predict(nb.cb, cbts),predict(nb.cb, cbts, type="prob"))
cm.nb.cb <- confusionMatrix(pred.nb.cb$P,cbts$Dropout,positive = "Y")
roc.nb.cb <- roc(cbts$Dropout, pred.nb.cb$Y)

pred.nn.cb <- cbind(P=predict(nn.cb, cbts),predict(nn.cb, cbts, type="prob"))
cm.nn.cb <- confusionMatrix(pred.nn.cb$P,cbts$Dropout,positive = "Y")
roc.nn.cb <- roc(cbts$Dropout, pred.nn.cb$Y)

pred.rf.cb <- cbind(P=predict(rf.cb, cbts),predict(rf.cb, cbts, type="prob"))
cm.rf.cb <- confusionMatrix(pred.rf.cb$P,cbts$Dropout,positive = "Y")
roc.rf.cb <- roc(cbts$Dropout, pred.rf.cb$Y)

pred.svml.cb <- cbind(P=predict(svml.cb, cbts),predict(svml.cb, cbts, type="prob"))
cm.svml.cb <- confusionMatrix(pred.svml.cb$P,cbts$Dropout,positive = "Y")
roc.svml.cb <- roc(cbts$Dropout, pred.svml.cb$Y)

pred.svmp.cb <- cbind(P=predict(svmp.cb, cbts),predict(svmp.cb, cbts, type="prob"))
cm.svmp.cb <- confusionMatrix(pred.svmp.cb$P,cbts$Dropout,positive = "Y")
roc.svmp.cb <- roc(cbts$Dropout, pred.svmp.cb$Y)

pred.xgb.cb <- cbind(P=predict(xgb.cb, cbts),predict(xgb.cb, cbts, type="prob"))
cm.xgb.cb <- confusionMatrix(pred.xgb.cb$P,cbts$Dropout,positive = "Y")
roc.xgb.cb <- roc(cbts$Dropout, pred.xgb.cb$Y)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# DATA SET 4 ~  REDUCED BALANCED #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
pred.dt.rb <- cbind(P=predict(dt.rb, rbts),predict(dt.rb, rbts, type="prob"))
cm.dt.rb <- confusionMatrix(pred.dt.rb$P,rbts$Dropout,positive = "Y")
roc.dt.rb <- roc(rbts$Dropout, pred.dt.rb$Y)

pred.knn.rb <- cbind(P=predict(knn.rb, rbts),predict(knn.rb, rbts, type="prob"))
cm.knn.rb <- confusionMatrix(pred.knn.rb$P,rbts$Dropout,positive = "Y")
roc.knn.rb <- roc(rbts$Dropout, pred.knn.rb$Y)

pred.lr.rb <- cbind(P=predict(lr.rb, rbts),predict(lr.rb, rbts, type="prob"))
cm.lr.rb <- confusionMatrix(pred.lr.rb$P,rbts$Dropout,positive = "Y")
roc.lr.rb <- roc(rbts$Dropout, pred.lr.rb$Y)

pred.nb.rb <- cbind(P=predict(nb.rb, rbts),predict(nb.rb, rbts, type="prob"))
cm.nb.rb <- confusionMatrix(pred.nb.rb$P,rbts$Dropout,positive = "Y")
roc.nb.rb <- roc(rbts$Dropout, pred.nb.rb$Y)

pred.nn.rb <- cbind(P=predict(nn.rb, rbts),predict(nn.rb, rbts, type="prob"))
cm.nn.rb <- confusionMatrix(pred.nn.rb$P,rbts$Dropout,positive = "Y")
roc.nn.rb <- roc(rbts$Dropout, pred.nn.rb$Y)

pred.rf.rb <- cbind(P=predict(rf.rb, rbts),predict(rf.rb, rbts, type="prob"))
cm.rf.rb <- confusionMatrix(pred.rf.rb$P,rbts$Dropout,positive = "Y")
roc.rf.rb <- roc(rbts$Dropout, pred.rf.rb$Y)

pred.svml.rb <- cbind(P=predict(svml.rb, rbts),predict(svml.rb, rbts, type="prob"))
cm.svml.rb <- confusionMatrix(pred.svml.rb$P,rbts$Dropout,positive = "Y")
roc.svml.rb <- roc(rbts$Dropout, pred.svml.rb$Y)

pred.svmp.rb <- cbind(P=predict(svmp.rb, rbts),predict(svmp.rb, rbts, type="prob"))
cm.svmp.rb <- confusionMatrix(pred.svmp.rb$P,rbts$Dropout,positive = "Y")
roc.svmp.rb <- roc(rbts$Dropout, pred.svmp.rb$Y)

pred.xgb.rb <- cbind(P=predict(xgb.rb, rbts),predict(xgb.rb, rbts, type="prob"))
cm.xgb.rb <- confusionMatrix(pred.xgb.rb$P,rbts$Dropout,positive = "Y")
roc.xgb.rb <- roc(rbts$Dropout, pred.xgb.rb$Y)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# CREATE CONTINGENCY MATRIX #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~#
cm <- data.frame(rbind(dt.cu = as.vector(prop.table(cm.dt.cu$table)),
                       knn.cu = as.vector(prop.table(cm.knn.cu$table)),
                       lr.cu = as.vector(prop.table(cm.lr.cu$table)),
                       nb.cu = as.vector(prop.table(cm.nb.cu$table)),
                       nn.cu = as.vector(prop.table(cm.nn.cu$table)),
                       rf.cu = as.vector(prop.table(cm.rf.cu$table)),
                       svml.cu = as.vector(prop.table(cm.svml.cu$table)),
                       svmp.cu = as.vector(prop.table(cm.svmp.cu$table)),
                       xgb.cu = as.vector(prop.table(cm.xgb.cu$table)),
                       
                       dt.ru = as.vector(prop.table(cm.dt.ru$table)),
                       knn.ru = as.vector(prop.table(cm.knn.ru$table)),
                       lr.ru = as.vector(prop.table(cm.lr.ru$table)),
                       nb.ru = as.vector(prop.table(cm.nb.ru$table)),
                       nn.ru = as.vector(prop.table(cm.nn.ru$table)),
                       rf.ru = as.vector(prop.table(cm.rf.ru$table)),
                       svml.ru = as.vector(prop.table(cm.svml.ru$table)),
                       svmp.ru = as.vector(prop.table(cm.svmp.ru$table)),
                       xgb.ru = as.vector(prop.table(cm.xgb.ru$table)),
                       
                       dt.cb = as.vector(prop.table(cm.dt.cb$table)),
                       knn.cb = as.vector(prop.table(cm.knn.cb$table)),
                       lr.cb = as.vector(prop.table(cm.lr.cb$table)),
                       nb.cb = as.vector(prop.table(cm.nb.cb$table)),
                       nn.cb = as.vector(prop.table(cm.nn.cb$table)),
                       rf.cb = as.vector(prop.table(cm.rf.cb$table)),
                       svml.cb = as.vector(prop.table(cm.svml.cb$table)),
                       svmp.cb = as.vector(prop.table(cm.svmp.cb$table)),
                       xgb.cb = as.vector(prop.table(cm.xgb.cb$table)),
                       
                       dt.rb = as.vector(prop.table(cm.dt.rb$table)),
                       knn.rb = as.vector(prop.table(cm.knn.rb$table)),
                       lr.rb = as.vector(prop.table(cm.lr.rb$table)),
                       nb.rb = as.vector(prop.table(cm.nb.rb$table)),
                       nn.rb = as.vector(prop.table(cm.nn.rb$table)),
                       rf.rb = as.vector(prop.table(cm.rf.rb$table)),
                       svml.rb = as.vector(prop.table(cm.svml.rb$table)),
                       svmp.rb = as.vector(prop.table(cm.svmp.rb$table)),
                       xgb.rb = as.vector(prop.table(cm.xgb.rb$table))
                       )
                 )

# CLEAN UP CONTINGENCY MATRIX
colnames(cm) <- c("TN","FP","FN","TP")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# CALCULATE SENSITIVITY SPECIFICITY AND ACCURACY #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
cm$Sensitivity <- with(cm,TP/(TP+FN))
cm$Specificity <- with(cm,TN/(TN+FP))
cm$Accuracy <- with(cm,TP+TN)
# REORDER COLUMNS
cm <- cm[c(5,6,7,4,2,1,3)]




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# CALCULATE AREA UNDER ROC CURVE (AUC) #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
AUC <- data.frame(ROC.Area = rbind(dt.cu = auc(roc.dt.cu),
                                   knn.cu = auc(roc.knn.cu),
                                   lr.cu = auc(roc.lr.cu),
                                   nb.cu = auc(roc.nb.cu),
                                   nn.cu = auc(roc.nn.cu),
                                   rf.cu = auc(roc.rf.cu),
                                   svml.cu = auc(roc.svml.cu),
                                   svmp.cu = auc(roc.svmp.cu),
                                   xgb.cu = auc(roc.xgb.cu),
                                   
                                   dt.ru = auc(roc.dt.ru),
                                   knn.ru = auc(roc.knn.ru),
                                   lr.ru = auc(roc.lr.ru),
                                   nb.ru = auc(roc.nb.ru),
                                   nn.ru = auc(roc.nn.ru),
                                   rf.ru = auc(roc.rf.ru),
                                   svml.ru = auc(roc.svml.ru),
                                   svmp.ru = auc(roc.svmp.ru),
                                   xgb.ru = auc(roc.xgb.ru),
                                   
                                   dt.cb = auc(roc.dt.cb),
                                   knn.cb = auc(roc.knn.cb),
                                   lr.cb = auc(roc.lr.cb),
                                   nb.cb = auc(roc.nb.cb),
                                   nn.cb = auc(roc.nn.cb),
                                   rf.cb = auc(roc.rf.cb),
                                   svml.cb = auc(roc.svml.cb),
                                   svmp.cb = auc(roc.svmp.cb),
                                   xgb.cb = auc(roc.xgb.cb),
                                   
                                   dt.rb = auc(roc.dt.rb),
                                   knn.rb = auc(roc.knn.rb),
                                   lr.rb = auc(roc.lr.rb),
                                   nb.rb = auc(roc.nb.rb),
                                   nn.rb = auc(roc.nn.rb),
                                   rf.rb = auc(roc.rf.rb),
                                   svml.rb = auc(roc.svml.rb),
                                   svmp.rb = auc(roc.svmp.rb),
                                   xgb.rb = auc(roc.xgb.rb)
                                   )
                  )




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# COMBINE ALL METRICS INTO ONE TABLE #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
results <- cbind(AUC,cm)
results <- round(results,4)
results <- cbind(Model=row.names(results),results)
results$Model <- as.character(results$Model)
results <- cbind(Algorithm=sapply(strsplit(results$Model,"[.]"), `[`, 1)
                 ,Data=sapply(strsplit(results$Model,"[.]"), `[`, 2)
                 ,results)




#~~~~~~~~~~~~~~#
# SAVE RESULTS #
#~~~~~~~~~~~~~~#
setwd("Z:/Advanced Analytics/One Year Retention Risk Indicator/Analyses/Saved Objects/03 Model Testing/")
write.csv(results,file = "Results.csv",row.names = F)
save(results, file = "results.rda")
