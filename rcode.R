
hre<-read.csv("D:/My Stuff/Hospital Readmission/SingHealth.csv")
subhre<-read.csv("D:/My Stuff/Hospital Readmission/SubsetHealthData.csv")

hre<-hre[,-c(1,4,5,12,13,14,15,19,44,45)]
names(hre)

table(hre$Add_Status)

for(i in seq(ncol(subhre)))
{
  barplot(table(subhre[,i]))
}

# hre[which(hre$Hlos>=4 & hre$Hlos<=6),]<-4
# hre[which(hre$Hlos>=7 & hre$Hlos<=13),]<-5
# hre[which(hre$Hlos>=14),]<-7


#------------------------

library(Boruta)
set.seed(123)
boruta_output <- Boruta(hre$Add_Status ~ ., data = hre, doTrace=2)
boruta_output$finalDecision

table(boruta_output$finalDecision)
# Tentative Confirmed  Rejected 
#  0        38         3

# boruta_output$finalDecision
# Name                            Confirmed
# SexNo                           Confirmed
# RaceNo                          Confirmed
# Age                             Confirmed
# new_Age                         Confirmed
# Age_group                       Confirmed
# DEMAdmNo                        Confirmed
# DisStatusNo                      Rejected
# DisPatClassNo                   Confirmed
# Hlos                            Confirmed
# new_Hlos                        Confirmed
# ICUCCUNo                        Confirmed
# new_ICU                         Confirmed
# ICD9_1Main                      Confirmed
# Pri_diag                        Confirmed
# three.digita                    Confirmed
# ICD_Class                       Confirmed
# ICD9_2                          Confirmed
# ICD9_3                          Confirmed
# ICD9_4                          Confirmed
# ICD9_5                          Confirmed
# ICD9_6                          Confirmed
# ICD9_7                          Confirmed
# ICD9_8                          Confirmed
# ICD9_9                          Confirmed
# ICD9_10                         Confirmed
# ICD_con                         Confirmed
# SICDmain                        Confirmed
# SICD2                           Confirmed
# SICD3                           Confirmed
# SICD4                           Confirmed
# SICD5                           Confirmed
# SICD6                           Confirmed
# SICD7                           Confirmed
# SICD8                           Confirmed
# SICD9                            Rejected
# SICD10                           Rejected
# SICD_con                        Confirmed
# DisYear                         Confirmed
# ICD_Class_1                     Confirmed
# ICD_Class_2                     Confirmed

final.boruta <- TentativeRoughFix(boruta_output)
final.boruta

getSelectedAttributes(boruta_output)
getSelectedAttributes(final.boruta, withTentative = F)
getSelectedAttributes(boruta_output)

boruta.df <- attStats(final.boruta)
print(boruta.df)
arrange(cbind(attr=rownames(attStats(boruta_output)), attStats(boruta_output)),desc(medianImp))
boruta.df[which(boruta.df$medianImp>2.8),]

#-------------------------------------------------------------------------------
## Important Variables
names(train2)
train_impVar<-train2[,getSelectedAttributes(boruta_output)]
names(train_impVar)

train_impVar
#-----------------------------------------------------------------------
# Using traditional RFE method for feature selection
library(randomForest)
library(caret)
set.seed(123)
features<-names(hre)
for(f in features)
{
  if(class(hre[[f]])=="factor")
  {
    hre[,f] <- as.numeric(hre[,f])
  }
}

control <- rfeControl(functions=rfFuncs, method="cv", number=10)
rfe.train <- rfe(hre[,2:39], hre[,1], sizes=1:12, rfeControl=control)
rfe.train
plot(rfe.train, type=c("g", "o"), cex = 1.0, col = 1:74)
predictors(rfe.train)

#-----------------------------------------------------
finalData<-hre
M <- cor(finalData)
library(corrplot)
corrplot(M, method="circle")
corrplot(M, type="lower")


cor.mtest <- function(mat, conf.level = 0.95){
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level)
      p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
      lowCI.mat[i,j] <- lowCI.mat[j,i] <- tmp$conf.int[1]
      uppCI.mat[i,j] <- uppCI.mat[j,i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}

res1 <- cor.mtest(train2,0.95)
res2 <- cor.mtest(train2,0.99)
## specialized the insignificant value according to the significant level
corrplot(M, p.mat = res1[[1]], sig.level=0.2)


#########################################################################
#################### Model Building #####################################
#########################################################################


 finalData<-hre
# good_columns<-names(train_impVar)
# finalData <- finalData[,c(good_columns,"SalePrice")]
library(caret)
inTraining <- createDataPartition(hre$Add_Status, p=0.6, list=FALSE)
trainingStd <- finalData[inTraining,]
testdataStd <- finalData[-inTraining,]
inVal <- createDataPartition(testdataStd$Add_status, p=0.5, list=FALSE)
crossvalStd <- testdataStd[inVal,]
testingStd <- testdataStd[-inVal,]

# glm
glm_fit<-glm(formula=Add_Status ~ .,data=hre,family=binomial)

# xgbTree
curr.model <- train(trainingStd$Add_Status ~ .,data = trainingStd,method = "xgbTree")
preds<- predict(curr.model,testdataStd)
preds
#sqrt(mean((log(testdataStd$SalePrice+1) - log(preds+1))^2))
#0.1393658
#0.1325826


curr.model <- train(SalePrice ~ .,data = finalData,method = "xgbTree")
preds<- predict(curr.model,test2)
preds
output_xgboost<-as.data.frame(preds)

#Knn
knn_model <- train(SalePrice ~ .,data = trainingStd,method = "knn")
knnpreds<- predict(knn_model,testdataStd)
knnpreds
sqrt(mean((log(testdataStd$SalePrice+1) - log(knnpreds+1))^2))
#0.2169576

#SVM
model <- svm(SalePrice ~ .,data = trainingStd)
svmoutput<- predict(object = model, newdata = testdataStd)
svmoutput
rmse(svmoutput, testdataStd$SalePrice)
sqrt(mean((log(testdataStd$SalePrice+1) - log(svmoutput+1))^2))
#0.1520209

##Random Forest
rpart.model <- train(SalePrice ~ .,data = trainingStd,method = "rpart")
rpart_preds<- predict(rpart.model,testdataStd)
rpart_preds
rmse(rpart_preds, testdataStd$SalePrice)
sqrt(mean((log(testdataStd$SalePrice+1) - log(rpart_preds+1))^2))
# 0.2658621

## Random Forest
rfFit=randomForest(SalePrice ~ .,data = trainingStd)
rfpreds<- predict(rfFit,testdataStd)
sqrt(mean((log(testdataStd$SalePrice+1) - log(rfpreds+1))^2))
#0.1415965

## GBM
gbmModel<-gbm(formula = trainingStd$SalePrice~ .,
              data = trainingStd[,-45],
              var.monotone = NULL,
              n.trees = 500000,
              interaction.depth = 1,
              n.minobsinnode = 10,
              shrinkage = 0.001,
              bag.fraction = 0.5,
              train.fraction = 1.0,
              cv.folds=0,
              keep.data = TRUE,
              verbose = "CV",
              class.stratify.cv=NULL,
              n.cores = NULL)

gbmpreds<- predict(gbmModel,testdataStd,n.trees = 500000)
sqrt(mean((log(testdataStd$SalePrice+1) - log(gbmpreds+1))^2))
# 0.1420983

## lm
lm_fit<-lm(trainingStd$SalePrice~ .,data=trainingStd[,-45])
lm_preds<- predict(lm_fit,testdataStd)
sqrt(mean((log(testdataStd$SalePrice+1) - log(lm_preds+1))^2))
#0.1655893

## Robust Fitting of Linear Models
library(MASS)
RLM_model <- rlm(trainingStd$SalePrice~ .,data=trainingStd[,-45])
RLM_preds <- predict(RLM_model, type="response", testdataStd)
sqrt(mean((log(testdataStd$SalePrice+1) - log(RLM_preds+1))^2))
#0.1514151
#0.1507289

##
library(earth)
EARTH_model <- earth(trainingStd$SalePrice~ .,data=trainingStd[,-45])
earth_pred <- predict(EARTH_model, testdataStd)
sqrt(mean((log(testdataStd$SalePrice+1) - log(earth_pred+1))^2))
#0.147303
# 0.1355549

#---------------------------------------------------------------------------------
write.csv(output_xgboost,"D:/My Stuff/Kaggle/House Pricing/xgboost_submission.csv")
preds

gbmImp <- varImp(rpart.model, scale = FALSE)
gbmImp
# rpart variable importance
# only 20 most important variables shown (out of 45)
# Overall

# OverallQual   0.8264
# GrLivArea     0.6621
# YearBuilt     0.5919
# GarageCars    0.3573
# ExterQual     0.3476
# FullBath      0.3003
# GarageFinish  0.2828
# Fireplaces    0.0000
# MSSubClass    0.0000
# MasVnrType    0.0000
# GarageCond    0.0000
# GarageType    0.0000
# YearRemodAdd  0.0000
# BsmtFullBath  0.0000
# HalfBath      0.0000
# PavedDrive    0.0000
# HouseStyle    0.0000
# LotArea       0.0000
# OverallCond   0.0000
# OpenPorchSF   0.0000

gbmImp <- varImp(model, scale = FALSE)
gbmImp


#------------------
# KMeans clustering

hre<-read.csv("D:/My Stuff/Hospital Readmission/SingHealth.csv")
hre<-hre[,-c(1,4,5,12,13,14,15,19,44,45)]
hre<-hre[,-c(5,10)]

library(randomForest)
library(caret)
set.seed(123)
features<-names(hre)
for(f in features)
{
  if(class(hre[[f]])=="factor")
  {
    hre[,f] <- as.numeric(hre[,f])
  }
}
hre1<-hre
hre1$Add_Status<-NULL

set.seed(1234)
(kc <- kmeans(hre1, 2))

hre[which(hre$Add_Status==1),1]<-2
hre[which(hre$Add_Status==0),1]<-1

table(hre$Add_Status)
table(hre$Add_Status, kc$cluster)



















