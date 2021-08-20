install.packages("readxl")
install.packages("devtools")
install.packages("dpylr")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("kernlab")
install.packages('caret')
library("tidyverse")
library("caret")
library("readxl")
library("dplyr")
library("ggplot2")
library("tools")
library("kernlab")


# Examining the Data ------------------------------------------------------

CardioDS <- read_excel("CardiotocographyDataSet.xlsx")
str(CardioDS)

#Summaries of Attributes
summary(CardioDS$LBE)
summary(CardioDS$LB)
summary(CardioDS$AC)
summary(CardioDS$FM)
summary(CardioDS$UC)
summary(CardioDS$ASTV)
summary(CardioDS$MSTV)
summary(CardioDS$ALTV)
summary(CardioDS$mLTV)
summary(CardioDS$DL)
summary(CardioDS$DS)
summary(CardioDS$DP)
summary(CardioDS$DR)

#most reason file, oldest file
head(arrange(CardioDS, Date), 1)
tail(arrange(CardioDS, Date), 1)
range(CardioDS$Date)



# Common Trends -----------------------------------------------------------

hist(CardioDS$LB, main="Histogram for FHR Baseline Value (SisPorto)", 
     xlab="FHR baseline", ylab = "Frequency") #Histogram of Baseline Value

hist(CardioDS$AC, main="Histogram for Accelerations", 
     xlab="Accelerations", ylab = "Frequency") #Histogram of Accelerations

hist(CardioDS$FM, main="Histogram for Fetal Movement", 
     xlab="Fetal Movement", ylab = "Frequency") #Histogram of Fetal Movements

hist(CardioDS$UC, main="Histogram for Uterine Contractions", 
     xlab="Uterine Contractions", ylab = "Frequency") #Histogram of Uterine Contractions

hist(CardioDS$DS, main="Histogram for Severe Decelerations", 
     xlab="Severe Decelerations", ylab = "Frequency") #Histogram of Severe Decelerations


#SAMPLING ACCERATIONS - following population distribution?

sample_size = nrow(CardioDS)*.1

samples100 <- CardioDS[sample(nrow(CardioDS), sample_size, replace = TRUE), ]

hist(samples100$LB, main="Histogram for SAMPLE FHR Baseline Value (SisPorto)", 
     xlab="FHR baseline", ylab = "Frequency") #Histogram of Baseline Value

summary(samples100$LB)


#Attribution Correlations?

CardioDS$LBPS <- CardioDS$LB*60 #LB per Second

#scatter for Severe Decceration and Baseline Value 
gscatter_DS_LBPS <- ggplot(CardioDS, aes(x=DS, y=LBPS, color=LBPS)) +  geom_point() + geom_smooth(method="lm", se=FALSE)
gscatter_DS_LBPS <- gscatter_DS_LBPS + labs(title="Severe Deceleration and Baseline Value Correlation", x="Severe Decelerations", y = "Baseline Value")
gscatter_DS_LBPS # inspect scatter plot

#scatter for Acceration and Baseline Value
gscatter_AC_LBPS <- ggplot(CardioDS, aes(x=AC, y=LBPS, color=LBPS)) +  geom_point() + geom_smooth(method="lm", se=FALSE)
gscatter_AC_LBPS <- gscatter_AC_LBPS + labs(title="Accelerations and Baseline Value Correlation", x="Accelerations", y = "Baseline Value")
gscatter_AC_LBPS # inspect scatter plot


#scatter for Severe Decceration and Uterine Activity
gscatter_DS_UC <- ggplot(CardioDS, aes(x=DS, y=UC, color=UC)) +  geom_point() + geom_smooth(method="lm", se=FALSE)
gscatter_DS_UC <- gscatter_DS_UC + labs(title="Severe Deceleration and Uterine Activity Correlation", x="Severe Decelerations", y = "Uterine Activity")
gscatter_DS_UC # inspect scatter plot

#scatter for Accerations and Uterine Activity
gscatter_AC_UC <- ggplot(CardioDS, aes(x=AC, y=UC, color=UC)) +  geom_point() + geom_smooth(method="lm", se=FALSE)
gscatter_AC_UC <- gscatter_AC_UC + labs(title="Accelerations and Uterine Activity Correlation", x="Accelerations", y = "Uterine Activity")
gscatter_AC_UC # inspect scatter plot


# Linear Model ------------------------------------------------------------

#Linear regression model

NSP_linearmodel <- lm(formula=NSP~LB, data=CardioDS) #LB and NPS
summary(NSP_linearmodel)

test1 <- lm(formula=NSP~AC, data=CardioDS) #AC and NPS
summary(test1)

test2 <- lm(formula=NSP~FM, data=CardioDS) #FM and NPS
summary(test2)

test3 <- lm(formula=NSP~UC, data=CardioDS) #UC and NPS
summary(test3)

#multiple regression

NSP_multiregression <- lm(formula=NSP~LB+AC+FM+UC, data=CardioDS)
summary(NSP_multiregression)

NSP_multiregression2 <- lm(formula=NSP~DL+DS+DP+DR, data=CardioDS)
summary(NSP_multiregression2)


# SVM Model ---------------------------------------------------------------

CardioDS$LBPS <- NULL

Train_List <- createDataPartition(y=CardioDS$NSP,p=.30,list=FALSE) 
Train_Set <- CardioDS[Train_List,]
numrows <- c(1:nrow(CardioDS))
Test_List <- numrows[!numrows %in% Train_List]
Test_Set <- CardioDS[Test_List,]

NSP_svm_model <- ksvm(NSP ~ LBE+LB+AC+FM+UC+ASTV+MSTV+ALTV+MLTV+DL+DS+DP+DR+Width+Min+Max+Nmax+Nzeros+Mode+Mean+Median+Variance+Tendency, data=Train_Set,, type = "C-svc", cross=10)
NSP_svm_model

svm_trainpred <- predict(NSP_svm_model, Train_Set)

str(svm_trainpred)
train_pred_results <- table(svm_trainpred, Train_Set$NSP)

predtrain_totalCorrect <- train_pred_results[1,1] + train_pred_results[2,2]
predtrain_total <- nrow(Train_Set)
trainpred_svmaccuracy <- predtrain_totalCorrect/predtrain_total
trainpred_svmaccuracy

install.packages("e1071")
confusionMatrix(factor(svm_trainpred), factor(Train_Set$NSP))	

