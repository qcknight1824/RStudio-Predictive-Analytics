install.packages("car")
install.packages("caret")

library(caret)
library(e1071)
library(pROC)
library(dplyr)

dfTB <- read.csv("inq2019.csv",na.strings=c("NA",""))

# Initial exploration
summary(dfTB)
str(dfTB)

#Removing Rejected Variables from Data Frame
dfTB <- subset(dfTB, select = -CONTACT_DATE)
dfTB <- subset(dfTB, select = -ACADEMIC_INTEREST_2)
dfTB <- subset(dfTB, select = -IRSCHOOL)
dfTB <- subset(dfTB, select = -CONTACT_CODE1)
dfTB <- subset(dfTB, select = -ACADEMIC_INTEREST_1)
dfTB <- subset(dfTB, select = -LEVEL_YEAR)
dfTB <- subset(dfTB, select = -ETHNICITY)
dfTB <- subset(dfTB, select = -TERRITORY)


#Imputations
#library(Hmisc)
dfTB$distance <- with(dfTB, impute(distance,mean))
dfTB$satscore <- with(dfTB, impute(satscore,mean))
dfTB$telecq <- with(dfTB, impute(telecq, mean))
dfTB$avg_income <- with(dfTB, impute(avg_income, mean))

#remove 3833 rows where sex = NA
install.packages("tidyverse")
library("tidyverse")
dfTB = dfTB %>% drop_na(sex)

#convert Target variable to factor
dfTB$Enroll <- factor(dfTB$Enroll)

#find the constant variable
values_count <- sapply(lapply(dfTB, unique), length)
values_count

# Using VIF to test multicollinearity
library(car)
vif(glm(formula=Enroll~., family = binomial(link='logit'),data = dfTB))

#Partition data

trainIndex <- createDataPartition(dfTB$Enroll,
                                  p=0.7,
                                  list=FALSE,
                                  times=1)

dfTB.train <- dfTB[trainIndex,]
dfTB.valid <- dfTB[-trainIndex,]

library(caret)

regressionTB.model <- train(Enroll~.,
                        data=dfTB.train,
                        method='glm',
                        family='binomial',
                        na.action=na.pass)

#Regression Summary
summary(regressionTB.model)

dfTB.train <- subset(dfTB.train, select = -SOLICITED_CNTCTS)
dfTB.train <- subset(dfTB.train, select = -stucar)
dfTB.train <- subset(dfTB.train, select = -TOTAL_CONTACTS)
dfTB.train <- subset(dfTB.train, select = -avg_income)
dfTB.train <- subset(dfTB.train, select = -Instate)


install.packages('pROC')
library(pROC)

#install.packages('dplyr')
library(dplyr)

#confusion matrix
prediction <- predict(regressionTB.model,newdata=dfTB.valid)
confusionMatrix(prediction,dfTB.valid$Enroll)

#ROC Curve
pred.probabilities <- predict(regressionTB.model,newdata=dfTB.valid,type='prob')
regression.ROC <- roc(predictor=pred.probabilities$`1`,
                      response=dfTB.valid$Enroll,
                      levels=levels(dfTB.valid$Enroll))
plot(regression.ROC)
regression.ROC$auc


#Decision Tree
treeTB.model <- train(Enroll~.,
                    data=dfTB.train,
                    method="rpart",
                    na.action=na.pass)

treeTB.model

install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

#View Tree Model Plot
prp(treeTB.model$finalModel,type=2,extra=106)

#Confusion Matrix
prediction <- predict(treeTB.model, newdata=dfTB.valid,na.action=na.pass)
confusionMatrix(prediction, dfTB.valid$Enroll)

#ROC Curve
tree.probabilities <- predict(treeTB.model,
                              newdata=dfTB.valid,
                              type='prob',
                              na.action=na.pass)

tree.ROC <- roc(predictor=tree.probabilities$`1`,
                response=dfTB.valid$Enroll,
                levels=levels(dfTB.valid$Enroll))
plot(tree.ROC)
tree.ROC$auc

