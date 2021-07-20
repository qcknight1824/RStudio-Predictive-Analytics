#---------------------------------------------------------------------------------
#----------------------------Random Forest Model----------------------------------
#---------------------------------------------------------------------------------

install.packages('caret')
install.packages('randomForest')

library(caret)
library(randomForest)

dfTB <- read.csv('BostonHousing.csv', na.strings=c('NA',''))


#dfTB <- na.omit(dfTB)
summary(dfTB)
str(dfTB)

#Removing continuous median value DV
dfTB <- subset(dfTB, select = -MEDV)

values_count <- sapply(lapply(dfTB, unique), length)
values_count

#CAT..MEDV is binary DV

dfTB$CAT..MEDV <- factor(dfTB$CAT..MEDV)
dfTB$CHAS <- factor(dfTB$CHAS)

#Partition
set.seed(101)
trainIndex <- createDataPartition(dfTB$CAT..MEDV,
                                  p=0.7,
                                  list=FALSE,
                                  times=1)

dfTB.train <- dfTB[trainIndex,]
dfTB.valid <-dfTB[-trainIndex,]

#Default random forest
RFTB <- train(CAT..MEDV~.,
                    data=dfTB.train,
                    method='rf',
                    metric='Accuracy',
                    ntree=100)

print(RFTB)

#detailed mtry
tuneGrid <- expand.grid(.mtry=c(1:12))

rfTB_mtry <- train(CAT..MEDV~.,
                 data=dfTB.train,
                 method='rf',
                 metric='Accuracy',
                 tuneGrid=tuneGrid,
                 importance=TRUE,
                 ntree=100)
print(rfTB_mtry)

# Evaluate model performance
prediction <- predict(rfTB_mtry,dfTB.valid)
confusionMatrix(prediction,dfTB.valid$CAT..MEDV)

# Variable importance
varImp(rfTB_mtry)


#---------------------------------------------------------------------------------
#-----------------------------------SVM Model-------------------------------------
#---------------------------------------------------------------------------------


# Create 10-fold cross validataion 
trControl <- trainControl(method='cv',
                          number=10,
                          search='grid')

# SVM Model with the linear Kernel function
# Pre-processing data with centering and scaling
SVMTB_Linear <- train(CAT..MEDV~.,
                    data=dfTB.train,
                    method='svmLinear',
                    trControl=trControl,
                    preProcess=c('center','scale'))

print(SVMTB_Linear)

# Evaluate the linear SVM model performance
linear_pred <- predict(SVMTB_Linear,dfTB.valid)
confusionMatrix(linear_pred,dfTB.valid$CAT..MEDV)


# SVM Model with the Radial Kernel function
SVMTB_Radial <- train(CAT..MEDV~.,
                    data=dfTB.train,
                    method='svmRadial',
                    trControl=trControl,
                    preProcess=c('center','scale'))

print(SVMTB_Radial)

# Evaluate the radial SVM model performance
radial_pred <- predict(SVMTB_Radial,dfTB.valid)
confusionMatrix(radial_pred,dfTB.valid$CAT..MEDV)

# Additional model tuning for the radial SVM model
grid_radial <- expand.grid(sigma = c(0.0,0.5,0.75,1.0,1.3,1.5),
                           C = c(0,0.05, 0.25, 0.5, 0.75, 1))

SVMTB_Radial_tune <- train(CAT..MEDV~.,
                         data=dfTB.train,
                         method='svmRadial',
                         trControl=trControl,
                         preProcess=c('center','scale'),
                         tuneGrid=grid_radial)
print(SVMTB_Radial_tune)

# Evaluate the radial SVM model performance
radial_tune_pred <- predict(SVMTB_Radial_tune,dfTB.valid)
confusionMatrix(radial_tune_pred,dfTB.valid$CAT..MEDV)


