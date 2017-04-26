#Load the files
train=read.csv("train2016.csv",na.strings = c("","NA"))
test=read.csv("test2016.csv",na.strings = c("","NA"))

#Examine the Structure
str(train)
str(test)

summary(train)
summary(test)

#Dirty Implementation
#Basic Fixing of NA Values - Fixing Using Median/Most Frequent Observation
library(randomForest)

train=train[,c(1:6,8:108,7)]
data=rbind(train[,-108],test)
data=na.roughfix(data)
train=cbind(head(data,nrow(train)),Party=train[,108])
test=tail(data,nrow(test))
rm(data)

library(caret)
library(doParallel)

tctrl=trainControl(method="repeatedcv",
                   number=4,
                   repeats=1,
                   classProbs = TRUE,
                   summaryFunction = defaultSummary,
                   allowParallel = TRUE
)

#Basic Modelling Using Logistic Regression
cl=makeCluster(6)
registerDoParallel(cl)
logregv1=train(Party~.,
               data=train[-1],
               method="glm",
               trControl=tctrl
)
stopCluster(cl)

#Basic Modelling Using CART
cl=makeCluster(6)
registerDoParallel(cl)
cartv1=train(Party~.,
             data=train[-1],
             method="rpart",
             trControl=tctrl
)
stopCluster(cl)

#Basic Modelling Using GBM
cl=makeCluster(6)
registerDoParallel(cl)
gbmv1=train(Party~.,
            data=train[-1],
            method="gbm",
            trControl=tctrl
)
stopCluster(cl)

#Basic Modelling Using XGBoost
cl=makeCluster(6)
registerDoParallel(cl)
xgbtreev1=train(Party~.,
                data=train[-1],
                method="xgbTree",
                trControl=tctrl
)
stopCluster(cl)

results=data.frame(USER_ID=test$USER_ID,Predictions=predict(gbmv1,newdata=test))
write.csv(results,"submission01.csv",row.names = FALSE)

results=data.frame(USER_ID=test$USER_ID,Predictions=predict(xgbtreev2,newdata=test))
write.csv(results,"submission02.csv",row.names = FALSE)

varImp(xgbtreev1)

