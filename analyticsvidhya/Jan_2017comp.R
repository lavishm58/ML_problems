library(dplyr)
library(randomForest)
library(gbm)
library(h2o)
train=read.csv('train.csv',na.strings = c(NA,'',' '))
test=read.csv('test.csv',na.strings = c(NA,'',' '))

data=rbind(train[,-13],test)
data=na.roughfix(data)
data=data[,-c(6,7,8)]
train2=cbind(head(data,nrow(train)),y=train[,13])
test2=tail(data,nrow(test))

  
  train2$pdi=(
    train2$Positive_Directional_Movement/train2$True_Range)
  train2$ndi=(
    train2$Negative_Directional_Movement/train2$True_Range)
  train2$dx=abs(pdi-ndi)/(pdi+ndi)
  test2$pdi=(
    test2$Positive_Directional_Movement/test2$True_Range)
  test2$ndi=(
    test2$Negative_Directional_Movement/test2$True_Range)
  test2$dx=abs(pdi_test-ndi_test)/(pdi_test+ndi_test)
  
  h2o.init(nthreads = -1,max_mem_size = "6G") 
  
  h2o_train <- as.h2o(train2)
  h2o_test <- as.h2o(test2)
  
  h2o_train$y <- h2o.asfactor(h2o_train$y)
  
  
  xd <- h2o.splitFrame(h2o_train,ratios = 0.6)
  
  split_val <- xd[[2]]
  
  y <- "u"
  x <- setdiff(colnames(F_train), c(y,"member_id"))
  
  gbm_clf <- h2o.gbm(x = x
                     ,y = y
                     ,training_frame = h2o_train
                     ,validation_frame = split_val
                     ,ignore_const_cols = T
                     ,ntrees = 1000
                     ,max_depth = 20
                     ,stopping_rounds = 10
                     ,model_id = "gbm_model"
                     ,stopping_metric = "logloss"
                     ,learn_rate = 0.05
                     ,col_sample_rate_per_tree = 0.8
                     ,sample_rate = 0.8
                     ,learn_rate_annealing = 0.99
                     
                     
  )
  
  gbm_clf #Validation Accuracy = 0.9858
  
  gbm_clf_pred <- as.data.table(h2o.predict(gbm_clf,h2o_test))
  head(gbm_clf_pred,10)
  
  sub_pred1 <- data.table(member_id = test$member_id, loan_status = gbm_clf_pred$p1)
  fwrite(sub_pred1,"h2o_gbm_sub_pred1.csv") 
