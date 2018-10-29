#--------------------------------  NOTE  ----------------------------------------
# 1 This code is to train the random forest model for the 1st layer;
# 2 Coder: Cong Feng        Date: 2016/06/24       @ DOES Lab, UTD
#--------------------------------------------------------------------------------

rf_train<-function(training_data,version_ctrl){ # For the random forest algorithm, no kernels need to be selected, but there are different ways to calibrate the parameters, different 
  library(randomForest)                         # types can be selected from 'type1', 'type2' and 'type3'
  
  #x is the model inputs and y is the model target
  x <- data.frame(training_data[,1:(ncol(training_data)-1)])
  y <- training_data[,(ncol(training_data))]
  #Train model directly (or used commented out caret module which took too long on the full dataset)
  #print("Training Random Forest model...")
  # 1st Version
  if (version_ctrl == 'type1') {
    model_rf <- randomForest(x,y, mtry=5,ntree = 1000, importance=TRUE, na.action=na.omit)
    
  }
  
  if (version_ctrl == 'type2') {
    model_rf <- tuneRF(x, y, mtryStart=1, ntreeTry=1000, stepFactor=2, improve=0.05,
                      trace=TRUE, plot=FALSE, doBest=TRUE)
  }
  if (version_ctrl == 'type3') {
    # train model
    control <- trainControl(method="repeatedcv", number=10, repeats=3)
    tunegrid <- expand.grid(.mtry=c(1:15))
    model_rf <- randomForest::train(x,y, method = 'rf',metric='RMSE', tuneGrid=tunegrid, trControl=control)
    summary(model_rf)
    plot(model_rf)
  }
  
  #print("Finish training Random Forest model")
  return(model_rf)
}
