#--------------------------------  NOTE  ----------------------------------------
# 1 This code is to train the svm model for the 1st layer;
# 2 Coder: Cong Feng        Date: 2016/07/07       @ DOES Lab, UTD
#--------------------------------------------------------------------------------


svm_predict<-function(model,input_data, type){
  library(e1071)
  library(caret)
  # Use predict to estimate the output of the test data
  #print("Predicting output...")

  x_test <- data.frame(input_data)
  colnames(x_test) <- colnames(input_data)
  if(type == 'type1'){
    y_predict <- predict(model,x_test, na.action = na.omit)
  }
  
  if(type == 'type3'){
    y_predict <- predict(model, newdata = x_test, na.action = na.omit)
  }
  #print("Finished Prediction")
  return(y_predict)
}