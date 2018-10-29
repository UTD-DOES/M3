#--------------------------------  NOTE  ----------------------------------------
# 1 This code is to predict wind speed with ann models;
# 2 Coder: Cong Feng        Date: 2016/07/07       @ DOES Lab, UTD
#--------------------------------------------------------------------------------


ann_predict<-function(model,input_data){
  library(caret)
  library(RSNNS)
  library(nnet)
  # Use predict to estimate the output of the test data
  #print("Predicting output...")
  #x_test is the feature data of the testing set, y_test is the output of the testing set, and y_predict is the ML output
  x_test <- data.frame(input_data)
  y_predict<-predict(model,x_test)
  #print("Finished Prediction")
  return(y_predict)
}