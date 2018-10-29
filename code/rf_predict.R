#--------------------------------  NOTE  ----------------------------------------
# 1 This code is to predict the wind speed with random forest;
# 2 Coder: Cong Feng        Date: 2016/07/11       @ DOES Lab, UTD
#--------------------------------------------------------------------------------
rf_predict<-function(model,input_data){
  library(randomForest)
  # Use predict to estimate the output of the test data
  #print("Predicting output...")
  #x_test is the feature data of the testing set, y_test is the output of the testing set, and y_predict is the ML output
  x_test <- data.frame(input_data)
  y_predict<-predict(model,x_test)
  #print("Finished Prediction")
  return(y_predict)
}
