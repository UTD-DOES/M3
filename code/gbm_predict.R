#--------------------------------  NOTE  ----------------------------------------
# 1 This code is to predict the wind speed with gbm;
# 2 Coder: Cong Feng        Date: 2016/07/11       @ DOES Lab, UTD
#--------------------------------------------------------------------------------
gbm_predict<-function(model,input_data,type_of_model){
  library(gbm)
  library(caret)
  # Use predict to estimate the output of the test data
  #print("Predicting output...")

  x_test<- data.frame(input_data)
 
  # two types of the model need different command to do the prediction
  if (type_of_model == 'type1') {
    y_predict<-predict(model[1],x_test,model[2])
  }
  
  if (type_of_model == 'type2') {
    y_predict<-predict(model,x_test)
  }
  
  #print("Finished Prediction")
  
  return(y_predict)
}
