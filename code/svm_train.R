#--------------------------------  NOTE  ----------------------------------------
# 1 This code is to train the svm model for the 1st layer;
# 2 Coder: Cong Feng        Date: 2016/06/24       @ DOES Lab, UTD
#--------------------------------------------------------------------------------

svm_train<-function(training_data,kernel_type,version_ctrl){ # the version_ctrl can be 'type1','type2' or 'type3'(but the tunning time for type2 models is too long);
  library(caret)                                             # when version_ctrl is 'type1', kernel_type can be 'linear' or 'polynomial'; when when version_ctrl is 'type2', kernel_type
  library(e1071)                                             # can be 'svmLinear', 'svmPoly' or 'svmRadial'
 
  #x is the model inputs and y is the model target
  x <- data.frame(training_data[,1:(ncol(training_data)-1)])
  y <- training_data[,(ncol(training_data))]

  #print("Training SVR model...")
  # 1st Version
  if (version_ctrl == 'type1') {
  model <-svm(x,y,type="eps-regression",kernal=kernel_type,gamma = 0.001,
              degree = 5,epsilon = 0.001)
  }
  
  # 2nd Version
  if (version_ctrl == 'type2') {
  #tune_result <- tune(svm, train.x=x, train.y=y,kernel=kernel_type, ranges=list(cost=seq(0.01,100,0.01), gamma=seq(0.001,10,0.001)))
  #svm_tuned_model <- tune_result$best.model  
  }
  
  # 3rd Version
  if (version_ctrl == 'type3') {
  cvCtrl <- trainControl(method = "repeatedcv", repeats = 5)
  #gridval=svmGrid(x,y,2)
  model<-caret::train(x,y,method=kernel_type,tuneLength = 1,preProc = c("center","scale"),metric="RMSE",trControl=cvCtrl)	# 9 values of the cost function
  }
  
  #print("Finish training SVR model")
  return(model)
}
  