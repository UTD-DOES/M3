#--------------------------------  NOTE  ----------------------------------------
# 1 This code is to train the gbm model;
# 2 Coder: Cong Feng        Date: 2016/06/24       @ DOES Lab, UTD
#--------------------------------------------------------------------------------

gbm_train<-function(training_data,distri_method,version_ctrl){ #the distri_method can be choose from 'gaussian','tdist' and 'laplace'; version_ctrl is either 'type1' or 'type2'
  library(gbm)
  library(caret)
  #x is the model inputs and y is the model target
  x<- data.frame(training_data[,1:(ncol(training_data)-1)])
  y<-training_data[,(ncol(training_data))]

  #print("Training GBM model...")
  # 1st Version
  if (version_ctrl == 'type1') {
    model_gbm <- gbm.fit(x,y,distribution = distri_method,n.trees = 10000,
                interaction.depth = 1,n.minobsinnode = 10,shrinkage = 0.001,bag.fraction = 0.5,verbose = FALSE)
    # check performance using an out-of-bag estimator
    # OOB underestimates the optimal number of iterations
    best.iter <- gbm.perf(model_gbm,method="OOB",plot.it = FALSE)
    
  }
  
  # 2nd version
  if (version_ctrl == 'type2') {
    #use the caret module to train the gbm algorithm using a grid of parameters taken from this source: http://stackoverflow.com/questions/8722247/r-caret-and-gbm-cant-find-ntrees-input
    gbmGrid <- expand.grid(interaction.depth = (1:5) * 2, n.trees = (1:5)*50, shrinkage = .1,n.minobsinnode=10)
    bootControl <- trainControl(number = 1)
    #model<-gbm.fit(x,y,distribution = "gaussian",n.trees=100)
    model_gbm <- caret::train(x, y, method = "gbm", tuneLength = 5, trControl = bootControl, tuneGrid = gbmGrid)
  }
  
  #print("Finish training GBM model")
  
  if (version_ctrl == 'type1') {
    return(list(model_gbm,best.iter))
  }
  if (version_ctrl == 'type2') {
    return(model_gbm)
  }
}
