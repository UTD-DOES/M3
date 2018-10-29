#--------------------------------  NOTE  ----------------------------------------
# 1 This code is to train the ann model for the 1st layer;
# 2 Coder: Cong Feng        Date: 2016/06/24       @ DOES Lab, UTD
#--------------------------------------------------------------------------------

ann_train_2<-function(training_data,learning_function,act_function,version_ctrl){
  library(caret)
  library(RSNNS)
  library(nnet)
  
  #x is the model inputs and y is the model target
  x<-training_data[,1:(ncol(training_data)-1)]
  y<-training_data[,(ncol(training_data))]
  #Train model directly (or used commented out caret module which took too long on the full dataset)
  #print("Training ANN model...")
  # 1st Version
  if (version_ctrl == 'type1') {
    model_ann <-mlp(x, y, size = c(10), maxit = 1000,
                 initFunc = "Randomize_Weights", initFuncParams = c(-0.3, 0.3),
                 learnFunc = learning_function, learnFuncParams = c(0.1,0),
                 updateFunc = "Topological_Order", updateFuncParams = c(0),
                 hiddenActFunc = act_function, shufflePatterns = TRUE, linOut = FALSE,
                 inputsTest = NULL, targetsTest = NULL, pruneFunc = NULL,
                 pruneFuncParams = NULL)
  }
  
  if (version_ctrl == 'type2') {
    model_ann <- train(x,y,
                   method = "nnet",
                   preProcess = "range", #scales the data to be within [0,1]
                   tuneLength = 5,
                   trace = FALSE,
                   maxit = 100)
  }
  
  if (version_ctrl == 'type3') {
    model_ann <- rbf(x, y, size=5, maxit=1000,
             initFuncParams=c(0, 1, 0, 0.01, 0.001),
             learnFuncParams=c(1e-8, 0, 1e-8, 0.1, 0.8), linOut=FALSE)
  }

 
  #print("Finish training ANN model")
  return(model_ann)
}
