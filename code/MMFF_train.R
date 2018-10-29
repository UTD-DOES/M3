#--------------------------------  NOTE  ----------------------------------------
# 1 This code is the Multi-Model Forecasting Framework training function;
# 2 The MMFF algorithm can be referred: 'A data-driven multi-model methodology 
#   with deep feature selection for short-term wind forecasting'
# 3 No feature selection is included in the code for a general input
# 4 Coder: Cong Feng        Date: 2017/12/18       @ DOES Lab, UTD
#--------------------------------------------------------------------------------

MMFF_train <- function(training_data, th, code_root1, root_savefile, part2, name_loc){
  
  print('Training Multi-Model Forecasting Framework ...')
  # setting up system time calculation
  begtime_sub <-Sys.time()

  # load in machine learning models
  source(paste0(code_root1,'svm_train.R'))
  source(paste0(code_root1,'svm_predict.R'))
  source(paste0(code_root1,'ann_train.R'))
  source(paste0(code_root1,'ann_train_2.R'))
  source(paste0(code_root1,'ann_predict.R'))
  source(paste0(code_root1,'gbm_train.R'))
  source(paste0(code_root1,'gbm_predict.R'))
  source(paste0(code_root1,'rf_train.R'))
  source(paste0(code_root1,'rf_predict.R')) 
  
  # Data partition
  datalength_train_1st_layer <- ceiling(part2*nrow(training_data))
  data_training_1stlyr <- training_data[1:datalength_train_1st_layer,]
  data_forecasting_2ndlyr <- training_data[(1+datalength_train_1st_layer):nrow(training_data),]# this is the training 
  data_forecasting_2ndlyrb <- training_data[(1+datalength_train_1st_layer):nrow(training_data),-ncol(training_data)]# this is the training 
  # data for the second layer
  #------------------ train 1st layer models -----------------------
  print('Training First-layer Models ... ')
  # svm models
  model1st_svm_radial <- svm_train(data_training_1stlyr,"svmRadial",'type3')
  model1st_svm_li <- svm_train(data_training_1stlyr,"svmLinear",'type3') 
  model1st_svm_poly <- svm_train(data_training_1stlyr,"svmPoly",'type3')
  model1st_svm_li2 <- svm_train(data_training_1stlyr,"linear",'type1')
  model1st_svm_poly2 <- svm_train(data_training_1stlyr,"polynomial",'type1')
  # ann models
  model1st_ann_1 <- ann_train_2(data_training_1stlyr,'Std_Backpropagation','Act_Logistic','type1')
  model1st_ann_2 <- ann_train_2(data_training_1stlyr,'BackpropChunk','Act_Logistic','type1')
  model1st_ann_3 <- ann_train_2(data_training_1stlyr,'BackpropBatch','Act_Logistic','type1')
  model1st_ann_4 <- ann_train_2(data_training_1stlyr,'BackpropMomentum','Act_Logistic','type1')
  model1st_ann_5 <- ann_train_2(data_training_1stlyr,'Rprop','Act_Logistic','type1')
  model1st_ann_6 <- ann_train_2(data_training_1stlyr,'SCG','Act_Logistic','type1')
  model1st_ann_7 <- ann_train_2(data_training_1stlyr,'None','None','type3')
  # gbm models
  model1st_gbm_1 <- gbm_train(data_training_1stlyr,'gaussian','type1')
  model1st_gbm_2 <- gbm_train(data_training_1stlyr,'tdist','type1')
  model1st_gbm_3 <- gbm_train(data_training_1stlyr,'laplace','type1')
  model1st_gbm_4 <- gbm_train(data_training_1stlyr,'None','type2')
  # random forest models
  model1st_rf1 <- rf_train(data_training_1stlyr,'type1')
  model1st_rf2 <- rf_train(data_training_1stlyr,'type2')
  #model1st_rf3 <- rf_train(data_training_1stlyr,'type3')
  #print('Finished Training First-layer Models.')
  
  #------ Prepare data to train the 2nd layer (Forecasting with 1st layer Models) ----------
  print('Preparing Data for Second-layer Model Training ... ')
  # svm models
  data_2nd_train_svm_radial <- data.frame(svm_predict(model1st_svm_radial,data_forecasting_2ndlyrb,'type3'))
  data_2nd_train_svm_linear <- data.frame(svm_predict(model1st_svm_li,data_forecasting_2ndlyrb,'type3'))
  data_2nd_train_svm_poly <- data.frame(svm_predict(model1st_svm_poly,data_forecasting_2ndlyrb,'type3'))
  data_2nd_train_svm_linear2 <- data.frame(svm_predict(model1st_svm_li2,data_forecasting_2ndlyrb,'type1')) # results are identical
  data_2nd_train_svm_poly2 <- data.frame(svm_predict(model1st_svm_poly2,data_forecasting_2ndlyrb,'type1'))
  # ann models
  data_2nd_train_ann_1 <- data.frame(ann_predict(model1st_ann_1,data_forecasting_2ndlyrb))
  data_2nd_train_ann_2 <- data.frame(ann_predict(model1st_ann_2,data_forecasting_2ndlyrb))
  #data_2nd_train_ann_3 <- data.frame(ann_predict(model1st_ann_3,data_forecasting_2ndlyr))
  data_2nd_train_ann_4 <- data.frame(ann_predict(model1st_ann_4,data_forecasting_2ndlyrb))
  data_2nd_train_ann_5 <- data.frame(ann_predict(model1st_ann_5,data_forecasting_2ndlyrb))
  data_2nd_train_ann_6 <- data.frame(ann_predict(model1st_ann_6,data_forecasting_2ndlyrb))
  #data_2nd_train_ann_7 <- data.frame(ann_predict(model1st_ann_7,data_forecasting_2ndlyr))
  # gbm models
  data_2nd_train_gbm_1 <- data.frame(gbm_predict(model1st_gbm_1,data_forecasting_2ndlyrb,'type1'))
  data_2nd_train_gbm_2 <- data.frame(gbm_predict(model1st_gbm_2,data_forecasting_2ndlyrb,'type1'))
  data_2nd_train_gbm_3 <- data.frame(gbm_predict(model1st_gbm_3,data_forecasting_2ndlyrb,'type1'))
  data_2nd_train_gbm_4 <- data.frame(gbm_predict(model1st_gbm_4,data_forecasting_2ndlyrb,'type2'))
  # random forest models
  data_2nd_train_rf1 <- data.frame(rf_predict(model1st_rf1,data_forecasting_2ndlyrb))
  data_2nd_train_rf2 <- data.frame(rf_predict(model1st_rf2,data_forecasting_2ndlyrb))
  #data_2nd_train_rf3 <- data.frame(rf_predict(model1st_rf3,data_forecasting_2ndlyr)) # taking too long time to train
  # persistence model
  #data_2nd_train_persis <- data_forecasting_2ndlyr[1:(nrow(data_forecasting_2ndlyr)-time_horizon),'target'] # two steps gap between persistence and target
  
  # compile data for 2nd-layer model training
  data_bf_modelselection <- data.frame(cbind(data_2nd_train_svm_radial,data_2nd_train_svm_linear,data_2nd_train_svm_poly,
                                             data_2nd_train_svm_linear2, data_2nd_train_svm_poly2, data_2nd_train_ann_1,
                                             data_2nd_train_ann_2,data_2nd_train_ann_4,data_2nd_train_ann_5,
                                             data_2nd_train_ann_6,data_2nd_train_gbm_1,data_2nd_train_gbm_2,
                                             data_2nd_train_gbm_3,data_2nd_train_gbm_4,data_2nd_train_rf1,
                                             data_2nd_train_rf2),
                                             data_forecasting_2ndlyr[,'target'])
  colnames(data_bf_modelselection) <- c('svm_r','svm_l','svm_p', 'svm_l2','svm_p2','ann1','ann2','ann4','ann5','ann6','gbm1','gbm2','gbm3','gbm4',
                                      'rf1','rf2','target')
  #print('Finished Preparing Data for Second-layer Model Training. ')
  
  # there is no model selection before the second layer
  data_training_2ndlyr <- data_bf_modelselection
  
  #------------------------------ train 2nd layer models -------------------------------
  print('Training Second-layer Models ... ')
  # svm
  model2nd_svm_radial <- svm_train(data_training_2ndlyr,"svmRadial",'type3')
  model2nd_svm_linear <- svm_train(data_training_2ndlyr,"svmLinear",'type3')
  model2nd_svm_poly <- svm_train(data_training_2ndlyr,"svmPoly",'type3')
  model2nd_svm_poly2 <- svm_train(data_training_2ndlyr,"svmPoly",'type3')
  model2nd_svm_li2 <- svm_train(data_training_2ndlyr,"svmLinear",'type3') 
  # ann
  model2nd_ann_1 <- ann_train_2(data_training_2ndlyr,'Std_Backpropagation','Act_Logistic','type1')
  model2nd_ann_2 <- ann_train_2(data_training_2ndlyr,'BackpropChunk','Act_Logistic','type1')
  #model2nd_ann_3 <- ann_train_2(data_training_2ndlyr,'BackpropBatch','Act_Logistic','type1')
  model2nd_ann_4 <- ann_train_2(data_training_2ndlyr,'BackpropMomentum','Act_Logistic','type1')
  model2nd_ann_5 <- ann_train_2(data_training_2ndlyr,'Rprop','Act_Logistic','type1')
  model2nd_ann_6 <- ann_train_2(data_training_2ndlyr,'SCG','Act_Logistic','type1')
  #model2nd_ann_7 <- ann_train_2(data_training_2ndlyr,'None','None','type3')
  # gbm
  model2nd_gbm_1 <- gbm_train(data_training_2ndlyr,'gaussian','type1')
  model2nd_gbm_2 <- gbm_train(data_training_2ndlyr,'tdist','type1')
  model2nd_gbm_3 <- gbm_train(data_training_2ndlyr,'laplace','type1')
  model2nd_gbm_4 <- gbm_train(data_training_2ndlyr,'None','type2')
  # random
  model2nd_rf1 <- rf_train(data_training_2ndlyr,'type1')
  model2nd_rf2 <- rf_train(data_training_2ndlyr,'type2')
  #model2nd_rf3 <- rf_train(data_training_2ndlyr,'type3')
  #print('Finished Second-layer Training.')
  print('Finished MMFF Training')
  
  # save all the data and models
  setwd(root_savefile)
  save_name <- paste0(name_loc, '_',th,'HA_models.RData')
  #save.image(save_name)
  save(list = ls(),file = save_name)
  
  cat("The models and data generated at the training stage have been saved as:", save_name, "\n")
}
