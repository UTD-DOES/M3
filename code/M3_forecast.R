#--------------------------------  NOTE  ----------------------------------------
# 1 This code is the Multi-Model Forecasting Framework forecasting function;
# 2 The MMFF algorithm can be referred: 'A data-driven multi-model methodology 
#   with deep feature selection for short-term wind forecasting'
# 3 No feature selection is included in the code for a general input
# 4 Coder: Cong Feng        Date: 2018/01/02       @ DOES Lab, UTD
#--------------------------------------------------------------------------------

M3_forecast <- function(forecasting_data, name_loc, th, root_model2, root_code2){
  cat("Forecasting with time horizon (hrs):", th, "\n")
  
  # load model
  load(paste0(root_model2, name_loc, '_', th, 'HA_models.RData'))
  # load functions
  source(paste0(root_code2,'svm_predict.R'))
  source(paste0(root_code2,'ann_predict.R'))
  source(paste0(root_code2,'gbm_predict.R'))
  source(paste0(root_code2,'rf_predict.R'))
  
  
  testing_data <- forecasting_data
  #-------------------------------------- Forecasting ---------------------------------------
  # ---------------------------- 1st-layer forecasting ----------------------------
  #svm models
  data_1st_forec_svm_radial <- data.frame(svm_predict(model1st_svm_radial,testing_data,'type3'))
  data_1st_forec_svm_linear <- data.frame(svm_predict(model1st_svm_li,testing_data,'type3'))
  data_1st_forec_svm_poly <- data.frame(svm_predict(model1st_svm_poly,testing_data,'type3'))
  data_1st_forec_svm_linear2 <- data.frame(svm_predict(model1st_svm_li2,testing_data,'type1'))
  data_1st_forec_svm_poly2 <- data.frame(svm_predict(model1st_svm_poly2,testing_data,'type1'))
  # ann models
  data_1st_forec_ann_1 <- data.frame(ann_predict(model1st_ann_1,testing_data))
  data_1st_forec_ann_2 <- data.frame(ann_predict(model1st_ann_2,testing_data))
  #data_1st_forec_ann_3 <- data.frame(ann_predict(model1st_ann_3,testing_data))
  data_1st_forec_ann_4 <- data.frame(ann_predict(model1st_ann_4,testing_data))
  data_1st_forec_ann_5 <- data.frame(ann_predict(model1st_ann_5,testing_data))
  data_1st_forec_ann_6 <- data.frame(ann_predict(model1st_ann_6,testing_data))
  #data_1st_forec_ann_7 <- data.frame(ann_predict(model1st_ann_7,testing_data))
  # gbm models
  data_1st_forec_gbm_1 <- data.frame(gbm_predict(model1st_gbm_1,testing_data,'type1'))
  data_1st_forec_gbm_2 <- data.frame(gbm_predict(model1st_gbm_2,testing_data,'type1'))
  data_1st_forec_gbm_3 <- data.frame(gbm_predict(model1st_gbm_3,testing_data,'type1'))
  data_1st_forec_gbm_4 <- data.frame(gbm_predict(model1st_gbm_4,testing_data,'type2'))
  # random forest models
  data_1st_forec_rf1 <- data.frame(rf_predict(model1st_rf1,testing_data))
  data_1st_forec_rf2 <- data.frame(rf_predict(model1st_rf2,testing_data))
  #data_1st_forec_rf3 <- data.frame(rf_predict(model1st_rf3,testing_data))
  # persistence model
  #data_1st_forec_persis <- data.frame(testing_data[1:(nrow(testing_data)-time_horizon),'target'])
  #data_1st_forec_target <- data.frame(testing_data[,'target'])
  # compile data for 2nd-layer model training
  data_input_2ndlyr_bf <- data.frame(cbind(data_1st_forec_svm_radial,data_1st_forec_svm_linear,data_1st_forec_svm_poly,
                                           data_1st_forec_svm_linear2, data_1st_forec_svm_poly2, data_1st_forec_ann_1,
                                           data_1st_forec_ann_2,data_1st_forec_ann_4,data_1st_forec_ann_5,
                                           data_1st_forec_ann_6,data_1st_forec_gbm_1,data_1st_forec_gbm_2,
                                           data_1st_forec_gbm_3,data_1st_forec_gbm_4,data_1st_forec_rf1,
                                           data_1st_forec_rf2))#,data_1st_forec_target)
  colnames(data_input_2ndlyr_bf) <- c('svm_r','svm_l','svm_p', 'svm_l2','svm_p2','ann1','ann2','ann4','ann5','ann6','gbm1','gbm2','gbm3','gbm4',
                                      'rf1','rf2')#,'target')
  data_input_2ndlyr <- data_input_2ndlyr_bf
  
  # ---------------------------- 2nd-layer forecasting ----------------------------
  #svm models
  data_2nd_forec_svm_radial <- data.frame(svm_predict(model2nd_svm_radial,data_input_2ndlyr,'type3'))
  data_2nd_forec_svm_linear <- data.frame(svm_predict(model2nd_svm_linear,data_input_2ndlyr,'type3'))
  data_2nd_forec_svm_poly <- data.frame(svm_predict(model2nd_svm_poly,data_input_2ndlyr,'type3'))
  data_2nd_forec_svm_linear2 <- data.frame(svm_predict(model2nd_svm_li2,data_input_2ndlyr,'type1'))
  data_2nd_forec_svm_poly2 <- data.frame(svm_predict(model2nd_svm_poly2,data_input_2ndlyr,'type1'))
  # ann models
  data_2nd_forec_ann_1 <- data.frame(ann_predict(model2nd_ann_1,data_input_2ndlyr))
  data_2nd_forec_ann_2 <- data.frame(ann_predict(model2nd_ann_2,data_input_2ndlyr))
  #data_2nd_forec_ann_3 <- data.frame(ann_predict(model2nd_ann_3,data_input_2ndlyr))
  data_2nd_forec_ann_4 <- data.frame(ann_predict(model2nd_ann_4,data_input_2ndlyr))
  data_2nd_forec_ann_5 <- data.frame(ann_predict(model2nd_ann_5,data_input_2ndlyr))
  data_2nd_forec_ann_6 <- data.frame(ann_predict(model2nd_ann_6,data_input_2ndlyr))
  #data_2nd_forec_ann_7 <- data.frame(ann_predict(model2nd_ann_7,data_input_2ndlyr))
  # gbm models
  data_2nd_forec_gbm_1 <- data.frame(gbm_predict(model2nd_gbm_1,data_input_2ndlyr,'type1'))
  data_2nd_forec_gbm_2 <- data.frame(gbm_predict(model2nd_gbm_2,data_input_2ndlyr,'type1'))
  data_2nd_forec_gbm_3 <- data.frame(gbm_predict(model2nd_gbm_3,data_input_2ndlyr,'type1'))
  data_2nd_forec_gbm_4 <- data.frame(gbm_predict(model2nd_gbm_4,data_input_2ndlyr,'type2'))
  # random forest models
  data_2nd_forec_rf1 <- data.frame(rf_predict(model2nd_rf1,data_input_2ndlyr))
  data_2nd_forec_rf2 <- data.frame(rf_predict(model2nd_rf2,data_input_2ndlyr))
  #data_2nd_forec_rf3 <- data.frame(rf_predict(model2nd_rf3,data_input_2ndlyr))
  
  
  # compile results
  results_bf_denorm <- data.frame(data_2nd_forec_svm_radial,data_2nd_forec_svm_linear,data_2nd_forec_svm_poly,
                                  data_2nd_forec_svm_linear2, data_2nd_forec_svm_poly2, data_2nd_forec_ann_1,
                                  data_2nd_forec_ann_2,data_2nd_forec_ann_4,data_2nd_forec_ann_5,data_2nd_forec_ann_6,
                                  data_2nd_forec_gbm_1,data_2nd_forec_gbm_2,data_2nd_forec_gbm_3,data_2nd_forec_gbm_4,
                                  data_2nd_forec_rf1,data_2nd_forec_rf2)#,data_1st_forec_target)
  colnames(results_bf_denorm) <- c('svm_r','svm_l','svm_p', 'svm_l2','svm_p2','ann1','ann2','ann4','ann5','ann6','gbm1','gbm2','gbm3','gbm4',
                                   'rf1','rf2')#,'target')
  return(results_bf_denorm)
}
