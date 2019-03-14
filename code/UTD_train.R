#--------------------------------  NOTE  ----------------------------------------
# 1 This code includes M3 method and pre-processing methods
# 2 Coder: Cong Feng        Date: 2017/12/18       @ DOES Lab, UTD
#--------------------------------------------------------------------------------
UTD_train <- function(root_code, root_data, root_model, root_savefile22){
  print('Start the training process...')
  
  # global variables
  part2 <- .5
  # load in functions
  source(paste0(root_code,'data_normalization1.R'))
  source(paste0(root_code,'format_data.R'))
  source(paste0(root_code,'M3_train.R'))
  source(paste0(root_code,'M3_forecast.R'))
  source(paste0(root_code,'data_denormalization.R'))
  
  # read in Main.csv
  info_main <- data.frame(read.table(paste0(root_data,'Main.csv'),header = TRUE,sep = ",", fill=TRUE))
  
  for(no_file in 1:nrow(info_main)){
    
    # Read in files of locations one by one
    file_input <- paste0(info_main[no_file,1], '.csv')
    data_loc <- data.frame(read.table(paste0(root_data,file_input),header = TRUE,sep = ","))
    data_loc <- data_loc[(nrow(data_loc)-100):nrow(data_loc),] # use the latest 8000 data
    data_loc[data_loc==-9999] <- 0
    # Normalization
    list_norm <- data_normalization1(data_loc[,6:ncol(data_loc)])
    data_norm <- list_norm[[1]] # normalized data
    max_min <- list_norm[[2]]   # min and max values
    
    # Different time horizons
    th_loc <- info_main[1,2]
    for(no_th in 1:th_loc){
      data_train <- format_data(data_norm, no_th, 'power')
      data_train1 <- data_train[1:floor(1/12*nrow(data_train)),]
      data_train2 <- data_train[(floor(1/12*nrow(data_train))+1):nrow(data_train),]
      # Train M3 models
      #M3_train(data_train, no_th, root_code, root_model, part2, info_main[no_file,1])
      M3_train(data_train, no_th, root_code, root_model, part2, info_main[no_file,1])
      result_hr <- M3_forecast(data_train1[,-ncol(data_train1)], info_main[no_file,1], no_th, root_model, root_code) # forecasting code
      result_hr2 <- data.frame(result_hr$gbm4, data_train1$target)
      # define repeat column function
      rep.col<-function(x,n){
        matrix(rep(x,each=n), ncol=n, byrow=TRUE)
      }
      result_denorm <- data.frame(data_denormalization(as.data.frame(result_hr2), 
                                                       rep.col(as.matrix(max_min[,1]),2)))
      colnames(result_denorm) <- c('Forecasts', 'Actuals')
      write.table(result_denorm,file = paste0(root_savefile22,info_main[no_file,1], '_',no_th,'_HA_SurrogateData.csv'),row.names = FALSE,na='',col.names = TRUE,sep = ',')
      
    }
    # write out min max statistics
    write.table(max_min,file = paste0(root_model,info_main[no_file,1], '_statnorm.csv'),row.names = TRUE,na='',col.names = TRUE,sep = ',')
  }
  print('Finished training process for all locations')
}
