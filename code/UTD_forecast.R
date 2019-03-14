#--------------------------------  NOTE  ----------------------------------------
# 1 This code includes M3 forecasting and post-process
# 2 Coder: Cong Feng        Date: 2018/01/02       @ DOES Lab, UTD
#--------------------------------------------------------------------------------
UTD_forecast <- function(root_code, root_data, root_fcdata, root_model, root_savefile, va_frc){
  print('Start the forecasting process...')
  
  # Load in functions
  source(paste0(root_code,'data_normalization2.R'))
  source(paste0(root_code,'data_denormalization.R'))
  source(paste0(root_code,'M3_forecast.R'))
  source(paste0(root_code,'EvaMetrics.R'))
  library(lubridate)
  # Load in Main.csv
  info_main <- data.frame(read.table(paste0(root_data,'Main.csv'),header = TRUE,sep = ",",fill=TRUE))

  for(no_file in 1:nrow(info_main)){
    cat("Forecasting the location:", info_main[no_file,1], "\n")
    
    # Read in files of locations one by one
    file_input <- paste0(info_main[no_file,1], '_test.csv')
    data_loc <- data.frame(read.table(paste0(root_fcdata,file_input),header = TRUE,sep = ","))
    
    # Normalization
    norm_info <- data.frame(read.table(paste0(root_model,paste0(info_main[no_file,1]
                                                                , '_statnorm.csv')),header = TRUE,sep = ","))
    data_norm <- data_normalization2(data_loc[,6:ncol(data_loc)], norm_info)
    
    # Different time horizon
    th_loc <- info_main[1,2]
    results_un <- data.frame(matrix(0,th_loc,22))
    # Forecast different time horizons
    for(no_th in 1:th_loc){
      data_forecast <- data_norm[(nrow(data_norm)-24),]
      result_hr <- M3_forecast(data_forecast, info_main[no_file,1], no_th, root_model, root_code) # forecasting code
      results_un[no_th,6:(ncol(results_un)-1)] <- result_hr
      # fill the other matrix elements
      time_start <- as.POSIXct(paste(data_loc$Year,data_loc$Month, data_loc$Day, data_loc$Hour, data_loc$Minute), 
                               format="%Y%m%d%H%M")
      time_end <- time_start + hours(no_th)
      results_un[no_th,1:5] <- c(year(time_end), month(time_end), day(time_end), hour(time_end), minute(time_end)) # timestamp
    }
      
      results_un[,ncol(results_un)] <- data_norm[2:(1+no_th),va_frc] # actual value
      colnames(results_un) <- c(colnames(data_loc)[1:5], colnames(result_hr))
      
    # Denormalization
    results_norm <- data.frame(matrix(0,th_loc,22))
    for(i in 1:(ncol(results_un)-5)){
      results_norm[,(i+5)] <- data.frame(data_denormalization(as.data.frame(results_un[,(i+5)]), as.matrix(norm_info[,1])))
    }  
    results_norm[,1:5] <- results_un[,1:5]
    colnames(results_norm) <- colnames(results_un)
    
    # Evaluation
    #eval_mtrx <- data.frame(matrix(0,(ncol(results_norm)-6),6))
    #for(i in 1:(ncol(results_norm)-6)){
    #  eval_mtrx[i,] <- EvaMetrics(data.frame(results_norm[,(i+5)], results_norm$Actual))
    #}
    #rownames(eval_mtrx) <- colnames(results_norm)[6:(ncol(results_norm)-1)]
    #colnames(eval_mtrx) <- colnames(EvaMetrics(data.frame(results_norm[,(i+5)], results_norm$Actual)))
    
    # write out the final output files
    #idx_alg <- which.min(eval_mtrx$mae)
    output_ts <- results_norm[,c(1:5, (14+5))]
    colnames(output_ts)[6] <- 'Forecast'
    #output_eval <- eval_mtrx[idx_alg,]
    
    write.table(output_ts,file = paste0(root_savefile, info_main[no_file,1], '_Forecast_hourly', 
                                        output_ts[1,1], "-", output_ts[1,2], "-", output_ts[1,3], '.csv'),
                row.names = F,na='',col.names = TRUE,sep = ',')
    #write.table(output_eval,file = paste0(root_savefile, info_main[no_file,1], '_Metrics_hourly', 
    #                                    output_ts[1,1], "-", output_ts[1,2], "-", output_ts[1,3], '.csv'),
    #            row.names = F,na='',col.names = TRUE,sep = ',')
  }
}
