#--------------------------------  NOTE  ----------------------------------------
# 1 This code is to create the target variable related to the x-h ahead forecasting;
# 2 Coder: Cong Feng        Date: 2017/04/17       @ DOES Lab, UTD
#--------------------------------------------------------------------------------
format_data <-function(data_input,time_horizon,forc_variable){
  cat('The data is arranged for forecasting horizon:',time_horizon,'\n')
  cat('The forecast target variable is:',forc_variable,'\n')
  
  data_format1 <- data.frame(data_input[1:(nrow(data_input)-time_horizon),],data_input[(1+time_horizon):nrow(data_input),forc_variable])
  data_add <- cbind(data_input[(nrow(data_input)-time_horizon+1):nrow(data_input),],data_input[(nrow(data_input)-time_horizon+1):nrow(data_input),forc_variable])
  
  colnames(data_format1) <- c(colnames(data_input),'target')
  colnames(data_add) <- c(colnames(data_input),'target')
  
  data_format <- data.frame(rbind(data_format1,data_add))
  return(data_format)
  }