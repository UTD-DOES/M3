#--------------------------------  NOTE  ----------------------------------------
# 1 This code is run the training and forecasting codes;
# 2 All you need to change is root_main;
# 3 root_main: WindView folder directory; root_code1: code directory; 
#   root_data1: data(Main.csv and other location.csv) directory; 
#   root_model1: model saving directory
# 4 Coder: Cong Feng        Date: 2017/12/18       @ DOES Lab, UTD
#--------------------------------------------------------------------------------


# clear R workspace and concole
rm(list=ls(all=TRUE))
cat("\014")

#___________________ Change This Root __________________________
root_main <- '/Users/joeyfueng/Desktop/Rearch/Projects/WindView/'
#_______________________________________________________________


# root entries
root_code1 <- paste0(root_main,'code/') # all the code
root_data1 <- paste0(root_main,'sample data/input/') # training data
root_model1<- paste0(root_main,'model/') # all the models trained by UTD_train.R
root_data2 <- paste0(root_main,'sample data/forecast/') # forecasting input data
root_savefile2 <- paste0(root_main,'inter_results/') # intermediate result files
root_savefile <- paste0(root_main,'output/') # output files
va_frc <- 'power' # forecasting target variable

source(paste0(root_code1,'UTD_train.R'))
source(paste0(root_code1,'UTD_forecast.R'))


# Run the program by two lines: training and forecasting, seperately

UTD_train(root_code1, root_data1, root_model1, root_savefile2)

UTD_forecast(root_code1, root_data1, root_data2, root_model1, root_savefile, va_frc)

