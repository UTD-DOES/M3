# clear R workspace and concole
rm(list=ls(all=TRUE))
cat("\014")

root_data <- '/Users/joeyfueng/Desktop/Rearch/Projects/WindView/sample data/complete/'
root_save1 <- '/Users/joeyfueng/Desktop/Rearch/Projects/WindView/sample data/input/'
root_save2 <- '/Users/joeyfueng/Desktop/Rearch/Projects/WindView/sample data/forecast/'

list_file <- list.files(root_data)

for(i in 1:length(list_file)){
  name_file <- list_file[i]
  data_1 <- data.frame(read.table(paste0(root_data,name_file),header = TRUE,sep = ","))
  data_1 <- data_1[data_1$Minute == 0,]
  
  # choose the window
  start <- 200
  
  
  data_train <- data_1[start:(start+8000),]
  data_test <- data_1[(start+8001):(start+8025),]
  
  # write out
  name_save <- sub('\\_.*', '', name_file)
  write.table(data_train,file = paste0(root_save1, name_save, '.csv'),row.names = FALSE,na='',col.names = TRUE,sep = ',')
  write.table(data_test,file = paste0(root_save2, name_save, '_test.csv'),row.names = FALSE,na='',col.names = TRUE,sep = ',')
}

