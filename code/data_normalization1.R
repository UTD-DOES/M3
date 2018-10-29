#--------------------------------  NOTE  ----------------------------------------
# 1 This code is to normalize the training data
# 2 Coder: Cong Feng        Date: 2017/09/27       @ DOES Lab, UTD
#--------------------------------------------------------------------------------

data_normalization1 <- function(read_in_data){
  #print('Normalize training data...')
  # initialize the data matrix for returning
  data_after_normliz <- data.frame(matrix(0,nrow(read_in_data),ncol(read_in_data)))
  colnames(data_after_normliz) <- colnames(read_in_data)
  min_max <- data.frame(matrix(0,2,ncol(read_in_data)))
  rownames(min_max) <- c('min','max')
  colnames(min_max) <- colnames(read_in_data)
  # normalization 
  for (i in 1:ncol(read_in_data)) { # question???: should normalize hour column?
    for (j in 1:nrow(read_in_data)) {
     # print(j)
      data_after_normliz[j,i] <- (read_in_data[j,i] - min(read_in_data[,i]))/(max(read_in_data[,i])-min(read_in_data[,i]))
    }
    min_max[,i] <- c(min(read_in_data[,i]), max(read_in_data[,i]))
  }
  # return normalized data
  data_after_normliz <- data_after_normliz[, colSums(is.na(data_after_normliz)) != nrow(data_after_normliz)]
  data_output <- list(data_after_normliz, min_max)
  return(data_output)
}