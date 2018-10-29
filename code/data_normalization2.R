#--------------------------------  NOTE  ----------------------------------------
# 1 This code is to normalize the testing data, using the same max and min as the
#   training data
# 2 Coder: Cong Feng        Date: 2017/09/27       @ DOES Lab, UTD
#--------------------------------------------------------------------------------

data_normalization2 <- function(read_in_data, min_max){
  #print('Normalize testing data...')
  # initialize the data matrix for returning
  data_after_normliz <- data.frame(matrix(0,nrow(read_in_data),ncol(read_in_data)))
  colnames(data_after_normliz) <- colnames(read_in_data)
  
  # normalization 
  for (i in 1:ncol(read_in_data)) { # question???: should normalize hour column?
    for (j in 1:nrow(read_in_data)) {
      data_after_normliz[j,i] <- (read_in_data[j,i] - min_max[1,i])/(min_max[2,i]-min_max[1,i])
    }
  }
  # return normalized data
  data_after_normliz <- data_after_normliz[, colSums(is.na(data_after_normliz)) != nrow(data_after_normliz)]
  data_output <- data_after_normliz
  return(data_output)
}