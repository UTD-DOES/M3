#--------------------------------  NOTE  ----------------------------------------
# 1 This code is to denormalize the data
# 2 Coder: Cong Feng        Date: 2017/09/27       @ DOES Lab, UTD
#--------------------------------------------------------------------------------

data_denormalization <- function(read_in_data, min_max){
  #print('Denormalize data...')
  # initialize the data matrix for returning
  data_after_denormliz <- data.frame(matrix(0,nrow(read_in_data),ncol(read_in_data)))
  colnames(data_after_denormliz) <- colnames(read_in_data)
  # normalization 
  for (i in 1:ncol(read_in_data)) { # question???: should normalize hour column?
    for (j in 1:nrow(read_in_data)) {
      data_after_denormliz[j,i] <- read_in_data[j,i]*(min_max[2,i]-min_max[1,i])+min_max[1,i]
    }
  }
  data_output <- data_after_denormliz
  
  return(data_output)
}