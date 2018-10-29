#--------------------------------  NOTE  ----------------------------------------
# 1 This code is to analyze the prediction result;
# 2 The format of the input data is [forecasts actuals];
# 3 Coder: Jie Zhang, Cong Feng         Date: 2016/02/19       @ DOES Lab, UTD
#--------------------------------------------------------------------------------

EvaMetrics <- function(anly) {
  library("stats")
  library("fBasics")
  #do the forecast error calculation and normalization
  cap <- max(anly[,2])
  fe1 <- as.matrix((anly[,1]-anly[,2])/anly[,2]) # different normalization standards
  fe2 <- as.matrix((anly[,1]-anly[,2])/cap)
  # remove the NAs
  fe1 <- fe1[!is.infinite(fe1)]
  fe1 <- fe1[!is.na(fe1)]
  fe2 <- fe2[!is.na(fe2)]
  
  #-------------------- error evaluation metrics ------------------
  mape <- mean(abs(fe1))     # mean absolute percentage error
  mae <- mean(abs(fe2))*cap  # mean absolute error
  nmae <- mean(abs(fe2))     # normalized mean absolute error
  mse <- mean((fe2*cap)^2)   # mean square error
  rmse <- sqrt(mse)          # root mean square error
  nrmse <- rmse/cap          # normalized root mean square error
  maxae <- max(anly[,1]-anly[,2])          # maximum absolute error
  mbe <- mean(anly[,1]-anly[,2])   # mean bias error
  rmqe <- (mean((fe2*cap)^4))^(1/4) # Root mean quartic error
  nrmqe <- (mean((fe2*cap)^4))^(1/4)/cap # normalized root mean quartic error
  sum_error <- sum(abs(fe2))*cap
  
  #-------------------- Pearson's correlation coefficient -----------------------
  correlation_coeff <- cov(anly[,2],anly[,1])/(sd(anly[,2])*sd(anly[,1]))
  
  result <- as.matrix(cbind(correlation_coeff, rmse, nrmse, maxae, mae, mbe))
  
  colnames(result) <- c('correlation_coeff', 'rmse', 'nrmse', 'maxae', 'mae', 'mbe')
    #c(mape,mae,nmae,mse,rmse,nrmse,maxae,mbe,rmqe,nrmqe,sum_error,skew,kurt,sigma,SS,renyi,correlation_coeff,
             # KSIPer,OVERPer,phi95)
#  colnames(result) <- c('mape','mae','nmae','mse','rmse','nrmse','maxae','mbe','rmqe','nrmqe','sum_error','skew','kurt','sigma','SS',
#                   'renyi','correlation_coeff','KSIPer','OVERPer','phi95')
  
  #cat("Mean Absolute Percentage Error", mape, "Mean Absolute Error: ", mae, "Normalized Mean Absolute Error",nmae, 
   #   "Mean Square Error", mse, "Root Mean Square Error", rsme,"Standard Deviation: ", sigma, "Skewness: ",skew,
   #   "Kurtosis: ", kurt, "Renyi entropy: ",renyi, sep = "\n")
  
  return(result)
}