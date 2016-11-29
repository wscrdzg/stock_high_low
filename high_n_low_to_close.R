load_stock_history <- function(ticker = "SPY",start_year = 1950,end_year = 2016) {
  download_address <- paste("http://chart.finance.yahoo.com/table.csv?s=",ticker,"&c=",start_year,"&f=",end_year,"&g=d&ignore=.csv",sep="")
  download.file(download_address,destfile = paste(ticker,"_hist_data.csv",sep=""))
  data <- read.csv(paste(ticker,"_hist_data.csv",sep=""))
  data
}

#for this one say those out of 5 and 95 quantile range
remove_outliers <- function(x, na.rm = TRUE, ...) {
  #those out of 5 and 95 quantile range
  qnt <- quantile(x, probs=c(.05, .95), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

# Calculate all daily high compare to previous close
high_n_low_to_close <- function(ticker, breaksofhist = 50){
  data <- load_stock_history(ticker = ticker)
  lengthofdata <- length(data$Date) - 1
  
  #High minus previous Close
  H_minus_pre_C <- vector(mode="numeric", length=lengthofdata)
  for (i in 1:lengthofdata){
    H_minus_pre_C[i] <- (data[i,]$High - data[i+1,]$Close) / (data[i+1,]$Close / 100)
  }
  
  #Low minus previous Close
  L_minus_pre_C <- vector(mode="numeric", length=lengthofdata)
  for (i in 1:lengthofdata){
    L_minus_pre_C[i] <- (data[i,]$Low - data[i+1,]$Close) / (data[i+1,]$Close / 100)
  }
  
  #remove outliers
  H_minus_pre_C_1 <- remove_outliers(H_minus_pre_C)
  L_minus_pre_C_1 <- remove_outliers(L_minus_pre_C)
  
  # output into pdf
  pdf(file = "High_Low_minus_pre_C.pdf")
  hist(H_minus_pre_C,labels = T, breaks = breaksofhist, main = "High minus previous Close")
  hist(H_minus_pre_C_1,labels = T, breaks = breaksofhist, main = "High minus previous Close w/ no outlier")
  hist(L_minus_pre_C,labels = T, breaks = breaksofhist, main = "Low minus previous Close")
  hist(L_minus_pre_C_1,labels = T, breaks = breaksofhist, main = "Low minus previous Close w/ no outlier")
  dev.off()
}