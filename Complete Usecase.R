library(forecast)
library(fpp)
Portfolio = list()
BestPrediction = vector()
Portfolio[[1]]=read.csv("http://ichart.finance.yahoo.com/table.csv?s=A&a=00&b=1&c=2000&d=03&e=1&f=2017&g=m&ignore=.csv", stringsAsFactors = FALSE) 
Portfolio[[2]]=read.csv("http://ichart.finance.yahoo.com/table.csv?s=F&a=00&b=1&c=2000&d=03&e=1&f=2017&g=m&ignore=.csv", stringsAsFactors = FALSE) 
Portfolio[[3]]=read.csv("http://ichart.finance.yahoo.com/table.csv?s=ABC&a=00&b=1&c=2000&d=03&e=1&f=2017&g=m&ignore=.csv", stringsAsFactors = FALSE) 
Portfolio[[4]]=read.csv("http://ichart.finance.yahoo.com/table.csv?s=INTC&a=00&b=1&c=2000&d=03&e=1&f=2017&g=m&ignore=.csv", stringsAsFactors = FALSE) 
Portfolio[[5]]=read.csv("http://ichart.finance.yahoo.com/table.csv?s=KO&a=00&b=1&c=2000&d=03&e=1&f=2017&g=m&ignore=.csv", stringsAsFactors = FALSE) 
Portfolio[[6]]=read.csv("http://ichart.finance.yahoo.com/table.csv?s=CSCO&a=00&b=1&c=2000&d=03&e=1&f=2017&g=m&ignore=.csv", stringsAsFactors = FALSE) 
Portfolio[[7]]=read.csv("http://ichart.finance.yahoo.com/table.csv?s=XOM&a=00&b=1&c=2000&d=03&e=1&f=2017&g=m&ignore=.csv", stringsAsFactors = FALSE) 
Portfolio[[8]]=read.csv("http://ichart.finance.yahoo.com/table.csv?s=COF&a=00&b=1&c=2000&d=03&e=1&f=2017&g=m&ignore=.csv", stringsAsFactors = FALSE) 
Portfolio[[9]]=read.csv("http://ichart.finance.yahoo.com/table.csv?s=HPQ&a=00&b=1&c=2000&d=03&e=1&f=2017&g=m&ignore=.csv", stringsAsFactors = FALSE) 
Portfolio[[10]]=read.csv("http://ichart.finance.yahoo.com/table.csv?s=AMZN&a=00&b=1&c=2000&d=03&e=1&f=2017&g=m&ignore=.csv", stringsAsFactors = FALSE) 

x <- c("fitr","NETfitr","HWStockr","HWStockr_ng","autofit2","fit12","fit2","stlStock1","stlStock2","stlStockr","NETfit2","HWStock2","HWStock2_ng","autofit1","fitlr","fitl1","fitl2","HWStock1_ng","NETfit1","HWStock1","HWStock1","HWStock1")
y <- 1:22

models <- data.frame(y,x)

findBestPrediction <- function(Stock)
{
  # Convert into timeseries object
  tsStock=ts(rev(Stock$Close),start = c(2000,1),frequency = 12)
  
  # Create Train and Test sets of the input stocks
  train <- window(tsStock, end=2013)
  test <- window(tsStock, start=2014)
  
  # Mean Absolute Errors of the 25 predictions are stored here
  mae = matrix(NA, 25, length(test)+1)
  
  #Generalize function as polynomial tzrend  (TREND="tsStocktrend1)
  tl = seq(2000,2013,length=length(tsStock))
  tl2 = tl^7
  polyStock = lm(tsStock ~ tl + tl2)
  tsStocktrend1=ts(polyStock$fit,start=c(2000, 1),frequency=12)
  #plot(tsStock,lw=2,col="blue",xlim=c(2000,2013))
  #lines(tsStocktrend1,lw=2,col="red")
  #abline(v=2013.25,lty=3)
  
  #Decompose a timeseries into seasonal, trend and irregular components 
  #Second generalised trend function (tsStocktrend2)
  stlStock = stl(tsStock,s.window="periodic")
  #plot(stlStock,col="blue",lwd=2)
  tsStocktrend2 = stlStock$time.series[,2]
  #plot(forecast(stlStock))
  #abline(v=2013.25,lty=3)
  
  # plot(tsStock,lwd=3)
  # lines(tsStocktrend1,col="purple",lwd=2)
  # lines(tsStocktrend2,col="red",lwd=2)
  # abline(v=2013.25,lty=3)
  # legend("topright",legend=c("Actual","polynomial trend","STL Trend"),col=c("black","purple","red"),lwd=2)
  
  #Start Predicting
  # Based on Polynomial function
  HWStock1_ng = HoltWinters(tsStocktrend1,gamma=FALSE)
  HWStock1 = HoltWinters(tsStocktrend1)
  NETfit1 <- nnetar(tsStocktrend1)
  autofit1 = auto.arima(tsStocktrend1)
  fit12 <- Arima(tsStocktrend1, order=c(1,0,0), list(order=c(2,1,0), period=12), method="CSS")
  fitl1 <- tslm(tsStocktrend1 ~ trend + season, lambda=0)
  stlStock1 = stl(tsStocktrend1,s.window="periodic")
  
  # plot(forecast(autofit1,h=24),xlim=c(2000,2015.2),ylim=c(-50,100),lwd=2,col="red",xlab="Time",ylab="Stock price",main="predictions of polinomial trend")
  # lines(forecast(stlStock1,h=24)$mean,col="red",lw=2)
  # lines(tsStock,lw=3)
  # lines(forecast(fitl1,h=24)$mean,col="orange")
  # lines(forecast(NETfit1,h=24)$mean,lw=3,lty="longdash",col="brown")
  # lines(predict(HWStock1_ng,n.ahead=24),lw=2,col="green")
  # lines(forecast(fit12,h=24)$mean,lw=2,col="purple")
  # lines(predict(HWStock1,n.ahead = 24,prediction.interval = T,level=0.95)[,1],lw=2,col="green")
  # lines(predict(HWStock1,n.ahead = 24,prediction.interval = T,level=0.95)[,2],col="green")
  # lines(predict(HWStock1,n.ahead = 24,prediction.interval = T,level=0.95)[,3],col="green")
  # legend("bottomleft",legend=c("Actual function","polynomial function","Prediction-Holt winters","Prediction-Arima(auto)","Prediction-Arima(Fixed)","Prediction-Neuralnet","Prediction-Linear model"),col=c("black","red","green","blue","purple","brown","orange"),lw=2)
  # abline(v=2013.25,lty=3)
  # 
  #Based on STL Function
  HWStock2_ng = HoltWinters(tsStocktrend2,gamma=FALSE)
  HWStock2 = HoltWinters(tsStocktrend2)
  NETfit2 <- nnetar(tsStocktrend2)
  autofit2 = auto.arima(tsStocktrend2)
  fit2 <- Arima(tsStocktrend2, order=c(15,3,3),method="CSS")
  fitl2 <- tslm(tsStocktrend2 ~ trend + season, lambda=0)
  #fit22=arima(tsStocktrend2,order = c(1,0,0),list(order=c(2,1,0),period=12))
  stlStock2 = stl(tsStocktrend2,s.window="periodic")
  
  # plot(forecast(autofit2,h=24),xlim=c(2000,2015.2),ylim=c(-50,100),lwd=2,col="blue",xlab="Time",ylab="Stock price",main="predictions of STL trend")
  # lines(tsStock,lw=3)
  # lines(forecast(stlStock2,h=24)$mean,col="red",lw=2)
  # lines(forecast(fitl2,h=24)$mean,col="orange")
  # lines(forecast(fit2,h=24)$mean,lw=2,col="purple")
  # #lines(forecast(fit22,h=24)$mean,lw=2,col="purple")
  # lines(tsStocktrend2,lw=2,col="red")
  # lines(forecast(NETfit2,h=24)$mean,lw=3,lty="longdash",col="brown")
  # lines(predict(HWStock2,n.ahead = 24),lw=2,col="green")
  # lines(predict(HWStock2_ng,n.ahead=24),lw=2,col="green")
  # 
  # lines(predict(HWStock2,n.ahead = 24,prediction.interval = T,level=0.95)[,2],col="orange")
  # lines(predict(HWStock2,n.ahead = 24,prediction.interval = T,level=0.95)[,3],col="orange")
  # legend("bottomleft",legend=c("Actual function","STL function","Prediction-Holt winters","Prediction-Arima(auto)","Prediction-Arima(Fixed)","Prediction-Neuralnet","Prediction-Linear model"),col=c("black","red","green","blue","purple","brown","orange"),lw=2)
  # abline(v=2013.25,lty=3)
  
  #Based on actual function
  HWStockr_ng=HoltWinters(tsStock,gamma = FALSE)
  HWStockr=HoltWinters(tsStock)
  NETfitr=nnetar(tsStock)
  autofitr=auto.arima(tsStock)
  fitr=arima(tsStock,order=c(15,3,3), method = "CSS")
  #fitr2=arima(tsStock,order=c(1,0,0),list(order=c(2,1,0),period=12))
  fitlr=tslm(tsStock~trend+season,lambda = 0)
  stlStockr=stl(tsStock,s.window = "periodic")
  
  # plot(forecast(autofitr,h=24),xlim=c(2000,2015.2),ylim=c(-50,100),lw=2,col="blue",xlab="Time",ylab="stock price",main="predictions of actual model")
  # lines(forecast(fitlr, h=24)$mean, col="orange")
  # lines(forecast(stlStockr, h=24)$mean, col="red", lw=2)
  # lines(forecast(fitr,h=24)$mean,lw=2,col="purple")
  # lines(forecast(fitr2,h=24)$mean,lw=2,col="purple")
  # lines(tsStock,lw=3)
  # lines(forecast(NETfitr,h=24)$mean,lw=3,lty="longdash",col="brown")
  # lines(predict(HWStockr,n.ahead=24),lw=2,col="green")
  # lines(predict(HWStockr_ng,n.ahead=24),lw=2,col="green")
  # abline(v=2013.25,lty=3)
  # legend("bottomleft",legend=c("Actual function","Prediction-Holt winters","Prediction-Arima(auto)","Prediction-Arima(Fixed)","Prediction-Neuralnet","Prediction-Linear model"),col=c("black","green","blue","purple","brown","orange"),lw=2)
  
  predfitr =  window(forecast(fitr,h=39)$mean, start=2014)
  # cat("4")
  #predfitr2  =   window(forecast(fitr2,h=39)$mean, start=2014)
  # cat("5")
  predNETfitr =  window(forecast(NETfitr,h=39)$mean, start=2014)
  # cat("6")
  predHWStockr =  window(predict(HWStockr,n.ahead=39), start=2014)
  # cat("7")
  predHWStockr_ng =  window(predict(HWStockr_ng,n.ahead=39), start=2014)
  # cat("8")
  predautofit2 =   window(forecast(autofit2,h=39)$mean, start=2014)
  # cat("9")
  predfit12 =  window(forecast(fit12,h=39)$mean, start=2014)
  # cat("10")
  predfit2 = window(forecast(fit2,h=39)$mean, start=2014)
  # cat("11")
  #predfit22 =  window(forecast(fit22,h=39)$mean, start=2014) 
  # cat("12")
  predstlStock1 = window( forecast(stlStock1, h=39)$mean, start=2014)
  # cat("13")
  predstlStock2 =  window(forecast(stlStock2, h=39)$mean, start=2014)
  # cat("14")
  predstlStockr =  window(forecast(stlStockr, h=39)$mean, start=2013)
  # cat("15")
  predNETfit2 =  window(forecast(NETfit2,h=39)$mean, start=2014)
  predHWStock2 =  window(predict(HWStock2,n.ahead=39), start=2014)
  predHWStock2_ng =  window(predict(HWStock2_ng,n.ahead=39), start=2014)
  predautofit1 =  window(forecast(autofit1,h=39)$mean, start=2014)
  # cat("after autofit")
  predfitlr = window(forecast(fitlr, h=39)$mean , start=2014)
  predfitl1 =   window(forecast(fitl1, h=39)$mean, start=2014)
  predfitl2 = window(forecast(fitl2, h=39)$mean , start=2014)
  predNETfit1 =  window(forecast(NETfit1,h=39)$mean, start=2014)
  predHWStock1_ng =  window(predict(HWStock1_ng,n.ahead=39), start=2014)
  predHWStock11 =  window(predict(HWStock1, n.ahead = 39, prediction.interval = T, level = 0.95)[,1], start=2014)
  predHWStock12 =  window(predict(HWStock1, n.ahead = 39, prediction.interval = T, level = 0.95)[,2], start=2014)
  predHWStock13 =  window(predict(HWStock1, n.ahead = 39, prediction.interval = T, level = 0.95)[,3], start=2014)
  
  # Calculate MAE
  for(i in 1:length(test))
  {
    mae[1,i] <-abs(predfitr[i]-test[i]) 
    mae[2,i] <- abs(predNETfitr[i]-test[i]) 
    mae[3,i] <- abs(predHWStockr[i]-test[i]) 
    mae[4,i] <- abs(predHWStockr_ng[i]-test[i]) 
    mae[5,i] <- abs(predautofit2[i]-test[i]) 
    mae[6,i] <- abs(predfit12[i]-test[i]) 
    mae[7,i] <-abs(predfit2[i]-test[i]) 
    mae[8,i] <- abs(predstlStock1[i]-test[i]) 
    mae[9,i] <- abs(predstlStock2[i]-test[i]) 
    mae[10,i] <- abs(predstlStockr[i]-test[i]) 
    mae[11,i] <- abs(predNETfit2[i]-test[i]) 
    mae[12,i] <- abs(predHWStock2[i]-test[i]) 
    mae[13,i] <- abs(predHWStock2_ng[i]-test[i]) 
    mae[14,i] <- abs(predautofit1[i]-test[i])
    mae[15,i] <- abs(predfitlr[i]-test[i]) 
    mae[16,i] <-  abs(predfitl1[i]-test[i]) 
    mae[17,i] <- abs(predfitl2[i]-test[i]) 
    mae[18,i] <- abs(predHWStock1_ng[i]-test[i] ) 
    mae[19,i] <- abs(predNETfit1[i]-test[i]) 
    mae[20,i] <- abs(predHWStock11[i]-test[i]) 
    mae[21,i] <- abs(predHWStock12[i]-test[i]) 
    mae[22,i] <- abs(predHWStock13[i]-test[i]) 
  }
  
  # Sum all Errors
  for(i in 1:22)
  {
    mae[i,36] = mean(mae[i,1:35])
  }
  # Find best Prediction
  best = which.min(mae[1:22,36])
  cat(" - winning model ID:", best)
  for(i in 1:22)
  {
    if (best == models[i,1])
    {
      fact = forecast(eval(as.name(paste(models[i,2]))))
      plot(fact,axes=FALSE)
      axis(side=1,at=seq(2000,2019,by=1))
      axis(side=2)
      box()
    }else{} 
  }
  return (best)
}

for (i in 1:length(Portfolio)) 
{
  BestPrediction[i] = findBestPrediction(Portfolio[[i]])
}
