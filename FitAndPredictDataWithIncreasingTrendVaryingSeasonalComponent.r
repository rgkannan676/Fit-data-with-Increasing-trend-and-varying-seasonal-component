##Below are the code used. Check Readme for detailed analysis.

#R code for reading data and converting it to timeseries object.
#Read data file
drugSalesData <-read.csv("data.txt",colClasses="character",na.strings="?",header=TRUE)
#convert date string to date type
drugSalesData$date <- as.Date(drugSalesData$date,format = "%Y-%m-%d")
#convert value to numeric type
drugSalesData$value <- as.numeric(drugSalesData$value)

#Convert the read data into time series format. Start and end date with frequency=12 is mentioned. 
drugSales_ts <- ts(zoo(drugSalesData$value, order.by=drugSalesData$date),frequency = 12,start = c(1991,07,01),end = c(2008,06,01))
#plot the time plot.
plot(drugSales_ts,ylab="Drug Sales" , xlab="Time in months" ,main="Sales Prediction")

#Below is the R code used to calculate the forecast.
#Fit the data with HW additive variance.
fit1<-hw(drugSales_ts, seasonal="additive" )
#Fit the model with HW multiplicative variance.
fit2<-hw(drugSales_ts, seasonal="multiplicative" )
#Plot the original data 
plot(drugSales_ts,ylab="Drug Sales" , xlab="Time in months" ,main="Sales Prediction",xlim=c(1991,2011),ylim=c(0,40))
plot (fit2 , ylab="Drug Sales",PI=FALSE, type="o", fcol="white", xlab="Time in months",main="Sales Prediction" )
#Plot the fitted points of HW additive variance.
lines(fitted(fit1), col="red" , lty=2)
#Plot the fitted points of HW multiplicative variance.
lines(fitted(fit2), col="green" , lty=2)
#Plotting the foretasted values for HW additive variance.
lines(fit1$mean, type="o", col="red")
#Plotting the foretasted values for HW multiplicative variance.
lines(fit2$mean, type="o", col="green")
#Creating the legend
legend( "topleft" , lty =1, pch=1, col=1:3 ,c ("Drug Sales", "HWADD", "HWMUL" ))
#Model Summary
fit1$model
fit2$model


#R code for BoxCox transform is :
#calculate the lambda of boxcox transform
lambda<- BoxCox.lambda(drugSalesData$value, method = c("guerrero", "loglik"))
#Tranform the data to remove varying seasonality. 
drugSales_transformed=BoxCox(drugSalesData$value,lambda = lambda )
#plot the transformed data
plot.ts(drugSales_transformed,ylab="Drug Sales" , xlab="Time in months" ,main="Box Cox tranformed Sales Prediction")

#ACF of transformed data
acf(drugSales_transformed)


#To make the data stationary differencing need to be done. Let us find the D of the SARIMA model.  
#Since S=12, differencing with a lag of 12 is performed. Following is the R code:

#Take a seasonal diff  with lag = S =12
drugSales_transformed_diff1 <-diff(drugSales_transformed,lag = 12)
#Plot the data after diff
plot.ts(drugSales_transformed_diff1)
#Plot the ACF of the diff data
acf(drugSales_transformed_diff1)
#Plot the PACF of the diff data
pacf(drugSales_transformed_diff1)

#Model A
#Fitting the model, lambda found for BoxCox is passed into the Arima Model 
fita1=Arima(drugSalesData$value, order=c(0,0,5), seasonal=list(order=c(0,1,2),period=12), lambda=lambda)
#diagnosing
tsdiag(fita1)
#get details
fita1

#Model B
#Fitting the model, lambda found for BoxCox is passed into the Arima Model 
fita2=Arima(drugSalesData$value, order=c(10,0,0), seasonal=list(order=c(0,1,2),period=12), lambda=lambda)
#diagnosing
tsdiag(fita2)
#get details
fita2

#Model C
#Fitting the model, lambda found for BoxCox is passed into the Arima Model 
fita3=Arima(drugSalesData$value, order=c(0,0,5), seasonal=list(order=c(2,1,0),period=12), lambda=lambda)
#diagnosing
tsdiag(fita3)
#get details
fita3

#Model D
#Fitting the model, lambda found for BoxCox is passed into the Arima Model
fita4=Arima(drugSalesData$value, order=c(10,0,0), seasonal=list(order=c(2,1,0),period=12), lambda=lambda)
#diagnosing
tsdiag(fita4)
#get details
fita4


#Following is the R Code used for forecasting and plotting.
#Plot the actual data
plot(1:204, drugSalesData$value, xlim = c(0, 250), ylim=c(0, 40), ylab="Drug Sales" , xlab="Time in months" ,main="Sales Prediction")
lines(1:204, drugSalesData$value, type="l" )
#Plot the fitted data of Model B
lines(1:204, drugSalesData$value-fita2$residuals, type="l", col="red")
#Compute the forecast using Model B
forecast = forecast(fita2)
#Plot the the Forecasted values
lines(205:228, forecast$mean, type="o", col="green")
#Add legend
legend( "topleft" , lty =1, pch=1, col=1:3 ,c ("Data", "Fitted", "Forecast" ))