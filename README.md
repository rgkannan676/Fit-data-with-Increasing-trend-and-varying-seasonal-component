# Fit and predict on a data with increasing trend and a varying seasonal component
Time Series Analysis:Fit and predict on a data with increasing trend and a varying seasonal component.

# 1. Data Analysis
Analysing the time plot of the data, we can see that the data is not stationary. It has an increasing trend and a varying seasonal component. The margin of the seasonal component increases with time.

![image](https://user-images.githubusercontent.com/29349268/121108531-7ca96000-c83c-11eb-847e-a827d20b663f.png)

Fig: Above is the data-time plot. We can observe an increasing trend and varying seasonality.

# 2. Holt-Winters’ Trend and Seasonality Method
The data contains both trend and seasonality components. So, we can use the Holt-Winters Trend and Seasonality Method. In this assignment we check both the additive and multiplicative variants of the HW.
The data is read from the data file and converted to time series object.
R code for reading data and converting it to timeseries object.

```
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
```

![image](https://user-images.githubusercontent.com/29349268/121108959-3c96ad00-c83d-11eb-91f0-802d84e868ba.png)


Fig: Above is the forecast using the multiplicative and additive variance of Holt-Winters Trend and Seasonality Method. Red line corresponds to additive and green multiplicative variance.
Below is the R code used to calculate the forecast.

```
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
```

Analysing the two models:

### HW Additive:

![image](https://user-images.githubusercontent.com/29349268/121109669-977cd400-c83e-11eb-849e-d01acefadd9f.png)

### HW Multiplicative:

![image](https://user-images.githubusercontent.com/29349268/121109703-a82d4a00-c83e-11eb-85b6-0d296a55c354.png)

Comparing the AIC values, we can see that HW multiplicative is a better than the HW additive forecast. Therefore, HW multiplicative can be used to predict the values for this data.

# 3. Seasonal ARIMA models
The data contains both trend and seasonality components. So, we can try fit a SARIMA model for the data. The data has a varying seasonal component. This need to be changes to a constant Seasonal variation.
## a.	BoxCox Transform
R code for BoxCox transform is :

```
#calculate the lambda of boxcox transform
lambda<- BoxCox.lambda(drugSalesData$value, method = c("guerrero", "loglik"))
#Tranform the data to remove varying seasonality. 
drugSales_transformed=BoxCox(drugSalesData$value,lambda = lambda )
#plot the transformed data
plot.ts(drugSales_transformed,ylab="Drug Sales" , xlab="Time in months" ,main="Box Cox tranformed Sales Prediction")
```

![image](https://user-images.githubusercontent.com/29349268/121109052-66e86a80-c83d-11eb-9b5a-face16b9971e.png)

Fig: Above is the data plot after BoxCox transformation. We can observe that the time plot shows constant seasonality behaviour.

From the time plot data, we can identify that the seasonal period S= 12.
From the time plot we can observe that the data is not stationary therefore has to take the differencing to make it stationary.
Taking the ACF of transformed data, we can see that the data is not stationary.

```
#ACF of transformed data
acf(drugSales_transformed)
```

![image](https://user-images.githubusercontent.com/29349268/121109069-6c45b500-c83d-11eb-8d6f-15a5d5c1bdcb.png)
 
Fig: Above is the ACF of the data after BoxCox transformation. The data is not stationary.

## b. Calculate D and d
To make the data stationary differencing need to be done. Let us find the D of the SARIMA model.  Since S=12, differencing with a lag of 12 is performed. Following is the R code:

```
#Take a seasonal diff  with lag = S =12
drugSales_transformed_diff1 <-diff(drugSales_transformed,lag = 12)
#Plot the data after diff
plot.ts(drugSales_transformed_diff1)
#Plot the ACF of the diff data
acf(drugSales_transformed_diff1)
#Plot the PACF of the diff data
pacf(drugSales_transformed_diff1)
```

![image](https://user-images.githubusercontent.com/29349268/121109090-7b2c6780-c83d-11eb-89ff-f47136e9c781.png)


Fig: Above is the time plot after taking Seasonal differencing with lag =12. We can observe that both the seasonal and trend component is gone, and the data is stationary.

![image](https://user-images.githubusercontent.com/29349268/121109106-81224880-c83d-11eb-8a64-24174cd07fd0.png)

Fig: ACF of the data after Seasonal differencing with lag =12.

![image](https://user-images.githubusercontent.com/29349268/121109115-867f9300-c83d-11eb-9653-b5a750335c01.png)

Fig: PACF of the data after Seasonal differencing with lag =12.

After the seasonal differencing  with lag =12 the seasonal and trend components are removed, and the data is stationary. D=1 with S=12 and d=0 since no trend component is present after seasonal differencing.

## c. Calculate the P and Q from ACF and PACF.
Checking the ACF plot, since S= 12, observe the values of lags = 12, 24, 36 and can see that the ACF is cut off after lag =24 (lag = 36 is cut off) i.e., 2 times S. Therefore, from ACF we get P= 0 and Q = 2
Checking the PACF  plot, since S= 12, observe the values of lags = 12, 24, 36 and can see that the PACF is cut off after lag =24(lag = 36 is cut off) i.e., 3 times S. Therefore, from PACF we get P= 2 and Q = 0

## d. Calculate the p and q from the ACF and PACF plot.
Checking the ACF plot, for calculating the p and q, we need to check the time lag 1 to S-1, here S =12 therefore 1 to 11. The ACF is cut off after lag=5.  Therefore p=0 and q=5.
Checking the PACF plot, for calculating the p and q, we need to check the time lag 1 to S-1, here S =12 therefore 1 to 11. The PACF is cut off after lag=10.  Therefore p=10 and q=0
Thus, combining all above, we have 4 combinations of P, Q, p and q.  Need to fit and compare these SARIMA models to choose the best one with D=1 and d=0.

## e. Evaluate and Compare Models

### i. Model A :  P=0, Q=2, p=0, q=5

```
#Fitting the model, lambda found for BoxCox is passed into the Arima Model 
fita1=Arima(drugSalesData$value, order=c(0,0,5), seasonal=list(order=c(0,1,2),period=12), lambda=lambda)
#diagnosing
tsdiag(fita1)
#get details
fita1
```

![image](https://user-images.githubusercontent.com/29349268/121109161-9bf4bd00-c83d-11eb-8b6e-a415be714b34.png)
 
Fig: Diagnostic of the 1st Model, p value is low and residual ACF has high values.

![image](https://user-images.githubusercontent.com/29349268/121110275-98facc00-c83f-11eb-8c9c-580672e27411.png)

Here we can observe the p value is low and the residual ACF has high values. Therefore, we can ignore this model.

### ii. Model B :  P=0, Q=2, p=10, q=0

```
#Fitting the model, lambda found for BoxCox is passed into the Arima Model 
fita2=Arima(drugSalesData$value, order=c(10,0,0), seasonal=list(order=c(0,1,2),period=12), lambda=lambda)
#diagnosing
tsdiag(fita2)
#get details
fita2
```

![image](https://user-images.githubusercontent.com/29349268/121109191-a6af5200-c83d-11eb-8817-3b4d8954e700.png)
 
Fig: Diagnostic of the 2nd Model, p value is high and residual ACF cut off.

![image](https://user-images.githubusercontent.com/29349268/121110304-a87a1500-c83f-11eb-9425-538921687c82.png)

Here we can observe the p value is high and the residual ACF cut-off . Therefore, this model can be considered as a good model that fits the data.

### iii. Model C :  P=2, Q=0, p=0, q=5

```
#Fitting the model, lambda found for BoxCox is passed into the Arima Model 
fita3=Arima(drugSalesData$value, order=c(0,0,5), seasonal=list(order=c(2,1,0),period=12), lambda=lambda)
#diagnosing
tsdiag(fita3)
#get details
fita3
```

![image](https://user-images.githubusercontent.com/29349268/121109215-b62e9b00-c83d-11eb-84a2-12629ae5589b.png)
 
Fig: Diagnostic of the 3rd  Model, p value is low and residual ACF has high values.

![image](https://user-images.githubusercontent.com/29349268/121110343-ba5bb800-c83f-11eb-9bfb-247f748a0164.png)

Here we can observe the p value is low and the residual ACF has high values. Therefore, we can ignore this model.

### iv. Model D :  P=2, Q=0, p=10, q=0

```
#Fitting the model, lambda found for BoxCox is passed into the Arima Model
fita4=Arima(drugSalesData$value, order=c(10,0,0), seasonal=list(order=c(2,1,0),period=12), lambda=lambda)
#diagnosing
tsdiag(fita4)
#get details
fita4
```

![image](https://user-images.githubusercontent.com/29349268/121109231-bdee3f80-c83d-11eb-8cce-b1e40eee6af4.png)

Fig: Diagnostic of the 4th Model, p value is high and residual ACF cut off.

![image](https://user-images.githubusercontent.com/29349268/121110391-cb0c2e00-c83f-11eb-80b6-7493aab28288.png)

Here we can observe the p value is high and the residual ACF cut-off . Therefore, this model can be considered as a good model that fits the data.

## f. Selecting Best Model

From the Model diagnostics, the models A and C are not considered since the p values were low and the residual ACF had large values.
Models B and D were good at fitting the values since the p values were large and the residual ACF cut-off. We can compare the AIC values of the two models to select the best one.
Model B AIC=-517.68 and Model D AIC=-513.12
Comparing the AIC, we can see model B has the lowest AIC therefore Model B(P=0, Q=2, p=10, q=0) is taken as the best model.

## g. Forecast using Best Model.

Forecast using the Model B(P=0, Q=2, p=10, q=0).

![image](https://user-images.githubusercontent.com/29349268/121109267-cd6d8880-c83d-11eb-9aa3-976db38ae7b6.png)

Fig: Shows the fitted values and predicted values using the Model B

Following is the R Code used for forecasting and plotting.

```
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
```

