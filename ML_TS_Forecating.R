# source: Using R for time series analysis
# Time-Series Analysis

# Package: TTR (Technical Trading Rules)

# Simple moving average: TTR::SMA(data_ts, n=3)
# Other moving average in TTR:
# SMA Simple moving average.
# EMA Exponential moving average.
# WMA Weighted moving average.
# DEMA Double-exponential moving average.
# EVWMA Elastic, volume-weighted moving average.
# ZLEMA Zero lag exponential moving average.
# VWMA Volume-weighed moving average (same as VWAP).
# VWAP Volume-weighed average price (same as VWMA).
# VWA Variable-length moving average.
# HMA Hull moving average.
# ALMA Arnaud Legoux moving average

# Decomposing Time Series and seasonally adjusting
# step1 decompose the time series data : data_ts_dc <- decompose(data_ts)
# step2 plot all component: plot(decompose(data_ts_dc))
# spep3 check seasonality from the plotting
# step4 if seasonality exist, remove it from the data: data_ts_easonallyAdjusted <- data_ts - data_ts_dc$seasonal 

# A. Forecasts using Exponential Smoothing for short-term forecasting
# Exponential smoothing methods are useful for making forecasts, and make no assumptions about the correlations between successive values of the time series.

#Based on exisitence of the times series components, use different exponential smooting models.

# Models
# when a time series that can be described using an additive model with constant level and no seasonality:
# 1. Simple Exponential Smoothing: HoltWinters(ts_data, beta=FALSE, gamma=FALSE)

# when a time series that can be described using an additive model with increasing or decreasing trend and no seasonality
# 2. Holt¡¯s Exponential Smoothing: HoltWinters(ts_data, gamma=FALSE)

# when a time series that can be described using an additive model with increasing or decreasing trend and seasonality
# 3. Holt-Winters Exponential Smoothing: HoltWinters(ts_data)

# Model Test after build a prediction model
# step1 correlogram of the in-sample forecast errors: acf(ts_data_forecast$residuals, na.action = na.omit, lag.max=20)
# step2 testing non-zero autocorrelations ofthe in-sample forecast errors: Box.test(ts_data_forecast$residuals, lag=20, type="Ljung-Box")

# Plotting
# plot.ts(data or time series forecast object)

# B. Forecasts using ARIMA


### 1. Quick overview
## 1) Time series data and plotting

# Load time series data
# scan(): read data of successive time points in a simple text file with one column
# data example 1
# kings data: the age of death of 42 succesive kings of England
kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat", skip =3)
head(kings)

# ts(): store the data in a time series object
king_ts <- ts(kings)  # start parameter ex) start = c(1986, 2) means the first data point corresopnds to the second quarter of 1986
king_ts  # frequnecy: 1 = year, 4 = quarterly, 12 = monthly

# data example 2
# the number of births per month in New York City from Jan. 1946 to Dec. 1959
births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
head(births)
births_ts <- ts(births, frequency = 12, start = c(1946, 1))
births_ts

# data example 3
# monthly sales for a sourvenir shop at beach resort town in Queensland, Austrailia, for Jan 1987 to Dec. 1993
souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
souvenir_ts <- ts(souvenir, frequency = 12, start = c(1987, 1))
souvenir_ts

# ploting time-series data
# plot.ts()
plot.ts(king_ts) # additive model since the random fluctuations in the data are roughly constant in size over time. 
plot.ts(births_ts) # seasonal variation in the number of births per month. Peak every summer and trough every winter. additive model since the seaonal fluctuations are roughly constant in size over time and do not seem to depend on the level of the time series. The random fluctuations also seem to be roughly constant over time. 
plot.ts(souvenir_ts)  # additive model is not appropriate for describing this time series, since the size of the seasonal fluctuations and random fluctuations seem to increase with the level of the time series. Thus, we may need to transform the ts in order to get a transformed ts that can be described using an additive model. 

# 2) Simple moving average of an additive time series data
if (!require("TTR")) install.packages("TTR")
library(TTR)
plot.ts(king_ts)
king_ts_sma3 <- SMA(x = king_ts, n = 3)  # n = the order (span)
lines(king_ts_sma3, col = "red")
x <- c(1:length(kings))
king_ts_sma8 <- SMA(king_ts, n = 8)
lines(x = x, y = king_ts_sma8, col = "blue")

# conclusion: The age of death of the kings seems to have decreased from about 55 years old to 38 years old during the reign of the first 20 kings, and then increased after that to about 73 years old by the end of the reign of the 40th king in the time series. 


# 3) transform the sourvenir_ts by the natural log
souvenir_ts_log <- log(souvenir_ts)
plot.ts(souvenir_ts)
plot.ts(souvenir_ts_log, col = 'orange')  
# the size of the seasonal fluctuations and random fluctuations in the log-transformed time series seem to be roughly constant over time, and do not depend on the level of the time series. Thus this transformed ts can probably described using an additive model. 

# 4) Decomposing time series
# seperating the ts data into its constituent components, wihch are usually a trend component and an irregular component, if it is a seasonal time series, a sasonal compoent. 

# Decomposing non-seasonal data
# TTR package::SMA() smooth time series data using a simple moving average. 
# a non-seasonal time series consists of a trend component and an irregular component. 

# Decomposing seasonal data
# decompose(): returns a list object as its result, where the estimates of the seasonal component, trend component and irregular component are stored in named elements of that list objects, called ¡°seasonal¡±, ¡°trend¡±, and ¡°random¡± respectively.
# A seasonal time series consists of a trend component, a seasonal component and an irregular component. 

birth_ts_decomposition <- decompose(x = births_ts)
str(birth_ts_decomposition)
birth_ts_decomposition$seasonal
plot.ts(births_ts)
plot(birth_ts_decomposition$seasonal)
birth_ts_decomposition$trend
plot(birth_ts_decomposition$trend)
plot(birth_ts_decomposition)

# 5) Seasonally adjusting
# if a seasonal time series that can be described using a additive model, you can seasonally adjust the time series by subtracting the seasonal component from the original time series.
birth_ts_seasonally_adjusted <- births_ts - birth_ts_decomposition$seasonal
plot(birth_ts_seasonally_adjusted)
# The chart shows that the seasonal trends are disappeared, and only trends and irregular component are left. 

#### 2. Forecast using expotential smoothing
### using HoltWinters()

## 1) Simple exponential smoothing

# If a time series that can be described using an additive model with constant level and no sesonality, you can use simple expotential smooting to make short-term forecasts.
# 0 < alpha <1, alpha is an weight on the observations. Close to 0 means that little weight in placed on the most recent observations. 

#data
# total annual rainfall in inches for London, from 1813-1912
rain <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
rain_ts <- ts(rain, frequency = 1, start = c(1813))
plot(rain_ts)
#You can see from the plot that there is roughly constant level (the mean stays constant at about 25 inches). The random fluctuations in the time series seem to be roughly constant in size over time, so it is probably appropriate to describe the data using an additive model.
# Therefore, we can apply the additive model using the simple exponential smoothing.
rain_ts_forecast <- HoltWinters(rain_ts, beta = FALSE, gamma = FALSE)
rain_ts_forecast
str(rain_ts_forecast)
rain_ts_forecast$fitted
plot(rain_ts_forecast)
# accuracy: sum of squared errors (SSE)
rain_ts_forecast$SSE
# It is common in simple exponential smoothing to use the first value in the time series as the initial value for the level. the first value in the data = 23.56
rain_ts_forecast_levelSet <- HoltWinters(rain_ts, beta = FALSE, gamma = FALSE, l.start = 23.56)
plot(rain_ts_forecast_levelSet)
rain_ts_forecast_levelSet$fitted
# forecast for further time point: forecast()
# the forecast() create residuals for in-sample error
if (!require("forecast")) install.packages("forecast")
rain_ts_forecast2 <- forecast(rain_ts_forecast, h = 10)
rain_ts_forecast2
plot(rain_ts_forecast2)

# autocorrelation function (acf): acf() 
# autocorrelation: autocorrelation is a characteristic of data in which the correlation between the values of the same variables is based on related objects.  It violates the assumption of instance independence, which underlies most of the conventional models. In time series data, autocorrelation is defined as the delayed correlation of a given series. Autocorrelation is a delayed correlation by itself, and is delayed by some specific number of time units.  On the other hand, serial autocorrelation is that type which defines the lag correlation between the two series in time series data. There is a very popular test called the Durbin Watson test that detects the presence of autocorrelation. 
# Forecast errors for the time period of the original time series data (in-sample error): residuals
# If there are correlations between forecast errors for successive predictions, it is likely that the simple smoothing forecastws could be improved upon by another forecasting technique. 
rain_ts_forecast2$residuals
plot(rain_ts_forecast2$residuals)
# check a correlogram of the forecast errors for lags 1-20 using the acf().
acf(rain_ts, na.action = na.omit, lag.max = 20) # the result is similar to the acf of the residuals
acf(rain_ts_forecast2$residuals, na.action = na.omit, lag.max = 20)
#You can see from the sample correlogram that the autocorrelation at lag 3 is just touching the significance bounds.

# portmanteau test (null hypothesis of independence in a given time series): Box.test()
# To test whether there is significant evidence for non-zero correlations at lags 1-20, we can carry out a Ljung-Box test. 
Box.test(rain_ts_forecast2$residuals, lag = 20, type = "Ljung-Box")
# Here the Ljung-Box test statistic is 17.4, and the p-value is 0.6, so there is little evidence of non-zero autocorrelations in the in-sample forecast errors at lags 1-20.
# To be sure that the predictive model cannot be improved upon, it is also a good idea to check whether the forecast errors are normally distributed with mean zero and constant variance. To check whether the forecast errors have constant variance, we can make a time plot of the in-sample forecast errors:
plot.ts(rain_ts_forecast2$residuals)
# The plot shows that the in-sample forecast errors seem to have roughly constant variance over time, although the size of the fluctuations in the start of the time series (1820-1830) may be slightly less than that at later dates (eg. 1840-1850).
# To check whether the forecast errors are normally distributed with mean zero, we can plot a histogram of the forecast errors, with an overlaid normal curve that has mean zero and the same standard deviation as the distribution of forecast errors. To do this, we can define an R function ¡°plotForecastErrors()¡±, below:

plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}
plotForecastErrors(rain_ts_forecast2$residuals[-1])

# Conclusion
#The plot shows that the distribution of forecast errors is roughly centred on zero, and is more or less normally distributed, although it seems to be slightly skewed to the right compared to a normal curve. However, the right skew is relatively small, and so it is plausible that the forecast errors are normally distributed with mean zero.
#The Ljung-Box test showed that there is little evidence of non-zero autocorrelations in the in-sample forecast errors, and the distribution of forecast errors seems to be normally distributed with mean zero. This suggests that the simple exponential smoothing method provides an adequate predictive model for London rainfall, which probably cannot be improved upon. Furthermore, the assumptions that the 80% and 95% predictions intervals were based upon (that there are no autocorrelations in the forecast errors, and the forecast errors are normally distributed with mean zero and constant variance) are probably valid.


## 2) Holt¡¯s Exponential Smoothing
# If you have a time series that can be described using an additive model with increasing or decreasing trend and no seasonality, you can use Holt¡¯s exponential smoothing to make short-term forecasts.
# Holt¡¯s exponential smoothing estimates the level and slope at the current time point. Smoothing is controlled by two parameters, alpha, for the estimate of the level at the current time point, and beta for the estimate of the slope b of the trend component at the current time point. As with simple exponential smoothing, the paramters alpha and beta have values between 0 and 1, and values that are close to 0 mean that little weight is placed on the most recent observations when making forecasts of future values.

# An example of a time series that can probably be described using an additive model with a trend and no seasonality is the time series of the annual diameter of women¡¯s skirts at the hem, from 1866 to 1911. 
skirts <- scan("http://robjhyndman.com/tsdldata/roberts/skirts.dat",skip=5)
skirtsseries <- ts(skirts, start=c(1866))
plot.ts(skirtsseries)
# We can see from the plot that there was an increase in hem diameter from about 600 in 1866 to about 1050 in 1880, and that afterwards the hem diameter decreased to about 520 in 1911.
# To make forecasts, we can fit a predictive model using the HoltWinters() function in R. To use HoltWinters() for Holt¡¯s exponential smoothing, we need to set the parameter gamma=FALSE (the gamma parameter is used for Holt-Winters exponential smoothing, as described below). Gamma set to False because there is no seasonality. 
skirtsseriesforecasts <- HoltWinters(skirtsseries, gamma=FALSE)
skirtsseriesforecasts
skirtsseriesforecasts$SSE
# The estimated value of alpha is 0.84, and of beta is 1.00. These are both high, telling us that both the estimate of the current value of the level, and of the slope b of the trend component, are based mostly upon very recent observations in the time series. This makes good intuitive sense, since the level and the slope of the time series both change quite a lot over time. The value of the sum-of-squared-errors for the in-sample forecast errors is 16954.
plot(skirtsseriesforecasts)
# We can see from the picture that the in-sample forecasts agree pretty well with the observed values, although they tend to lag behind the observed values a little bit.
# if you wish, you can specify the initial values of the level and the slope b of the trend component by using the ¡°l.start¡± and ¡°b.start¡± arguments for the HoltWinters() function. It is common to set the initial value of the level to the first value in the time series (608 for the skirts data), and the initial value of the slope to the second value minus the first value (9 for the skirts data). For example, to fit a predictive model to the skirt hem data using Holt¡¯s exponential smoothing, with initial values of 608 for the level and 9 for the slope b of the trend component, we type:
skirtsseriesforecasts2 <- HoltWinters(skirtsseries, gamma=FALSE, l.start=608, b.start=9)
plot(skirtsseriesforecasts2)
# Foreast
skirtsseriesforecasts3 <- forecast(skirtsseriesforecasts, h=19)
plot(skirtsseriesforecasts3)
# The forecasts are shown as a blue line, with the 80% prediction intervals as an dark gray shaded area, and the 95% prediction intervals as a gray shaded area.

# As for simple exponential smoothing, we can check whether the predictive model could be improved upon by checking whether the in-sample forecast errors show non-zero autocorrelations at lags 1-20. For example, for the skirt hem data, we can make a correlogram, and carry out the Ljung-Box test, by typing:
acf(skirtsseriesforecasts3$residuals, na.action = na.omit, lag.max=20)
Box.test(skirtsseriesforecasts3$residuals, lag=20, type="Ljung-Box")
# Here the correlogram shows that the sample autocorrelation for the in-sample forecast errors at lag 5 exceeds the significance bounds. However, we would expect one in 20 of the autocorrelations for the first twenty lags to exceed the 95% significance bounds by chance alone. Indeed, when we carry out the Ljung-Box test, the p-value is 0.47, indicating that there is little evidence of non-zero autocorrelations in the in-sample forecast errors at lags 1-20.

# As for simple exponential smoothing, we should also check that the forecast errors have constant variance over time, and are normally distributed with mean zero. We can do this by making a time plot of forecast errors, and a histogram of the distribution of forecast errors with an overlaid normal curve:
plot.ts(skirtsseriesforecasts3$residuals)
# The time plot of forecast errors shows that the forecast errors have roughly constant variance over time.

# conclusion:
# Thus, the Ljung-Box test shows that there is little evidence of autocorrelations in the forecast errors, while the time plot and histogram of forecast errors show that it is plausible that the forecast errors are normally distributed with mean zero and constant variance. Therefore, we can conclude that Holt¡¯s exponential smoothing provides an adequate predictive model for skirt hem diameters, which probably cannot be improved upon. In addition, it means that the assumptions that the 80% and 95% predictions intervals were based upon are probably valid.


## 3) Holt-Winters Exponential Smoothing

# If you have a time series that can be described using an additive model with increasing or decreasing trend and seasonality, you can use Holt-Winters exponential smoothing to make short-term forecasts. 
# Smoothing is controlled by three parameters: alpha - the estimates of the level, beta - slope b of the trend component, and gamma - the seasonal component, respectively, at the current time point. The parameters alpha, beta and gamma all have values between 0 and 1, and values that are close to 0 mean that relatively little weight is placed on the most recent observations when making forecasts of future values.
# HoltWinters(x, alpha = NULL, beta = NULL, gamma = NULL,
#           seasonal = c("additive", "multiplicative"),
#           start.periods = 2, l.start = NULL, b.start = NULL,
#           s.start = NULL,
#           optim.start = c(alpha = 0.3, beta = 0.1, gamma = 0.1),
#           optim.control = list())

souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
souvenir_ts <- ts(souvenir, frequency = 12, start = c(1987, 1))
souvenir_ts
plot(souvenir_ts)
logsouvenirtimeseries <- log(souvenir_ts) # log transfrom to make the size of the seasonal fluctuations and random fluctuations in the log-transformed time series be roughly constant over time, and do not depend on the level of the time series.
plot(logsouvenirtimeseries)
# Holt-Winters exponential smoothing with trend and additive seasonal component.
souvenirtimeseriesforecasts <- HoltWinters(logsouvenirtimeseries)
souvenirtimeseriesforecasts
souvenirtimeseriesforecasts$SSE
# Interpretation of the result:
# The estimated values of alpha, beta and gamma are 0.41, 0.00, and 0.96, respectively. The value of alpha (0.41) is relatively low, indicating that the estimate of the level at the current time point is based upon both recent observations and some observations in the more distant past. The value of beta is 0.00, indicating that the estimate of the slope b of the trend component is not updated over the time series, and instead is set equal to its initial value. This makes good intuitive sense, as the level changes quite a bit over the time series, but the slope b of the trend component remains roughly the same. In contrast, the value of gamma (0.96) is high, indicating that the estimate of the seasonal component at the current time point is just based upon very recent observations.

plot(souvenirtimeseriesforecasts)
# We see from the plot that the Holt-Winters exponential method is very successful in predicting the seasonal peaks, which occur roughly in November every year.

# To make forecasts for future times not included in the original time series, we use the ¡°forecast.HoltWinters()¡± function in the ¡°forecast¡± package. For example, the original data for the souvenir sales is from January 1987 to December 1993. If we wanted to make forecasts for January 1994 to December 1998 (48 more months), and plot the forecasts, we would type:
souvenirtimeseriesforecasts2 <- forecast(souvenirtimeseriesforecasts, h=48)
plot(souvenirtimeseriesforecasts2)
# The forecasts are shown as a blue line, and the gredy shaded areas show 80% and 95% prediction intervals, respectively.

# We can investigate whether the predictive model can be improved upon by checking whether the in-sample forecast errors show non-zero autocorrelations at lags 1-20, by making a correlogram and carrying out the Ljung-Box test:
acf(souvenirtimeseriesforecasts2$residuals, na.action = na.omit, lag.max=20)
Box.test(souvenirtimeseriesforecasts2$residuals, lag=20, type="Ljung-Box")
# The correlogram shows that the autocorrelations for the in-sample forecast errors do not exceed the significance bounds for lags 1-20. Furthermore, the p-value for Ljung-Box test is 0.6, indicating that there is little evidence of non-zero autocorrelations at lags 1-20.

# We can check whether the forecast errors have constant variance over time, and are normally distributed with mean zero, by making a time plot of the forecast errors and a histogram (with overlaid normal curve):
plot.ts(souvenirtimeseriesforecasts2$residuals)  
# From the time plot, it appears plausible that the forecast errors have constant variance over time.

# Conclusion:
# Thus,there is little evidence of autocorrelation at lags 1-20 for the forecast errors, and the forecast errors appear to be normally distributed with mean zero and constant variance over time. This suggests that Holt-Winters exponential smoothing provides an adequate predictive model of the log of sales at the souvenir shop, which probably cannot be improved upon. Furthermore, the assumptions upon which the prediction intervals were based are probably valid.



#### 3. Forecast using ARIMA

# Exponential smoothing methods are useful for making forecasts, and make no assumptions about the correlations between successive values of the time series. However, if you want to make prediction intervals for forecasts made using exponential smoothing methods, the prediction intervals require that the forecast errors are uncorrelated and are normally distributed with mean zero and constant variance.
# While exponential smoothing methods do not make any assumptions about correlations between successive values of the time series, in some cases you can make a better predictive model by taking correlations in the data into account. 
# Autoregressive Integrated Moving Average (ARIMA) models include an explicit statistical model for the irregular component of a time series, that allows for non-zero autocorrelations in the irregular component.

