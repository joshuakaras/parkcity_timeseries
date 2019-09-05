#Joshua KAras
#9/5/2019

##Packages
library(readr)
library(dplyr)
library(forecast)
library(smooth)
library(tseries)
library(ggplot2)
library(TTR)



#### Data Cleaning ####
data <- read.csv("AirPassengers.csv")
data <- ts(data$X.Passengers, start = c(1949, 1), end = c(1960, 12), frequency = 12)

data1 <- window(data, start = c(1949, 1), end = c(1958, 12))
data2 <- window(data, start = c(1959, 1), end = c(1960, 12))
####



#### My Labels and Themes ####
my_labels <- list(xlab("Year"), ylab("Air Passengers"))
my_theme <- theme(axis.title = element_text(size = 16), plot.title = element_text(size = 30))
####  



#### Plot of Time Series ####
png("time_series.png", width = 1600, height = 900)
autoplot(data, lwd = 0.75) + 
  ggtitle("Monthly Air Passengers") + 
  my_labels + my_theme
dev.off()
####



#### Linear Model ####
linear <- tslm(data1 ~ trend)
summary(linear)

png("linear_plot.png", width = 1600, height = 900)
autoplot(data1, lwd = 1, alpha = 0.4) + 
  ggtitle("Linear Regression of Air Passengers") + 
  my_labels + my_theme +
  autolayer(linear$fitted.values, lwd = 0.75, color = "red")
dev.off()
# More or less captures trend, but not seasonality

lin.fcast <- forecast(linear, h = 24)

png("linear_forecast.png", width = 1600, height = 900)
autoplot(data1, lwd = 1, alpha = 0.4) + 
  ggtitle("Linear Forecast of Air Passengers") + 
  my_labels + my_theme +
  autolayer(lin.fcast, lwd = 0.75, color = "red", "Forecast") + 
  autolayer(data2, lwd = 1, color = "black", series = "Data")
dev.off()
# Doesn't capture the seasonality in the forecast

accuracy(lin.fcast, data2)
####



#### Exponential Smoothing Model ####
ets <- ets(data1)
summary(ets)
# Triple exponential smoothing (alpha, beta, and gamma)

png("ets_plot.png", width = 1600, height = 900)
autoplot(data1, lwd = 1, alpha = 0.4) + 
  ggtitle("Exponential Smoothing of Air Passengers") + 
  my_labels + my_theme +
  autolayer(ets$fitted, lwd = 0.75, color = "red")
dev.off()
# Capture both seasonality and trend

ets.fcast <- forecast(ets, h = 24)

png("ets_forecast.png", width = 1600, height = 900)
autoplot(data1, lwd = 1, alpha = 0.4) + 
  ggtitle("Exponential Smoothing Forecast of Air Passengers") + 
  my_labels + my_theme +
  autolayer(ets.fcast, lwd = 0.75, color = "red", "Forecast") + 
  autolayer(data2, lwd = 1, color = "black", series = "Data")
dev.off()
# 

accuracy(ets.fcast, data2)
####



#### STL Decomposition Model ####
stl <- stl(data1, s.window = "periodic")
summary(stl)
# Seasonal and trend decomposition using Loess (STL)

png("stl_plot.png", width = 1600, height = 900)
autoplot(stl, lwd = 0.75) + 
  ggtitle("STL Decomposition of Air Passengers") +
  my_theme
dev.off()
#

stl.fcast <- forecast(stl, h = 24)

png("stl_forecast.png", width = 1600, height = 900)
autoplot(data1, lwd = 1, alpha = 0.4) + 
  ggtitle("STL Forecast of Air Passengers") + 
  my_labels + my_theme +
  autolayer(stl.fcast, lwd = 0.75, color = "red", "Forecast") + 
  autolayer(data2, lwd = 1, color = "black", series = "Data")
dev.off()
# Similar to ETS but underperforms

accuracy(stl.fcast, data2)
####



#### ARIMA Model ####
# Data is non-stationary, differencing required (auto arima does that though!)
arima <- auto.arima(data1)
summary(arima)
# Autoregressive with non-seasonal differencing and seasonal differencing
# (1,1,0)x(0,1,0)

png("arima_plot.png", width = 1600, height = 900)
autoplot(data1, lwd = 1, alpha = 0.4) + 
  ggtitle("ARIMA(1,1,0)x(0,1,0) Model of Air Passengers") + 
  my_labels + my_theme +
  autolayer(arima$fitted, lwd = 0.75, color = "red")
dev.off()
# Captures Seasonality and trend

arima.fcast <- forecast(arima, h = 24)

png("arima_forecast.png", width = 1600, height = 900)
autoplot(data1, lwd = 1, alpha = 0.4) + 
  ggtitle("ARIMA(1,1,0)x(0,1,0) Forecast of Air Passengers") + 
  my_labels + my_theme +
  autolayer(arima.fcast, lwd = 0.75, color = "red", "Forecast") + 
  autolayer(data2, lwd = 1, color = "black", series = "Data")
dev.off()
#

accuracy(arima.fcast, data2)
####



#### ARIMA Model of Log Data ####
# Seasonal ARIMA is inherently additive, but what if we want to capture a multiplicative seasonal pattern
log1 <- log(data1)
arimalog <- auto.arima(log1)
summary(arimalog)
# Moving average MA(1) with non-seasonal differencing and seasonal moving average SMA(1) with seasonal differencing
# (0,1,1)x(0,1,1)

png("arimalog_plot.png", width = 1600, height = 900)
autoplot(log1, lwd = 1, alpha = 0.4) + 
  ggtitle("ARIMA(0,1,1)x(0,1,1) Model of Log Air Passengers") + 
  xlab("Year") + 
  ylab("Log Air Passengers") + 
  my_theme +
  autolayer(arimalog$fitted, lwd = 0.75, color = "red")
dev.off()
# Looks good! 

arimalog.fcast <- forecast(arimalog, h = 24)
log2 <- log(data2)

png("arimalog_forecast.png", width = 1600, height = 900)
autoplot(log1, lwd = 1, alpha = 0.4) + 
  ggtitle("ARIMA(0,1,1)x(0,1,1) Forecast of Log Air Passengers") + 
  xlab("Year") + 
  ylab("Log Air Passengers") + 
  my_theme +
  autolayer(arimalog.fcast, lwd = 0.75, color = "red", "Forecast") + 
  autolayer(log2, lwd = 1, color = "black", series = "Data")
dev.off()

accuracy(arimalog.fcast, log2)
####



#### ETS Model of Log Data ####
# What if we want to capture a multiplicative seasonal pattern instead of additive
etslog <- ets(log1)
summary(etslog)
# Single smooth (very small gamma and beta)

png("etslog_plot.png", width = 1600, height = 900)
autoplot(log1, lwd = 1, alpha = 0.4) + 
  ggtitle("Exponential Smoothing Model of Log Air Passengers") + 
  xlab("Year") + 
  ylab("Log Air Passengers") + 
  my_theme +
  autolayer(etslog$fitted, lwd = 0.75, color = "red")
dev.off()
# Looks good! 

etslog.fcast <- forecast(etslog, h = 24)

png("etslog_forecast.png", width = 1600, height = 900)
autoplot(log1, lwd = 1, alpha = 0.4) + 
  ggtitle("Exponential Smoothing Forecast of Log Air Passengers") + 
  xlab("Year") + 
  ylab("Log Air Passengers") + 
  my_theme +
  autolayer(etslog.fcast, lwd = 0.75, color = "red", "Forecast") + 
  autolayer(log2, lwd = 1, color = "black", series = "Data")
dev.off()

accuracy(etslog.fcast, log2)
####



#### Accuracy of Each Method ####
tribble(~type, ~theil_u,
       #--/--
       "linear", accuracy(lin.fcast, data2)[2,8],
       "ets", accuracy(ets.fcast, data2)[2,8],
       "stl", accuracy(stl.fcast, data2)[2,8],
       "arima", accuracy(arima.fcast, data2)[2,8],
       "arima log", accuracy(arimalog.fcast, log2)[2,8],
       "ets log", accuracy(etslog.fcast, log2)[2,8]
)
# ETS of Log Data performs best followed by ARIMA of Log Data, Linear and ETS
####