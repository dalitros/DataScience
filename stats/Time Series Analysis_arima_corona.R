### create a time-series dataset based on the data

##dts <- ts(AirPassengers, start=c(1949,1), end=c(1960,12), frequency=12)
CP <- ts(corona_positives$positives, min(corona_positives$test_date), max(corona_positives$test_date))
CP

## change the size of the graphs
##options(repr.plot.width = 4, repr.plot.height = 4)
options(repr.plot.width = 4,repr.plot.height = 4 )
##plot(dts)
plot(CP)

##stationary_dts <- decompose(dts)
stationary_CP <- decompose(CP)

plot(stationary_CP)

acf(CP)

pacf(CP)

dtCP_arima <- arima(CP, order=c(15,0,0))
CP_arima

library(forecast)
CP_fit <- forecast(CP_arima)
CP_fit

plot(CP_fit)

CP_autoarima <- auto.arima(CP)
CP_autoarima

CP_autoforecast <- forecast(CP_autoarima)
CP_autoforecast

plot(CP_autoforecast)

