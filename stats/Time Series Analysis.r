### create a time-series dataset based on the data

corona_positives <- read.csv("C:/Users/dalit/DataScience/data/corona_positives.csv")
View(corona_positives)

dts <- ts(corona_positives$positives, start=(min(corona_positives$positives)), end=(max(corona_positives$positives)),7)

## change the size of the graphs
options(repr.plot.width = 8, repr.plot.height = 8)
plot(dts)

stationary_dts <- decompose(dts)

plot(stationary_dts)

acf(dts)

pacf(dts)

dts_arima <- arima(dts, order=c(3,0,1))
dts_arima

BIC(dts_arima)

library(forecast)
dts_fit <- forecast(dts_arima)
dts_fit

plot(dts_fit)

dts_autoarima <- auto.arima(dts)
dts_autoarima

dts_autoforecast <- forecast(dts_autoarima)
dts_autoforecast

plot(dts_autoforecast)
