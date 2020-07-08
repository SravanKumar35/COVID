library(xts)
library(forecast)
data = read.csv("G:\\Required\\College\\8th sem\\Major Project\\COVID\\lowmiddle\\final\\India.csv", header = T)
data$date
class(data$date)
data$date = as.Date(data$date, format = "%d-%m-%y")
class(data$date)
data
inds = seq(as.Date("2019-12-31"), as.Date("2020-04-25"), by = "day")

#Using Timeseries

ts_data = ts(data$total_cases, start=c(2019,12,31), end=c(2020,4,25), frequency = 124)
ts_data

fit_ses_ts = ses(ts_data)
summary(fit_ses_ts)
plot(fit_ses_ts)

#Using Zoo
index(zoo_data)
zoo_data= zoo(data$total_cases, inds)
zoo_data

fit_ses_zoo = ses(zoo_data)
summary(fit_ses_zoo)
plot(fit_ses_zoo)


#Using Timeseries

fit_holt_ts = holt(ts_data)
summary(fit_holt_ts)
plot(fit_holt_ts)


#Using Zoo

fit_holt_zoo = holt(zoo_data)
summary(fit_holt_zoo)
plot(fit_holt_zoo)


