setwd("G:\\Required\\College\\8th sem\\Major Project\\COVID\\Final\\LowMiddle\\")

library(dplyr)
library(forecast)

lowMiddle = c("Angola", "Bangladesh", "Bhutan", "Bolivia", "Cape Verde", 
              "Cambodia", "Cameroon", "Congo", "Cote d'Ivoire", "Djibouti", 
              "Egypt", "El Salvador", "Swaziland", "Ghana", "Honduras", "India", 
              "Indonesia", "Kenya", "Kyrgyzstan", "Laos", "Mauritania", "Moldova",
              "Mongolia", "Morocco", "Myanmar", "Nicaragua", "Nigeria", "Pakistan",
              "Papua New Guinea", "Philippines", "Sao Tome and Principe", "Senegal",
              "Sudan", "Timor", "Tunisia", "Ukraine", "Uzbekistan", "Vietnam", 
              "Zambia", "Zimbabwe")
ext_csv = ".csv"
par(mfrow=c(1,2),oma = c(0, 0, 2, 0))

for (i in lowMiddle) {
  
  temp = read.csv(paste("CSVs\\Pre-Processed Data\\",i,ext_csv, sep = ""))

  
  cases_df <- subset(temp, select = c(total_cases))
  deaths_df <- subset(temp, select = c(total_deaths))
  
  ts_cases <- ts(cases_df)
  ts_deaths <- ts(deaths_df)
  
  #jpeg(paste(dir_Save, i, ext_plot))
  model_arima = auto.arima(ts_cases)
  Y = forecast(model_arima, h = 10)
  
  model_arima2 = auto.arima(ts_deaths)
  Y1 = forecast(model_arima2, h = 10)
  
  
  
  plot(Y, ylab = "Total Cases", main = "Confirmed Cases ")
  plot(Y1, ylab = "Total Deaths", main = "Total Deaths")
  mtext(paste0("Country: ", i), outer = TRUE, cex = 1.2, font = 1)
  
  
  write.csv(x = Y, file = paste("Predictions\\Total Cases\\",i,ext, sep = "" ), row.names = FALSE)
  write.csv(x = Y1, file = paste("Predictions\\Total Deaths\\",i,ext, sep = "" ), row.names = FALSE)
  
  
}

######################################################################

setwd("G:\\Required\\College\\8th sem\\Major Project\\COVID\\Final\\SouthAsia\\")

south_asia = c("Bangladesh", "Bhutan", "India", "Maldives", "Nepal", "Pakistan",
               "Sri Lanka" )
ext_csv = ".csv"
par(mfrow=c(1,2),oma = c(0, 0, 2, 0))

for (i in south_asia) {
  
  temp = read.csv(paste("CSVs\\Pre-Processed Data\\",i,ext_csv, sep = ""))
  
  
  cases_df <- subset(temp, select = c(total_cases))
  deaths_df <- subset(temp, select = c(total_deaths))
  
  ts_cases <- ts(cases_df)
  ts_deaths <- ts(deaths_df)
  
  #jpeg(paste(dir_Save, i, ext_plot))
  model_arima = auto.arima(ts_cases)
  Y = forecast(model_arima, h = 10)
  
  model_arima2 = auto.arima(ts_deaths)
  Y1 = forecast(model_arima2, h = 10)
  
  
  
  plot(Y, ylab = "Total Cases", main = "Confirmed Cases ")
  plot(Y1, ylab = "Total Deaths", main = "Total Deaths")
  mtext(paste0("Country: ", i), outer = TRUE, cex = 1.2, font = 1)
  
  write.csv(x = Y, file = paste("Predictions\\Total Cases\\",i,ext, sep = "" ), row.names = FALSE)
  write.csv(x = Y1, file = paste("Predictions\\Total Deaths\\",i,ext, sep = "" ), row.names = FALSE)
  
}
