setwd("G:\\Required\\College\\8th sem\\Major Project\\COVID\\Final\\LowMiddle\\")

library(dplyr)
library(forecast)

lowMiddle = c("Angola", "Bangladesh", "Bhutan", "Bolivia", "Cape Verde", "Cambodia", "Cameroon", "Congo", "Cote d'Ivoire", "Djibouti", "Egypt", "El Salvador", "Swaziland", "Ghana", "Honduras", "India", "Indonesia", "Kenya", "Kyrgyzstan", "Laos", "Mauritania", "Moldova", "Mongolia", "Morocco", "Myanmar", "Nicaragua", "Nigeria", "Pakistan", "Papua New Guinea", "Philippines", "Sao Tome and Principe", "Senegal", "Sudan", "Timor", "Tunisia", "Ukraine", "Uzbekistan", "Vietnam", "Zambia", "Zimbabwe")
ext = ".csv"
par(mfrow=c(1,2))

for(i in lowMiddle){
  temp = read.csv(paste0("CSVs\\Pre-Processed Data\\", i, ext))
  
  cases_df <- subset(temp, select = c(total_cases))
  deaths_df <- subset(temp, select = c(total_deaths))
  
  ts_cases <- ts(cases_df)
  ts_deaths <- ts(deaths_df)
  
  jpeg(paste(dir_Save, i, ext_plot))
  model_arima = auto.arima(ts_cases)
  Y1 = forecast(model_arima, h = 10)
  
  model_arima2 = auto.arima(ts_deaths)
  Y2 = forecast(model_arima2, h = 10)
  
  plot(Y1, ylab = "Total Cases" , main = paste("Total Cases in ", i, sep = ""))
  plot(Y2, ylab = "Total Deaths", main = paste("Total Deaths in ", i, sep = ""))
  
  write.csv(x = Y1, paste( "Predictions\\Total Cases\\", i, ext, sep = ""), row.names = FALSE)
  write.csv(x = Y2, paste( "Predictions\\Total Deaths\\", i, ext, sep = ""), row.names = FALSE)
  
}





setwd("G:\\Required\\College\\8th sem\\Major Project\\COVID\\Final\\SouthAsia\\")

south_asia = c("Bangladesh", "Bhutan", "India", "Maldives", "Nepal", "Pakistan","Sri Lanka" )

for(i in south_asia){
  temp = read.csv(paste0("CSVs\\Pre-Processed Data\\", i, ext))
  
  cases_df <- subset(temp, select = c(total_cases))
  deaths_df <- subset(temp, select = c(total_deaths))
  
  ts_cases <- ts(cases_df)
  ts_deaths <- ts(deaths_df)
  
  Y1 = holt(ts_cases)
  Y2 = holt(ts_deaths)
  
  plot(Y1, ylab = "Total Cases" , main = paste("Total Cases in ", i, sep = ""))
  plot(Y2, ylab = "Total Deaths", main = paste("Total Deaths in ", i, sep = ""))
  
  write.csv(x = Y1, paste( "Predictions\\Total Cases\\", i, ext, sep = ""), row.names = FALSE)
  write.csv(x = Y2, paste( "Predictions\\Total Deaths\\", i, ext, sep = ""), row.names = FALSE)
  
}