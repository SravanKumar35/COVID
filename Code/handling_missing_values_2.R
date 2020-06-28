library(zoo)

setwd("G:\\Required\\College\\8th sem\\Major Project\\COVID\\Final\\LowMiddle\\CSVs\\Pre-Processed Data\\")
lowMiddle = c("Angola", "Bangladesh", "Bhutan", "Bolivia", "Cape Verde", "Cambodia", "Cameroon", "Congo", "Cote d'Ivoire", "Djibouti", "Egypt", "El Salvador", "Swaziland", "Ghana", "Honduras", "India", "Indonesia", "Kenya", "Kyrgyzstan", "Laos", "Mauritania", "Moldova", "Mongolia", "Morocco", "Myanmar", "Nicaragua", "Nigeria", "Pakistan", "Papua New Guinea", "Philippines", "Sao Tome and Principe", "Senegal", "Sudan", "Timor", "Tunisia", "Ukraine", "Uzbekistan", "Vietnam", "Zambia", "Zimbabwe")

for(i in lowMiddle){
  
  temp = read.csv(paste(i, ext, sep = ""))
  print(i)
  v2 = na.locf(temp)
  v2[is.na(temp)] = v2[is.na(temp)]
  
  write.csv(x = v2, file = paste(i,ext,  sep = ""), row.names = FALSE)
}

setwd("G:\\Required\\College\\8th sem\\Major Project\\COVID\\Final\\SouthAsia\\CSVs\\Pre-Processed Data\\")
south_asia = c("Bangladesh", "Bhutan", "India", "Maldives", "Nepal", "Pakistan","Sri Lanka" )

for(i in south_asia){
  temp = read.csv(paste(i, ext, sep = ""))
  print(i)
  v2 = na.locf(temp)
  v2[is.na(temp)] = v2[is.na(temp)]
  
  write.csv(x = v2, file = paste(i,ext,  sep = ""), row.names = FALSE)
}