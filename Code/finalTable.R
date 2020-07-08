setwd("G:\\Required\\College\\8th sem\\Major Project\\COVID\\Final\\LowMiddle\\Predictions\\")


lowMiddle_1 = c("Angola", "Bangladesh", "Bhutan", "Bolivia", "Cape Verde", 
              "Cambodia", "Cameroon", "Congo", "Cote d'Ivoire", "Djibouti", 
              "Egypt", "El Salvador", "Swaziland", "Ghana", "Honduras", "India", 
              "Indonesia", "Kenya", "Kyrgyzstan", "Laos", "Mauritania", "Moldova",
              "Mongolia", "Morocco", "Myanmar", "Nicaragua", "Nigeria", "Pakistan",
              "Papua New Guinea", "Philippines", "Sao Tome and Principe", "Senegal",
              "Sudan", "Timor", "Tunisia", "Ukraine", "Uzbekistan", "Vietnam", 
              "Zambia", "Zimbabwe")


dfc = read.csv(paste(dir_cases, "Bangladesh.csv",sep = ""))
dfc[,c(1:3)]

dfd = read.csv(paste(dir_deaths, "Bangladesh.csv",sep = ""))
dfd[,c(1:3)]

dfc = cbind(dfc[,c(1:3)], dfd[,c(1:3)])
dfc
dfc[ncol(dfc)+1] = "Bangladesh"
dfc[, -1]

for(i in lowMiddle_1){
  temp = read.csv(paste("Total Cases\\", i, ".csv", sep = ""))
  temp1 = read.csv(paste("Total Deaths\\", i, ".csv", sep = ""))
  
  temp = temp[,c(1:3)]
  temp1 = temp1[,c(1:3)]
  
  temp = cbind(temp[,c(1:3)], temp1[,c(1:3)])
  temp[ncol(temp)+1] = i
  
  dfc = rbind(dfc, temp)
  
}

write.csv(x = dfc, "final.csv", row.names = FALSE)



#########################################################################



setwd("G:\\Required\\College\\8th sem\\Major Project\\COVID\\Final\\SouthAsia\\Predictions\\")

south_asia_1 = c( "Bhutan", "India", "Maldives", "Nepal", "Pakistan","Sri Lanka" )

dfc = read.csv(paste(dir_cases, "Bangladesh.csv",sep = ""))
dfc[,c(1:3)]

dfd = read.csv(paste(dir_deaths, "Bangladesh.csv",sep = ""))
dfd[,c(1:3)]

dfc = cbind(dfc[,c(1:3)], dfd[,c(1:3)])
dfc
dfc[ncol(dfc)+1] = "Bangladesh"
dfc[, -1]

for(i in south_asia_1){
  temp = read.csv(paste("Total Cases\\", i, ".csv", sep = ""))
  temp1 = read.csv(paste("Total Deaths\\", i, ".csv", sep = ""))
  
  temp = temp[,c(1:3)]
  temp1 = temp1[,c(1:3)]
  
  temp = cbind(temp[,c(1:3)], temp1[,c(1:3)])
  temp[ncol(temp)+1] = i
  
  dfc = rbind(dfc, temp)
  
}

write.csv(x = dfc, "final.csv", row.names = FALSE)




