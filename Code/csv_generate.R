setwd("G:\\Required\\College\\8th sem\\Major Project\\COVID\\Final")

lowMiddle = c("Angola", "Bangladesh", "Bhutan", "Bolivia", "Cape Verde", "Cambodia", "Cameroon", "Congo", "Cote d'Ivoire", "Djibouti", "Egypt", "El Salvador", "Swaziland", "Ghana", "Honduras", "India", "Indonesia", "Kenya", "Kyrgyzstan", "Laos", "Mauritania", "Moldova", "Mongolia", "Morocco", "Myanmar", "Nicaragua", "Nigeria", "Pakistan", "Papua New Guinea", "Philippines", "Sao Tome and Principe", "Senegal", "Sudan", "Timor", "Tunisia", "Ukraine", "Uzbekistan", "Vietnam", "Zambia", "Zimbabwe")
south_asia = c("Bangladesh", "Bhutan", "India", "Maldives", "Nepal", "Pakistan","Sri Lanka" )

df = read.csv(url("https://covid.ourworldindata.org/data/owid-covid-data.csv"))
df = df[,c('location', 'date', 'total_cases', 'total_deaths')]
df

ext = ".csv"
lm_path = "LowMiddle//CSVs//Original Data//"
sa_path = "SouthAsia//CSVs//Original Data//"

for( i in lowMiddle)
{
  temp = df[df$location == i, ]
  write.csv(x = temp, file = paste(lm_path,i,ext, sep = "" ), row.names = FALSE)
}


for( i in south_asia)
{
  temp = df[df$location == i, ]
  write.csv(x = temp, file = paste(sa_path,i,ext, sep = "" ), row.names = FALSE)
}

