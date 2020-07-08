#####Air Quality########

df <- read.csv("G:\\Required\\College\\8th sem\\Major Project\\COVID\\World-20200428T111157Z-001\\World\\Air Quality\\waqi-covid19-airqualitydata-2020.csv")

head(df)

library(ggplot2)
theme_set(theme_minimal())
library(plotly)
library(dplyr)
library(gghighlight)
library(scales)

df$Date <- as.Date(df$Date, format = "%d-%m-%Y")

df_ind_pm25 <- filter(df, Country == 'IN' & Specie == 'pm25')
df_ind_pm25 <- filter(df_ind_pm25, City == 'Delhi' | City == 'Bengaluru' | City == 'Mumbai' | City == 'Kolkata' | City == 'Chennai')
#df_ind$Date <- as.Date(df_ind$Date, format = "%d-%m-%Y")

#head(df_ind_pm25)

#fig1 <- ggplot(df_ind, aes(x = Date, y = median)) +
#  geom_bar(stat = 'identity')

#fig1

fig2 <- ggplot(df_ind_pm25, aes(x = Date, y = median)) + geom_line(aes(color = City)) +  scale_x_date(labels = date_format("%Y-%m-%d"), date_breaks = '2 day') + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  #+gghighlight(df_ind_pm25$Date >= '2020-03-25' ) 

pm25 <- ggplotly(fig2) 

pm25 <- pm25 %>%  layout(title = "pm25 levels in Indian Metropolitan cities")

pm25

#head(df_ind_pm25)
####Percentage Change in vlaues of pm25######

day1 <- df_ind_pm25[df_ind_pm25$Date == "2020-03-25",]
lastDay <- df_ind_pm25[df_ind_pm25$Date == '2020-04-10', ]

day <- rbind(day1, lastDay)
day <- day %>% group_by(City) %>%  arrange(City, .by_group = TRUE) %>%
  mutate(pct_change = (lag(median) - median)/median * 100)

  #mutate(pct_change = -diff(median) / median[-1] * 100)


pct_change_pm25 <- day%>%
  select(Date, City, median, pct_change)

pct_change_pm25[complete.cases(pct_change_pm25),]

library(flextable)
library(officer)
ft <- flextable(pct_change_pm25[complete.cases(pct_change_pm25),])
ft <- autofit(ft)
print(ft, preview = "pptx")


#####CO levels########
df_ind_co <- filter(df, Country == 'IN' & Specie == 'co')
df_ind_co <- filter(df_ind_co, City == 'Delhi' | City == 'Bengaluru' | 
                        City == 'Mumbai' | City == 'Kolkata' | City == 'Chennai')

fig3 <- ggplot(df_ind_co, aes(x = Date, y = median)) +
  geom_line(aes(color = City)) +
  scale_x_date(labels = date_format("%Y-%m-%d"), date_breaks = '1 day') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

co <- ggplotly(fig3)

co <- co %>%
  layout(title = "CO levels in Indian Metropolitan Cities")
co

####Percentage Change in vlaues of co######

day1 <- df_ind_co[df_ind_co$Date == "2020-03-25",]
lastDay <- df_ind_co[df_ind_co$Date == '2020-04-10', ]

day <- rbind(day1, lastDay)
day <- day %>%
  group_by(City) %>%
  arrange(City, .by_group = TRUE) %>%
  mutate(pct_change = (lag(median) - median)/median * 100)

pct_change_co <- day%>%
  select(Date, City, median, pct_change)

pct_change_co[complete.cases(pct_change_co),]

ft2 <- flextable(pct_change_co[complete.cases(pct_change_co),])
ft2 <- autofit(ft2)
print(ft2, preview = "pptx")


#####pm10 levels####
df_ind_pm10 <- filter(df, Country == 'IN' & Specie == 'pm10')
df_ind_pm10 <- filter(df_ind_pm10, City == 'Delhi' | City == 'Bengaluru' | 
                      City == 'Mumbai' | City == 'Kolkata' | City == 'Chennai')

fig4 <- ggplot(df_ind_pm10, aes(x = Date, y = median)) +
  geom_line(aes(color = City)) +
  scale_x_date(labels = date_format("%Y-%m-%d"), date_breaks = '1 day') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


pm10 <- ggplotly(fig4)

pm10 <- pm10 %>%
  layout(title = "pm10 levels in Indian Metropolitan Cities")
pm10

####Percentage Change in vlaues of pm10######

day1 <- df_ind_pm10[df_ind_pm10$Date == "2020-03-25",]
lastDay <- df_ind_pm10[df_ind_pm10$Date == '2020-04-10', ]

day <- rbind(day1, lastDay)

day <- day %>%
  group_by(City) %>%
  arrange(City, .by_group = TRUE) %>%
  mutate(pct_change = (lag(median) - median)/median * 100)

pct_change_pm10 <- day%>%
  select(Date, City, median, pct_change)

pct_change_pm10[complete.cases(pct_change_pm10),]

ft3 <- flextable(pct_change_pm10[complete.cases(pct_change_pm10),])
ft3 <- autofit(ft3)
print(ft3, preview = "pptx")


#####o3 levels####
df_ind_o3 <- filter(df, Country == 'IN' & Specie == 'o3')
df_ind_o3 <- filter(df_ind_o3, City == 'Delhi' | City == 'Bengaluru' | 
                        City == 'Mumbai' | City == 'Kolkata' | City == 'Chennai')

fig5 <- ggplot(df_ind_o3, aes(x = Date, y = median)) +
  geom_line(aes(color = City)) +
  scale_x_date(labels = date_format("%Y-%m-%d"), date_breaks = '1 day') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

o3 <- ggplotly(fig5)

o3 <- o3 %>%
  layout(title = "o3 levels in Indian Metropolitan Cities")
o3


####Percentage Change in vlaues of o3######

day1 <- df_ind_o3[df_ind_o3$Date == "2020-03-25",]
lastDay <- df_ind_o3[df_ind_o3$Date == '2020-04-10', ]

day <- rbind(day1, lastDay)
day <- day %>%
  group_by(City) %>%
  arrange(City, .by_group = TRUE) %>%
  mutate(pct_change = (lag(median) - median)/median * 100)

pct_change_o3 <- day%>%
  select(Date, City, median, pct_change)

pct_change_o3[complete.cases(pct_change_o3),]
ft4 <- flextable(pct_change_o3[complete.cases(pct_change_o3),])
ft4 <- autofit(ft4)
print(ft4, preview = "pptx")

#####no2 levels######
df_ind_no2 <- filter(df, Country == 'IN' & Specie == 'no2')
df_ind_no2 <- filter(df_ind_no2, City == 'Delhi' | City == 'Bengaluru' | 
                      City == 'Mumbai' | City == 'Kolkata' | City == 'Chennai')

fig6 <- ggplot(df_ind_no2, aes(x = Date, y = median)) +
  geom_line(aes(color = City)) +
  scale_x_date(labels = date_format("%Y-%m-%d"), date_breaks = '1 day') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

no2 <- ggplotly(fig6)

no2 <- no2 %>%
  layout(title = "no2 levels in Indian Metropolitan Cities")
no2

####Percentage Change in vlaues of no2######

day1 <- df_ind_no2[df_ind_no2$Date == "2020-03-25",]
lastDay <- df_ind_no2[df_ind_no2$Date == '2020-04-10', ]

day <- rbind(day1, lastDay)
day <- day %>%
  group_by(City) %>%
  arrange(City, .by_group = TRUE) %>%
  mutate(pct_change = (lag(median) - median)/median * 100)

pct_change_no2 <- day%>%
  select(Date, City, median, pct_change)

pct_change_no2[complete.cases(pct_change_no2),]

ft5 <- flextable(pct_change_no2[complete.cases(pct_change_no2),])
ft5 <- autofit(ft5)
print(ft5, preview = "pptx")

#####so2 levels#########
df_ind_so2 <- filter(df, Country == 'IN' & Specie == 'so2')
df_ind_so2 <- filter(df_ind_so2, City == 'Delhi' | City == 'Bengaluru' | 
                       City == 'Mumbai' | City == 'Kolkata' | City == 'Chennai')

fig7 <- ggplot(df_ind_so2, aes(x = Date, y = median)) +
  geom_line(aes(color = City)) +
  scale_x_date(labels = date_format("%Y-%m-%d"), date_breaks = '1 day') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

so2 <- ggplotly(fig7)

so2 <- so2 %>%
  layout(title = "so2 levels in Indian Metropolitan Cities")
so2

####Percentage Change in vlaues of so2######

day1 <- df_ind_so2[df_ind_so2$Date == "2020-03-25",]
lastDay <- df_ind_so2[df_ind_so2$Date == '2020-04-10', ]

day <- rbind(day1, lastDay)
day <- day %>%
  group_by(City) %>%
  arrange(City, .by_group = TRUE) %>%
  mutate(pct_change = (lag(median) - median)/median * 100)

pct_change_so2 <- day%>%
  select(Date, City, median, pct_change)

pct_change_so2[complete.cases(pct_change_so2),]

ft6 <- flextable(pct_change_so2[complete.cases(pct_change_so2),])
ft6 <- autofit(ft6)
print(ft6, preview = "pptx")
