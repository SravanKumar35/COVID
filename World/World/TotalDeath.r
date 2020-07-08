df <- read.csv('G:\\Required\\College\\8th sem\\Major Project\\COVID\\World-20200428T111157Z-001\\World\\total-daily-covid-deaths.csv')

df$Date <- as.Date(df$Date, format = "%b %d, %Y")
#df <- subset(df, select = c(Date, TotalConfirmedDeaths, DailyNewConfirmedDeaths))

head(df)

library(ggplot2)
library(plotly)
library(dplyr)

df2 <- df[df$Entity == 'World', ] 
df2


fig2 <- plot_ly(df2, x = ~Date, y = ~TotalConfirmedDeaths, type = 'scatter',
                mode = 'lines+markers')
fig2 <- fig2 %>%
  add_trace(y = ~DailyNewConfirmedDeaths, type = 'scatter',
            mode = 'lines+markers')

fig2 <- fig2 %>%
  layout(hovermode = 'compare',
         title = 'Daily and total deaths worldwide')

fig2

#####Total Percentage change in deaths in the World######
df3 <- df2 %>%
  mutate(TotalConfirmedDeathsChange = c(NA, -diff(TotalConfirmedDeaths) / TotalConfirmedDeaths[-1] * 100))

TotalChangeInDeaths <- df3%>%
  select(Date, TotalConfirmedDeaths, TotalConfirmedDeathsChange)
TotalChangeInDeaths

#####Daily Percentage change in deaths in the World######
df3 <- df2 %>%
mutate(DailyConfirmedDeathsChange = c(NA, -diff(DailyNewConfirmedDeaths) / DailyNewConfirmedDeaths[-1] * 100))

DailyChangeInDeaths <- df3%>%
  select(Date, DailyNewConfirmedDeaths, DailyConfirmedDeathsChange)

DailyChangeInDeaths

library(flextable)

library(officer)

ft <- flextable(tail(TotalChangeInDeaths, 15))
ft <- autofit(ft)
print(ft, preview = "pptx")

ft2 <- flextable(tail(DailyChangeInDeaths, 15))
ft2 <- autofit(ft2)
print(ft2, preview = "pptx")


#####Total Percentage change in deaths in India######
df4 <- df[df$Entity == 'India', ] 
df4

fig4 <- plot_ly(df4, x = ~Date, y = ~TotalConfirmedDeaths, type = 'scatter',
                mode = 'lines+markers')

fig4 <- fig4 %>%
  add_trace(y = ~DailyNewConfirmedDeaths, type = 'scatter',
            mode = 'lines+markers')

fi4 <- fig4 %>%
  layout(title = 'Daily and total deaths(India)', hovermode = 'compare')

fig4


df5 <- df4 %>%
  mutate(TotalConfirmedDeathsChangeIndia = c(NA, -diff(TotalConfirmedDeaths) / TotalConfirmedDeaths[-1] * 100))

TotalChangeInDeathsIndia <- df5%>%
  select(Date, TotalConfirmedDeaths, TotalConfirmedDeathsChangeIndia)
TotalChangeInDeathsIndia

ft3 <- flextable(tail(TotalChangeInDeathsIndia, 15))
ft3 <- autofit(ft3)
print(ft3, preview = "pptx")

#####Daily Percentage change in deaths in India######
df5 <- df4 %>%
  mutate(DailyConfirmedDeathsChangeIndia = c(NA, -diff(DailyNewConfirmedDeaths) / DailyNewConfirmedDeaths[-1] * 100))

DailyChangeInDeathsIndia <- df5%>%
  select(Date, DailyNewConfirmedDeaths, DailyConfirmedDeathsChangeIndia)

DailyChangeInDeathsIndia


ft4 <- flextable(tail(DailyChangeInDeathsIndia, 15))
ft4 <- autofit(ft4)
print(ft3, preview = "pptx")
