df <- read.csv('G:\\Required\\College\\8th sem\\Major Project\\COVID\\World-20200428T111157Z-001\\World\\full-list-cumulative-total-tests-per-million.csv')

library(RColorBrewer)
library(flextable)
library(officer)
# Define the number of colors you want
nb.cols <- 42
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)


head(df)
df$Date <- as.Date(df$Date, format = "%b %d, %Y")

#df <- subset(df, select = c(Entity, Date, Cumulative.total.tests.per.thousand))


library(plotly)
fig1 <- plot_ly(df, x = ~Date, y = ~Cumulative.total.tests.per.thousand,
                type = 'scatter', mode = 'lines+markers', color = ~Entity)

library(dplyr)
fig1 <- fig1 %>% layout(title = "Number of tests per thousand people", margin = margin,
                      autosize = TRUE,
                      showlegend = TRUE,
                      annotations = df$Entity)

fig1


df_sorted <- df[order(df$Cumulative.total.tests.per.thousand, decreasing = TRUE),]

#df_sorted

df_sorted <- df_sorted[1:54,]

fig2 <- plot_ly(df_sorted, x = ~Date, y = ~Cumulative.total.tests.per.thousand,
                type = 'scatter', mode = 'lines+markers', color = ~Entity)
fig2 <- fig2 %>%
  layout(hovermode = 'compare',
         title = "Top 5 countries with maximum tests per thousand people")

fig2

df_ind <- filter(df, Code == 'USA' | Code == 'FRA' | Code == 'DEU' | Code == 'ITA' |
                   Code == 'IND')
head(df_ind)
fig3 <- plot_ly(df_ind, x = ~Date, y = ~Cumulative.total.tests.per.thousand,
                type = 'scatter', mode = 'lines+markers', color = ~Entity)

fig3 <- fig3 %>%
  layout(hovermode = 'compare',
         title = 'Top 5 countries with maximum cases & their number of tests per thousand people')

fig3


confirmed_cases <- read.csv('G:\\Required\\College\\8th sem\\Major Project\\COVID\\World-20200428T111157Z-001\\World\\total-and-daily-cases-covid-19.csv')
confirmed_cases$Date <- as.Date(confirmed_cases$Date, format = "%b %d, %Y")
head(confirmed_cases)

TopConfirmedCases <- filter(confirmed_cases, Code == 'USA' | Code == 'FRA' | Code == 'DEU' | Code == 'ITA' |
           Code == 'IND' | Code == 'ESP' | Code == 'CHN')

#Total Confirmed Cases
fig5 <- plot_ly(TopConfirmedCases, x = ~Date, y = ~TopConfirmedCases$Total.confirmed.cases..cases.,
                type = 'scatter', mode = 'lines+markers', color = ~Code)

fig5 <- fig5 %>%
  layout(hovermode = 'compare',
         title = "Top 6 countries with India: total number of cases")
fig5

##Daily COnfirmed Cases
fig6 <- plot_ly(TopConfirmedCases, x = ~Date, y = ~TopConfirmedCases$Daily.new.confirmed.cases..cases.,
                type = 'scatter', mode = 'lines+markers', color = ~Code) 
fig6 <- fig6 %>%
  layout(hovermode = 'compare',
         title = 'Daily confirmed cases in top 6 countries') 
fig6


IndianDailyCases <- filter(confirmed_cases, Code == 'IND')
fig_ind <- plot_ly(IndianDailyCases, x = ~Date, y = ~IndianDailyCases$Daily.new.confirmed.cases..cases.,
                   type = 'scatter', mode = 'lines+markers')

fig_ind <- fig_ind %>%
  layout(hovermode = 'compare',
         
         title = 'Daily and total confirmed cases in India')

fig_ind <- fig_ind %>%
  add_trace(y = ~IndianDailyCases$Total.confirmed.cases..cases.)
fig_ind


library(gganimate)


p <- ggplot(IndianDailyCases, aes(Date, IndianDailyCases$Total.confirmed.cases..cases., color = Code)) +
  geom_line() +
  geom_point() +
  transition_reveal(Date)
p


head(df)
head(confirmed_cases)

length(df$Date)
length(confirmed_cases$Date)


CasesTest <- merge(df, confirmed_cases, by.x = c('Entity', 'Code', 'Date'))
head(CasesTest)



#Comparison between Daily Tests and Daily Confirmed Cases
fig7 <- plot_ly(CasesTest, x = ~Date, y = ~Cumulative.total.tests.per.thousand,
                type = 'scatter', mode = 'lines+markers', color = ~Code)

fig7 <- fig7 %>%
  add_trace(y = ~CasesTest$Daily.new.confirmed.cases..cases., type = 'scatter', mode = 'lines+markers',
            color = ~Entity)
fig7 <- layout(fig7, yaxis = list(type = "log"))

fig7  

print("Correlation between Total Number of cases and The Daily Tests: " + 
        cor(CasesTest$Cumulative.total.tests.per.thousand, CasesTestGrp$TotalConfirmedCases))
          


#######IND
IndiaConfirmedCases <- filter(confirmed_cases, Code == 'IND')
IndiaConfirmedCases <- IndiaConfirmedCases %>%
  mutate(TotalPercentIncrease = c(NA, -diff(IndiaConfirmedCases$Total.confirmed.cases..cases.) / IndiaConfirmedCases$Total.confirmed.cases..cases.[-1] * 100))
IndiaConfirmedCases <- IndiaConfirmedCases %>%
  mutate(DailyPercentIncrease = c(NA, -diff(IndiaConfirmedCases$Daily.new.confirmed.cases..cases.) / IndiaConfirmedCases$Daily.new.confirmed.cases..cases.[-1] * 100))

ft <- flextable(tail(IndiaConfirmedCases, 15))
ft <- autofit(ft)
print(ft, preview = "pptx")


#########USA
USConfirmedCases <- filter(confirmed_cases, Code == 'USA')

USConfirmedCases <- USConfirmedCases %>%
  mutate(TotalPercentIncrease = c(NA, -diff(USConfirmedCases$Total.confirmed.cases..cases.) / USConfirmedCases$Total.confirmed.cases..cases.[-1] * 100))
USConfirmedCases <- USConfirmedCases %>%
  mutate(DailyPercentIncrease = c(NA, -diff(USConfirmedCases$Daily.new.confirmed.cases..cases.) / USConfirmedCases$Daily.new.confirmed.cases..cases.[-1] * 100))

ft2 <- flextable(tail(USConfirmedCases, 15))
ft2 <- autofit(ft2)
print(ft2, preview = "pptx")


#######CHN
CHNConfirmedCases <- filter(confirmed_cases, Code == 'CHN')
CHNConfirmedCases <- CHNConfirmedCases %>%
  mutate(TotalPercentIncrease = c(NA, -diff(CHNConfirmedCases$Total.confirmed.cases..cases.) / CHNConfirmedCases$Total.confirmed.cases..cases.[-1] * 100))
CHNConfirmedCases <- CHNConfirmedCases %>%
  mutate(DailyPercentIncrease = c(NA, -diff(CHNConfirmedCases$Daily.new.confirmed.cases..cases.) / CHNConfirmedCases$Daily.new.confirmed.cases..cases.[-1] * 100))

ft3 <- flextable(tail(CHNConfirmedCases, 15))
ft3 <- autofit(ft3)
print(ft3, preview = "pptx")


#####Germany
DEUConfirmedCases <- filter(confirmed_cases, Code == 'DEU')
DEUConfirmedCases <- DEUConfirmedCases %>%
  mutate(TotalPercentIncrease = c(NA, -diff(TotalConfirmedCases) / TotalConfirmedCases[-1] * 100))
DEUConfirmedCases <- DEUConfirmedCases %>%
  mutate(DailyPercentIncrease = c(NA, -diff(DailyNewConfirmedCases) / DailyNewConfirmedCases[-1] * 100))

ft4 <- flextable(tail(DEUConfirmedCases, 15))
ft4 <- autofit(ft4)
print(ft4, preview = "pptx")


#####Spain
ESPConfirmedCases <- filter(confirmed_cases, Code == 'ESP')
ESPConfirmedCases <- ESPConfirmedCases %>%
  mutate(TotalPercentIncrease = c(NA, -diff(ESPConfirmedCases$Total.confirmed.cases..cases.) / ESPConfirmedCases$Total.confirmed.cases..cases.[-1] * 100))
ESPConfirmedCases <- ESPConfirmedCases %>%
  mutate(DailyPercentIncrease = c(NA, -diff(ESPConfirmedCases$Daily.new.confirmed.cases..cases.) / ESPConfirmedCases$Daily.new.confirmed.cases..cases.[-1] * 100))

ft5 <- flextable(tail(ESPConfirmedCases, 15))
ft5 <- autofit(ft5)
print(ft5, preview = "pptx")

#####France
FRAConfirmedCases <- filter(confirmed_cases, Code == 'FRA')
FRAConfirmedCases <- FRAConfirmedCases %>%
  mutate(TotalPercentIncrease = c(NA, -diff(FRAConfirmedCases$Total.confirmed.cases..cases.) / FRAConfirmedCases$Total.confirmed.cases..cases.[-1] * 100))
FRAConfirmedCases <- FRAConfirmedCases %>%
  mutate(DailyPercentIncrease = c(NA, -diff(FRAConfirmedCases$Daily.new.confirmed.cases..cases.) / FRAConfirmedCases$Daily.new.confirmed.cases..cases.[-1] * 100))

ft6 <- flextable(tail(FRAConfirmedCases, 15))
ft6 <- autofit(ft6)
print(ft6, preview = "pptx")

#####Italy
ITAConfirmedCases <- filter(confirmed_cases, Code == 'ITA')
ITAConfirmedCases <- ITAConfirmedCases %>%
  mutate(TotalPercentIncrease = c(NA, -diff(ITAConfirmedCases$Total.confirmed.cases..cases.) / ITAConfirmedCases$Total.confirmed.cases..cases.[-1] * 100))
ITAConfirmedCases <- ITAConfirmedCases %>%
  mutate(DailyPercentIncrease = c(NA, -diff(ITAConfirmedCases$Daily.new.confirmed.cases..cases.) / ITAConfirmedCases$Daily.new.confirmed.cases..cases.[-1] * 100))

ft7 <- flextable(tail(ITAConfirmedCases, 15))
ft7 <- autofit(ft7)
print(ft7, preview = "pptx")

