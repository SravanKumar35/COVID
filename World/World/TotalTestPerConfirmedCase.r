######### Number of tests per confirmed cases####

df <- read.csv('G:\\Required\\College\\8th sem\\Major Project\\COVID\\World-20200428T111157Z-001\\World\\number-of-covid-19-tests-per-confirmed-case.csv')

df$Date <- as.Date(df$Date, format = "%b %d, %Y")

library(plotly)
library(ggplot2)
library(gganimate)

fig1 <- plot_ly(df, x = ~Date, y = ~NumberOfTestsPerConfirmedCase,
                type = 'scatter', mode = 'lines+markers', color = ~Entity)

fig1

p <- ggplot(df, aes(x = Entity, group = Entity, fill = Entity)) +
  geom_tile(aes(y = NumberOfTestsPerConfirmedCase / 2, height = NumberOfTestsPerConfirmedCase,
                width = 0.9), alpha = 0.8, show.legend = FALSE) +
  geom_text(aes(y = 0, label = paste(Entity, " ")), 
            vjust = 0.2, hjust = 1, size = 4) +
  geom_text(aes(y = NumberOfTestsPerConfirmedCase, label = NumberOfTestsPerConfirmedCase,
                hjust = 0), size = 4) +
  coord_flip(expand = TRUE, clip = "off") +
  theme_minimal() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold",     colour="red", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="red"),
        plot.caption =element_text(size=12, hjust=0.5, face="italic", color="red"),
        plot.background=element_blank(),
        plot.margin = margin(1,4, 1, 8, "cm")) +
  transition_states(Date, transition_length = 6, state_length = 1) +
  ease_aes('sine-in-out') +
  labs(title = "Number Tests per confirmed case on Date: {closest_state}") 

animate(p, 200, fps = 20,  width = 1200, height = 1000,
        renderer = gifski_renderer("gganim.gif"))

animate(p, 200, fps = 20,  width = 1200, height = 1000, 
        renderer = ffmpeg_renderer()) -> for_mp4anim_save("animation.mp4", animation = for_mp4 )
    

