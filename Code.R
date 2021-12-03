library(tidyverse)
library(tidytuesdayR)
tuesdata <- tidytuesdayR::tt_load('2021-11-30')
tuesdata$matches->matches
glimpse(matches)
library(lubridate)
library(jpeg)
library(cowplot)

matches%>%
  mutate(score=ifelse(score_team1>score_team2, score_team1,score_team2))%>%
  select(score,match_date,winner,series,venue)%>%
  mutate(match_date=mdy(match_date))%>%
  drop_na()->data

glimpse(matches)


matches%>%
  mutate(score=(ifelse(team1=="India",score_team1,NA_character_)))%>%
  drop_na()%>%
  select(score,series,venue,match_date,winner)%>%
  mutate(match_date=mdy(match_date))%>%
  drop_na()->data1
  
matches%>%
  mutate(score=(ifelse(team2=="India",score_team2,NA_character_)))%>%
  drop_na()%>%
  select(score,series,venue,match_date,winner)%>%
  mutate(match_date=mdy(match_date))%>%
  drop_na()->data2

rbind(data1,data2)->data
data%>%
  mutate(year=year(match_date))->data
data%>%
  mutate(fill=ifelse(winner=="India","blue","gray50"))->data

data$id <- seq(1, nrow(data))
data
label_data <- data
number_of_bar <- nrow(data)
angle <- 90 - 360 * (label_data$id-0.5)/number_of_bar     
label_data$hjust <- ifelse(angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

as.numeric(data$score)->data$score
data

ggplot(data, aes(x=as.factor(id), y=score,fill=fill)) +       
  geom_bar(aes(x=as.factor(id), y=score, fill=fill), colour="white",stat="identity", alpha=1) +
  ylim(-1200,1200) +
  coord_polar(start=0) +
  scale_fill_manual(values=c('green',"gray50"),labels=c("Won","Lost"))+
  geom_text(data=label_data, aes(x=id, y=score+30, label=paste("The", series,",",year),hjust=hjust), color="white", fontface="bold",alpha=0.8, size=1.5, angle= label_data$angle, inherit.aes = FALSE )+
  theme(plot.background = element_rect(fill="black"),
        panel.background = element_rect(fill="black"),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(c(.5, 1, .5, 1), "cm"),
        panel.border = element_blank(),
        legend.position = "top",
        legend.text = element_text(colour="white",size=10,face="bold"),
        legend.title = element_blank(),
        legend.background = element_rect(fill="black"),
        legend.key = element_rect(fill="black"),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title=element_text(size=16, colour = "white", face="bold"),
        plot.subtitle=element_text(size=12, colour="white", margin=margin(b=15)),
        plot.caption=element_text(hjust=0, size=9, colour="white",margin=margin(t=15)))+
  labs(title="ODIs THAT INDIA PLAYED FROM 1996-2005",
       subtitle=str_wrap("The below data visualization shows all the One Day International matches that India played from 1996 to 2005. The length of the bar is a reflection of the score",75),
       caption = "Data from ESPN Cricinfo via Tidy Tuesday| Analysis and design: @annapurani93")->plot

library(ggtext)
readJPEG("C:/Users/Annapurani/Desktop/image.jpg")->logo
ggdraw(plot) +
  draw_image(logo, x = +.002, y =-.075, scale = .19)->plot1

ggsave("cricketgrond.png",plot1,width=10,height=8)
ggsave("cricketgrond.pdf",plot1,width=10,height=8)


