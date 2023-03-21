library(dplyr)
library(magrittr)
library(cfbfastR)
library(tidyverse)
library(ggthemes)

picks <- data.frame()  
for(i in 2000:2022){
  df <- cfbd_draft_picks(i)
  picks <- bind_rows(picks,df)
}

analysis <- picks %>% group_by(year) %>% mutate(capital=abs(overall-max(overall)-1)) %>% 
  ungroup() %>% group_by(position,year) %>% 
  summarise(capital=sum(capital),
            n = n()) %>% 
  mutate(avg = capital/n)

plot1 <- ggplot(data=analysis %>% filter(position=="Fullback"),aes(x=year,y=capital)) +
  geom_smooth(linewidth=3) +
  geom_point(size=5) +
  labs(x="Draft Year",y="Total Draft Capital",
       title="Total NFL Draft Capital Allocated to Fullbacks by Year",
       caption="For reference: the first overall pick is equal to roughly 250 capital, the last pick is equal to 1 capital") +
  theme_fivethirtyeight() + 
  theme(panel.background = element_rect(fill ="white"),
        plot.background = element_rect(fill="white"),
        plot.title = element_text(size=22,hjust=.5),
        plot.subtitle = element_text(size=20,hjust=.5),
        axis.text = element_text(size=15),
        axis.title = element_text(size=20),
        axis.title.x = element_blank(),
        plot.caption = element_text(size=12),
        legend.position = "none")

ggsave("PS6a_Plaisance.png",plot1)

plot2 <- ggplot(data=analysis %>% filter(position=="Quarterback"),aes(x=year,y=avg)) +
  geom_smooth(linewidth=3) +
  geom_point(size=5) +
  labs(x="Draft Year",y="Average Draft Capital",
       title="Average NFL Draft Capital Allocated to Quarterbacks by Year",
       caption="For reference: the first overall pick is equal to roughly 250 capital, the last pick is equal to 1 capital") +
  theme_fivethirtyeight() + 
  theme(panel.background = element_rect(fill ="white"),
        plot.background = element_rect(fill="white"),
        plot.title = element_text(size=20,hjust=.5),
        plot.subtitle = element_text(size=20,hjust=.5),
        axis.text = element_text(size=15),
        axis.title = element_text(size=20),
        axis.title.x = element_blank(),
        plot.caption = element_text(size=12),
        legend.position = "none")

ggsave("PS6b_Plaisance.png",plot2)

plot3 <- ggplot(data=analysis %>% filter(position=="Inside Linebacker"),aes(x=year,y=capital)) +
  geom_smooth(linewidth=3) +
  geom_point(size=5) +
  labs(x="Draft Year",y="Total Draft Capital",
       title="Total NFL Draft Capital Allocated to Inside Linebackers by Year",
       caption="For reference: the first overall pick is equal to roughly 250 capital, the last pick is equal to 1 capital") +
  theme_fivethirtyeight() + 
  theme(panel.background = element_rect(fill ="white"),
        plot.background = element_rect(fill="white"),
        plot.title = element_text(size=18,hjust=.5),
        plot.subtitle = element_text(size=20,hjust=.5),
        axis.text = element_text(size=15),
        axis.title = element_text(size=20),
        axis.title.x = element_blank(),
        plot.caption = element_text(size=12),
        legend.position = "none")

ggsave("PS6c_Plaisance.png",plot3)

