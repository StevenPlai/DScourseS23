library(dplyr)
library(magrittr)
library(rvest)
library(cfbfastR)
library(tidyverse)

page <- read_html("https://en.wikipedia.org/wiki/List_of_Heisman_Trophy_winners")

table <- page %>% html_elements(".wikitable") 
table <- table[[2]] %>% html_table()

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

ggplot(data=analysis %>% filter(position=="Fullback"),aes(x=year,y=avg)) +
  geom_point() + geom_smooth()
