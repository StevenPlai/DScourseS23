library(dplyr)
library(magrittr)
library(mice)
library(modelsummary)
library(readr)
library(kableExtra)
library(tidyr)

wages <- read_csv("wages.csv") %>% filter(!is.na(tenure),!is.na(hgc)) %>% 
  mutate(tenure2=tenure^2)

table <- datasummary_skim(data=wages)

save_kable(table,file = "summaryTable.png",zoom=1.5)

model1 <- lm(logwage ~ hgc + college + tenure + tenure2 + age + married,
             wages %>% filter(!is.na(logwage)))

model2 <- lm(logwage ~ hgc + college + tenure + tenure2 + age + married,
             wages %>% replace_na(list(logwage=mean(wages$logwage,na.rm=T))))

wages2 <- wages %>% filter(is.na(logwage)) 
pred <- predict(model1,wages2)
wages2$logwage <- pred
wages2 %<>% bind_rows(wages %>% filter(!is.na(logwage)))

model3 <- lm(logwage ~ hgc + college + tenure + tenure2 + age + married,
             wages2)

dat_mice <- mice(wages, m = 5, printFlag = FALSE)

model4 <- with(dat_mice, lm(logwage ~ hgc + college + tenure + tenure2 + age + married)) 

table <- modelsummary(list(model1,model2,model3,model4))

save_kable(table,file = "modelTable.png",zoom=1.5)
