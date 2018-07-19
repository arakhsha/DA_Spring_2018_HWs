library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(highcharter)
library(ggplot2)


#Q1
bsa_rds = read_rds('timss_data/bsa.rds')
bsg_rds = read_rds('timss_data/bsg.rds')

homeState = bsg_rds %>% 
  select(idcntry:idgrade, bsbg06a:bsbg06g) %>% 
  gather(key = 'question', value = 'existence', bsbg06a:bsbg06g) %>% 
  group_by(idcntry, idbook, idschool, idclass, idstud, idgrade) %>% 
  summarise(state = 2 - mean(existence))

studentScores = bsa_rds %>% 
  select(idcntry:idstud, bsmmat01:bsssci05) %>%
  mutate(score = (bsmmat01 + bsmmat01 + bsmmat01 + bsmmat01 + bsmmat05 +
                    bsssci01 + bsssci02 + bsssci03 + bsssci04 + bsssci05) / 10) %>% 
  select(idcntry:idstud, score)

studentScores = left_join(
  studentScores,
  homeState
)

filteredScores = studentScores %>% 
  filter(!is.na(score), !is.na(state)) %>% 
  mutate(state = as.numeric(state), score = as.numeric(score))

summary(aov(
  score ~ state,
  data = filteredScores))

t.test(
  filteredScores %>% filter(state == 0) %>% .$score,
  filteredScores %>% filter(state == 1) %>% .$score
)


ggplot(filteredScores, aes(group = state, y = score, x = state, fill = as.factor(state))) +
  geom_boxplot() +
  labs(x = 'Home Facilities') +
  guides(fill = F)


hcboxplot(x = filteredScores$score,
          var = round(filteredScores$state, 1),
          outliers = F) %>%
  hc_chart(type = "column") %>%
  hc_title(text = "Home Facilities and Score Relation") %>%
  hc_xAxis(title = list(text = "Home Facilities")) %>%
  hc_yAxis(title = list(text = "Score")) 