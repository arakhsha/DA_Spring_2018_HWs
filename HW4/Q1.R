library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(highcharter)
library(ggthemes)

#Q1
bsa_rds = read_rds('timss_data/bsa.rds')
bts_rds = read_rds('timss_data/bts.rds')
btm_rds = read_rds('timss_data/btm.rds')
bst_rds = read_rds('timss_data/bst.rds')
theme_set(theme_gdocs())

teacherSatisfaction = rbind (
    bts_rds %>% select(idcntry:idlink, btbg10a:btbg10g) %>% mutate(subject = 'sci'),
    btm_rds %>% select(idcntry:idlink, btbg10a:btbg10g) %>% mutate(subject = 'math')
  ) %>%
  gather(key = "question", value = "answer", btbg10a:btbg10g) %>% 
  group_by(idcntry, idschool, itcourse, idtealin, idteach, idlink, subject) %>%
  summarise(satisfaction = 4 - mean(answer))

links = bst_rds %>% 
  select(idcntry:idsubj)
  
studentScores = bsa_rds %>% 
  select(idcntry:idstud, bsmmat01:bsssci05) %>%
  mutate(math = (bsmmat01 + bsmmat01 + bsmmat01 + bsmmat01 + bsmmat05) / 5,
         sci = (bsssci01 + bsssci02 + bsssci03 + bsssci04 + bsssci05) / 5) %>% 
  gather(key = "subject", value = "score", math:sci) %>% 
  group_by(idcntry, idbook, idschool, idclass, idstud, subject) %>%
  summarise(score = mean(score))

studentScores = left_join(
  studentScores,
  links
)

studentScores = left_join(
  studentScores,
  teacherSatisfaction
)

filteredScores = studentScores %>% 
  filter(!is.na(score), !is.na(satisfaction))

cor.test(
  filteredScores$satisfaction,
  filteredScores$score,
  alternative = "greater",
  method = 'spearman')

means = filteredScores %>% 
  group_by(satisfaction) %>% 
  summarise(avgScore = round(mean(score), 2))

plotData = filteredScores %>% rowwise() %>% sample_n(1000)
ggplot() +
  geom_point(data = plotData, aes(x = satisfaction, y = score), color = 'gray', alpha = 0.1) +
  geom_point(data = means, aes(x = satisfaction, y = avgScore), color = 'red') +
  geom_line(data = means, aes(x = satisfaction, y = avgScore), color = 'red')

plotData %>% 
  hchart(type = 'scatter',
         hcaes(x = satisfaction, y = score),
         color = 'rgba(130, 130, 130, 0.2)',
         enableMouseTracking = FALSE) %>% 
  hc_add_series(data = means, type = 'line', hcaes(x = satisfaction, y = avgScore), name = 'average', color = 'red')
  
  
  