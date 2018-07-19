library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(highcharter)
library(ggthemes)

theme_set(theme_gdocs())

#Q2
bsa_rds = read_rds('timss_data/bsa.rds')
bsg_rds = read_rds('timss_data/bsg.rds')

parentEducation = bsg_rds %>% 
  select(idcntry:idgrade, bsbg07a:bsbg07b) %>% 
  rename(mother = bsbg07a, father = bsbg07b) %>% 
  gather(key = 'parent', value = 'education', mother:father) %>% 
  filter(education != 8) 

studentScores = bsa_rds %>% 
  select(idcntry:idstud, bsmmat01:bsssci05) %>%
  mutate(score = (bsmmat01 + bsmmat01 + bsmmat01 + bsmmat01 + bsmmat05 +
                    bsssci01 + bsssci02 + bsssci03 + bsssci04 + bsssci05) / 10) %>% 
  select(idcntry:idstud, score)

studentScores = left_join(
  studentScores,
  parentEducation
)

filteredScores = studentScores %>% 
  filter(!is.na(score), !is.na(education)) %>% 
  mutate(education = as.numeric(education), score = as.numeric(score))

summary(aov(
  score ~ education,
  data = filteredScores))

t.test(
  filteredScores %>% filter(education == 1) %>% .$score,
  filteredScores %>% filter(education == 7) %>% .$score
)


ggplot(filteredScores, aes(group = education, y = score, x = education, fill = as.factor(education))) +
  geom_boxplot() +
  labs(x = 'Education Level') +
  guides(fill = F)


hcboxplot(x = filteredScores$score,
          var = filteredScores$education,
          outliers = F) %>%
  hc_chart(type = "column") %>%
  hc_title(text = "Parent's Education and Score Relation") %>%
  hc_xAxis(title = list(text = "Education Level")) %>%
  hc_yAxis(title = list(text = "Score")) 

