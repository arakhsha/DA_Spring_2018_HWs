library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(highcharter)
library(ggplot2)


#Q9
bsa_rds = read_rds('timss_data/bsa.rds')
bsg_rds = read_rds('timss_data/bsg.rds')

presence = bsg_rds %>% 
  select(idcntry:idgrade, bsbg11) %>% 
  rename(presence = bsbg11) %>% 
  mutate(presence = as.numeric(presence))

studentScores = bsa_rds %>% 
  select(idcntry:idstud, bsmmat01:bsssci05) %>%
  mutate(score = (bsmmat01 + bsmmat01 + bsmmat01 + bsmmat01 + bsmmat05 +
                    bsssci01 + bsssci02 + bsssci03 + bsssci04 + bsssci05) / 10) %>% 
  select(idcntry:idstud, score)

studentScores = left_join(
  studentScores,
  presence
)

filteredScores = studentScores %>% 
  filter(!is.na(score), !is.na(presence)) %>% 
  mutate(presence = as.numeric(presence), score = as.numeric(score))

summary(aov(
  score ~ presence,
  data = filteredScores))

t.test(
  filteredScores %>% filter(presence == 1) %>% .$score,
  filteredScores %>% filter(presence == 4) %>% .$score
)


ggplot(filteredScores, aes(group = presence, y = score, x = presence, fill = as.factor(presence))) +
  geom_boxplot() +
  labs(x = 'Presence In Class', title = 'Presence In Class and Score Relation') +
  guides(fill = F)


hcboxplot(x = filteredScores$score,
          var = round(filteredScores$presence, 1),
          outliers = F) %>%
  hc_chart(type = "column") %>%
  hc_title(text = "Presence In Class and Score Relation") %>%
  hc_xAxis(title = list(text = "Presence In Class")) %>%
  hc_yAxis(title = list(text = "Score")) 

