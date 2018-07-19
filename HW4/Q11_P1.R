library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(highcharter)
library(readxl)
library(weights)
library(ggplot2)
library(gridExtra)
library(coin)


#Q9
bsa_rds = read_rds('timss_data/bsa.rds')
bsg_rds = read_rds('timss_data/bsg.rds')

gamingSystem = bsg_rds %>% 
  select(idcntry:idgrade, bsbg06g) %>% 
  rename(gamingSystem = bsbg06g) %>% 
  mutate(gamingSystem = ifelse(gamingSystem == 1, 'yes', 'no'))

studentScores = bsa_rds %>% 
  select(idcntry:idstud, bsmmat01:bsssci05) %>%
  mutate(score = (bsmmat01 + bsmmat01 + bsmmat01 + bsmmat01 + bsmmat05 +
                    bsssci01 + bsssci02 + bsssci03 + bsssci04 + bsssci05) / 10) %>% 
  select(idcntry:idstud, score)

studentScores = left_join(
  studentScores,
  gamingSystem
)

filteredScores = studentScores %>% 
  filter(!is.na(score), !is.na(gamingSystem)) %>% 
  mutate(score = as.numeric(score))

t.test(
  formula = score ~ gamingSystem,
  data = filteredScores,
  alternative = 'less'
)

means = filteredScores %>% 
  group_by(gamingSystem) %>% 
  summarise(avgScore = round(mean(score), 2))

plotData = filteredScores %>% rowwise() %>% sample_n(1000)


ggplot(data = filteredScores, aes(group = gamingSystem, fill = gamingSystem, x = score)) +
  geom_density(alpha = 0.3) +
  geom_vline(data = means, aes(xintercept = avgScore, color = gamingSystem)) +
  scale_x_continuous(limits = c(0, 1000)) +
  theme(legend.position = 'bottom', legend.direction = 'horizontal') +
  labs(fill = 'Gaming Systam', color  = 'Gaming Systam')

withSystem <- filteredScores %>% filter(gamingSystem == 'yes') %>% .$score
withoutSystem <- filteredScores %>% filter(gamingSystem == 'no') %>% .$score

hchart(type = "area",
       density(withSystem),
       name = 'With Gaming System') %>% 
  hc_add_series(type = 'area',
                data = density(withoutSystem),
                name = 'Without Gaming System')
