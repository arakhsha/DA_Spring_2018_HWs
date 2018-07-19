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

immigration = bsg_rds %>% 
  select(idcntry:idgrade, bsbg10a) %>% 
  rename(immigration = bsbg10a) %>% 
  mutate(immigration = ifelse(immigration == 1, 'no', 'yes'))

studentScores = bsa_rds %>% 
  select(idcntry:idstud, bsmmat01:bsssci05) %>%
  mutate(score = (bsmmat01 + bsmmat01 + bsmmat01 + bsmmat01 + bsmmat05 +
                    bsssci01 + bsssci02 + bsssci03 + bsssci04 + bsssci05) / 10) %>% 
  select(idcntry:idstud, score)

studentScores = left_join(
  studentScores,
  immigration
)

filteredScores = studentScores %>% 
  filter(!is.na(score), !is.na(immigration)) %>% 
  mutate(score = as.numeric(score))

t.test(
  formula = score ~ immigration,
  data = filteredScores,
  alternative = 'less'
)

means = filteredScores %>% 
  group_by(immigration) %>% 
  summarise(avgScore = round(mean(score), 2))

plotData = filteredScores %>% rowwise() %>% sample_n(1000)


ggplot(data = filteredScores, aes(fill = immigration, x = score)) +
  geom_density(alpha = 0.3) +
  geom_vline(data = means, aes(xintercept = avgScore, color = immigration)) +
  scale_x_continuous(limits = c(0, 1000)) +
  theme(legend.position = 'bottom', legend.direction = 'horizontal') +
  labs(fill = 'Immigirant', color  = 'Immigirant')

immigirant <- filteredScores %>% filter(immigration == 'yes') %>% .$score
nonimmigirant <- filteredScores %>% filter(immigration == 'no') %>% .$score

hchart(type = "area",
       density(immigirant),
       name = 'Immigirant') %>% 
  hc_add_series(type = 'area',
                data = density(nonimmigirant),
                name = 'Nonimmigirant')
