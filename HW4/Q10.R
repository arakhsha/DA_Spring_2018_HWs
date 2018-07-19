library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(highcharter)
library(ggthemes)
theme_set(theme_gdocs())

#Q10
bsa_rds = read_rds('timss_data/bsa.rds')
studentScores = bsa_rds %>% 
  select(idcntry:idstud, bsmapp01:bsmrea05, bssapp01:bssrea05) %>% 
  filter(idcntry == 364) %>% 
  group_by(idcntry, idbook, idschool, idclass, idstud) %>%
  summarise(
    applying = mean(c(bsmapp01:bsmapp05, bssapp01:bssapp05)),
    reasoning = mean(c(bsmrea01:bsmrea05, bssrea01:bssrea05))
  ) %>% 
  gather(key = 'type', value = 'score', applying:reasoning) %>% 
  unique()


applying <- studentScores %>% filter(type == 'applying') %>% .$score
reasoning <- studentScores %>% filter(type == 'reasoning') %>% .$score

t.test(reasoning,
       applying, 
       alternative = 'less') 

ggplot(data = studentScores, aes(group = type, fill = type, x = score)) +
  geom_density(alpha = 0.3) +
  scale_fill_gdocs() +
  labs(title = 'Applying Scores vs Reasoning Scores for Iranian Students') +
  
  
hchart(type = "area",
         density(applying),
         name = 'applying') %>% 
  hc_add_series(type = 'area',
                data = density(reasoning),
                name = 'reasoning') %>% 
  hc_title(text = "Applying Scores vs Reasoning Scores for Iranian Students")