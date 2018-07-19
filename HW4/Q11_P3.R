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


bsg_rds = read_rds('timss_data/bsg.rds')

educationLength = bsg_rds %>% 
  select(idcntry:idgrade, bsbg08, itsex) %>% 
  rename(educationLength = bsbg08) %>% 
  filter(!is.na(educationLength), !is.na(itsex))

oneway_test(
  educationLength ~ as.factor(itsex),
  data = educationLength,
  alternative = 'greater'
)

plotData = educationLength %>% 
  group_by(itsex) %>% 
  mutate(totalCount = n()) %>% 
  group_by(itsex, educationLength, totalCount) %>% 
  summarise(count = n()) %>% 
  mutate(prop = count / totalCount) %>% 
  ungroup() %>% 
  mutate(itsex = ifelse(itsex == 1, 'Female', 'Male'))

ggplot(plotData, aes(x = educationLength, fill = as.factor(itsex), y = prop)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  labs(fill = 'Gender', x = 'Education Length', y = 'Proportion')


  