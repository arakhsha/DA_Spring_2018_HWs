stocksData = data %>% 
  group_by(Date, Sector) %>% 
  summarize(Open = mean(Open)) %>% 
  spread(key = 'Sector', value = 'Open') %>% 
  select(-`<NA>`) %>% 
  ungroup()

indexes = read_csv('data/indexes.csv')
stocksData = stocksData %>% 
  left_join(indexes) %>% 
  na.omit() 

stocksPCA = prcomp(stocksData %>% select(-Date), scale = T)

library(ggbiplot)
ggbiplot(stocksPCA, 1:2) +
  theme_minimal()
