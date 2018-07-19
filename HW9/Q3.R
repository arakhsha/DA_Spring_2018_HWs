tradeVolume = data %>% 
  group_by(Date) %>% 
  summarise(tradeVolume = sum(Volume * `Adj Close`)) %>% 
  arrange(tradeVolume) %>% 
  top_n(10, wt = tradeVolume)

hchart(tradeVolume, type = 'column', hcaes(x = as.factor(Date), y = tradeVolume), name = 'Volume') %>% 
  hc_xAxis(title = list(text = 'Date'))
