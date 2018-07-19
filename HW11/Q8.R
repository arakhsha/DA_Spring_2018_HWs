testData = earthquakes %>% 
  select(depth, mag) %>% 
  drop_na()
cor.test(testData$depth, testData$mag, method = 'spearman')
