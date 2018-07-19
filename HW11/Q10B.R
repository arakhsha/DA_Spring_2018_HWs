testData = disaster %>% 
  filter(YEAR > 1900)

cor.test(testData$YEAR, testData$TOTAL_DEATHS, method = "spearman", alternative = "less")
