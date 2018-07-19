sp500 = indexes %>% 
  select(Date, SP500) %>% 
  mutate(diff = (SP500 - lag(SP500)) / lag(SP500)) %>% 
  mutate(result = ifelse(diff > 0, 1, 0))
ggplot(sp500, aes(x = diff)) +
  geom_density() +
  xlim(-25, 25)

qqnorm(sp500$diff)
qqline(sp500$diff)



sp500resultPrediction = 
  cbind(data.frame(Date = pcaData$Date),
                   pca$x[,1:10]) %>% 
  inner_join(sp500 %>% select(Date, result)) %>% 
  na.omit()

trainIndexes = sample(1:nrow(sp500resultPrediction), nrow(sp500resultPrediction) * 4 / 5)
train = sp500resultPrediction %>% slice(trainIndexes)
test = sp500resultPrediction %>% slice(-trainIndexes)
sp500predictionModel = glm(result ~ .,
                            train %>% select(-Date),
                            family = 'binomial')

y = test$result
yhat = predict.glm(sp500predictionModel, test, type = 'response')
thresholds = seq(0, 1, 0.005)
predPerformance = data.frame()
for(t in thresholds) {
  E1 = sum(y == 1 & yhat < t) / sum(y == 1)
  E2 = sum(y == 0 & yhat > t) / sum(y == 0)
  ACC = 1 - (E1 * sum(y == 1) + E2 * sum(y == 0)) / length(y)
  predPerformance = rbind(predPerformance, data.frame(threshold = t, E1, E2, ACC))
}

bestIndex = which.max(predPerformance$ACC)
paste('ACC =', predPerformance[bestIndex,]$ACC, 'for threshold =', predPerformance[bestIndex,]$threshold)


