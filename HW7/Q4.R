yHat = predict.glm(model, type = 'response')
plotData <- data.frame(yHat, MannerOfDeath = ifelse(data$MannerOfDeath == 0, "0 (Suicide)", "1 (Homicide)"))
plotData %>% 
  ggplot(aes(x = yHat, color = MannerOfDeath)) +
  geom_density() +
  labs(x = 'Predict', y = 'density')

ggplot(plotData, aes(x = MannerOfDeath, y = yHat, color = MannerOfDeath)) +
  geom_boxplot() +
  labs(x = 'Actual', y = 'Prediction') +
  guides(color = F)

plotData <- data.frame(Age = data$Age, yHat, MannerOfDeath = ifelse(data$MannerOfDeath == 0, "0 (Suicide)", "1 (Homicide)"))
ggplot(plotData, aes(x = Age, y = yHat, color = MannerOfDeath)) +
  geom_jitter(size = 0.1) +
  labs(x = 'Age', y = 'Prediction', title = 'Prediction By Age') 

