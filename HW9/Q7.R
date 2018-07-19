applePCA = prcomp(apple %>% select(Open, High, Low, Close, Volume), scale = T)
appleFirstPCA = data.frame(Date = apple$Date, PC1 = applePCA$x[,1])
kvalues = 1:15
set.seed(17)
error = numeric(length(kvalues))
model = list()
for(k in kvalues) {
  finals = apple %>% 
    filter(row_number() %% (k+1) == 0) %>% 
    .$Open
  modelData = appleFirstPCA %>% 
    arrange(Date) %>% 
    slice(1:(nrow(appleFirstPCA) - nrow(appleFirstPCA) %% (k + 1))) %>% 
    .$PC1 %>% 
    matrix(ncol = k + 1, byrow = T) %>% 
    as.data.frame()
  colnames(modelData) = c(paste('day', 1:k), 'final')
  modelData$final = finals
  model[[k]] = h2o.glm(y = 'final', x = paste('day', 1:k), family = 'gaussian',
                       training_frame = as.h2o(modelData), nfolds = 5)
  error[k] = h2o.mse(model[[k]], xval = T)
}
error
ggplot(data.frame(k = kvalues, error), aes(x = k, y = error)) + 
  geom_point() +
  geom_line() +
  theme_minimal()

paste('Min Error is for k =', which.min(error), 'with mse =', error[which.min(error)])
