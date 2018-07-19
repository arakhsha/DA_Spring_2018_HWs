apple = data %>% filter(Symbol == 'AAPL')
kvalues = 1:15
set.seed(17)
error = numeric(length(kvalues))
model = list()
library(h2o)
h2o.init()
for(k in kvalues) {
  modelData = apple %>% 
    arrange(Date) %>% 
    slice(1:(nrow(apple) - nrow(apple) %% (k + 1))) %>% 
    .$Open %>% 
    matrix(ncol = k + 1, byrow = T) %>% 
    as.data.frame()
  colnames(modelData) = c(paste('day', 1:k), 'final')
  model[[k]] = h2o.glm(y = 'final', x = paste('day', 1:k),
                       training_frame = as.h2o(modelData), nfolds = 5)
  error[k] = h2o.mse(model[[k]], xval = T)
}
# error
ggplot(data.frame(k = kvalues, error), aes(x = k, y = error)) + 
  geom_point() +
  geom_line() +
  theme_minimal()

paste('Min Error is for k =', which.min(error), 'with mse =', error[which.min(error)])
