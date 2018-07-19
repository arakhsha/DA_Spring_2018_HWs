RealVSPred = selected %>% mutate(pred = fitted(model1)) %>% select(SalePrice, pred)
ggplot(RealVSPred, aes(x = SalePrice, y = pred)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = 'red')
