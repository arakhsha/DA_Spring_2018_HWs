PriceVsQual = house %>% filter(-1299, -524) %>% 
  select(OverallQual, SalePrice)

varName = 'OverallQual'
ggplot(PriceVsQual, aes(y = SalePrice, x = OverallQual)) +
  geom_point(color = 'dodgerblue', alpha = 0.1) +
  geom_smooth(method = 'lm', color = 'red') +
  geom_smooth(method = 'lm', formula = y ~ x + I(x^2),color = 'green') + 
  labs(title = paste('SalePrice vs teransformed OverallQual'), x = 'teransformed OverallQual')

OQModel = lm(PriceVsQual, formula = SalePrice ~ OverallQual + I(OverallQual^2))
summary(OQModel)

PriceVsQual$OverallQual = (PriceVsQual$OverallQual - 2.85)^2


varName = 'OverallQual'
ggplot(PriceVsQual, aes(y = SalePrice, x = OverallQual)) +
  geom_point(color = 'dodgerblue', alpha = 0.1) +
  geom_smooth(method = 'lm', color = 'red') +
  geom_smooth(color = 'green') + 
  labs(title = paste('SalePrice vs teransformed OverallQual'), x = 'teransformed OverallQual')
