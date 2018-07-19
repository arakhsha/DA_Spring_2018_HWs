PriceVsAge = house %>% filter(-1299, -524) %>% 
  mutate(age = 2010 - YearBuilt) %>% 
  select(age, SalePrice)

varName = 'age'
ggplot(PriceVsAge, aes(y = SalePrice, x = age)) +
  geom_point(color = 'dodgerblue', alpha = 0.1) +
  geom_smooth(method = 'lm', color = 'red') +
  geom_smooth(method = 'lm', formula = y ~ x + I(x^2),color = 'green') + 
  labs(title = paste('SalePrice vs ', varName), x = varName)

ageModel = lm(PriceVsAge, formula = SalePrice ~ age + I(age^2))
summary(ageModel)

PriceVsAge$age = (PriceVsAge$age - 87)^2
ggplot(PriceVsAge, aes(y = SalePrice, x = age)) +
  geom_point(color = 'dodgerblue', alpha = 0.1) +
  geom_smooth(method = 'lm', color = 'red') +
  geom_smooth(color = 'green') + 
  labs(title = paste('SalePrice vs teransformed age'), x = 'teransformed age')
