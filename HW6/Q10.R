finalVars = house %>% 
  select(topVars, SalePrice, Neighborhood, OverallCond, MSSubClass, YrSold) %>% 
  select(-GarageArea , -TotRmsAbvGrd) %>% 
  mutate(age = YearBuilt - YrSold,
         remoded = ifelse(YearBuilt == YearRemodAdd, 0, 1),
         OverallQual = (OverallQual - 2.85)^2) %>% 
  select(-YearBuilt, - YearRemodAdd, -YrSold) %>% 
  filter(-1299, -524)
finalVars$MSSubClass = as.factor(finalVars$MSSubClass)

finalModel = lm(SalePrice ~ .,
                data = finalVars)
summary(finalModel)

test = read_csv('test.csv') 
test = test %>% 
  mutate(age = YearBuilt - 2010,
         remoded = ifelse(YearBuilt == YearRemodAdd, 0, 1),
         OverallQual = (OverallQual - 2.85)^2 ) %>% 
  select(Id, topVars, age, Neighborhood, remoded, OverallCond, MSSubClass) %>% 
  select(-YearBuilt, - YearRemodAdd)
test$MSSubClass = as.factor(test$MSSubClass)
test[661, "TotalBsmtSF"] = mean(test$TotalBsmtSF, na.rm = TRUE)
test[1117, "GarageCars"] = mean(test$GarageCars, na.rm = TRUE)
test[1359, "MSSubClass"] = 120
testPred = predict.lm(finalModel, test)
result = data.frame(Id = test$Id, SalePrice = testPred)
write_csv(result, 'result.csv')
