y = selected$SalePrice
rsquare = sum((fitted(model1) - mean(y))^2) / sum((y - mean(y))^2)
rsquare