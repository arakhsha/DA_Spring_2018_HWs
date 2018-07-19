transformed = selected %>% rename(age = YearBuilt)
transformed$age = PriceVsAge$age
transformed$OverallQual = PriceVsQual$OverallQual

model4 = lm(data = transformed,
            SalePrice ~ .- GarageArea - TotRmsAbvGrd)
summary(model4)
