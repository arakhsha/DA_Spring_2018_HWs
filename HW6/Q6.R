model2 = lm(data = selected,
            SalePrice ~ . - GarageArea - TotRmsAbvGrd)
summary(model2)
