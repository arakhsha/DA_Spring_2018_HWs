n <- nrow(selected)
set.seed(17)
part1indexes = sample(1:n, 4 * n / 5)
part1 = selected %>% slice(part1indexes)
part2 = selected %>% slice(-part1indexes)

model3 = lm(data = part1,
            SalePrice ~ . - GarageArea - TotRmsAbvGrd)

MSE = mean((part2$SalePrice - predict.lm(model3, part2))^2)
MSE
sqrt(MSE)

