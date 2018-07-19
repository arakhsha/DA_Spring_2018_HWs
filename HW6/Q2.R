pairs(selected, pch = 19, cex = 0.5)
library(mctest)
omcdiag(selected %>% select(-SalePrice), selected %>% select(SalePrice))