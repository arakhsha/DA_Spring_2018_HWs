numericVarnames = colnames(house)[sapply(house, typeof) == 'integer']
numericVars = house %>% 
  select(numericVarnames)

corr_mat = cor(numericVars, use="pairwise.complete.obs")
#sort on decreasing correlations with SalePrice
cor_sorted <- as.matrix(sort(corr_mat[,'SalePrice'], decreasing = TRUE))
#select only high corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) TRUE)))
corr_mat <- corr_mat[CorHigh, CorHigh]

p_mat = corr_mat
for(varName1 in numericVarnames) {
  for(varName2 in numericVarnames) {
    p_mat[varName1, varName2] = cor.test(numericVars[[varName1]], numericVars[[varName2]])$p.value
  }
}


corrplot.mixed(corr_mat, tl.col="black", tl.pos = "lt", tl.cex = 0.8,
               number.cex = 0.5, number.digits = 1, lower.col = 'black')

