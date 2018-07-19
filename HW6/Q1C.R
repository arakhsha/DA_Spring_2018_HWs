corr_tidy = melt(corr_mat)
topVarsCor = corr_tidy %>% 
  filter(Var2 == 'SalePrice', Var1 != 'SalePrice') %>% 
  arrange(desc(value)) %>% 
  slice(1:10) %>% 
  select(var = Var1, cor = value)

topVars = as.character(topVarsCor$var)

selected = house %>% select(topVars, SalePrice)

selected_corr_mat = cor(selected)
corrplot.mixed(selected_corr_mat, tl.col="black", tl.pos = "lt", tl.cex = 0.8,
               number.cex = 0.5, number.digits = 1, lower.col = 'black')
