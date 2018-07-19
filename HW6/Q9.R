for(varName in topVarsCor$var) {
  p = ggplot(selected, aes(y = SalePrice, x = selected[[varName]])) +
    geom_point(color = 'dodgerblue', alpha = 0.1) +
    geom_smooth(method = 'lm', color = 'red') +
    geom_smooth(color = 'green') + 
    labs(title = paste('SalePrice vs ', varName), x = varName)
  print(p)
}