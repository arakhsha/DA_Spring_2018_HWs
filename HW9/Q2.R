thirteen = data %>% filter(as.integer(format(Date, "%d")) == 13) %>% 
  select(Open, Close)

wilcox.test(thirteen$Open, thirteen$Close, alternative = 'greater')
