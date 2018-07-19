Iran.PP = data %>% 
  filter(`Country Code` == 'IRN', `Indicator Code` == 'NE.CON.PRVT.PC.KD') %>% 
  gather(key = "year", value = "PP", `1967`:`2016`) %>% 
  select(year, PP) %>% 
  mutate(year = as.numeric(year))

ggplot(Iran.PP, aes(x = year, y = PP, group = 1, color = 1)) +
  geom_line() +
  guides(color = F) +
  labs(y = "Iran's Purchasing Power")

cor.test(Iran.PP$year, Iran.PP$PP, method = 'spearman')
