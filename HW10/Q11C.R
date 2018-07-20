GDPgrowthMigration = data %>% 
  select(-X63) %>% 
  filter(`Indicator Code` %in% c('NY.GDP.MKTP.KD.ZG', 'SM.POP.NETM')) %>% 
  gather("year", "value", `1960`:`2017`) %>% 
  drop_na() %>% 
  mutate(year = as.numeric(year)) %>% 
  arrange(year) %>% 
  group_by(`Country Name`, `Indicator Code`) %>% 
  summarise(lastValue = last(value)) %>% 
  spread("Indicator Code", "lastValue") %>% 
  rename(country = `Country Name`, gdpGrowth = `NY.GDP.MKTP.KD.ZG`, migration = `SM.POP.NETM`) %>% 
  mutate(gdpGrowth = round(gdpGrowth), migration = ifelse(migration > 0, "receiver", "Sender")) %>%
  drop_na()

senders = GDPgrowthMigration %>% filter(migration == "Sender") %>% .$gdpGrowth
receivers = GDPgrowthMigration %>% filter(migration == "receiver") %>% .$gdpGrowth
wilcox.test(senders, receivers)

paste("Senders Avg. GDP growth:", mean(senders))
paste("Receivers Avg. GDP growth:", mean(receivers))

GDPgrowthMigration %>% 
  select(country, migration) %>% 
  filter(country %in% c('Syrian Arab Republic', 'Germany', 'United States')) %>% 
  kable(caption = "Some Of GDP growth vs. net immigration data")
