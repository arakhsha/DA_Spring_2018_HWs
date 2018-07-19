GDP.growth= data %>% 
  filter(`Indicator Code` == 'NY.GDP.MKTP.KD') %>% 
  gather(key = 'year', value = 'current', `1960`:`2015`) %>% 
  select(`Country Name`, year, current) %>% 
  group_by(`Country Name`) %>% 
  arrange(year) %>% 
  mutate(year = as.numeric(year),
          start = year - 10,
         before = lag(current, n = 10),
         growth = (current / lag(current, n = 10)) * 100 - 100) %>% 
  drop_na() %>% 
  top_n(1, wt = growth) %>% 
  ungroup() %>% 
  arrange(-growth) %>% 
  mutate(rank = row_number()) %>% 
  top_n(20, wt = growth) %>% 
  rename(end = year, final = current)

kable(GDP.growth, caption = "Greatest Economic Explosions!")
