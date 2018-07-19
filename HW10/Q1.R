poorestCountries = data %>% 
  filter(`Indicator Code` == 'NY.GDP.PCAP.PP.CD') %>% 
  select(`Country Name`, `Country Code`, GDP = `2016`) %>% 
  arrange(GDP) %>% 
  top_n(10, -GDP) %>% 
  mutate(GDP = round(GDP))

PHR = data %>% 
  select(-X63) %>% 
  filter(`Indicator Code` == 'SI.POV.LMIC') %>% 
  gather("year", "value", `1960`:`2017`) %>% 
  drop_na() %>% 
  mutate(year = as.numeric(year)) %>% 
  arrange(year) %>% 
  group_by(`Country Name`, `Indicator Code`) %>% 
  summarise(lastValue = last(value)) %>% 
  spread("Indicator Code", "lastValue") %>% 
  rename(PHR = `SI.POV.LMIC`) %>% 
  drop_na()


LE = data %>% 
  select(-X63) %>% 
  filter(`Indicator Code` == 'SP.DYN.LE00.IN') %>% 
  gather("year", "value", `1960`:`2017`) %>% 
  drop_na() %>% 
  mutate(year = as.numeric(year)) %>% 
  arrange(year) %>% 
  group_by(`Country Name`, `Indicator Code`) %>% 
  summarise(lastValue = last(value)) %>% 
  spread("Indicator Code", "lastValue") %>% 
  rename(LE = `SP.DYN.LE00.IN`) %>% 
  drop_na()



poorestCountries = poorestCountries %>% 
  left_join(PHR) %>% 
  left_join(LE)

poorestCountries %>% 
  hchart(type = 'column', hcaes(x = `Country Name`, y = GDP), name = 'GDP per capita') %>% 
  hc_yAxis(title = list(text = "GDP per capita")) %>% 
  hc_xAxis(title = list(text = "")) %>% 
  hc_title(text = "Poorest Countries")

poorestCountries %>% 
  hchart(type = 'column', hcaes(x = `Country Name`, y = PHR), name = 'Poverty gap') %>% 
  hc_yAxis(title = list(text = "Poverty gap at $5.50 a day")) %>% 
  hc_xAxis(title = list(text = "")) %>% 
  hc_title(text = "Poverty gap of Poorest Countries")


poorestCountries %>% 
  hchart(type = 'column', hcaes(x = `Country Name`, y = LE), name = 'Life Expectancy') %>% 
  hc_yAxis(title = list(text = "Life Expectancy")) %>% 
  hc_xAxis(title = list(text = "")) %>% 
  hc_title(text = "Life Expectancy of Poorest Countries")


