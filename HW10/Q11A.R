GDP.growth_Young.pop = data %>% 
  select(-X63) %>% 
  filter(`Indicator Code` %in% c('NY.GDP.MKTP.KD.ZG', 'SP.POP.1564.TO.ZS')) %>% 
  gather("year", "value", `1960`:`2017`) %>% 
  drop_na() %>% 
  mutate(year = as.numeric(year)) %>% 
  arrange(year) %>% 
  group_by(`Country Name`, `Indicator Code`) %>% 
  summarise(lastValue = last(value)) %>% 
  spread("Indicator Code", "lastValue") %>% 
  rename(country = `Country Name`, youngPop = `SP.POP.1564.TO.ZS`, gdpGrowth = `NY.GDP.MKTP.KD.ZG`) %>% 
  mutate(youngPop = round(youngPop, 2), gdpGrowth = round(gdpGrowth, 2)) %>% 
  filter(country != "Libya") %>% 
  left_join(eco.clus.results) %>% 
  mutate(type = ifelse(cluster == 1, 'Undeveloped', 'Developed')) %>% 
  drop_na()


hchart(GDP.growth_Young.pop, type = "scatter",
       hcaes(x = youngPop, y = gdpGrowth, group = type, name = type)) %>% 
  hc_tooltip(pointFormat = "<b> {point.country} </b> <br/>
             Young Population: {point.youngPop} <br/>
             GDP growth: {point.gdpGrowth} <br/>") %>% 
  hc_xAxis(title = list(text = "Population ages 15-64 (% of total)")) %>% 
  hc_yAxis(title = list(text = "GDP growth"))


cor.test(GDP.growth_Young.pop$gdpGrowth, GDP.growth_Young.pop$youngPop,
         method = "spearman",
         alternative = "greater")
