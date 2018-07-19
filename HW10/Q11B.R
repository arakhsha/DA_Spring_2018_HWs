GDP.growth_GH.decrease = data %>% 
  select(-X63) %>% 
  filter(`Indicator Code` %in% c('NY.GDP.MKTP.KD.ZG', 'EN.ATM.GHGT.ZG')) %>% 
  gather("year", "value", `1960`:`2017`) %>% 
  drop_na() %>% 
  mutate(year = as.numeric(year)) %>% 
  arrange(year) %>% 
  group_by(`Country Name`, `Indicator Code`) %>% 
  summarise(lastValue = last(value)) %>% 
  spread("Indicator Code", "lastValue") %>% 
  rename(country = `Country Name`, ghDecrease = `EN.ATM.GHGT.ZG`, gdpGrowth = `NY.GDP.MKTP.KD.ZG`) %>% 
  mutate(ghDecrease = round(ghDecrease, 2), gdpGrowth = round(gdpGrowth, 2)) %>% 
  filter(!country %in% c("Equatorial Guinea", "Libya")) %>% 
  drop_na()

hchart(GDP.growth_GH.decrease, type = "scatter",regression = T,
       hcaes(x = ghDecrease, y = gdpGrowth)) %>% 
  hc_tooltip(pointFormat = "
             <b> {point.country} </b> <br/>
             Greenhouse Emission Change: {point.ghDecrease} <br/>
             GDP growth: {point.gdpGrowth} <br/>") %>%
  hc_xAxis(title = list(text = "Greenhouse Gas Emission Change Since 1990 (%)")) %>% 
  hc_yAxis(title = list(text = "GDP growth"))

cor.test(GDP.growth_GH.decrease$gdpGrowth, GDP.growth_GH.decrease$ghDecrease,
         method = "spearman")


