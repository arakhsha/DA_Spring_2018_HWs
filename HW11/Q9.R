us.states = state.name %>% paste(collapse = "|") %>% tolower()

earthquakes = earthquakes %>% 
  mutate(year = time %>% as.POSIXlt() %>% .$year + 1900) %>% 
  mutate(country = str_extract(place,',.*$') %>% str_sub(2, -1)) %>% 
  mutate(country = ifelse(str_detect(tolower(country), us.states), 'United States', country))

avg.eq.year = earthquakes %>% 
  group_by(country, year) %>% 
  summarise(avg.mag = mean(mag, na.rm = T))

countries = unique(earthquakes$country)
results = data.frame()
for (cnt in countries) {
  countryData = earthquakes %>%
    filter(str_detect(country,cnt))
  if(length(unique(countryData$year)) < 2)
    next
  testResult = kruskal.test(mag ~ year, data = countryData)
  pvalue = testResult$p.value
  results = rbind(results, data.frame(cnt, pvalue))
}

harped = results %>% filter(pvalue < 0.001) %>% rename(country = cnt)

kable(harped)
