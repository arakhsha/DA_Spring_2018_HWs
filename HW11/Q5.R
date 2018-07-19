disastersByCountry = 
  disaster %>% 
  group_by(COUNTRY) %>% 
  summarise(count = n(), 
            avg.deaths = round(mean(TOTAL_DEATHS, na.rm = T)),
            log.avg.deaths = log10(round(mean(TOTAL_DEATHS, na.rm = T))))
disastersByCountry[tolower(disastersByCountry$COUNTRY) == 'czech republic', 'COUNTRY'] <- 'Czechia'
disastersByCountry[tolower(disastersByCountry$COUNTRY) %in% c('usa', 'usa territory'), 'COUNTRY'] = 'United States'
disastersByCountry[tolower(disastersByCountry$COUNTRY) %in% c('uk', 'uk territory'), 'COUNTRY'] = 'United Kingdom'

codes = codelist %>% 
  mutate(COUNTRY = tolower(country.name.en)) %>% 
  select(code = iso2c, COUNTRY)

disastersByCountry = disastersByCountry %>% 
  mutate(COUNTRY = tolower(COUNTRY)) %>% 
  left_join(codes) %>% 
  drop_na()


hcmap(data = disastersByCountry,
      value = 'avg.deaths',
      joinBy = c('hc-a2', 'code'),
      name = "Average Deaths") %>% 
  hc_title(text = "Average Deaths In Disasters")

hcmap(data = disastersByCountry,
      value = 'log.avg.deaths',
      joinBy = c('hc-a2', 'code'),
      name = "Average Deaths") %>% 
  hc_title(text = "Logarithm of Average Deaths  In Disasters")
