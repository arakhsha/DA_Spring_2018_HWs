iranPowerfullEQs = disaster %>% 
  filter(EQ_PRIMARY >= 7) %>% 
  filter(COUNTRY == 'IRAN') %>% 
  select(year = YEAR, month = MONTH)

iranPowerfullEQs = iequake %>% 
  filter(Mag >= 7) %>% 
  mutate(year = OriginTime %>% as.POSIXlt() %>% .$year + 1900,
         month = OriginTime %>% as.POSIXlt() %>% .$mon + 1) %>% 
  select(year, month) %>% 
  union(iranPowerfullEQs)

iranPowerfullEQs = earthquakes %>% 
  filter(mag >= 7, str_detect(tolower(place), "iran")) %>% 
  mutate(year =  as.POSIXlt(time)$year + 1900,
         month = as.POSIXlt(time)$mon + 1) %>%
  select(year, month) %>% 
  union(iranPowerfullEQs)

iranPowerfullEQs = iranPowerfullEQs %>% filter(year > 1900) %>% arrange(year, month)
diffs = iranPowerfullEQs$year * 12 + iranPowerfullEQs$month
diffs = (diffs - lag(diffs))[-1]

prob = sum(diffs <= 72 & diffs >= 12) / sum(diffs >= 12)
prob
