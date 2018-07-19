testData = disaster %>% 
  filter(EQ_PRIMARY > 6) %>% 
  mutate(FLAG_TSUNAMI = ifelse(is.na(FLAG_TSUNAMI), "Regular", "Tsunami")) %>% 
  mutate(class = floor(EQ_PRIMARY))

regular = testData %>% filter(FLAG_TSUNAMI == "Regular", !is.na(TOTAL_DEATHS)) %>% .$TOTAL_DEATHS
tsunami = testData %>% filter(FLAG_TSUNAMI == "Tsunami", !is.na(TOTAL_DEATHS)) %>% .$TOTAL_DEATHS

wilcox.test(regular, tsunami, alternative = "less")
  