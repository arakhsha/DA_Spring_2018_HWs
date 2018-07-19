filteredPairs = words %>% 
  mutate(word = str_to_lower(word)) %>% 
  rename(word1 = word) %>% 
  mutate(word2 = lead(word1)) %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word)

filteredPairsStat = filteredPairs %>% 
  group_by(word1, word2) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  ungroup()

topPairs = filteredPairsStat %>% top_n(30, wt = count)

hchart(topPairs, type = 'bar', hcaes(x = paste(word1, word2), y = count), name = 'Count') %>% 
  hc_xAxis(title = list(text = NA))
