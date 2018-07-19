pairs = words %>% 
  mutate(word = str_to_lower(word)) %>% 
  rename(word1 = word) %>% 
  mutate(word2 = lead(word1)) %>% 
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word2 %in% aux)

pairStat = pairs %>% 
  group_by(word1, word2) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  ungroup()

hePairs = pairStat %>% 
  filter(word1 == 'he') %>% 
  mutate(prop = count / sum(count)) %>% 
  top_n(20, count)

shePairs = pairStat %>% 
  filter(word1 == 'she') %>% 
  mutate(prop = count / sum(count)) %>% 
  top_n(20, count) 

heshePairs = rbind(hePairs, shePairs)

topVerbs = full_join(hePairs %>% select(word2, prop), 
                     shePairs%>% select(word2, prop),
                     by = 'word2') %>% 
  rename(heProp = prop.x, sheProp = prop.y)
topVerbs[is.na(topVerbs)] = 0
topVerbs = topVerbs %>% 
  mutate(ratio = heProp / sheProp)
  
heshePairs = rbind(hePairs, shePairs)
heshePairs = left_join(heshePairs, topVerbs)

hchart(hePairs, type = 'bar', hcaes(x = word2, y = count)) %>% 
  hc_xAxis(title = list(text = "")) %>% 
  hc_title(text = 'Most Common Verbs For Men')
hchart(shePairs, type = 'bar', hcaes(x = word2, y = count)) %>% 
  hc_xAxis(title = list(text = "")) %>% 
  hc_title(text = 'Most Common Verbs For Women')

ggplot(heshePairs, aes(x = reorder(word2, ratio), y = prop, fill = word1)) +
  geom_bar(stat = 'identity', position = 'fill') +
  coord_flip()
  