wordFreqByBook = words %>% 
  filter(!str_to_lower(word) %in% stop_words$word) %>% 
  filter(!word %in% aux) %>% 
  group_by(gutenberg_id, word) %>% 
  summarise(count = n()) %>% 
  ungroup()
names = wordFreqByBook %>% 
  filter(!str_to_lower(word) %in% stop_words$word) %>% 
  filter(!str_to_lower(word) %in% word) %>% 
  rename(name = word)

names = left_join(names, 
                  gutenberg_metadata %>% select(gutenberg_id, title))

topNames = names %>% 
  group_by(title) %>% 
  top_n(5, count)

  
ggplot(topNames, aes(x = paste(title, str_pad(count, 5, pad = "0"), name), y = count, fill = title)) +
  geom_bar(stat = 'identity') +
  scale_x_discrete(labels = topNames$name) +
  scale_fill_stata() +
  coord_flip() +
  theme(legend.position="bottom", legend.direction = 'horizontal',
        axis.text=element_text(size=7)) +
  labs(x = 'Name', y = 'Count', fill = 'Title')
