library(wordcloud2)
library(tidytext)

words = movies$Title %>% 
  str_replace_all("[:punct:]", " ") %>% 
  str_replace_all("\"", " ") %>% 
  str_split("\\s") %>% 
  unlist() %>% 
  table() %>% 
  as.data.frame(stringsAsFactor = F)
colnames(words) = c('word', 'count')
words = words %>% 
  filter(!str_to_lower(word) %in% stop_words$word) %>% 
  filter(str_length(word) > 2) %>% 
  filter(!str_detect(word, "\\d")) %>% 
  arrange(desc(count)) %>% 
  filter(word != 'III') %>% 
  top_n(100, count)

wordcloud2(words,size = 1, minRotation = -pi/2, maxRotation = -pi/2)
