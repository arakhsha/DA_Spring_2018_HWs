bookCodes = c(580, 730, 967, 700, 917, 968, 821, 766, 1023, 786, 963, 1400, 883, 564)
# View(gutenberg_metadata %>% filter(gutenberg_id %in% novelCodes))
# books = gutenberg_download(bookCodes)
# saveRDS(books, file="books.Rda")
books = readRDS(file="books.Rda")

words = books$text %>% 
  str_replace_all("[[:punct:]]", " ") %>% 
  str_replace_all("\"", " ") %>% 
  str_split_fixed("[\\s]", n = Inf) %>% 
  as.data.frame(stringsAsFactors = F)
colnames(words) = 1:72
words = cbind(books, words) %>% 
  select(-text) %>% 
  mutate(line = row_number()) %>% 
  gather(key = 'index', value = 'word', 2:73) %>% 
  mutate(index = as.numeric(index)) %>% 
  arrange(line, index) %>% 
  select(-line, -index) %>% 
  filter(str_length(word) > 1)


wordFreq = words$word %>% 
  str_to_lower() %>% 
  table() %>% 
  as.data.frame(stringsAsFactors = F)
colnames(wordFreq) = c('word', 'count')
aux <- c('don', 'll', 'isn', 'aren', 're', 'wouldn', 'couldn')
wordFreq = wordFreq %>% 
  filter(!str_to_lower(word) %in% stop_words$word) %>% 
  filter(!word %in% aux) %>% 
  arrange(desc(count))

topWords = wordFreq %>% 
  top_n(n = 20, count)


hchart(topWords, type = 'column', hcaes(x = word, y = count), name = 'Count')

