austinsBookCodes = c(105,   121,   141,   158,   161,   946,  1212,  1342)
# austinBooks = gutenberg_download(austinsBookCodes)
# saveRDS(austinBooks, file = "austinBooks.rds")
austinBooks = readRDS(file = "austinBooks.rds")

austinWords = austinBooks$text %>% 
  str_replace_all("[[:punct:]]", " ") %>% 
  str_replace_all("\"", " ") %>% 
  str_split_fixed("[\\s]", n = Inf) %>% 
  as.data.frame(stringsAsFactors = F)
colnames(austinWords) = 1:61
austinWords = cbind(austinBooks, austinWords) %>% 
  select(-text) %>% 
  mutate(line = row_number()) %>% 
  gather(key = 'index', value = 'word', 2:62) %>% 
  mutate(index = as.numeric(index)) %>% 
  arrange(line, index) %>% 
  select(-line, -index) %>% 
  filter(str_length(word) > 1) %>% 
  mutate(word = str_to_lower(word))

austinWords = austinWords %>% 
  group_by(gutenberg_id) %>% 
  mutate(chapter = as.numeric(str_detect(str_to_lower(word), 'chapter'))) %>% 
  mutate(chapter = cumsum(chapter))

austinChaptersText = austinWords %>% 
  filter(chapter > 0) %>% 
  mutate(word = str_to_lower(word)) %>% 
  group_by(gutenberg_id, chapter) %>% 
  summarise(text = paste(word, sep = ' ', collapse = ' '), count = n()) %>% 
  filter(count > 30)

austinChaptersNgramStat = list()
for(i in 1:nrow(austinChaptersText)) {
  text = austinChaptersText[i,]$text %>% as.character()
  ng1 = ngram(as.character(text), n = 1)
  ng2 = ngram(as.character(text), n = 2)
  s1 <- get.phrasetable(ng1) %>% 
    mutate(n = 1) %>% 
    filter(!str_trim(ngrams) %in% stop_words$word) %>% 
    filter(!str_trim(ngrams) %in% aux)
  s2 <- get.phrasetable(ng2) %>% 
    mutate(n = 2) %>% 
    separate(ngrams, c("word1", "word2"), sep = " ") %>% 
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word) %>% 
    mutate(ngrams = paste(word1, word2)) %>% 
    select(-word1, -word2)
  summary = rbind(s1,s2)
  summary = summary %>% select(-freq)
  colnames(summary) = c('ngram', 'prop', 'n')
  id = austinChaptersText[i,]$gutenberg_id
  ch = austinChaptersText[i,]$chapter
  summary = summary %>% mutate(gutenburg_id = id, chapter = ch)
  austinChaptersNgramStat[[i]] = summary
}
austinChaptersNgramStat = plyr::rbind.fill(austinChaptersNgramStat)
austinChaptersNgramStat = austinChaptersNgramStat %>% mutate(author = 'Austin')

authors <- c('Austin', 'Dickens')
authorsText = data.frame(author = authors,
           text = c(
             paste(austinWords$word, sep = ' ', collapse = ' '),
             paste(str_to_lower(words$word), sep = ' ', collapse = ' ')
           ))


authorsNgramStat = data.frame(ngram = character(0), n = numeric(0), stringsAsFactors = F)
for(a in authors) {
  text = authorsText %>% filter(author == a) %>% .$text %>% as.character()
  ng1 = ngram(as.character(text), n = 1)
  ng2 = ngram(as.character(text), n = 2)
  s1 <- get.phrasetable(ng1) %>% 
    mutate(n = 1) %>% 
    filter(!str_trim(ngrams) %in% stop_words$word) %>% 
    filter(!str_trim(ngrams) %in% aux)
  s2 <- get.phrasetable(ng2) %>% 
    mutate(n = 2) %>% 
    separate(ngrams, c("word1", "word2"), sep = " ") %>% 
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word) %>% 
    mutate(ngrams = paste(word1, word2)) %>% 
    select(-word1, -word2)
  summary = rbind(s1,s2)
  summary = summary %>% select(-freq)
  colnames(summary) = c('ngram', a, 'n')
  authorsNgramStat = full_join(authorsNgramStat, summary) %>% distinct()
}
testData <- authorsNgramStat %>% 
  select(-ngram, -n)
testData[is.na(testData)] = 0
testData = testData * 1000000

# chisq.test(testData)
saveRDS(testData, 'authorsComparisonTest.rds')

authorsNgramStat$total = rowSums(authorsNgramStat %>% select(-ngram, -n))

plotData = authorsNgramStat %>% 
  group_by(n) %>% 
  top_n(5, total) %>% 
  arrange(total) %>% 
  ungroup() %>% 
  gather(key = 'author', value = 'prop', 3:4)

authorsComparison = hchart(plotData, type = 'column', hcaes(x = ngram, y = prop, group = author))
authorsComparison
saveRDS(authorsComparison, file = 'authorsComparison.rds')
