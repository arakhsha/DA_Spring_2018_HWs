words = words %>% 
  group_by(gutenberg_id) %>% 
  mutate(chapter = as.numeric(str_detect(str_to_lower(word), 'chapter'))) %>% 
  mutate(chapter = cumsum(chapter))

chaptersText = words %>% 
  filter(chapter > 0) %>% 
  mutate(word = str_to_lower(word)) %>% 
  group_by(gutenberg_id, chapter) %>% 
  summarise(text = paste(word, sep = ' ', collapse = ' '), count = n()) %>% 
  filter(count > 30)

booksText = words %>% 
  mutate(word = str_to_lower(word)) %>% 
  group_by(gutenberg_id) %>% 
  summarise(text = paste(word, sep = ' ', collapse = ' '))


library(ngram)
chaptersNgramStat = list()
for(i in 1:nrow(chaptersText)) {
  text = chaptersText[i,]$text %>% as.character()
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
  id = chaptersText[i,]$gutenberg_id
  ch = chaptersText[i,]$chapter
  summary = summary %>% mutate(gutenburg_id = id, chapter = ch)
  chaptersNgramStat[[i]] = summary
}
chaptersNgramStat = plyr::rbind.fill(chaptersNgramStat)
chaptersNgramStat = chaptersNgramStat %>% mutate(author = 'Dickens')



ngramStat = data.frame(ngram = character(0), n = numeric(0), stringsAsFactors = F)
for(code in bookCodes) {
  text = booksText %>% filter(gutenberg_id == code) %>% .$text %>% as.character()
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
  colnames(summary) = c('ngram', code, 'n')
  ngramStat = full_join(ngramStat, summary) %>% distinct()
}
testData <- ngramStat %>% 
  select(-ngram, -n)
testData[is.na(testData)] = 0
testData = testData * 100000000
# chisq.test(testData)
saveRDS(testData, 'booksComparison.rds')

ngramStat$total = rowSums(ngramStat %>% select(-ngram, -n))

plotData = ngramStat %>% 
  group_by(n) %>% 
  top_n(5, total) %>% 
  arrange(total) %>% 
  ungroup() %>% 
  gather(key = 'gutenberg_id', value = 'prop', 3:15) %>% 
  mutate(gutenberg_id = as.numeric(gutenberg_id)) %>% 
  left_join(gutenberg_metadata %>% select(gutenberg_id, title))

dickensBooksComparison = hchart(plotData, type = 'column', hcaes(x = ngram, y = prop, group = title))
dickensBooksComparison
saveRDS(dickensBooksComparison, 'dickensBooksComparison.rds')
