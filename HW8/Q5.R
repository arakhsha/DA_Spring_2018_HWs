miserables_id = 135
# miserablesBook = gutenberg_download(miserables_id)
# saveRDS(miserablesBook, file="miserables.Rda")
miserablesBook = readRDS(file="miserables.Rda")
miserablesWords = miserablesBook$text %>% 
  str_replace_all("[[:punct:]]", " ") %>% 
  str_replace_all("\"", " ") %>% 
  str_split("[\\s]+") %>% 
  unlist() %>% 
  as.data.frame(stringsAsFactors = F)
colnames(miserablesWords) = 'word'
miserablesWords = miserablesWords %>% 
  filter(str_length(word) > 0) %>% 
  mutate(part = ceiling(row_number() * 200 / n())) %>% 
  mutate(word = str_to_lower(word))

nrcSentiments = sentiments %>% 
  filter(lexicon == 'nrc') %>% 
  select(word, sentiment)

miserablesWords = inner_join(miserablesWords, nrcPosNeg, by = 'word')

storyMood = miserablesWords %>% 
  group_by(part, sentiment) %>% 
  summarise(count = n()) %>% 
  group_by(part) %>% 
  mutate(prop = 100 * count / sum(count)) %>%

  ungroup()

hchart(storyMood, type = 'line', hcaes(x = part, y = prop, group = sentiment)) %>% 
  hc_colors(colors = c('#e84118', '#4cd137'))
