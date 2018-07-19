nrcPosNeg = sentiments %>% 
  filter(lexicon == 'nrc', sentiment %in% c('negative', 'positive')) %>% 
  select(word, sentiment)

for(code in bookCodes) {
  title = gutenberg_metadata$title[gutenberg_metadata$gutenberg_id == code]
    
  np = wordFreqByBook %>%
    filter(gutenberg_id == code) %>% 
    filter(word %in% nrcPosNeg$word)
  np = left_join(np, nrcPosNeg, by = 'word')
  
  topNP = np %>% 
    group_by(sentiment) %>% 
    top_n(20, count) %>% 
    ungroup() %>% 
    arrange(desc(count)) %>% 
    mutate(index = row_number())
  
  p = ggplot(topNP, aes(x = reorder(word, count), y = count, fill = sentiment)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    facet_grid(.~sentiment, scale = "free") +
    theme(axis.text = element_text(size = 7),
          axis.text.x = element_text(hjust = 1, vjust = 1, angle = 45),
          legend.position="bottom", legend.direction = 'horizontal') +
    labs(x = '', y = 'Count', fill = 'Sentiment',
         title = 'Top Positive and Negative Words', subtitle = paste('in', title))
  print(p)
}
