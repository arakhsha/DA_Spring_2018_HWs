topWords = wordFreq %>% 
  top_n(n = 200, count)
library(wordcloud2)
wordcloud2(topWords)
wordcloud2(topWords, figPath = 'fig.png', size = 0.2)
