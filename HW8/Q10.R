topWords = authorsNgramStat %>% 
  group_by(n) %>% 
  top_n(20, total) %>% 
  arrange(total) %>% 
  .$ngram
bothChaptersNgram = rbind(chaptersNgramStat, austinChaptersNgramStat)
bothChaptersNgram = bothChaptersNgram %>%
  filter(ngram %in% topWords) %>% 
  spread(ngram, prop)
bothChaptersNgram[] = lapply(bothChaptersNgram[], unlist)
bothChaptersNgram[bothChaptersNgram == 'NULL'] = 0
bothChaptersNgram[is.na(bothChaptersNgram)] = 0
bothChaptersNgram[5:ncol(bothChaptersNgram)] = lapply(bothChaptersNgram[5:ncol(bothChaptersNgram)], 
                                                      function(x){x * 10000})
bothChaptersNgram$author = as.numeric(as.factor(bothChaptersNgram$author)) - 1
saveRDS(bothChaptersNgram, 'bothChaptersNgram.rds')

bothChaptersNgram = readRDS('bothChaptersNgram.rds')
train = bothChaptersNgram %>% 
  filter(!gutenburg_id %in% c(158, 730)) %>% 
  select(-gutenburg_id, -n, -chapter) 
test = bothChaptersNgram %>% 
  filter(gutenburg_id %in% c(158, 730)) %>% 
  select(-gutenburg_id, -n, -chapter)

model = glm(
  author ~ .,
  data = train,
  family = 'binomial')
summary(model)

y <- train$author
predict = fitted.values(model)

cutoff = seq(0, 1, 0.001)
acc = sapply(cutoff, function(c) {
  TP <- sum(predict >= c & y == 1)
  TN <- sum(predict < c & y == 0)
  return( (TP + TN) / length(predict))
})

bestIndex <- which.max(acc)
paste('Cutoff:', cutoff[bestIndex],
      'Acc:', acc[bestIndex])

y <- test$author
predict = predict.glm(model, test, type = 'response')
results = data.frame(actual = as.factor(ifelse(y == 0, '0 (Austin)', '1 (Dickens)')), predict)
ggplot(results, aes(x = predict, fill = actual, group = actual)) +
  geom_density()

TP <- sum(predict >= cutoff[bestIndex] & y == 1)
TN <- sum(predict < cutoff[bestIndex] & y == 0)
paste('Error:',1 - (TP + TN) / length(predict))
