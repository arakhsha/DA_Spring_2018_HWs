cutoff = seq(0, 1, 0.001)
acc = sapply(cutoff, function(c) {
  TP <- sum(predict >= c & y == 1)
  TN <- sum(predict < c & y == 0)
  return( (TP + TN) / (P + N) )
})

ggplot(data.frame(acc, cutoff), aes(x = cutoff, y = acc)) +
  geom_line()

bestIndex <- which.max(acc)
paste('Cutoff:', cutoff[bestIndex],
      'Acc:', acc[bestIndex])

