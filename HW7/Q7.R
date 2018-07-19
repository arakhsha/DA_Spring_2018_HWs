TPR = sapply(cutoff, function(c) {
  return (sum(y == 1 & predict >= c) / sum(y == 1))
})

FPR = sapply(cutoff, function(c) {
  return (sum(y == 0 & predict >= c) / sum(y == 0))
})

plotData = data.frame(FPR, TPR)

bestPoint <- data.frame(x = FPR[bestIndex], y = TPR[bestIndex])
ggplot(plotData, aes(x = FPR, y = TPR)) +
  geom_point(size = 0.3) +
  geom_point(data = bestPoint, aes(x, y), color = 'red', size = 2) +
  geom_label_repel(data = bestPoint, aes(x, y), label = "best point",
                   size = 4, box.padding = 0.25, nudge_x = 0.2) +
  geom_line()
