data = c(102, 300, 102, 100, 205, 105, 71, 92)
p = rep(1/length(data), length(data))
chisq.test(data, p = p)