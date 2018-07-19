library(readr)
consumption = read_csv('consumption.csv')
cor.test(consumption$A,
         consumption$B,
         method = 'spearman')