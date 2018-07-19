library(readr)
tv = read_csv('tv.csv')
friedman.test(cbind(tv$March, tv$April, tv$May, tv$Jun))