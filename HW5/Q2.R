classicalStores = c(50, 50, 60, 70, 75, 80, 90, 85)
modernStores = c(55, 75, 80, 90, 105, 65)

wilcox.test(classicalStores, modernStores, paired=FALSE)
