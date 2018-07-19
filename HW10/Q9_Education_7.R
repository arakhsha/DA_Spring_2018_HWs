pca = prcomp(numericData, center = T, scale = T)
ggbiplot(pca,
         groups = as.factor(kcl$cluster))+
  labs(title = "Clusteres Based on Education Indicators")
