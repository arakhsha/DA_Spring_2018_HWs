clusterData = data %>% 
  select(-X63) %>% 
  filter(`Indicator Code` %in% c(economicIndicators, healthIndicators, educationIndicators)) %>% 
  gather("year", "value", `1960`:`2017`) %>% 
  drop_na() %>% 
  mutate(year = as.numeric(year)) %>% 
  arrange(year) %>% 
  group_by(`Country Name`, `Indicator Name`) %>% 
  summarise(lastValue = last(value)) %>% 
  spread("Indicator Name", "lastValue") %>% 
  drop_na() %>% 
  as.data.frame()

rownames(clusterData) = clusterData$`Country Name`
clusterData = clusterData[,-1] %>% scale()

dist = stats::dist(clusterData, method = "euclidean")
clus = hclust(dist,method = "complete")
hcut = cutree(clus, k = 3)
results = data.frame(country = rownames(clusterData), cluster = hcut, stringsAsFactors = F)

for(i in 1:3) {
  clusterCountries = results %>% filter(cluster == i) %>% .$country
  cat("\n\nCluster No.", i, ":\n")
  cat(clusterCountries, sep = ", ")
}

iranClusterNo = results %>% filter(country == "Iran, Islamic Rep.") %>% .$cluster
paste('Iran is in cluster no.', iranClusterNo)

library(ggdendro)
ggdendrogram(clus, size = 0.5)
