recentData = data %>% 
  select(-X63) %>% 
  filter(`Indicator Code` %in% healthIndicators) %>% 
  gather("year", "value", `1960`:`2017`) %>% 
  drop_na() %>% 
  mutate(year = as.numeric(year)) %>% 
  arrange(year) %>% 
  group_by(`Country Name`, `Indicator Name`) %>% 
  summarise(lastValue = last(value)) %>% 
  spread("Indicator Name", "lastValue") %>% 
  drop_na()

numericData = recentData[,-1] %>% scale()
kcl = kmeans(numericData, centers = 3)
results = data.frame(country = recentData$`Country Name`, cluster = kcl$cluster, stringsAsFactors = F)

for(i in 1:3) {
  clusterCountries = results %>% filter(cluster == i) %>% .$country
  cat("\n\nCluster No.", i, ":\n")
  cat(clusterCountries, sep = ", ")
}

iranClusterNo = results$cluster[results$country == 'Iran, Islamic Rep.']
paste('Iran is in cluster no.', iranClusterNo)
