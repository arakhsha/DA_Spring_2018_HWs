pcaData = data_frame(Date = dfs[[1]]$Date)
for(i in 1:length(paths)) {
  selectedDF = dfs[[i]] %>% 
    select(Date, Open)
  colnames(selectedDF)[2] = names[[i]]
  pcaData = full_join(pcaData, selectedDF)
  print(names[[i]])
}

numberOfNAs = lapply(pcaData[], function(x){sum(is.na(x))}) %>% unlist()
pcaData = pcaData[,numberOfNAs < 500]
pcaData = pcaData[complete.cases(pcaData),]
pca = prcomp(pcaData %>% select(-Date))
vars = pca$sdev^2
cumvar = cumsum(vars) / sum(vars)

plotData = data.frame(n = 1:50, variance = cumvar[1:50])
hchart(plotData, type = 'line', hcaes(x = n, y = variance), name = 'Variance') 

paste('Variance for 3 first components:', round(cumvar[3] * 100, 2), "%")
