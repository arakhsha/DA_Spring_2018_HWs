# iranMap = get_map("Iran", zoom = 5)
# saveRDS(iranMap, 'iranMap.rds')
iranMap = readRDS('iranMap.rds')
ggmap(iranMap) +
  stat_density_2d(data = iequake, aes(x = Long, y = Lat, fill = ..level..), geom = "polygon") +
  guides(fill = F)
