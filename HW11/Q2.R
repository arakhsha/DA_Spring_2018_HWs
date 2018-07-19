tsunamies = disaster %>% 
  filter(FLAG_TSUNAMI == "Tsu")
# bbox <- c(left = -170, bottom = -60, right = 170, top = 80)
# worldMap = get_stamenmap(bbox, zoom = 3, maptype="toner-lite")
# saveRDS(worldMap, 'worldMap_toner_lite.rds')
# worldMap = readRDS('worldMap_toner_lite.rds')
# p = ggmap(worldMap, extent = "device") +
#   geom_point(data = tsunamies, aes(x = LONGITUDE, y = LATITUDE, frame = YEAR, color = 'red'))
# p = gganimate(p, "q2_1.gif")

worldMap = map_data('world')
p = ggplot() +
  geom_polygon(
    data = worldMap,
    aes(x = long, y = lat, group = group),
    fill = "gray70",
    color = 'gray80'
  ) +
  coord_fixed() +
  geom_point(
    data = tsunamies,
    aes(x = LONGITUDE, y = LATITUDE, frame = YEAR, color = EQ_PRIMARY)
  ) +
  scale_color_gradient2(low = '#59f442', mid = '#f4f142', high = '#f22626', midpoint = 5.5) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  guides(color=guide_legend(title="Magnitude"))
# p
# p = gganimate(p, "q2_2.gif", ani.height = 350, ani.width = 800)
