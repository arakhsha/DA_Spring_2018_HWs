plot_ly(sequake, x = ~Longitude, y = ~Latitude, z = ~(-Depth), size = ~(10 * Magnitude)) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Longitude'),
                      yaxis = list(title = 'Latitude'),
                      zaxis = list(title = 'Depth')))


