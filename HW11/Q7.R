eq.aprox = earthquakes %>% 
  mutate(longitude = round(longitude / 1.5) * 1.5,
         latitude = round(latitude / 1.5) * 1.5,
         day = as.POSIXlt(time)$yday + as.POSIXlt(time)$year * 365) %>% 
  mutate(day = round(day / 5)* 5) %>% 
  arrange(time) %>% 
  group_by(longitude, latitude, day) %>% 
  summarise(firstForeshockTime = first(time),
            primaryTime = time[which.max(mag)],
            count = n()) %>% 
  filter(count >= 10) %>% 
  mutate(diff = primaryTime - firstForeshockTime)


ggplot(eq.aprox, aes(x = diff)) + geom_density() +
  labs(x = "Time Between The First and Strongest Wave (s)", y = "Density")
