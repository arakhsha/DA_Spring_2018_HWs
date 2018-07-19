library('dplyr')
library('readr')
library('ggplot2')
library('ggthemes')
library('ggrepel')
#setting theme
theme_set(theme_gdocs())

#reading data
setwd(dirname(parent.frame(2)$ofile))
mobile = read_csv('mobile_data.csv')

#correcting data
dimensions = data.frame(thickness = mobile$dim_thickness,
                        length = mobile$dim_length,
                        breadth = mobile$dim_breadth)

mobile$dim_thickness = pmin(dimensions$thickness, dimensions$length, dimensions$breadth)
mobile$dim_length = pmax(dimensions$thickness, dimensions$length, dimensions$breadth)
mobile$dim_breadth = dimensions$thickness + dimensions$length + dimensions$breadth - mobile$dim_length - mobile$dim_thickness


#Q1
modelStat = mobile %>% group_by(company) %>%
  summarise(count = n())
modelStat = modelStat[order(modelStat$count, decreasing = TRUE), ]

modelStat %>% top_n(n = 1, wt = count)

ggplot(modelStat[1:20,], aes(x = reorder(company, count), y = count)) +
  geom_bar(stat = 'identity', fill = "dodgerblue4") +
  labs(y = 'Number of models', x = '', title = 'Companies with Highest Device Models') +
  coord_flip()

#Q2
ggplot(mobile, aes(x = year, y = dim_length)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(y = 'Length', x = 'Year', title = 'Length vs. Year')
         
ggplot(mobile, aes(x = year, y = dim_breadth)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(y = 'Breadth', x = 'Year', title = 'Breadth vs. Year')

ggplot(mobile, aes(x = year, y = dim_thickness)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(y = 'Thickness', x = 'Year', title = 'Thickness vs. Year')

ggplot(mobile, aes(x = year, y = cam_px)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(y = 'Camera Resolution', x = 'Year', title = 'Camera Resolution vs. Year')

#Q3
priceBySimAndLTE = mobile %>% group_by(LTE, sim_no) %>% 
  summarise(avgPrice = mean(price, na.rm = TRUE))

ggplot(priceBySimAndLTE, aes(x = sim_no, y = avgPrice)) +
  geom_bar(aes(fill = LTE), stat = 'identity', position = 'dodge') +
  labs(y = 'Average Price', x = 'Number of Sims', title = 'Average Price by LTE and Number of Sims')

#Q4
ggplot(mobile[which(mobile$year == 2017), ], aes(x = audio_jack, y = dim_thickness)) +
  geom_boxplot(aes(fill = audio_jack)) +
  labs(y = 'Thickness', x = 'Audio Jack', title = 'Thikness of Devices With And Without Audio Jack') +
  guides(fill=FALSE)

#Q5
mobile = mobile %>% mutate(ppi = sqrt(px_row^2 + px_col^2) / display_size)
ggplot(mobile, aes(x = ppi)) + 
  geom_histogram(fill = "dodgerblue4") +
  labs(y = 'Count', x = 'PPI', title = 'PPI histogram')


ppiStat = mobile %>% group_by(year) %>% summarise(avgPPI = mean(ppi, na.rm = TRUE))
ggplot(ppiStat, aes(x = year, y = avgPPI)) +
  geom_line(color = "dodgerblue4") + geom_point(color = "dodgerblue4") +
  ylim(0, 400) +
  labs(y = 'Average PPI', x = 'Year', title = 'Average PPI vs Year') 

topPPI = mobile %>% top_n(wt = ppi, n = 1)
paste(topPPI$company, topPPI$device, "with ", topPPI$ppi, "ppi")
#Q6
gooshkubiat = function(x) {
  kolofty = x$dim_thickness / (x$dim_length * x$dim_breadth)
  density = x$weight / (x$dim_length * x$dim_breadth)
  result = kolofty + density
  result[which(x$dim_length < 65)] = rep(0, length(which(x$dim_length < 65)))
  result[which(!is.finite(result))] = rep(NaN, length(which(!is.finite(result))))
  return(result)
}
mobileWithGooshkubiat = mobile
mobileWithGooshkubiat$gooshkubiat = gooshkubiat(mobile)
topGooshkub = mobileWithGooshkubiat %>% top_n(10, gooshkubiat)
ggplot(topGooshkub, aes(x = reorder(paste(company, device), gooshkubiat), y = gooshkubiat)) +
  geom_bar(stat = 'identity', fill = "springgreen4") +
  labs(y = 'Gooshkubiat', x = '', title = 'Top 10 Gooshkubs') +
  coord_flip() 

#Q7
dinsity = function(x) {
  result = x$weight / (x$dim_length * x$dim_breadth *x$dim_thickness) * 1000
  result[which(!is.finite(result))] = rep(NaN, length(which(!is.finite(result))))
  return(result)
}
mobile$density = dinsity(mobile)
mobile %>% filter(density < 1) %>% select(company, device, density) %>% arrange(density)

ggplot(mobile[which(!is.nan(mobile$density)), ],
  aes(x = density)) + xlim(0, 4) +
  geom_histogram(aes(fill = density < 1), binwidth = 0.2, center = 0.1) +   
  geom_vline(xintercept = 1, color = "dodgerblue4") +
  labs(y = 'Frequency', x = 'Density', title = 'Density Histogram', fill = "Floats on Water")

#Q8
ggplot(mobile, aes(x = battery_mah, y = weight)) + 
  geom_point(color = "dodgerblue", alpha = 0.3) +
  ylim(0, 1000) +
  labs(y = 'Weight', x = 'Battery Capacity', title = 'Weight vs. Battery Capacity')
cor(mobile$battery_mah, mobile$weight, use = "complete.obs")

#Q9
samsungFlagship = mobile %>% filter(company == "Samsung") %>% na.omit() %>% 
  group_by(year) %>% summarise(flagship = device[which.max(price)[1]], price = price[which.max(price)[1]])

ggplot(samsungFlagship, aes(x = year, y = price, label = flagship)) + 
  geom_line(color = "orangered") +
  geom_point(color = "red", size = 2) +
  geom_label_repel(size = 2, box.padding = 0.25) +
  ylim(0, 1000) +
  labs(y = 'Price', x = 'Year', title = 'Samsung Flagships Prices')

#Q10
topBrands = c("HTC", "LG", "Sony", "Apple", "Samsung")
screenRatioFunc = function(x) {
  area = x$dim_length * x$dim_breadth
  screenArea = x$aspect_col * x$aspect_row / (x$aspect_col^2 + x$aspect_row^2) * x$display_size^2 * 25.4^2
  result <- screenArea / area
  result[which(!is.finite(result))] = rep(NaN, length(which(!is.finite(result))))
  return(result)
}
screenRatioStat = mobile
screenRatioStat$screenRatio = screenRatioFunc(mobile)
screenRatioStat = screenRatioStat %>% group_by(company, year) %>% 
  summarise(maxRatio = max(screenRatio, na.rm = TRUE)) %>%
  filter(company %in% topBrands)
ggplot(screenRatioStat,
       aes(x = year, y = maxRatio, group = company, color = company)) +
    geom_point() + geom_line() +
  xlim(2012, 2017) + ylim(0.5, 1) +
  labs(y = 'Highest Screen To Body Ratio', x = 'Year', title = "Highest Screen To Body Ratio of companies",
       color = "Company")
  

topBrands = c("HTC", "LG", "Sony")
batteryStat = mobile %>% filter(company %in% topBrands, price < 1500) %>%  group_by(company, year) %>%
  summarise(avgBattery = mean(battery_mah, na.rm = TRUE), count = n()) %>% na.omit() 
ggplot(batteryStat, aes(x = year, y = avgBattery, group = company, color = company)) +
  geom_point(alpha = 0.3) + geom_line(alpha = 0.3) + geom_smooth(method = "lm", se = FALSE) +
  xlim(2010, 2017) +
  labs(y = 'Average Battery', x = 'Year', title = "Average Battery Capacity by Year", color = "Company")

samsung = mobile %>% filter(company == 'Samsung', !is.na(os_type))
ggplot(samsung, aes(x = year)) +
  geom_bar(aes(fill = os_type), position = 'fill') +
  labs(y = '', x = 'Year', title = "OS used by Samsung", fill = "OS type")

