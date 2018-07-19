Life.Expectancy = data %>% 
  filter(`Indicator Code` == 'SP.DYN.LE00.IN') %>% 
  select(-`Indicator Name`, -`Indicator Code`, -X63) %>% 
  gather('year', 'LE', `1960`:`2016`)

ggplot(Life.Expectancy, aes(x = year, y = LE, fill = 1)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7)) +
  labs(y = 'Life Expectancy') +
  guides(fill = F)

Rwanda.LE = Life.Expectancy %>% 
  filter(`Country Name`== 'Rwanda')

ggplot() +
  geom_line(data = Rwanda.LE, aes(x = year, y = LE, group = 1, color = 'red')) +
  geom_boxplot(data = Life.Expectancy,
               aes(x = Life.Expectancy$year, y = Life.Expectancy$LE, fill = 1)) +
  labs(y = 'Life Expectancy') +
  scale_color_manual(labels = 'Rwanda', values = 'red') +
  guides(fill = F) +
  theme(legend.title=element_blank()) +
  theme(legend.position="bottom") + 
  scale_x_discrete(breaks = seq(1960, 2010, by = 5))
