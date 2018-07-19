paths = list.files('data/stock_dfs', full.names = T)
names = list.files('data/stock_dfs', full.names = F) %>% 
  str_replace('.csv', '') 
dfs = list()
for(i in 1:length(paths)) {
  df = read_csv(paths[[i]]) %>% 
    mutate(Symbol = names[[i]])
  dfs[[i]] = df
  print(names[[i]])
}
data = bind_rows(dfs)

data$Symbol[data$Symbol == 'BRK-B'] = 'BRK.B'
data$Symbol[data$Symbol == 'BF-B'] = 'BF.B'


constituents = read_csv('data/constituents.csv')

data = data %>% 
  left_join(constituents) %>% 
  mutate(Name = ifelse(is.na(Name), Symbol, Name))

monthData = data %>% 
  mutate(month = as.integer(format(Date, '%m')) + (as.integer(format(Date, '%Y')) - 2000 )* 12) %>% 
  arrange(Date) %>% 
  group_by(Symbol, Symbol, Sector, Name, month) %>% 
  summarise(Value = first(`Adj Close`)) %>% 
  ungroup()

growths = monthData %>% 
  group_by(Symbol, Sector, Name) %>% 
  mutate(oneYearAgo = lag(Value, n = 12),
         twoYearAgo = lag(Value, n = 24),
         fiveYearAgo = lag(Value, n = 60)) %>% 
  mutate(`1 year` = Value - oneYearAgo,
         `2 years` = Value - twoYearAgo,
         `5 years` = Value - fiveYearAgo)
records = growths %>% 
  gather(key = 'period', value = 'growth', 9:11) %>% 
  arrange(desc(growth)) %>% 
  group_by(period, Name) %>% 
  top_n(1, wt = growth) %>% 
  ungroup() %>% 
  select(Name, period, growth)

topRecords = records %>% 
  group_by(period) %>% 
  top_n(1, growth)

top10records = records %>% 
  group_by(period) %>% 
  top_n(10, growth)

ggplot(top10records, aes(x = reorder(Name, growth), y = growth, fill = 1)) +
  geom_bar(stat = 'identity') +
  facet_grid(~period, scale = 'free') +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
  guides(fill = F) +
  xlab('Name')


sectorGrowths = monthData %>% 
  filter(!is.na(Sector)) %>% 
  mutate(oneYearAgo = lag(Value, n = 12),
         twoYearAgo = lag(Value, n = 24),
         fiveYearAgo = lag(Value, n = 60)) %>% 
  group_by(Sector, month) %>% 
  summarise(`1 year` = sum(Value - oneYearAgo),
            `2 years` = sum(Value - twoYearAgo),
            `5 years` = sum(Value - fiveYearAgo))
sectorRecords = sectorGrowths %>% 
  gather(key = 'period', value = 'growth', 3:5) %>% 
  arrange(desc(growth)) %>% 
  group_by(period, Sector) %>% 
  top_n(1, growth) %>% 
  ungroup() %>% 
  select(Sector, period, growth)

topSectorRecords = sectorRecords %>% 
  group_by(period) %>% 
  top_n(1, growth)

top10SectorRecords = sectorRecords %>% 
  group_by(period) %>% 
  top_n(10, growth)

ggplot(top10SectorRecords, aes(x = reorder(Sector, growth), y = growth, fill = 1)) +
  geom_bar(stat = 'identity') +
  facet_grid(~period, scale = 'free') +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
  guides(fill = F) +
  xlab('Sector')
  