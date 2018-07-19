LE.HCC = data %>% 
  filter(`Indicator Code` %in% c('SH.XPD.PVTD.PP.CD', 'SP.DYN.LE00.IN')) %>% 
  select(`Country Name`, `Country Code`, `2015`, `Indicator Code`) %>% 
  spread(key = "Indicator Code", value = "2015") %>% 
  rename(HCC = `SH.XPD.PVTD.PP.CD`, LE = `SP.DYN.LE00.IN`)

ggplot(LE.HCC, aes(x = HCC, y = LE)) +
  geom_point(aes(color = 1)) +
  guides(color = F) +
  labs(x = "Health Care Costs", y = "Life Expectancy")

ggplot(LE.HCC, aes(x = log(HCC), y = LE)) +
  geom_point(aes(color = 1)) +
  guides(color = F) +
  labs(x = "log(Health Care Costs)", y = "Life Expectancy")
