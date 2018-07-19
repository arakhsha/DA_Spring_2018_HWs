iran.availableIndicators = data %>% 
  select(`Country Name`, `Indicator Code`, `Indicator Name`, `1996`:`2015`) %>% 
  filter(`Country Name` == "Iran, Islamic Rep.") %>% 
  gather(key = "year", value = "value", `1996`:`2015`) %>%
  drop_na() %>% 
  group_by(`Indicator Name`, `Indicator Code`) %>% 
  summarise(count = n()) %>% 
  left_join(series %>% select(`Series Code`, Topic), by = c("Indicator Code" = "Series Code")) %>% 
  filter(count >= 20)

availability = data %>% 
  select(-X63) %>% 
  gather("year", "value", `1960`:`2017`) %>% 
  drop_na() %>% 
  group_by(`Indicator Code`, `Indicator Name`) %>%
  summarise(count = n()) %>% 
  left_join(series %>% select(`Series Code`, Topic, `Long definition`), by = c("Indicator Code" = "Series Code")) %>% 
  filter(`Indicator Code` %in% iran.availableIndicators$`Indicator Code`)


availability.economics = availability %>% filter(str_detect(Topic, "Econo"))

economicIndicators = c(
  'NY.GDP.PCAP.CD',
  'NY.ADJ.AEDU.GN.ZS',
  'NE.EXP.GNFS.ZS',
  'NE.IMP.GNFS.ZS',
  'NE.TRD.GNFS.ZS',
  'NY.GNP.PCAP.CD',
  'NE.GDI.TOTL.ZS',
  'NY.GDS.TOTL.ZS',
  'NV.IND.TOTL.CD',
  'NV.IND.MANF.CD',
  'BX.KLT.DINV.WD.GD.ZS',
  'NV.AGR.TOTL.ZS',
  'NV.SRV.TETC.ZS',
  'NY.GSR.NFCY.CD',
  'NE.CON.TETC.CD',
  'NE.DAB.TOTL.CD',
  'SL.UEM.TOTL.ZS',
  'FP.CPI.TOTL.ZG',
  'NY.GDP.MKTP.KD.ZG',
  'NY.ADJ.DMIN.GN.ZS'
)


selectedSeries = series %>% filter(`Series Code` %in% economicIndicators)


plots = list()
for(i in 1:length(economicIndicators)) {
  code = economicIndicators[i]
  name = series %>% filter(`Series Code` == code) %>%
    .$`Indicator Name` %>% 
    str_extract("^[^\\(]*(?=\\()")
  sub = series %>% filter(`Series Code` == code) %>%
    .$`Indicator Name` %>% 
    str_extract("\\(.*$") %>% 
    str_replace_all("\\(", "") %>% 
    str_replace_all("\\)", "") 

  plotData = data %>% 
    filter(`Indicator Code` == code) %>% 
    select(-`Indicator Name`, -`Indicator Code`, -X63) %>% 
    gather('year', 'value', `1960`:`2016`) %>% 
    filter(year >= 1996)
  
  iranData = plotData %>% 
    filter(`Country Name`== 'Iran, Islamic Rep.')

  yLimits = numeric(2)
  yLimits[1] = min(quantile(plotData$value, c(0.1, 0.9), na.rm = T)[1], min(iranData$value))
  yLimits[2] = max(quantile(plotData$value, c(0.1, 0.9), na.rm = T)[2], max(iranData$value))
  
  plots[[i]] = ggplot() +
    geom_boxplot(data = plotData, aes(x = year, y = value, fill = 1), outlier.shape = NA) +
    geom_line(data= iranData, aes(x = year, y = value, group = 1, color = "red")) +
    ylim(yLimits) +
    labs(title = name, y = "", x = "", subtitle = sub) +
    scale_color_manual(labels = 'Iran', values = 'red') +
    guides(fill = F) +
    theme(legend.title=element_blank()) +
    theme(plot.title = element_text(size=8), plot.subtitle = element_text(size=6)) +
    theme(axis.text.x = element_text(size = 7)) +
    scale_x_discrete(breaks = seq(2000, 2015, by = 5))
  
}

ggarrange(plotlist = plots, common.legend = T)
