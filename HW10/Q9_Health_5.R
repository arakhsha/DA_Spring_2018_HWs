availability.health = availability %>% filter(str_detect(Topic, "Health"))

healthIndicators = c(
  'SP.POP.GROW',
  'SP.DYN.CBRT.IN',
  'SP.DYN.CDRT.IN',
  'SP.DYN.TFRT.IN',
  'SP.DYN.LE00.IN',
  'SP.DYN.TO65.FE.ZS',
  'SP.DYN.TO65.MA.ZS',
  'SP.POP.DPND',
  'SP.DYN.AMRT.FE',
  'SP.DYN.AMRT.MA',
  'SH.DYN.MORT',
  'SH.DYN.NMRT',
  'SP.DYN.IMRT.IN',
  'SH.IMM.IDPT',
  'SH.IMM.MEAS',
  'SH.MMR.RISK.ZS',
  'SH.IMM.HEPB',
  'SN.ITK.DFCT',
  'SH.ANM.CHLD.ZS',
  'SH.STA.MMRT'
)

plots = list()
for(i in 1:length(healthIndicators)) {
  code = healthIndicators[i]
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
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))
}

ggarrange(plotlist = plots, common.legend = T)
