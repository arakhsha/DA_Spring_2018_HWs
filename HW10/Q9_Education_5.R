availability.education = availability %>% filter(str_detect(Topic, "Education"))

educationIndicators = c(
  'SE.PRM.DURS',
  'SE.SEC.DURS',
  'SE.PRE.DURS',
  'SE.PRM.AGES',
  'SE.SEC.AGES',
  'SE.PRM.ENRR.FE',
  'SE.PRM.ENRR.MA',
  'SE.PRM.ENRL.FE.ZS',
  'SE.SEC.ENRL.GC.FE.ZS',
  'SE.ENR.PRIM.FM.ZS',
  'SE.SEC.ENRR',
  'SE.ENR.SECO.FM.ZS',
  'SE.SEC.ENRL.FE.ZS',
  'SE.TER.ENRR',
  'SE.PRE.ENRR',
  'SE.PRM.GINT.FE.ZS',
  'SE.PRM.GINT.MA.ZS',
  'SE.PRM.REPT.ZS',
  'SE.ENR.TERT.FM.ZS',
  'SE.XPD.TOTL.GD.ZS'
)


plots = list()
for(i in 1:length(educationIndicators)) {
  code = educationIndicators[i]
  name = series %>% filter(`Series Code` == code) %>%
    .$`Indicator Name` %>% 
    str_extract("^[^\\(]*(?=\\()") %>% 
    str_wrap(width = 40)
    
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
