library(h2o)
# h2o.init()

predictors = c(
  'BM.KLT.DINV.WD.GD.ZS',
  'GC.TAX.TOTL.GD.ZS',
  'BX.GSR.TOTL.CD',
  'NY.GNP.MKTP.KD',
  'NY.GDP.MKTP.KD.ZG',
  'BM.GSR.TOTL.CD'
)

predictorsInfo = series %>% 
  filter(`Series Code` %in% predictors) %>% 
  select(`Series Code`,`Indicator Name`)

kable(predictorsInfo, caption = "Model Predictors")


modelData = data %>% 
  select(-X63, -`Indicator Name`, -`Country Code`) %>% 
  filter(`Indicator Code`%in% c(predictors, 'NY.GDP.MKTP.KD.ZG')) %>% 
  filter(`Country Name` == 'Iran, Islamic Rep.') %>%
  gather(key = 'year', value = 'value', `1960`:`2017`) %>% 
  spread('Indicator Code', 'value') %>% 
  arrange(year) %>% 
  mutate(result = lead(`NY.GDP.MKTP.KD.ZG`)) %>% 
  select(- `Country Name`, - year) %>% 
  drop_na() %>% 
  as.data.frame()

model = h2o.glm(y = "result", x = setdiff(colnames(modelData), "result"),
        training_frame = as.h2o(modelData),
        family = "gaussian")

h2o.mse(model)
