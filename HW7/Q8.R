library(h2o)
h2o.init()
hdata = as.h2o(data)
hmodel = h2o.glm(x = setdiff(colnames(data), c('MannerOfDeath' , 'CauseRecode', 'MonthOfDeath', 'ActivityCode')),
                 y = 'MannerOfDeath',
                 training_frame = hdata, 
                 family = "binomial",
                 nfolds = 5)
h2o.confusionMatrix(hmodel)
