# library(h2o)
# h2o.init()  

model = h2o.glm(
  x = c('LONGITUDE', 'LATITUDE', 'FOCAL_DEPTH', 'EQ_PRIMARY'),
  y = 'TOTAL_DEATHS',
  family = 'poisson',
  training_frame = as.h2o(disaster),
  nfolds = 5)

summary(model)