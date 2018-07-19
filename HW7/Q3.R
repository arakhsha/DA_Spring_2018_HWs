modelParams = data%>% filter(complete.cases(.))
model = glm(MannerOfDeath ~ .,
            data = modelParams, 
            family = "binomial")
summary(model)

modelParams = data %>%
  select(-HispanicOrigin, -CauseRecode, - MonthOfDeath, - ActivityCode) %>%
  filter(complete.cases(.))
model = glm(MannerOfDeath ~ .,
            data = modelParams, 
            family = "binomial")
summary(model)

data = modelParams

library(boot)
diag = glm.diag(model)
glm.diag.plots(model, diag)
