library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(highcharter)
library(readxl)
library(weights)
library(ggplot2)
library(coin)


#Q6
bsa_rds = read_rds('timss_data/bsa.rds')
qdata1 = bind_rows(
          read_xlsx('timss_data/T15_G8_ItemInformation.xlsx', sheet = 'MAT'),
          read_xlsx('timss_data/T15_G8_ItemInformation.xlsx', sheet = 'SCI'))
qdata2 = read_xlsx('timss_data/T15_G8_Codebook.xlsx', sheet = 'BSA')
qdata = left_join(
  qdata1 %>% rename(question = `Item ID`),
  qdata2 %>% rename(question = Variable),
  by = 'question'
)
qdata$`Value Scheme Detailed` = as.factor(qdata$`Value Scheme Detailed`)
qdata$markingType = as.numeric(qdata$`Value Scheme Detailed`)
qdata$question = tolower(qdata$question)
qdata$Key = replace(qdata$Key, qdata$Key == 'A', 1) %>% 
  replace(qdata$Key == 'B', 2) %>% 
  replace(qdata$Key == 'C', 3) %>% 
  replace(qdata$Key == 'D', 4)
qdata$Key = as.numeric(qdata$Key)

geometryApplyingQuestions = qdata %>%
  filter(`Content Domain` == 'Geometry', `Cognitive Domain` == 'Applying') %>% 
  .$question

studentGeomScores = bsa_rds %>% 
  select(idcntry:m062120, itsex) %>% 
  gather(key = 'question', value = 'value', m042182:m062120) %>% 
  filter(question %in% geometryApplyingQuestions) %>% 
  select(idcntry, idstud, itsex, question, value) %>% 
  filter(!is.na(value)) %>% 
  unique()

studentGeomScores = left_join(
  studentGeomScores,
  qdata %>% select(question, Key, markingType, `Maximum Points`),
  by = 'question'
)


#marking schemes:
# View(qdata %>% select(`Value Scheme Detailed`, markingType) %>% unique())

calcScore = function(markingType, value, Key) {
  result = rep(NA, length(value))
  correct = which(
    (!is.na(Key) & (value == Key)) |
    (value >= 19 & value <= 21) |
    (value >= 10 & value <= 12 & markingType < 24)
  )
  partial = which(value >= 10 & value <= 12 & markingType >= 24)
  incorrect = which(
    (!is.na(Key) & value != Key) |
    (value >= 70)
  )
  result[correct] = rep(100, length(correct))
  result[partial] = rep(50, length(partial))
  result[incorrect] = rep(0, length(incorrect))
  return(result)
}

studentGeomScores = studentGeomScores %>% 
  mutate(score = calcScore(markingType, value, Key))

testResult = oneway_test(score ~ as.factor(itsex),
                     data = studentGeomScores,
                     alternative = 'less')

testResult

plotData = studentGeomScores %>% 
  filter(!is.na(itsex)) %>% 
  group_by(itsex) %>% 
  summarise(avg = round(weighted.mean(score,  `Maximum Points`), 2)) %>% 
  mutate(gender = ifelse(itsex == 1, 'female', 'male')) %>% 
  ungroup() 


ggplot(plotData, aes(x = gender, y = avg, fill = as.factor(gender))) +
  geom_bar(stat = 'identity') +
  labs(fill = 'score', y = 'average score') +
  coord_flip() +
  guides(fill = F) +
  labs(title = 'Applying Geometry Average Score By Gender')

hchart(plotData, type = 'bar', hcaes(x = gender, y = avg, color = as.factor(gender))) %>% 
  hc_title(text = "Applying Geometry Average Score By Gender")


# studentsGeometry = studentGeomScores %>% 
#   group_by(idcntry, idstud, itsex) %>% 
#   summarise(score = weighted.mean(score, `Maximum Points`))
# 
# male <- studentGeomScores %>% filter(itsex == 2)
# female <- studentGeomScores %>% filter(itsex == 1)
# 
# testResult = wtd.t.test(
#   x = male$score,
#   y = female$score,
#   weight = male$`Maximum Points`,
#   weighty = female$`Maximum Points`
# )
# 
# 
# plotData = data.frame(
#   gender = c('Female', 'Male'),
#   mean = c(weighted.mean(female$score, female$`Maximum Points`),
#            weighted.mean(male$score, male$`Maximum Points`))
# )
# paste("P-value = ", testResult$coefficients[3])




