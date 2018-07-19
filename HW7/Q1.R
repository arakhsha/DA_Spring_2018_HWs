data = data %>% 
  select(
    -Id,
    -Education1989Revision, -EducationReportingFlag, 
    -AgeSubstitutionFlag, -AgeRecode52, -AgeRecode12, -InfantAgeRecode22, -AgeRecode27,
    -CurrentDataYear, -Icd10Code,
    -CauseRecode358, -CauseRecode113, -InfantCauseRecode130,
    -BridgedRaceFlag, -RaceRecode3, -Race, -RaceImputationFlag,
    -HispanicOrigin,
    -PlaceOfInjury,
    -NumberOfRecordAxisConditions, -NumberOfEntityAxisConditions
  ) %>% 
  mutate(
    Age = ifelse(AgeType == 9, 
                 NA,
                 ifelse(AgeType == 1, Age, 0)
                 )
  ) %>% 
  rename (
    Race = RaceRecode5,
    Education = Education2003Revision,
    CauseRecode = CauseRecode39,
    HispanicOrigin = HispanicOriginRaceRecode
  ) %>% 
  select(-AgeType)

data$Education[data$Education %in% c(0, 9)] = NA
data$Age[data$Age > 150] = NA
data$PlaceOfDeathAndDecedentsStatus[data$PlaceOfDeathAndDecedentsStatus == 9] = NA
data$MaritalStatus[data$MaritalStatus == 'U'] = NA
data$DayOfWeekOfDeath[data$DayOfWeekOfDeath == 9] = NA
data$InjuryAtWork[data$InjuryAtWork == 'U'] = NA
data$MethodOfDisposition[data$MethodOfDisposition == 'U'] = NA
data$Autopsy[data$Autopsy == 'U'] = NA
data$ActivityCode[data$ActivityCode == 99] = NA
data$HispanicOrigin[data$HispanicOrigin > 990] = NA
data$MannerOfDeath = data$MannerOfDeath - 2

factorColumns = setdiff(colnames(data), 'Age')
data[factorColumns] = lapply(data[factorColumns], factor)

numericed = data
numericed[] = lapply(data[], as.numeric)

corr_mat = cor(numericed, use="pairwise.complete.obs")
cor_sorted <- as.matrix(sort(corr_mat[,'MannerOfDeath'], decreasing = TRUE))
CorHigh <- names(which(apply(cor_sorted, 1, function(x) TRUE)))
corr_mat <- corr_mat[CorHigh, CorHigh]

corrplot.mixed(corr_mat, tl.col="black", tl.pos = "lt", tl.cex = 0.6,
               number.cex = 0.5, number.digits = 1, lower.col = 'black')

sampled = data %>% sample_n(1000)
pairs(sampled, pch = 19)
