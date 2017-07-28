PARENT_ID = "syn10154504"

parkinsonEnrolV1 = synTableQuery("select * from syn5720400")@values
parkinsonEnrolV2 = synTableQuery("select * from syn5752777")@values
parkinsonEnrolV3 = synTableQuery("select * from syn8333998")@values

EnrolSurveyV1 = synTableQuery("select * from syn3474927")@values
EnrolSurveyV2 = synTableQuery("select * from syn4898431")@values
#EnrolSurveyV3 = synTableQuery("select * from syn3474927")@values
EnrolSurveyV4 = synTableQuery("select * from syn5706988")@values
EnrolSurveyV5 = synTableQuery("select * from syn5752774")@values

allNames = unique(union_all(colnames(parkinsonEnrolV1), colnames(parkinsonEnrolV2), colnames(parkinsonEnrolV3)))

for(i in setdiff(allNames, colnames(parkinsonEnrolV1)))
  parkinsonEnrolV1[,i] = NA

for(i in setdiff(allNames, colnames(parkinsonEnrolV2)))
  parkinsonEnrolV2[,i] = NA

for(i in setdiff(allNames, colnames(parkinsonEnrolV3)))
  parkinsonEnrolV3[,i] = NA

aggregateParkinsonEnrol = rbindlist(list(parkinsonEnrolV1[,allNames], parkinsonEnrolV2[,allNames], parkinsonEnrolV3[,allNames]))

allNamesEnroll = unique(union_all(colnames(EnrolSurveyV1), colnames(EnrolSurveyV2), colnames(EnrolSurveyV4), colnames(EnrolSurveyV5)))

for(i in setdiff(allNamesEnroll, colnames(EnrolSurveyV1)))
  EnrolSurveyV1[,i] = NA
for(i in setdiff(allNamesEnroll, colnames(EnrolSurveyV2)))
  EnrolSurveyV2[,i] = NA
for(i in setdiff(allNamesEnroll, colnames(EnrolSurveyV4)))
  EnrolSurveyV4[,i] = NA
for(i in setdiff(allNamesEnroll, colnames(EnrolSurveyV5)))
  EnrolSurveyV5[,i] = NA

aggEnrollmentSurvey = rbind(EnrolSurveyV1[,allNamesEnroll], EnrolSurveyV2[,allNamesEnroll])
aggEnrollmentSurvey = rbind(aggEnrollmentSurvey[,allNamesEnroll], EnrolSurveyV4[,allNamesEnroll])
aggEnrollmentSurvey = rbind(aggEnrollmentSurvey[,allNamesEnroll], EnrolSurveyV5[,allNamesEnroll])

combCols = unique(union(colnames(aggEnrollmentSurvey), colnames(aggregateParkinsonEnrol)))

for(i in setdiff(combCols, colnames(aggEnrollmentSurvey)))
  aggEnrollmentSurvey[,i] = NA

for(i in setdiff(combCols, colnames(aggregateParkinsonEnrol)))
  aggregateParkinsonEnrol[,i] = NA

write.csv(aggEnrollmentSurvey[,combCols], file = "combinedEnrolDemographics.csv", row.names=F)
write.table(aggregateParkinsonEnrol[,combCols, with=F], file = "combinedEnrolDemographics.csv", row.names=F, col.names = F, append=T)

combinedDataSet = read.csv("combinedEnrolDemographics.csv", as.is=T) %>% data.table
combinedDataSet = combinedDataSet[, lapply(.SD, function(x) gsub("[[:punct:]]","",x))]

aggregatedStringData = combinedDataSet[,lapply(.(externalId, dataGroups, appVersion, phoneInfo, education, employment, Enter_State, gender, health.history, 
                                                 healthcare.provider, maritalStatus, race, smartphone), 
                                               function(x) paste(unique(x[!is.na(x)]), collapse=";")), by="healthCode"]
aggregatedStringData[(aggregatedStringData == "")] = NA
colnames(aggregatedStringData) = c('healthCode','externalId', 'dataGroups', 'appVersion', 'phoneInfo', 'education', 'employment', 'Enter_State', 'gender', 'health.history', 
                                   'healthcare.provider', 'maritalStatus', 'race', 'smartphone')
####X!=0 as x==0 in cases without medicine
aggregatedMinData = combinedDataSet[, lapply(.(diagnosis.year, medication.start.year, onset.year, when.deep.brain.stimulation)
                                             , function(x) min(c(x[!is.na(x)], Inf), na.rm=T)), by="healthCode"]
aggregatedMinData[(aggregatedMinData == Inf)] = NA
colnames(aggregatedMinData) = c('healthCode', 'diagnosis.year', 'medication.start.year', 'onset.year', 'when.deep.brain.stimulation.earliest')

aggregatedMaxData = combinedDataSet[, lapply(.(age, are.caretaker, deep.brain.stimulation, last.smoked, medical.usage, medical.usage.yesterday,home.usage,
                                               packs.per.day, smoked, surgery, years.smoking, living.alone.status, medication.bool, past.participation, professional.diagnosis,
                                               video.usage, when.deep.brain.stimulation)
                                             , function(x) max(c(x[!is.na(x)], -Inf), na.rm=T)), by="healthCode"]
aggregatedMaxData[(aggregatedMaxData == -Inf)] = NA
colnames(aggregatedMaxData) = c('healthCode','age', 'are.caretaker', 'deep.brain.stimulation', 'last.smoked', 'medical.usage', 'medical.usage.yesterday','home.usage',
                                'packs.per.day', 'smoked', 'surgery', 'years.smoking', 'living.alone.status', 'medication.bool', 'past.participation', 'professional.diagnosis',
                                'video.usage', 'when.deep.brain.stimulation.latest')

aggregatedCountData = combinedDataSet[, lapply(.(recordId), function(x) length(unique(x))), by=healthCode]
colnames(aggregatedCountData) = c('healthCode', 'Num_recordId')

cleanedData = merge(aggregatedCountData, aggregatedStringData)
cleanedData = merge(cleanedData, aggregatedMinData)
cleanedData = merge(cleanedData, aggregatedMaxData)

unlink("combinedEnrolDemographics.csv")

write.csv(cleanedData, file="cleanedMergeEnrolDemo.csv", row.names=F)
obj = File("cleanedMergeEnrolDemo.csv", parentId = PARENT_ID)
synStore(obj, executed = "https://github.com/vp1801/dataMerge/blob/master/dataMerge.R", 
         used = list("syn5720400", "syn5752777", "syn8333998", 
                     "syn3474927", "syn4898431", "syn5706988", "syn5752774"))
unlink("combinedEnrolDemographics.csv")
unlink("cleanedMergeEnrolDemo.csv")

