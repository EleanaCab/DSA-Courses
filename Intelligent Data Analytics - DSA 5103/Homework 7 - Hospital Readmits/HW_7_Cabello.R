library(viridis)
library(naniar)
library(Amelia)
library(mice)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(MASS)
library(caret)
library(plyr)
library(gridExtra)
library(rattle)
library(MLmetrics)
library(corrplot)
library(pastecs)
library(earth)

# Reading in data
readminData = read.csv('hm7-Train-r1.csv')

# Some columns have no values but are ""
# Converting those to Na
colSums(readminData == "")
readminData = readminData %>% mutate(gender = na_if(gender, ""))
readminData = readminData %>% mutate(age = na_if(age, ""))
colSums(is.na(readminData))

# Set of attributes with no missing data
noMissingData = subset(readminData, select = -c(race, time_in_hospital, payer_code, medical_specialty, indicator_level, num_lab_procedures, diagnosis, gender, age))

# Set of attribuites with missing values
missingData = subset(readminData, select = c(patientID, race, time_in_hospital, payer_code, medical_specialty, indicator_level, num_lab_procedures, diagnosis, gender, age))

# Visualizing missing values
gg_miss_var(missingData)
md.pattern(missingData, plot = FALSE)

######################################################
# IMPUTATION OF MISSING VALUES

# Set for imputed data to go
imput_data = missingData

#Finding mode of diagnosis and filling missing values with it
no_missing_diag = subset(missingData, !is.na(missingData$diagnosis))
imput_data$diagnosis[is.na(imput_data$diagnosis)] = names(which.max(table(no_missing_diag$diagnosis)))

#Finding mode of gender and filling missing values with it
no_missing_gender = subset(missingData, !is.na(missingData$gender))
imput_data$gender[is.na(imput_data$gender)] = names(which.max(table(no_missing_gender$gender)))

#Finding mode of age and filling missing values with it
no_missing_age = subset(missingData, !is.na(missingData$age))
imput_data$age[is.na(imput_data$age)] = names(which.max(table(no_missing_age$age)))

#Finding mode of race and filling missing values with it
no_missing_race = subset(missingData, !is.na(missingData$race))
imput_data$race[is.na(imput_data$race)] = names(which.max(table(no_missing_age$race)))

# LM model for imputing time_in_hospital

# Set of numeric varaibles
numericAtts = select_if(noMissingData, is.numeric)

# set of all completed attributes and time_in_hospital
all_miss_com_time = cbind(numericAtts, time_in_hospital = missingData$time_in_hospital)
# set of just completed cases of the set above (all_miss_com_time)
complete_time = all_miss_com_time[complete.cases(all_miss_com_time),]
# set of just cases with missing time_in_hospital of the set above (all_miss_com_time)
missing_time = subset(all_miss_com_time[is.na(all_miss_com_time$time_in_hospital),], select = -c(time_in_hospital))

#linea regression models made used to pick the best combination of attributes and intercations
imput_fit = lm(time_in_hospital~., data = subset(complete_time, select = -c(patientID)))
summary(imput_fit)

# Missing values imputed from model 
imput_time = data.frame(patientID = missing_time$patientID, preds = round(predict(imput_fit, newdata = missing_time), 0))

# Adding imputed values based on patientID 
for (x in 1:4) {
  imput_data$time_in_hospital[imput_data$patientID == imput_time$patientID[x]] = imput_time$preds[x]
}

# LM model for imputing indication_level

all_miss_com_inlevel = cbind(numericAtts, indicator_level = missingData$indicator_level)
complete_inlevel = all_miss_com_inlevel[complete.cases(all_miss_com_inlevel),]
missing_inlevel = subset(all_miss_com_inlevel[is.na(all_miss_com_inlevel$indicator_level),], select = -c(indicator_level))

imput_fit = lm(indicator_level~., data = subset(complete_inlevel, select = -c(patientID)))
summary(imput_fit)

imput_inlevel = data.frame(patientID = missing_inlevel$patientID, preds = round(predict(imput_fit, newdata = missing_inlevel), 2))

for (x in 1:6) {
  imput_data$indicator_level[imput_data$patientID == imput_inlevel$patientID[x]] = imput_inlevel$preds[x]
}


# LM model for imputing num_lab_procedures

all_miss_com_numlab = cbind(numericAtts, num_lab_procedures = missingData$num_lab_procedures)
complete_numLab = all_miss_com_numlab[complete.cases(all_miss_com_numlab),]
missing_numLab = subset(all_miss_com_numlab[is.na(all_miss_com_numlab$num_lab_procedures),], select = -c(num_lab_procedures))

imputation_fit = lm(num_lab_procedures~., data = subset(complete_numLab, select = -c(patientID, admission_source, number_inpatient, num_procedures)))
summary(imputation_fit)

imput_numlab = data.frame(patientID = missing_numLab$patientID, preds = round(predict(imputation_fit, newdata = missing_numLab), 0))

imput_data$num_lab_procedures[imput_data$patientID == imput_numlab$patientID[1]] = imput_numlab$preds[1]

# For remaining attributes with missing value, assigning unknown to them
imput_data[is.na(imput_data)] = 'NotRecorded'

# New set of all the imputed and modified data 
completeData = cbind(noMissingData, subset(imput_data, select = -c(patientID)))
glimpse(completeData)

# Changing certain attributes to factors
cols = c(11:36,38, 40, 41, 44, 45, 46)
completeData[,cols] = lapply(completeData[,cols] , factor)

# Looking at basic statistics
summary(completeData)
stat.desc(completeData)

#####################################################
# DATA SPLITTING
set.seed(10)

train_data = subset(completeData, select = -c(patientID))
train_data$readmitted = as.factor(train_data$readmitted)

# Set of numeric variables 
numericAtts = select_if(train_data, is.numeric)
numericAtts$readmitted = train_data$readmitted

# Set of non-numeric variables 
nonnumericAtts = select_if(train_data, negate(is.numeric))
nonnumericAtts$readmitted = train_data$readmitted

#####################################################
# MODELS: 

# DECISCION TREE OF NON-NUMERIC
tree_fit_cat = train(readmitted ~ ., 
                 subset(nonnumericAtts, select = -c(glimepiride.pioglitazone, examide, citoglipton, troglitazone, medical_specialty, diagnosis)), 
                 method="rpart", 
                 trControl = trainControl(method = "cv", number = 10))
print(tree_fit_cat)
fancyRpartPlot(tree_fit_cat$finalModel)


# DECISCION TREE OF NUMERIC
tree_fit_num = train(readmitted ~ ., 
                 numericAtts, 
                 method="rpart", 
                 trControl = trainControl(method = "cv", number = 10))
print(tree_fit_num)
fancyRpartPlot(tree_fit_num$finalModel)


# SET OF ATTRIBUTES FROM NODE SPLITS OF TREES ABOVE
optSet = subset(train_data, select = c(readmitted, diabetesMed, metformin, insulin, payer_code, A1Cresult, number_diagnoses, number_inpatient, number_emergency, number_outpatient, num_lab_procedures))

# DECSION TREE OF OPTIMAL SUBSET
tree_fit = train(readmitted ~ ., 
                 optSet, 
                  method="rpart", 
                  trControl = trainControl(method = "cv", number = 10, savePredictions = TRUE),
                 tuneLength = 5)
print(tree_fit)
fancyRpartPlot(tree_fit$finalModel)

# PREDICTIONS FROM HOLDOUT FOLDS OF CV
predResults = data.frame(tree_fit$pred)
predResults = predResults[predResults$cp == tree_fit$bestTune$cp, ]

# splitting based on fold number
resamp_fold_1 = predResults[predResults$Resample == 'Fold01', ]
resamp_fold_2 = predResults[predResults$Resample == 'Fold02', ]
resamp_fold_3 = predResults[predResults$Resample == 'Fold03', ]
resamp_fold_4 = predResults[predResults$Resample == 'Fold04', ]
resamp_fold_5 = predResults[predResults$Resample == 'Fold05', ]
resamp_fold_6 = predResults[predResults$Resample == 'Fold06', ]
resamp_fold_7 = predResults[predResults$Resample == 'Fold07', ]
resamp_fold_8 = predResults[predResults$Resample == 'Fold08', ]
resamp_fold_9 = predResults[predResults$Resample == 'Fold09', ]
resamp_fold_10 = predResults[predResults$Resample == 'Fold10', ]

resample_list = list(resamp_fold_1, resamp_fold_2,resamp_fold_3, resamp_fold_4,resamp_fold_5,resamp_fold_6,resamp_fold_7,
                     resamp_fold_8, resamp_fold_9, resamp_fold_10)

# F1 SCORES
f1_results = data.frame(Resampling_Holdout_Fold = 1:10)
f1_results$F1_score = lapply(resample_list, function(x) MLmetrics::F1_Score(x$pred,x$obs, positive = "1"))
plot(f1_results, type='o', pch=19)

# ACCURACY 
acc_results = data.frame(Resampling_Holdout_Fold = 1:10)
acc_results$Accuracy = lapply(resample_list, function(x) MLmetrics::Accuracy(x$pred,x$obs))
plot(acc_results, type='o', pch=19)

#AUC 
auc_results = data.frame(Resampling_Holdout_Fold = 1:10)
auc_results$AUC = lapply(resample_list, function(x) MLmetrics::AUC(x$pred,x$obs))
plot(auc_results, type='o', pch=19)

# PRECISION
prec_results = data.frame(Resampling_Holdout_Fold = 1:10)
prec_results$Precision = lapply(resample_list, function(x) MLmetrics::Precision(x$pred,x$obs))
plot(prec_results, type='o', pch=19)

# GLM 
glm_fit = train(readmitted ~.,
                  data = optSet,
                  trControl = trainControl(method = "cv", number = 10),
                  method = "glm",
                  family = "binomial", 
                  preProcess = c("center", "scale"))
summary(glm_fit)
print(glm_fit)

# LDA
lda_fit = train(readmitted ~ .,
                data = optSet,
                trControl = trainControl(method = "cv", number = 10),
                method = "lda", 
                preProcess = c("center", "scale"))

print(lda_fit)

# RANDOM FOREST Model
rf_fit = train(readmitted ~., 
               optSet, 
               method = "rf",
               ntree = 20,
               tuneGrid = expand.grid(.mtry=c(3)),
               trControl = trainControl(method = "cv", number = 10))

print(rf_fit)

# NEURAL NET Model
neuralNet_fit = train(readmitted ~ ., 
                      optSet, 
                      method="nnet", 
                      trControl = trainControl(method = "cv", number = 10), 
                      tuneGrid = expand.grid(size = 3,decay = 0.2))

print(neuralNet_fit)

# MARS Model
mars_fit = train(readmitted ~., 
                  optSet, 
                  method = "earth",
                  preProcess = c("center","scale"),
                  trControl = trainControl(method = "cv", number = 10), 
                  tuneGrid = expand.grid(degree = 2, nprune = 10))

print(mars_fit)

# KNN
numericAtts = select_if(optSet, is.numeric)
numericAtts$readmitted = optSet$readmitted

knn_fit = train(readmitted ~., 
                numericAtts, 
                method = "knn",
                preProcess = c("center","scale"), 
                trControl = trainControl(method = "cv", number = 10), 
                tuneGrid = expand.grid(k = 45))
print(knn_fit)

#####################################################
# PREPARING TESTING DATA AND KAGGLE PREDICTIONS
testData_patientIDS= read.csv('hm7-Test-r1.csv')

# Subset from of attributes from optimal set
testData = subset(testData_patientIDS, select = c(diabetesMed, metformin, insulin, payer_code, A1Cresult, number_diagnoses, number_inpatient, number_emergency, number_outpatient, num_lab_procedures))

# Missing values in payer code become NotRecorded
testData$payer_code[is.na(testData$payer_code)] = 'NotRecorded'

# Changing certain attributes to factors
cols = c(1:5)
testData[,cols] = lapply(testData[,cols] , factor)

#####################################################
# Predictions of test data
preds = predict(tree_fit, newdata = testData, type = "prob")

#Creating submission
submission = data.frame(patientID = testData_patientIDS$patientID, predReadmit = preds[,2])

# Saving CSV
write.csv(submission,'kaggle_preds.csv', row.names = FALSE)
