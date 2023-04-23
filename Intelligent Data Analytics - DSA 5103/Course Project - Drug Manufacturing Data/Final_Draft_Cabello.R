## Libraries Used
library(mice)
library(naniar)
library(dplyr)
library(tidyverse)
library(stringr)
library(Amelia)
library(ggplot2)
library(lattice)
library(corrplot)
library(outliers)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(tidytext)
library(scales)
library(gridExtra)
library(caret)
library(earth)
library(MLmetrics)

## Importing data
drugCosts = read.csv('drug_manufact_cost.csv')

## Turning strings values to numeric
glimpse(drugCosts)

# Deleting commas from strings of numbers
drugCosts$Manufacturing_Cost = str_replace(drugCosts$Manufacturing_Cost, "\\,", "")
drugCosts$Max_Retailer_Price = str_replace(drugCosts$Max_Retailer_Price, "\\,", "")
drugCosts$Max_Consumer_Price = str_replace(drugCosts$Max_Consumer_Price, "\\,", "")
drugCosts$Max_Consumer_VAT_Price = str_replace(drugCosts$Max_Consumer_VAT_Price, "\\,", "")

# Taking strings of numbers and converting into the numeric class 
drugCosts$Manufacturing_Cost = as.numeric(str_extract(drugCosts$Manufacturing_Cost, "\\d+\\.*\\d*"))
drugCosts$Max_Retailer_Price = as.numeric(str_extract(drugCosts$Max_Retailer_Price, "\\d+\\.*\\d*"))
drugCosts$Max_Consumer_Price = as.numeric(str_extract(drugCosts$Max_Consumer_Price, "\\d+\\.*\\d*"))
drugCosts$Max_Consumer_VAT_Price = as.numeric(str_extract(drugCosts$Max_Consumer_VAT_Price, "\\d+\\.*\\d*"))

# Turning all characters in medication_name to uppercase for easier analysis later
drugCosts$Medication_Name = toupper(drugCosts$Medication_Name)
glimpse(drugCosts)

## Missing Values 
gg_miss_var(drugCosts)
missmap(drugCosts, legend = TRUE) # No missing values observed in any of the columns

## Outliers
# Observing any outliers of numeric
qqnorm(drugCosts$Package_Size, main = "Normal Q-Q Plot - Package Size")
qqline(drugCosts$Package_Size, col = "steelblue", lwd = 2)
qqnorm(drugCosts$Manufacturing_Cost, main = "Normal Q-Q Plot - Manufacturing Cost")
qqline(drugCosts$Manufacturing_Cost, col = "steelblue", lwd = 2)
qqnorm(drugCosts$Max_Retailer_Price, main = "Normal Q-Q Plot - Max Retailer Price")
qqline(drugCosts$Max_Retailer_Price, col = "steelblue", lwd = 2)
qqnorm(drugCosts$Max_Consumer_Price, main = "Normal Q-Q Plot - Max Consumer Price")
qqline(drugCosts$Max_Consumer_Price, col = "steelblue", lwd = 2)
qqnorm(drugCosts$Max_Consumer_VAT_Price, main = "Normal Q-Q Plot - Max Consumer VAT Price")
qqline(drugCosts$Max_Consumer_VAT_Price, col = "steelblue", lwd = 2)

# Grubbs Test of Outliers
grubbs.test(drugCosts$Package_Size)
grubbs.test(drugCosts$Manufacturing_Cost)
grubbs.test(drugCosts$Max_Retailer_Price)
grubbs.test(drugCosts$Max_Consumer_Price)
grubbs.test(drugCosts$Max_Consumer_VAT_Price)

## Initial Analysis
numericAtts = subset(drugCosts, select = c(Package_Size, Manufacturing_Cost, Max_Retailer_Price, Max_Consumer_Price, Max_Consumer_VAT_Price))
pairs(numericAtts)

## Correlation of Numeric Attributes
corMat = cor(numericAtts)
corrplot(corMat, type = 'upper')

## Distribution of consumer price
ggplot(drugCosts) + geom_histogram(aes( x = Max_Consumer_Price)) + theme_minimal() + scale_x_continuous(labels = comma) + labs(y = "Frequency")

## Relationship of between package size and consumer price
p1 = ggplot(drugCosts) + geom_point(aes(x = Package_Size, y = Max_Consumer_Price)) + theme_minimal() + scale_y_continuous(labels = comma) + scale_x_continuous(labels = comma)
g1 = grid.arrange(p1, nrow = 1, ncol = 1)
p2 = ggplot(subset(drugCosts, Package_Size < 200), aes(x = Package_Size, y = Max_Consumer_Price)) + geom_point() + geom_smooth(method = 'lm') + theme_minimal() + scale_y_continuous(labels = comma) + scale_x_continuous(labels = comma)
p3 = ggplot(subset(drugCosts, Package_Size >= 200 & Package_Size < 1000), aes(x = Package_Size, y = Max_Consumer_Price)) + geom_point() + geom_smooth(method = 'lm') + theme_minimal() + scale_y_continuous(labels = comma) + scale_x_continuous(labels = comma)
p4 = ggplot(subset(drugCosts, Package_Size >= 1000), aes(x = Package_Size, y = Max_Consumer_Price)) + geom_point() + geom_smooth(method = 'lm') + theme_minimal() + scale_y_continuous(labels = comma) + scale_x_continuous(labels = comma)
g2 = grid.arrange(p2,p3,p4, nrow = 1, ncol = 3)
grid.arrange(g1,g2, nrow = 2, ncol = 1)

## Relationship of between manufacturing cost and consumer price
p1 = ggplot(drugCosts) + geom_point(aes(x = Manufacturing_Cost, y = Max_Consumer_Price)) + theme_minimal() + scale_y_continuous(labels = comma) + scale_x_continuous(labels = comma)
g1 = grid.arrange(p1, nrow = 1, ncol = 1)
p2 = ggplot(subset(drugCosts, Manufacturing_Cost < 50000), aes(x = Manufacturing_Cost, y = Max_Consumer_Price)) + geom_point() + geom_smooth(method = 'lm') + theme_minimal() + scale_y_continuous(labels = comma) + scale_x_continuous(labels = comma)
p3 = ggplot(subset(drugCosts, Manufacturing_Cost >= 50000), aes(x = Manufacturing_Cost, y = Max_Consumer_Price)) + geom_point() + geom_smooth(method = 'lm') + theme_minimal() + scale_y_continuous(labels = comma) + scale_x_continuous(labels = comma)
g2 = grid.arrange(p2,p3, nrow = 1, ncol = 2)
grid.arrange(g1,g2, nrow = 2, ncol = 1)

## Wordcloud of frequent methods
#Cleaning strings 
name_text = Corpus(VectorSource(drugCosts$Medication_Name))
name_text_clean = tm_map(name_text, removeNumbers)
name_text_clean = tm_map(name_text_clean, tolower)
name_text_clean = tm_map(name_text_clean, removeWords, c("mg", "ml", " \ ", "mcg"))
wordcloud(words = name_text_clean, min.freq = 10, scale=c(5, 1), random.order = FALSE, random.color = FALSE, colors= rainbow(5))

## Looking at most frequent methods types captured
#Cleaning strings of specific characters and substrings
drug_names = data.frame(names = drugCosts$Medication_Name)
drug_names$names = gsub("MG", "", drug_names$names)
drug_names$names = gsub("ML", "", drug_names$names)
drug_names$names = gsub("MCG", "", drug_names$names)
drug_names$names = gsub('[0-9]*', "", drug_names$names)

#Table of most frequent strings
method_words = drug_names %>% unnest_tokens(output = word, input = names)
method_cord_count = method_words %>% count(word, sort = TRUE)
method_cord_count

## Creating methods column 
# Based on the most frequent methods found previously
drugCosts$method[str_detect(drugCosts$Medication_Name, "TAB")] = "TAB"
drugCosts$method[str_detect(drugCosts$Medication_Name, "VIAL")] = "VIAL"
drugCosts$method[str_detect(drugCosts$Medication_Name, "CAP")] = "CAP"
drugCosts$method[str_detect(drugCosts$Medication_Name, "SYR")] = "SYR"
drugCosts$method[str_detect(drugCosts$Medication_Name, "INJ")] = "INJECT"
drugCosts$method[str_detect(drugCosts$Medication_Name, "PEN")] = "PEN"
drugCosts$method[str_detect(drugCosts$Medication_Name, "DROP")] = "DROPS"
drugCosts$method[str_detect(drugCosts$Medication_Name, "POWDER")] = "POWDER"
drugCosts$method[str_detect(drugCosts$Medication_Name, "SOLU")] = "SOLUTION"
drugCosts$method[str_detect(drugCosts$Medication_Name, "CREAM")] = "CREAM"
drugCosts$method[str_detect(drugCosts$Medication_Name, "PATCH")] = "PATCH"
drugCosts$method[str_detect(drugCosts$Medication_Name, "GEL")] = "GEL"
drugCosts$method[str_detect(drugCosts$Medication_Name, "OINT")] = "OINT"
drugCosts$method[str_detect(drugCosts$Medication_Name, "SPRAY")] = "SPRAY"
drugCosts$method[str_detect(drugCosts$Medication_Name, "BAG")] = "BAG"
drugCosts$method[is.na(drugCosts$method)] = "OTHER"
drugCosts$method = as.factor(drugCosts$method)

## Distribution of methods
ggplot(drugCosts) + geom_bar(aes(x = method)) + theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + labs(y = 'Frequency')

## Methods and manufacturing cost comparison
p1 = ggplot(subset(drugCosts, Manufacturing_Cost < 50000), aes(fill = method, x = Manufacturing_Cost)) + geom_histogram() + theme_minimal() + scale_y_continuous(labels = comma) + scale_x_continuous(labels = comma) + labs(y = "Frequency")
p2 = ggplot(subset(drugCosts, Manufacturing_Cost >= 50000), aes(fill = method, x = Manufacturing_Cost)) + geom_histogram() + theme_minimal() + scale_y_continuous(labels = comma) + scale_x_continuous(labels = comma) + labs(y = "Frequency")
g1 = grid.arrange(p1,p2, nrow = 1, ncol = 2)

## Methods and consumer price comparison
ggplot(drugCosts, aes(fill = method, x = Max_Consumer_Price)) + geom_histogram() + theme_minimal() + labs(y = 'Frequency') + scale_y_continuous(labels = comma) + scale_x_continuous(labels = comma)

#Split into numerical and categorical data
#Extract numeric and factor columns
dataNumeric = drugCosts %>% dplyr::select_if(is.numeric)
dataFactor = drugCosts %>% dplyr::select_if(negate(is.numeric))

# Create a function for data quality report for Numerical Variable
Q1 = function(x, na.rm= TRUE) {
  quantile(x,na.rm=na.rm)[2]
}

Q3 = function(x, na.rm= TRUE) {
  quantile(x,na.rm=na.rm)[4]
}

theNumericSummary = function(x){
  c(length(x), n_distinct(x), sum(is.na(x)), mean(x,na.rm=TRUE),
    min(x,na.rm=TRUE), Q1(x,na.rm=TRUE), median(x,na.rm=TRUE),
    Q3(x,na.rm=TRUE), max(x,na.rm=TRUE), sd(x,na.rm=TRUE))
}

numericSummary = dataNumeric %>% dplyr::summarise(round(across(everything(),theNumericSummary)))
numericSummary = cbind(stat = c("n", "unique", "missing", "mean", "min",
                                 "Q1", "median", "Q3", "max", "sd"), numericSummary)

numericSummaryFinal = numericSummary %>%
  pivot_longer("Package_Size":"Year", names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(missing_pct = 100*missing/n, unique_pct = 100*unique/n) %>%
  dplyr::select(variable, n, missing, missing_pct, unique, unique_pct, everything())


#Final Data Quality Report Table
options(digits=3)
options(scipen=99)
numericSummaryFinal

#Create a function for data quality report for Factor Variable
#Mode Functions
getmodes = function(v,type=1) {
  tbl = table(v)
  m1=which.max(tbl)
  if (type==1) {
    return (names(m1)) #1st mode
  }
  else if (type==-1) {
    return (names(which.min(tbl))) #least common mode
  }
  else {
    stop("Invalid type selected")
  }
}


#Mode Frequency Function
getmodesCnt = function(v,type=1) {
  tbl = table(v)
  m1=which.max(tbl)
  if (type==1) {
    return (max(tbl)) #1st mode freq
  }
  else if (type==-1) {
    return (min(tbl)) #least common freq
  }
  else {
    stop("Invalid type selected")
  }
}

#create a new function called thefactorsummary
theFactorSummary = function(x){
  c(length(x), n_distinct(x), sum(is.na(x)),
    getmodes(x, type=1), getmodesCnt(x, type=1),
    getmodes(x, type=-1), getmodesCnt(x, type=-1))
}
#apply function to all columns of datafactor and summarize 
factorSummary = dataFactor %>% dplyr::summarise(across(everything(), theFactorSummary))


#add labels
factorSummary = cbind(stat = c("n", "unique", "missing", "1st mode",
                                "1st mode freq",
                                "least common", "least common freq"), factorSummary)

#create final data quality report for factor variables
factorSummaryFinal = factorSummary %>%
  pivot_longer("Medication_Name":"method", names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(missing_pct = 100*as.double(missing)/as.double(n),
         unique_pct = 100*as.double(unique)/as.double(n)) %>%
  dplyr::select(variable, n, missing, missing_pct, unique, unique_pct, everything())

#visualize data quality report for factor variables
options(digits=3)
options(scipen=99)
factorSummaryFinal

#Data Preparation
#Log Transformation of Max_Consumer_Price, Numerical Input Variables
dataNumeric$Max_Consumer_Price = log(dataNumeric$Max_Consumer_Price + 1)

glimpse(dataNumeric)

#Feature Engineering
#Feature Creation (One-Hot Encoding of Non Numeric Variable)
#The Medication_Name attributes have so many unique variables, which will make it difficult for our analysis,
#resulting in a multidimensional dataset. So we are going to drop it from our data set.

#The Year attributes has only one unique variables, which will make it difficult for our analysis,
#resulting in a multidimensional dataset. So we are going to drop it from our data set.
dataFactor = subset(dataFactor, select = -c(Medication_Name))
dataNumeric = subset(dataNumeric, select = -c(Year))

#Define one-hot encoding function
dummy = dummyVars("~.", data = dataFactor)

#Implement the one-hot encoding function
dataFactor = data.frame(predict(dummy, newdata=dataFactor))
head(dataFactor)

#Feature Extraction using Principal Component Analysis (PCA)
#First combine numerical and categorical variables
final_df = cbind(dataNumeric, dataFactor)
summary(final_df)

#Create target label
y_pca = subset(final_df, select = c(Max_Consumer_Price))

#Create numeric dataframe for PCA analysis
x_pca = subset(final_df, select = -c(Max_Consumer_Price))

#Analyze the proportion of variance from the different principal components
pca = prcomp(x_pca, center = TRUE, scale. = TRUE)
summary(pca)

#Model Development

#use 85% of dataset as training set and 15% as test set
sample_ft = sample(c(TRUE, FALSE), nrow(final_df), replace=TRUE, prob=c(0.85,0.15))
train_ft  = final_df[sample_ft, ]
test_ft   = final_df[!sample_ft, ]

#define predictor and response variables in test set
test_x_ft = subset(test_ft, select = -c(Max_Consumer_Price))
test_y_ft = subset(test_ft, select = c(Max_Consumer_Price))

# GLM Model using all train data
set.seed(10)
glm_fit = train(Max_Consumer_Price ~.,
                data = subset(train_ft, select = -c(Max_Consumer_VAT_Price)),
                trControl = trainControl(method = "cv", number = 10),
                method = "glm",
                preProcess = c("center", "scale"))
summary(glm_fit)
print(glm_fit)

# GLM Model using significant attributes from previous glm results
glm_set = subset(train_ft, select = c(Max_Consumer_Price, Package_Size, Max_Retailer_Price, method.CAP, method.DROPS, method.OTHER, method.SYR, method.TAB, method.CREAM, method.GEL, method.OINT, method.SOLUTION))
glm_best_fit = train(Max_Consumer_Price ~.,
                data = glm_set,
                trControl = trainControl(method = "cv", number = 10),
                method = "glm",
                preProcess = c("center", "scale"))
print(glm_best_fit)

#plotting r-squared and rmse results through CV
par(mfrow=c(1,2))
plot(x = seq(1, 10, 1), y = glm_best_fit$resample$Rsquared, xlab="CV Fold", ylab="R-Squared", type="b")
plot(x = seq(1, 10, 1), y = glm_best_fit$resample$RMSE, xlab="CV Fold", ylab="RMSE", type="b")

# Taking subset of testing data and producing predictions from glm model
test_glm = subset(test_x_ft, select = c(Package_Size, Max_Retailer_Price, method.CAP, method.DROPS, method.OTHER, method.SYR, method.TAB, method.CREAM, method.GEL, method.OINT, method.SOLUTION))
predictions = data.frame(glm = predict(glm_best_fit, test_glm))

# Performance measures of model on testing
glm_rmse = RMSE(predictions$glm, test_y_ft$Max_Consumer_Price)
glm_rsq = R2_Score(predictions$glm, test_y_ft$Max_Consumer_Price)

#LASSO Model using all train data
#Tuning of lambda hyperparameters
lasso_fit_all = train(Max_Consumer_Price ~ ., 
                      data = train_ft,
                       method= "glmnet",
                       trControl = trainControl(method = "cv", number = 10),
                       preProcess = c("center", "scale"),
                       tuneGrid = expand.grid(alpha = 1, lambda = seq(0.1, 1, 0.001)))
# Plotting results
ggplot(lasso_fit_all) + geom_vline(aes(xintercept=lasso_fit_all$bestTune$lambda), color = 'red')
lasso_fit_all

#LASSO Model using subset of training data previously used 
#Tuning of lambda hyperparameters
lasso_fit_subset = train(data = glm_set,
                       Max_Consumer_Price ~ .,
                       method= "glmnet",
                       trControl = trainControl(method = "cv", number = 10),
                       preProcess = c("center", "scale"), 
                       tuneGrid = expand.grid(alpha = 1, lambda = seq(0,0.01,0.0001)))
# Plotting results
ggplot(lasso_fit_subset) + geom_vline(aes(xintercept=lasso_fit_subset$bestTune$lambda), color = 'red')
lasso_fit_subset

# BEST LASSO MODEL using subset of data 
lasso_best_fit = train(data = glm_set,
                  Max_Consumer_Price ~ .,
                  method= "glmnet",
                  trControl = trainControl(method = "cv", number = 10),
                  preProcess = c("center", "scale"), 
                  tuneGrid = expand.grid(alpha = 1, lambda = lasso_fit_subset$bestTune$lambda))
lasso_best_fit

#plotting r-squared and rmse results through CV
par(mfrow=c(1,2))
plot(x = seq(1, 10, 1), y = lasso_best_fit$resample$Rsquared, xlab="CV Fold", ylab="R-Squared", type="b")
plot(x = seq(1, 10, 1), y = lasso_best_fit$resample$RMSE, xlab="CV Fold", ylab="RMSE", type="b")

# predicting test subset
test_lasso = subset(test_x_ft, select = c(Package_Size, Max_Retailer_Price, method.CAP, method.DROPS, method.OTHER, method.SYR, method.TAB, method.CREAM, method.GEL, method.OINT, method.SOLUTION))
predictions$lasso = predict(lasso_best_fit, test_lasso)

lasso_rmse = RMSE(predictions$lasso, test_y_ft$Max_Consumer_Price)
lasso_rsq = R2_Score(predictions$lasso, test_y_ft$Max_Consumer_Price)

# MARS Model using all train data
# finding optimal hyperparameters
mars_fit_all= train(Max_Consumer_Price~., 
                  data=train_ft, 
                  method = "earth",
                  preProcess = c("center","scale"), 
                  trControl = trainControl(method = "cv", number = 10),  
                  tuneGrid = expand.grid(degree = 1:4, nprune = seq(2,6,1) %>% floor()))
plot(mars_fit_all)
mars_fit_all

# MARS Model using subset of training data
# finding optimal hyperparameters
mars_fit_subset = train(Max_Consumer_Price~., 
                    data=glm_set, 
                    method = "earth",
                    preProcess = c("center","scale"), 
                    trControl = trainControl(method = "cv", number = 10),  
                    tuneGrid = expand.grid(degree = 1:3, nprune = seq(2,15,1) %>% floor()))
plot(mars_fit_subset)
mars_fit_subset

# BEST MARS MODEL using subset of train data 
mars_best_fit= train(Max_Consumer_Price~., 
                     data=glm_set, 
                     method = "earth",
                     preProcess = c("center","scale"), 
                     trControl = trainControl(method = "cv", number = 10),  
                     tuneGrid = expand.grid(degree = mars_fit_subset$bestTune$degree, nprune = mars_fit_subset$bestTune$nprune))
mars_best_fit

#plotting r-squared and rmse results through CV
par(mfrow=c(1,2))
plot(x = seq(1, 10, 1), y = mars_best_fit$resample$Rsquared, xlab="CV Fold", ylab="R-Squared", type="b")
plot(x = seq(1, 10, 1), y = mars_best_fit$resample$RMSE, xlab="CV Fold", ylab="RMSE", type="b")

# use test data to evaluate model performance
predictions$mars = predict(mars_best_fit, test_x_ft)
mars_rmse = RMSE(test_y_ft$Max_Consumer_Price, predictions$mars)
mars_rsq = R2_Score(predictions$mars, test_y_ft$Max_Consumer_Price)

# RANDOM FOREST 
rf_fit_subset = train(Max_Consumer_Price ~ ., 
               glm_set, 
               method = "rf",
               ntree = 20,
               tuneGrid = expand.grid(.mtry=c(1,2,3,4,5)),
               trControl = trainControl(method = "cv", number = 10))

plot(rf_fit_subset)
rf_fit_subset

##BEST RANDOM FOREST FIT
rf_best_fit = train(Max_Consumer_Price ~ ., 
                      glm_set, 
                      method = "rf",
                      ntree = 20,
                      tuneGrid = expand.grid(.mtry=c(5)),
                      trControl = trainControl(method = "cv", number = 10))

rf_best_fit

par(mfrow=c(1,2))
plot(x = seq(1, 10, 1), y = rf_best_fit$resample$Rsquared, xlab="CV Fold", ylab="R-Squared", type="b")
plot(x = seq(1, 10, 1), y = rf_best_fit$resample$RMSE, xlab="CV Fold", ylab="RMSE", type="b")

# use test data to evaluate model performance
predictions$rf = predict(rf_best_fit, test_x_ft)
rf_rmse = RMSE(test_y_ft$Max_Consumer_Price, predictions$rf)
rf_rsq = R2_Score(predictions$rf, test_y_ft$Max_Consumer_Price)

# Overall comparison  of R2 and RMSE throughout CV between models
r_sq_results = data.frame(fold = seq(1, 10, 1), glm = glm_best_fit$resample$Rsquared, lasso = lasso_best_fit$resample$Rsquared, mars = mars_best_fit$resample$Rsquared, rf = rf_best_fit$resample$Rsquared)
ggplot(r_sq_results) + geom_line(aes(x = fold, y= lasso), color = "red", size = 1) + geom_point(aes(x = fold, y= lasso), color = "red") + geom_line(aes(x = fold, y= mars), color = "blue", size = 1) + geom_point(aes(x = fold, y= mars), color = "blue") + geom_line(aes(x = fold, y= glm), color = "black",size = 1) + geom_point(aes(x = fold, y= glm), color = "black") + geom_line(aes(x = fold, y= rf), color = "green",size = 1) + geom_point(aes(x = fold, y= rf), color = "green")  + labs(x = "CV Fold", y = "R-Squared")

rmse_results = data.frame(fold = seq(1, 10, 1), glm = glm_best_fit$resample$RMSE, lasso = lasso_best_fit$resample$RMSE, mars = mars_best_fit$resample$RMSE, rf = rf_best_fit$resample$RMSE)
ggplot(rmse_results) + geom_line(aes(x = fold, y= lasso), color = "red", size = 1) + geom_point(aes(x = fold, y= lasso), color = "red") + geom_line(aes(x = fold, y= mars), color = "blue", size = 1) + geom_point(aes(x = fold, y= mars), color = "blue") + geom_line(aes(x = fold, y= glm), color = "black", size = 1) + geom_point(aes(x = fold, y= glm), color = "black") + geom_line(aes(x = fold, y= rf), color = "green",size = 1) + geom_point(aes(x = fold, y= rf), color = "green") + labs(x = "CV Fold", y = "RMSE")

# Table of testing results 
test_results = matrix(c(glm_rmse, glm_rsq, lasso_rmse, lasso_rsq, mars_rmse, mars_rsq), ncol=2, byrow=TRUE)
colnames(test_results) = c('RMSE','R-Squared')
rownames(test_results) = c('GLM','LASSO','MARS')
test_results = as.table(test_results)
test_results
