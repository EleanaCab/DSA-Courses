#DSA 5103-995

# Daniel Addokwei Tetteh
# Hernandez Carlos M
# Cabello Eleana
# Homework 5

library(robustbase)
library(earth)
library(glmnet)
library(elasticnet)
library(naniar)
library(Amelia)
library(dplyr)
library(car)
library(Metrics)
library(MASS)
library(ggplot2)  
library(pls)
library(caret)
library(Hmisc)
library(corrplot)
library (reshape2)
library(MASS)
library(caret)
library(AppliedPredictiveModeling)
library(kernlab)
library(stringr)
library(viridis)
library(forcats)
library(ggridges)
library(hrbrthemes)
library(lars)
library(VIM)
library(corrplot)
library(mice)
library(tidyr)
library(ggbiplot)
library(cvTools)
library(gridExtra)

### Data Pre-processing 

# Reading in the housing data 

housingData <- read.csv("housingData.csv")
head(housingData)

#Creating new variables in the dataset
#i.e. age, ageSinceRemodel and ageOfGarage
housingData <- housingData %>%
  dplyr::mutate(age = YrSold - YearBuilt,
                ageSinceRemodel = YrSold - YearRemodAdd,
                ageofGarage = YrSold - GarageYrBlt)

###view(housingData)

# Checking data summary
summary(housingData)

#Structure of the data
a= str(housingData)
# 1000 rows and 77 columns (variables) including the new ones created

#Creating a new tibble containing only numeric variables
housingNumeric <- dplyr::select(housingData, where(is.numeric))
#view(housingNumeric)

#Creating a new tibble containing only non-numeric variables
housingFactor <- housingData %>%
  as_tibble() %>% dplyr::select_if(is.character) %>%
  dplyr::mutate_if(is.character, as.factor)

#view(housingFactor)

### Missing Values Visualization 
#Numeric Variables

gg_miss_var(housingNumeric)
missmap(housingNumeric, legend = TRUE)
#view(miss_var_summary(housingNumeric))
#There are missing values in LotFrontage, GarageYrBlt,AgeofGarage,MasVnrArea
# Correlations between numeric variables 
heatmap(cor(housingNumeric, use= "complete.obs"))


#Factor Variables
gg_miss_var(housingFactor)
missmap(housingFactor, legend = TRUE)
#view(miss_var_summary(housingFactor))
# We see missing values in 15 different variables 

#Imputations 

#Numeric Variables Imputation 
#1. Impute missing values by PMM
#2. Analyze the variance 

dfMiss <- housingNumeric # Create a new dataframe for housingNumeric
missing_1 <- is.na(dfMiss$LotFrontage) # Get the missing data for LotFrontage
missing_2 <- is.na(dfMiss$GarageYrBlt) # Get the missing data
missing_3 <- is.na(dfMiss$ageofGarage) # Get the missing data
missing_4 <- is.na(dfMiss$MasVnrArea) # Get the missing data


#Populate dfMiss with missing_values indicators
dfMiss$missing_1 <- missing_1 # Putting missing data in dfMiss
dfMiss$missing_2 <- missing_2 # Putting missing data in dfMiss
dfMiss$missing_3 <- missing_3 # Putting missing data in dfMiss
dfMiss$missing_4 <- missing_4 # Putting missing data in dfMiss

#Predict Mean Matching 
#Imputation for LotFrontage

# First create a new dataframe for plotting
x<- housingNumeric$LotArea
y<-housingNumeric$LotFrontage
z<-housingNumeric$SalePrice

df<-data.frame(x,y,z)

dfMiss[missing_1,"LotFrontage"] <- mice.impute.pmm(dfMiss$LotFrontage, !dfMiss$missing_1, dfMiss$LotArea)
dfMiss[missing_2,"GarageYrBlt"] <- mice.impute.pmm(dfMiss$GarageYrBlt, !dfMiss$missing_2, dfMiss$LotArea)
dfMiss[missing_3,"ageofGarage"] <- mice.impute.pmm(dfMiss$ageofGarage, !dfMiss$missing_3, dfMiss$LotArea)
dfMiss[missing_4,"MasVnrArea"] <- mice.impute.pmm(dfMiss$LotFrontage, !dfMiss$missing_4, dfMiss$LotArea)

#Plot for LotFrontage
par(mfrow=c(2,1))
trueMV<-round(mean(df$y, na.rm = TRUE),3)                               
trueVar<-round(var(df$y, na.rm = TRUE),3)
hist(df$y,main="All Data", xlab="x")
abline(v = trueMV, col = "blue", lwd = 2)
text(200, 205, label=paste("Mean:",trueMV, "  Var:", trueVar))

hist(dfMiss$LotFrontage, main="Predictive Mean Matching", xlab="x")
mv<-round(mean(dfMiss$LotFrontage),3)
svar<-round(var(dfMiss$LotFrontage),3)
abline(v = mv, col = "orange", lwd = 2)
text(200, 220, label=paste("Mean:",mv, "  Var:", svar))

plot(dfMiss$LotArea, dfMiss$LotFrontage, col = factor(dfMiss$missing_1), xlab="LotArea", ylab="LotFrontage")

#We can see even distribution of the imputed variables in lot frontage. This can be explained by the variance.
#It is pretty close to that of the original dataset

missmap(dfMiss, legend = TRUE)
#Missing values have been imputed 

#Data Cleaning and Feature Selection 

heatmap(cor(housingNumeric, use= "complete.obs"))
#We will select relevant Features based on correlations
new_numeric <- subset(dfMiss, select = -c(MSSubClass,OverallQual,PoolArea, MoSold, 
                                          YrSold, OverallCond,BsmtFinSF2,EncPorchSF,
                                          missing_1,KitchenAbvGr,
                                          missing_2, missing_3,missing_4,Id))

glimpse(new_numeric)


#Factor Variables Imputation 
#Lets #view the factor variables again 

gg_miss_var(housingFactor)
missmap(housingFactor, legend = TRUE)
#view(miss_var_summary(housingFactor))

#Using a benchmark of 45% missing_data, we will drop some of the variables
housingFactor1<-subset(housingFactor, select = -c(PoolQC, MiscFeature,Alley,Fence,
                                                  FireplaceQu))

glimpse(housingFactor1)
# The remaining data has only few datasets and we can use mode imputations
#We will now create use a mode function to handle this 

getmodes <- function(v,type=1) {
  tbl <- table(v)
  m1<-which.max(tbl)
  if (type==1) {
    return (names(m1)) #1st mode
  }
  else if (type==2) {
    return (names(which.max(tbl[-m1]))) #2nd mode
  }
  else if (type==-1) {
    return (names(which.min(tbl))) #least common mode
  }
  else {
    stop("Invalid type selected")
  }
}


#Apply the mode function to the factor variables with missing_values

housingFactor2<-housingFactor1 %>%
  mutate(across(everything(), ~replace_na(.x,getmodes(.x, type=1 ))))

#change ordinal data to factors
X<- subset(dfMiss, select = c(OverallQual,MSSubClass, MoSold, YrSold, OverallCond))

new_factor= cbind(housingFactor2,X)

glimpse(new_factor)


# Feature Selection
#lets get the target variable 
Y <- housingNumeric$SalePrice
Y <- log(Y) #set output variable as natural log of sale price

str(new_factor)

#Select the right features from numeric dataset based on correlations with SalePrice
cor(new_numeric, method = "pearson", use = "complete.obs")


X_numeric <- subset(new_numeric, select = c(age, ageSinceRemodel, ageofGarage,BsmtFinSF1,
                                            TotalBsmtSF, BsmtUnfSF,MasVnrArea,YearRemodAdd,YearBuilt,
                                            LotArea,LotFrontage,X1stFlrSF,X2ndFlrSF,LowQualFinSF,
                                            GrLivArea,FullBath))


#Select appropriate factor variables based on some domain knowledge
glimpse(new_factor)

X_factor <- subset(new_factor, select = c(MSZoning,LotShape,Neighborhood,BldgType,RoofStyle,
                                          BsmtQual,BsmtCond,BsmtFinType2, BsmtFinType1,
                                          CentralAir,GarageType,MoSold,YrSold,OverallCond,
                                          ExterQual,MasVnrType,GarageFinish,Heating,HeatingQC,
                                          ExterCond,PavedDrive))

# Lets combine the pieces of data to form a new dataframe called data_new

X_new= cbind(X_numeric,X_factor)
data_new= cbind(Y, X_new)

glimpse(data_new)
names(data_new)[1] <-'LogSalePrice'
glimpse(data_new)

#PCA
# We will create a new dataset with PCA on the numeric dataset. 
y_for_pca<- subset(data_new, select = c(LogSalePrice))

gg_miss_var(X_numeric)
missmap(X_numeric, legend = TRUE)
#view(miss_var_summary(housingFactor))

pca <- prcomp(X_numeric, center =TRUE,scale. =TRUE)
summary(pca)

####################################################
## Part A

hold_val = data_new[1:100,]
train_data = data_new[101:1000, ]

y = train_data$LogSalePrice
x = as.matrix(train_data[,2:38])
x = cbind(intercept=1,x) 

ols1 = lm(data=train_data, LogSalePrice ~ age+ ageSinceRemodel+ageofGarage+BsmtFinSF1+
             TotalBsmtSF+ BsmtUnfSF+MasVnrArea+
             LotArea+LotFrontage+X1stFlrSF+X2ndFlrSF+LowQualFinSF+
             FullBath)
summary(ols1)
anova(ols1)
AIC(ols1)
BIC(ols1)
vif(ols1)
rmse(train_data$LogSalePrice, ols1$fitted.values)

ols2 = stepAIC(ols1, direction="both")
rmse(train_data$LogSalePrice, ols2$fitted.values)

# Best interactions are between attributes that have high influence on results, based on pvalues from previous ols model
ols3 = lm(data=train_data, LogSalePrice ~ age*ageSinceRemodel*ageofGarage*TotalBsmtSF*LotFrontage*LotArea*X1stFlrSF*X2ndFlrSF+BsmtFinSF1+
             BsmtUnfSF+MasVnrArea+LowQualFinSF+FullBath)
summary(ols3)
anova(ols3)
AIC(ols3)
BIC(ols3)
rmse(train_data$LogSalePrice, ols3$fitted.values)

#Factor df
train_factor = select_if(train_data, is.factor)
train_factor$LogSalePrice = y

#OLS model to find the significant variables and relationships
ols_factor = lm(data=train_factor, LogSalePrice ~ .)
summary(ols_factor)
anova(ols_factor)
AIC(ols_factor)
BIC(ols_factor)

#new dataset with numeric and factor 
ols4 = lm(data=train_data, LogSalePrice ~ age*ageSinceRemodel*ageofGarage*TotalBsmtSF*LotFrontage*LotArea*X1stFlrSF*X2ndFlrSF+BsmtFinSF1+
                  BsmtUnfSF+MasVnrArea+LowQualFinSF+FullBath+MSZoning+Neighborhood+BldgType+CentralAir+ExterQual+GarageFinish)
summary(ols4)
anova(ols4)
AIC(ols4)
BIC(ols4)
rmse(train_data$LogSalePrice, ols4$fitted.values)

ols5 = stepAIC(ols4, direction="both")
rmse(train_data$LogSalePrice, ols5$fitted.values)

####################################################
#Residuals
residual_data = subset(train_data, select = c(LogSalePrice,age,ageSinceRemodel,ageofGarage,TotalBsmtSF,LotFrontage,LotArea,X1stFlrSF,X2ndFlrSF,BsmtFinSF1,
                                            BsmtUnfSF,MasVnrArea,LowQualFinSF,FullBath,MSZoning,Neighborhood,BldgType,CentralAir,ExterQual,GarageFinish))
residual_data$residuals = ols4$residuals
residual_data$fittedVals = ols4$fitted.values

ggplot(data = residual_data, aes(x =fittedVals , y=residuals)) + geom_point() + geom_hline(yintercept = 0,color = 'red')
ggplot(data = residual_data, aes(x = residuals)) + geom_histogram()
ggplot(data = residual_data, aes(sample = residuals)) + geom_qq() + geom_qq_line() + labs(x = 'Theoretical Quantiles', y = 'Sample Quantiles')

p1 = ggplot(data = residual_data, aes(x =LogSalePrice , y=residuals)) + geom_point() + geom_hline(yintercept = 0,color = 'red')
p2 = ggplot(data = residual_data, aes(x =age , y=residuals)) + geom_point() + geom_hline(yintercept = 0,color = 'red')
p3 = ggplot(data = residual_data, aes(x =ageSinceRemodel , y=residuals)) + geom_point() + geom_hline(yintercept = 0,color = 'red')
p4 = ggplot(data = residual_data, aes(x =ageofGarage , y=residuals)) + geom_point() + geom_hline(yintercept = 0,color = 'red')
p5 = ggplot(data = residual_data, aes(x =TotalBsmtSF , y=residuals)) + geom_point() + geom_hline(yintercept = 0,color = 'red')
p6 = ggplot(data = residual_data, aes(x =LotFrontage , y=residuals)) + geom_point() + geom_hline(yintercept = 0,color = 'red')
p7 = ggplot(data = residual_data, aes(x =LotArea , y=residuals)) + geom_point() + geom_hline(yintercept = 0,color = 'red')
p8 = ggplot(data = residual_data, aes(x =X1stFlrSF , y=residuals)) + geom_point() + geom_hline(yintercept = 0,color = 'red')
p9 = ggplot(data = residual_data, aes(x =X2ndFlrSF , y=residuals)) + geom_point() + geom_hline(yintercept = 0,color = 'red')
p10 = ggplot(data = residual_data, aes(x =BsmtFinSF1 , y=residuals)) + geom_point() + geom_hline(yintercept = 0,color = 'red')
grid.arrange(p1, p2, p3, p4,p5,p6,p7,p8,p9,p10, nrow = 2, ncol = 5)

p1 = ggplot(data = residual_data, aes(x =BsmtUnfSF , y=residuals)) + geom_point() + geom_hline(yintercept = 0,color = 'red')
p2 = ggplot(data = residual_data, aes(x =MasVnrArea , y=residuals)) + geom_point() + geom_hline(yintercept = 0,color = 'red')
p3 = ggplot(data = residual_data, aes(x =LowQualFinSF , y=residuals)) + geom_point() + geom_hline(yintercept = 0,color = 'red')
p4 = ggplot(data = residual_data, aes(x =FullBath , y=residuals)) + geom_point() + geom_hline(yintercept = 0,color = 'red')
p5 = ggplot(data = residual_data, aes(x =MSZoning , y=residuals)) + geom_point() + geom_hline(yintercept = 0,color = 'red')
p6 = ggplot(data = residual_data, aes(x =Neighborhood , y=residuals)) + geom_point() + geom_hline(yintercept = 0,color = 'red')
p7 = ggplot(data = residual_data, aes(x =BldgType , y=residuals)) + geom_point() + geom_hline(yintercept = 0,color = 'red')
p8 = ggplot(data = residual_data, aes(x =CentralAir , y=residuals)) + geom_point() + geom_hline(yintercept = 0,color = 'red')
p9 = ggplot(data = residual_data, aes(x =ExterQual , y=residuals)) + geom_point() + geom_hline(yintercept = 0,color = 'red')
p10 = ggplot(data = residual_data, aes(x =GarageFinish , y=residuals)) + geom_point() + geom_hline(yintercept = 0,color = 'red')
grid.arrange(p1, p2, p3, p4,p5,p6,p7,p8,p9,p10, nrow = 2, ncol = 5)

plot(hatvalues(ols4),ols4$residuals,col = "black", pch = 21, bg = "red") 
abline(h=0,v=2*20/900)

plot(rstandard(ols4),col = "black", pch = 21, bg = "red")
abline(h=c(-2,2), lty = 2)

plot(ols4$fitted.values,rstandard(ols4),col = "black", pch = 21, bg = "red")
abline(h=c(-2,2), lty = 2)

residualPlots(ols4)
####################################################
### Creating optimal dataset from previous highest performing OLS model
train_data_v2 = subset(data_new, select = c(LogSalePrice,age,ageSinceRemodel,ageofGarage,TotalBsmtSF,LotFrontage,LotArea,X1stFlrSF,X2ndFlrSF,BsmtFinSF1,
              BsmtUnfSF,MasVnrArea,LowQualFinSF,FullBath,MSZoning,Neighborhood,BldgType,CentralAir,ExterQual,GarageFinish))

#Creating interactions variable
train_data_v2 = train_data_v2 %>% mutate(interRes = as.numeric(age)*ageSinceRemodel*ageofGarage*TotalBsmtSF*LotFrontage*LotArea*X1stFlrSF*X2ndFlrSF)
train_data_v2 = train_data_v2 %>% mutate_if(is.factor,as.numeric) 

pca = prcomp(train_data_v2, center =TRUE,scale. =TRUE)
summary(pca)
ggbiplot(pca, alpha = 0)

#Ncomp = 14 preserves about 90% of variance
pca_train_data = data.frame(pca$x[, 1:14], LogSalePrice = train_data_v2$LogSalePrice)

####################################################
### Part B - PLS

set.seed(10)

## PCA DATA - BETTER
pls_model_1= train(LogSalePrice ~., pca_train_data, method = "pls", trControl =  trainControl(method = "cv", number = 10), preProcess = c("center", "scale"), tuneLength = 14)
print(pls_model_1)
## OPTIMAL NCOMP = 1
##0.1115307  0.9063855

## TRAINING DATA
pls_model_2 = train(LogSalePrice ~., train_data_v2, method = "pls", trControl =  trainControl(method = "cv", number = 10), preProcess = c("center", "scale"), tuneLength = 20)
print(pls_model_2)
## OPTIMAL NCOMP = 8 
##  0.1367885  0.8581508

####################################################
## Part C - LASSO

## PCA DATA - BETTER
lasso_model_1 = train(LogSalePrice ~., pca_train_data, method = "glmnet", trControl =  trainControl(method = "cv", number = 10), preProcess = c("center", "scale"), tuneGrid = expand.grid(alpha = 1, lambda = seq(0,0.004,0.00001)))
print(lasso_model_1)
ggplot(lasso_model_1)
results = data.frame(lambda = lasso_model_1$results$lambda, rmse = lasso_model_1$results$RMSE,  rsquared = lasso_model_1$results$Rsquared)
results[which(results$lambda == lasso_model_1$finalModel$lambdaOpt), ]
## OPTIMAL LAMBDA = 0.00068
## 0.1139311 0.9021301

## TRAINING DATA
lasso_model_2 = train(LogSalePrice ~., train_data_v2, method = "glmnet", trControl =  trainControl(method = "cv", number = 10), preProcess = c("center", "scale"), tuneGrid = expand.grid(alpha = 1, lambda = seq(0,0.005,0.00001)))
print(lasso_model_2)
ggplot(lasso_model_2)
results = data.frame(lambda = lasso_model_2$results$lambda, rmse = lasso_model_2$results$RMSE,  rsquared = lasso_model_2$results$Rsquared)
results[which(results$lambda == lasso_model_2$finalModel$lambdaOpt), ]
## OPTIMAL LAMBDA = 0.00239
## 0.1357942 0.8598524

####################################################
## Part D 
## PCR

##PCA DATA
pcr_model_1 = train(LogSalePrice ~., pca_train_data, method = "pcr", trControl =  trainControl(method = "cv", number = 10), preProcess = c("center", "scale"), tuneLength = 14)
print(pcr_model_1)
# OPTIMAL NCOMP = 13
# 0.1529001  0.81256414

##TRAINING DATA
pcr_model_2 = train(LogSalePrice ~., train_data_v2, method = "pcr", trControl =  trainControl(method = "cv", number = 10), preProcess = c("center", "scale"), tuneLength = 20)
print(pcr_model_2)
# OPTIMAL NCOMP = 19
# 0.1376434  0.8567367

## Ridge Reg

## PCA DATA
ridgereg_model_1 = train(LogSalePrice ~., pca_train_data, method = "glmnet", trControl =  trainControl(method = "cv", number = 10), preProcess = c("center", "scale"), tuneGrid = expand.grid(alpha = 0, lambda = seq(0,0.05,0.0001)))
print(ridgereg_model_1)
ggplot(ridgereg_model_1)
results = data.frame(lambda = ridgereg_model_1$results$lambda, rmse = ridgereg_model_1$results$RMSE,  rsquared = ridgereg_model_1$results$Rsquared)
results[which(results$lambda == ridgereg_model_1$finalModel$lambdaOpt), ]
#OPTIMAL LAMBDA = 0.0319
#0.1185901 0.8982747

## TRAINING DATA
ridgereg_model_2 = train(LogSalePrice ~., train_data_v2, method = "glmnet", trControl =  trainControl(method = "cv", number = 10), preProcess = c("center", "scale"), tuneGrid = expand.grid(alpha = 0, lambda = seq(0,0.05,0.0001)))
print(ridgereg_model_2)
ggplot(ridgereg_model_2)
results = data.frame(lambda = ridgereg_model_2$results$lambda, rmse = ridgereg_model_2$results$RMSE,  rsquared = ridgereg_model_2$results$Rsquared)
results[which(results$lambda ==  ridgereg_model_2$finalModel$lambdaOpt), ]
# OPTIMAL LAMBDA = 0.0223
# 0.1372416 0.8578982

## KNN 

## PCA DATA
knn_model_1 = train(LogSalePrice ~., pca_train_data, method = "knn",preProcess = c("center","scale"), trControl = trainControl(method = "cv", number = 10), tuneLength = 20)
print(knn_model_1)
results = data.frame(lambda = knn_model_1$results$k, rmse = knn_model_1$results$RMSE,  rsquared = knn_model_1$results$Rsquared)
results[which(results$lambda ==  knn_model_1$finalModel$k), ]
#OPTIMAL k = 7
#0.1773745 0.7698007

## TRAINING DATA
knn_model_2 = train(LogSalePrice ~., train_data_v2, method = "knn",preProcess = c("center","scale"), trControl = trainControl(method = "cv", number = 10), tuneLength = 20)
print(knn_model_2)
results = data.frame(lambda = knn_model_2$results$k, rmse = knn_model_2$results$RMSE,  rsquared = knn_model_2$results$Rsquared)
results[which(results$lambda ==  knn_model_2$finalModel$k), ]
#OPTIMAL k = 5
#0.167061 0.7919043

## MARS

## PCR DATA
mars_model_1 = train(LogSalePrice ~., pca_train_data, method = "earth",preProcess = c("center","scale"), trControl = trainControl(method = "cv", number = 10),  tuneGrid = expand.grid(degree = 1:3, nprune = seq(2,14,2) %>% floor()))
print(mars_model_1)
#OPTIMAL d = 1 nprune = 14
#0.1107492  0.9093659

## TRAINING DATA
mars_model_2 = train(LogSalePrice ~., train_data_v2, method = "earth",preProcess = c("center","scale"), trControl = trainControl(method = "cv", number = 10),  tuneGrid = expand.grid(degree = 1:3, nprune = seq(2,14,2) %>% floor()))
print(mars_model_2)
#OPTIMAL d = 1 nprune = 14
#0.1394956  0.8518500


