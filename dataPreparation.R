pacman::p_load(mice)
library(mice)
#featuring engineering
#converting gender attribute into 0 and 1, it is factor level conversion.
data_csv <- read.csv("/Users/farhat/Documents/1st-Trimester/631Data-mining/projects/indian_liver_patient2.csv",header = T)
str(data_csv)
tem_data <- data_csv
feature_engi <- dummyVars("~.",data=tem_data,fullRank = TRUE)
final_liver_data <- data.frame(predict(feature_engi, newdata = tem_data)) 
str(final_liver_data)
#----------------------------------------------------------------------------------#
# using predictive  modeling for data-imputing
# method used: regression to impute the missing values, based on other observation.
# missing data type is 'numeric', so we are using predictive mean matching i.e:'pmm' for imputation.
# m=4 implies, it will create 4 impute data-set.
# number of iteration=20 i.e maxit=20
# seed:offsetting the random number generator
data_csv <- read.csv("/Users/farhat/Documents/1st-Trimester/631Data-mining/projects/indian_liver_patient2.csv",header = T)

impute_data <- mice(data_csv, m=4, maxit = 20, method = 'pmm', seed=500)
impute_data_incol <- complete(impute_data) # imputing data in the missing data column
table(is.na(impute_data_incol)) # checking the countof missing data after imputation
table(is.na(data_csv))
#----------------------------------------------------------------------------------#
#handling outliers
#storing influential  row number, rows with outlier
data_csv <- read.csv("/Users/farhat/Documents/1st-Trimester/631Data-mining/projects/indian_liver_patient2.csv",header = T)
#data_csv <- na.omit(data_csv)
mod <- glm(Dataset~., data=data_csv)
print(summary(mod))
par(mfrow=c(2,2))
plot(mod)
par(mfrow=c(1,1))

#print(summary(mod_outlier_deleted))
cooksd <- cooks.distance(mod)
influential <- as.numeric(names(cooksd)[cooksd > 3*mean(cooksd, na.rm=T)])
#deleteding the outliers
data_without_outlier <- impute_data_incol[-influential,]
#Model observing after deleting outliers.
mod_outlier_deleted <- glm(Dataset~., data=data_csv)
print(summary(mod_outlier_deleted))
# not much difference because we have less outlier rows
par(mfrow=c(2,2))
plot(mod_outlier_deleted)
par(mfrow=c(1,1))
#----------------------------------------------------------------------------------#
#normalization and standarization of data
# z score normalization: because all the features will be transformed in sach a way that it will 
#have the properties of standard normal distribution with mean(u) and standard devi(σ)=1.
normalize_function <- function(x){
  return((x-mean(x))/sd(x)) 
}

#applying normalization on all attributes except:'Gender', 'Dataset'
normalized_data <- as.data.frame(lapply( data_without_outlier[,c(1, 3:10)], normalize_function))

Dataset <- data_without_outlier$Dataset
Gender <- data_without_outlier$Gender

#binding the dataset and gender to normalize them
normalized_data$Gender <- Gender
normalized_data$Dataset <- Dataset
summary(normalized_data)
liver_clean_data <- normalized_data
#----------------------------------------------------------------------------------#
pacman::p_load(kernlab, caret)
library(kernlab)
library(caret)
liver_clean_data_lm <- liver_clean_data
#spliting 80%=training dataset & 20%=testing dataset.
index <- createDataPartition(liver_clean_data$Dataset, p=.8,list=FALSE)
training_data <- liver_clean_data[index,] #nrow(training_data)=467
testing_data <- liver_clean_data[-index,]  #nrow(testing_data)=116=583
#-------------------------------------------------------------------------------------------------#
# support vector machine: to segregate the two data.
#creating training model using training dataset and SVM
training_model_svm <- svm(Dataset~. , data=training_data, kernel='linear', cost=.1)
print(training_model_svm)
tune <- tune(svm, Dataset~. ,data=training_data, kernel='linear', ranges=list(cost=c(.001,.1,.2)))
summary(tune)
#predicting value for test dataset
testing_pred_svm <- predict(training_model_svm, testing_data)
summary(testing_pred_svm )
hist(testing_pred_svm)
#-------------------------------------------------------------------------------------------------#
#spliting data variable with response varibale into numeric, for logistic regression model.
training_data_lm <- liver_clean_data_lm[index,]
testing_data_lm <- liver_clean_data_lm[-index,]

