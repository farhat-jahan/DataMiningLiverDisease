#prerequisite pkgs
pacman::p_load(e1071)#,SparseM)#MASS#caret#e1071
library(e1071)
data_csv <- read.csv("/Users/farhat/Documents/1st-Trimester/631Data-mining/projects/indian_liver_patient2.csv",header = T)
# constructiona and evaluation phase: creating training data and validation dataset.
#spliting the dataset into training and testing dataset.
set.seed(80)
liver_clean_data_lm <- data_csv
data_csv$Dataset <- as.factor(data_csv$Dataset)
#spliting 80%=training dataset & 20%=testing dataset.
index <- createDataPartition(data_csv$Dataset, p=.8,list=FALSE)
training_data <- data_csv[index,] #nrow(training_data)=467
testing_data <- data_csv[-index,]  #nrow(testing_data)=116
#-------------------------------------------------------------------------------------------------#
pacman::p_load(kernlab)
library(kernlab)
# support vector machine
set.seed(200)
new_data <- data.frame(data_csv)
x=new_data[,c(1,2,3,4,5,6,7,8,9)]
x1=as.matrix(x)
#creating training model using training dataset and SVM
training_model_svm <- svm(new_data$Dataset, data=new_data, kernel='linear', cost=.1, scale=FALSE)
plot(testing_pred_svm,x1)
#predicting value for test dataset
testing_pred_svm <- predict(training_model_svm, training_data)

#confusionMatrix for SVM
confusionMatrix(table(testing_pred_svm, training_data$Dataset))
#-------------------------------------------------------------------------------------------------#

#spliting data variable with response varibale into numeric, for logistic regression model.
#training model using training dataset, logistic regression based on p-value, eliminitaing features whose
# p-value>.5
liver_clean_data_lm <- data_csv
training_data_lm <- liver_clean_data_lm[index,]
testing_data_lm <- liver_clean_data_lm[-index,]

model_gm <- lm(formula = Dataset~., family = binomial, data=training_data_lm)
summary(model_gm)
model_gm_try <- lm(, family = binomial, data=training_data_lm)
#predicting the values of teh dataset
#pred_lm <- predict(model_gm, testing_data_lm[,c(1,4,5,7,8,9)], type = "response")
pred_lm <- predict(model_gm, testing_data_lm, type = "response")
#converting the probability to 1 or  0
pred_lm <- ifelse(pred_lm>=.5,1,0)
#confusion matrix for logistic regression
confusionMatrix(as.factor(pred_lm), as.factor(testing_data_lm$Dataset))#The nummer of false b=negative here comparison to SVM is lesse
#-------------------------------------------------------------------------------------------------#
#K-nearest-neighbor
#training model using trainin dataset and KNN
ctrl <- trainControl(method = "repeatedcv", repeats = 3)
model_knn <- train(Dataset~., data=testing_data, method='knn', trControl=ctrl, preProcess=c('center','scale'),tuneLength=20)
#predicting values for test data
pred_knn <- predict(model_knn, newdata=testing_data)
#confusion matrix for knn
confusionMatrix(pred_knn, testing_data$Dataset)#this model has very few cases of negative and a good accuracy.Hence KNN model perfomered much better
#-------------------------------------------------------------------------------------------------#
#Random forest
#training model using tarining dataset and randomforest
model_randomforest <- randomForest(Dataset~.,data=training_data, importance=TRUE)
#predicting values formtestdataset
pred_randomforest <- predict(model_randomforest, testing_data)
#confusdion matrix for randomforest
confusionMatrix(pred_randomforest, testing_data$Dataset)
#-------------------------------------------------------------------------------------------------#
#comparision of model
#storing thenaccurance of all the model
SVM <- confusionMatrix(testing_pred_svm, training_data$Dataset)$overall['Accuracy']
GLM <- confusionMatrix(as.factor(pred_lm), as.factor(testing_data_lm$Dataset) )$overall['Accuracy']
KNN <- confusionMatrix(pred_knn, testing_data$Dataset)$overall['Accuracy']
RandomForest <- confusionMatrix(pred_randomforest, testing_data$Dataset)$overall['Accuracy']
#Creating a dataframe of all accurancy models
accuracy <- data.frame(Model=c("support vector machine", 'Logistic regression', 'KNN', 'Random Forest'))
Accurancy <- c(SVM, GLM, KNN, RandomForest)

#Plotting accurancy of models using ggplot
ggplot(accuracy, aes(x=Model, y=Accurancy))+
  geom_bar(stat = 'Identity')+
  theme_bw()+
  ggtitle('Comparision of model accurancy')

#Reference for confusiin matrix:
#https://rcompanion.org/rcompanion/e_05.html





