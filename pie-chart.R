#exploratory data using plots
data_csv <- read.csv("/Users/farhat/Documents/1st-Trimester/631Data-mining/projects/indian_liver_patient.csv", header = T)
tem_data <- data_csv

tem_data$Dataset <- as.factor(tem_data$Dataset)
tem_data$Gender <- as.numeric(tem_data$Gender)

#Calculating affected and not affected patients
counttable <- table(tem_data$Dataset)

data_lebels <- c("patients with liver disease", "patients without liver disease")
data_lebels <- paste(data_lebels,"\n",counttable)
# pie chart is created based on 'dataset' attribute which is response variable.
pie(counttable, labels = data_lebels,main = "Count of dataset attribute",
    col = rainbow(length(data_lebels)))

