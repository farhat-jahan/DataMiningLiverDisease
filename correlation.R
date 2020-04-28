library(psych)
data_csv <- read.csv("/Users/farhat/Documents/1st-Trimester/631Data-mining/projects/indian_liver_patient.csv",header = T)

corr_data <- data_csv
#scatterplot matrix to understand the correlation between different variables.
#scatterplot of Total_Bilirubin and Direct_Bilirubin to visualize the Collinearity between them.
pairs.panels(data_csv, pch=3)
#better view for below variables
#Total_Bilirubin, Direct_Bilirubin, Alkaline_Phosphotase, Alamine_Aminotransferase Aspartate_Aminotransferase, Total_Protiens, Albumin.
pairs.panels(data_csv[,c(3,4,5,6,7,8,9)])

boxplot_data <- data_csv
boxplot_data$Dataset <- as.factor(boxplot_data$Dataset)
dataset <- boxplot_data$Dataset
ggplot(data=corr_data, aes(x=corr_data$Total_Bilirubin, y=corr_data$Direct_Bilirubin))+
  geom_point(aes(colour=dataset, shape=dataset))+
  xlab('Total_Bilirubin')+
  ylab('Direct_Bilirubin')+
  ggtitle('correlation B/W Total_Bilirubin & Direct_Bilirubin')
  

