
library(ggplot2)
data_csv <- read.csv("/Users/farhat/Documents/1st-Trimester/631Data-mining/projects/indian_liver_patient.csv",header = T)

#analysing the distribution of age with respect to data-set, using box plot.
#Result:we can see many patients with liver disease are above the "median"

boxplot_data <- data_csv
boxplot_data$Dataset <- as.factor(boxplot_data$Dataset)

ggplot(boxplot_data, aes(x=factor(1), y=Age))+
  geom_boxplot(fill="white", width=.5)+
  geom_jitter(aes(color=Dataset, shape=Dataset),width = .1, size=1)+
  scale_color_manual(values = c('#00AFBB','#E7B800'))+
  labs(x=NULL)