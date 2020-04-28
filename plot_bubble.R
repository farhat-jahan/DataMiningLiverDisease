library(ggplot2)
library(ggpubr)
library(viridis)
library(hrbrthemes)
theme_set(
  theme_minimal() +
    theme(legend.position = "top")
)

#import data
data_csv <- read.csv("/Users/farhat/Documents/1st-Trimester/631Data-mining/projects/indian_liver_patient.csv", header = T)

meddata <- data_csv

finaldata <- select(meddata, Gender, Dataset) %>% group_by(Gender, Dataset) %>% count(Gender, Dataset) %>% mutate(affected = ifelse(Dataset <2 , "Affected", "Nonaffected"))

#b <- ggplot(finaldata, aes(x = affected , y = Gender))
# Scatter plot with regression line

#b + geom_point(aes(color = Gender, size = n), alpha = 0.5) +
 # scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))
ggplot(finaldata, aes(fill=affected, y=n, x=Gender)) + 
  c(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle(" Conclusion") +
  xlab("Gender") + ylab("Count")


finaldata <- select(meddata, Gender, Dataset) %>% group_by(Gender, Dataset) %>% count(Gender, Dataset) %>% mutate(affected = ifelse(Dataset <2 , "Affected", "Nonaffected"))

temp <- select(meddata, Gender, Dataset) %>% mutate(my_key = paste(Gender, Dataset, sep = "-")) %>% group_by(my_key) %>% count(my_key)

ggplot(temp, aes(y=n, x=my_key)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle(" Conclusion") +
  xlab("Gender") + ylab("Count")

