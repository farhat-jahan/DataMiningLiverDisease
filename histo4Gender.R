
library(ggplot2)
library(ggpubr)
library(viridis)
library(hrbrthemes)
library(dplyr)
#import data
data_csv <- read.csv("/Users/farhat/Documents/1st-Trimester/631Data-mining/projects/indian_liver_patient.csv", header = T)

meddata <- data_csv
temp <- select(meddata, Gender, Dataset) %>% mutate(my_key = paste(Gender, Dataset, sep = "-")) %>% group_by(my_key) %>% count(my_key)
#histogram for the gender attribute
ggplot(temp, aes(y=n, x=my_key)) + 
  geom_bar(position="stack", stat="identity",fill="#FF6666", width=.5) +
  scale_fill_viridis(discrete = T) +
  ggtitle("Disease comparision in Male & Female") +
  xlab("Gender") + ylab("Count of infected & not-infected")



