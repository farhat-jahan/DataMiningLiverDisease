#We are plotting density plot for each attribute.This shows the distribution
#of the data with respect to feature Data-set attribute.
library(caret)
# load libraries
library(lattice)

data_csv <- read.csv("/Users/farhat/Documents/1st-Trimester/631Data-mining/projects/indian_liver_patient.csv",header = T)
density_data <- data_csv
# for age:
x <- density_data[,1]
y <- as.factor(density_data$Dataset)
scale <- list(x=list(relation="free"), y=list(relation="free") )
featurePlot(x=x, y=y, plot='density',scales=scale)

x <- density_data[,3:5]
y <- as.factor(density_data$Dataset)
scale <- list(x=list(relation="free"), y=list(relation="free") )
featurePlot(x=x, y=y, plot='density',scales=scale)

x <- density_data[,5:10]
y <- as.factor(density_data$Dataset)
scale <- list(x=list(relation="free"), y=list(relation="free") )
featurePlot(x=x, y=y, plot='density',scales=scale)
