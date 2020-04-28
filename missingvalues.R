# prerequisite pkgs 
library(reshape2)
library(ggplot2)
library(dplyr)
library(wakefield)
library(Amelia)
pacman::p_load(VIM)
data_csv <- read.csv("/Users/farhat/Documents/1st-Trimester/631Data-mining/projects/indian_liver_patient2.csv",header = T)

#we are using table function ot count the total number of missing data
table(is.na(data_csv))

#number of 'NA' values with each features
sapply(data_csv, function(x) sum(is.na(x)))
#analysing the missing value with vim "pacman::p_load(VIM)" package.
missing_attribute_name <- aggr(data_csv)
missing_values <- aggr(data_csv, col=c('#4a4948','yellow'),numbers=TRUE,
                        labels=names(data_csv),cex.axis=.5,
                       gap=2,ylab=c('proportion of missing data','pattern'))


  
missmap(data_csv)