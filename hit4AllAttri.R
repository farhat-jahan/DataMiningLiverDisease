data_csv <- read.csv("/Users/farhat/Documents/1st-Trimester/631Data-mining/projects/indian_liver_patient.csv", header = T)

color_hist <- c('#cd853f','#ff6a6a','#cd5c5c','#ECEC27','#0DAA31','#E30BEA','#0F60B0','#F09784')

#plotting histogram for all the attributes to understand skewness.
noBreaks <- 15
length_of_column <- ncol(data_csv)-1
hist(data_csv$Age, col = '#999999',xlab = 'Age',main = 'Histrogram of age')

par(mfrow=c(3,3))

for (i in 3:10){
  print(names(data_csv[i]))
  hist(data_csv[,i], col = color_hist[i-2],main = paste("Histogram of ", 
          names(data_csv[i])), xlab = names(data_csv)[i], cex.axis=.5,  las=1)
}
par(mfrow=c(1,1))

