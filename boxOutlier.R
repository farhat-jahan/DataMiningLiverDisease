
data_csv <- read.csv("/Users/farhat/Documents/1st-Trimester/631Data-mining/projects/indian_liver_patient.csv", header = T)

# we are usign boxplot to analyse the Ouliers.
#in the boxplot outliers are represented by circle.
#outliers lies beyond inter-quartile range of data.

color_box <- c('#cd853f','#ff6a6a','#cd5c5c','#ECEC27','#0DAA31','#E30BEA','#0F60B0','#F09784')
boxplot(data_csv$Age, cex.axis=.5, col = '#999999',main='Age')

par(mfrow=c(3,3))
for (i in 3:10){
  boxplot(data_csv[,i], cex.axis=.5,  col=color_box[i-2],main=names(data_csv)[i] )
}
par(mfrow=c(1,1))
#-----------------------------------------------------------------------------------------------------#

#dipicting the outlier using the cook-distance.This is a multivariate approach
#here showing the values which are 3 cook distance away from mean as a outlier

mod <- glm(Dataset~., data=data_csv)
cooksd <- cooks.distance(mod)
#plot(cooksd, pch="*",cex=2, main="influential obs by cook distance")
plot(mod, which=c(4), pch=18, col='red')
dis <- cooks.distance(mod)
print(sort(round(dis, 5)))
abline(h=3*mean(cooksd,na.rm=T), col='red') # added a cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>3*mean(cooksd, na.rm=T), names(cooksd), ""),
     col='red')

print(summary(mod))
