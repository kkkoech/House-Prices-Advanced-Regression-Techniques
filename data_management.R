load(train)

df <- read.csv("data/train.csv", header = T)
test <- read.csv("data/test.csv", header = T, sep)
train <- read.csv("data/train.csv", header = T)
str(df)
names(df)


#Dealing with missing values

#1. LotFrontage
sum(is.na(df$LotFrontage))
#Scatter plot for possible linear relationship
scatter.smooth(y=df$LotFrontage, x=df$LotArea, main = "LotFrontage~LotArea")

#Outliers
par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(df$LotFrontage, main="LotFrontage", sub=paste("Outlier rows: ", boxplot.stats(df$LotFrontage)$out)) 
boxplot(df$LotArea, main="LotArea", sub=paste("Outlier rows: ", boxplot.stats(df$LotArea)$out))  

#Density plot to check for closeness of the "response variable" to normality.
library(e1071)

par(mfrow=c(1,2))
plot(density(df$LotFrontage), main="LotFrontage", 
     ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(df$LotFrontage), 2)))
plot(density(df$LotFrontage), col="green")
plot(density(df$LotArea), main="LotArea", 
     ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(df$LotArea), 2)))
plot(density(df$LotArea), col="green")

#Correlation to measure linear dependence between LotArea and LotFrontage
library(GGally)
ggcorr(df, label=T)

#Linear Model?
lm_LotFrontage <- lm(LotFrontage ~ LotArea, data=df)
#LotFrontagePredictions <- predict(lm_LotFrontage, df)
#LotFrontageNAs <- df$LotFrontage[is.na(df$LotFrontage)]
lm_LotFrontage #LotFrontage = 57.055042 + 0.001306*LotArea 

LotFrontage2 <- (57.055042 + 0.001306 * df$LotArea)

#df$LotFrontage[is.na(df$LotFrontage)] <- 57.055042 + (0.001306 * df$LotArea)
#Finding errors

#2. Alley, PoolQC, Fence, MiscFeature, FireplaceQuality
#Too many missing observations. I'll get rid of them
df$LotFrontage <- NULL
df$Alley <- NULL
df$PoolQC <- NULL
df$Fence <- NULL
df$MiscFeature <- NULL
df$FireplaceQu <- NULL



