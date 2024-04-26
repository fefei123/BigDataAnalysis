## the whole idea, find a good model to explain or predict the relationship
## between the response variable and predictor variables.

## Step 1: input data and use str() & summary()to see the detail of the data

library(readr)
column_names <- c("Case Number","Brozek's.equation","Siri's.equation","Density",
                  "Age","Weight","Height","Adiposity.index","Fat.Free.Weight",
                  "Neck.cir","Chest.cir","Abdomen.cir","Hip.cir","Thigh.cir",
                  "Knee.cir","Ankle.cir","Extended.biceps.cir","Forearm.cir",
                  "Wrist.cir")
fatdata <- read.csv("D:/Fei/OneDrive - University of Missouri/R/BigData/fat.dat.txt",
                    header = FALSE, sep = "", col.names = column_names)
fatdata
str(fatdata)
summary(fatdata)

## Step 2: clear the data.
## Obviously, the case number(column 1) is not a significant variable,keep it,
## but not use it as a predictor variable.
## case 48, 76 and 96 have one digit in error; case 182, the body fat is zero, 
## it means the raw data is wrong. these all can be defined as bad data, so delete them.
## case 42, fix the data value.

(newdata <- fatdata [c(-48, -76, -96, -182), ])
str(newdata)
newdata $ Height[42] <- 69.5

## Step 3 find the most significant predictor variables(X1, X2...) to 
## response variable (Y) why our team use Density as Y? 
pairs(newdata) ##plot.1
fit <- lm(Density ~ Age+Weight+Height+Adiposity.index+Fat.Free.Weight+Neck.cir+
            Abdomen.cir+Hip.cir+Thigh.cir+Knee.cir+Ankle.cir+Extended.biceps.cir+Forearm.cir+
            Wrist.cir, data=newdata)
summary(fit)   ##fig.1

## See the fig.1 some interesting stuff happened
## (1) 6 out of 19 variables affect the Density
## (2) the Age do nothing significance to Density? deeply find Age~Density
## (3) the Thigh.Cir affect Density? deeply find Thigh.Cir~Density

fit1 <- lm(Density ~ Weight+Height+Adiposity.index+Fat.Free.Weight+
             Abdomen.cir+Thigh.cir, data=newdata)
summary(fit1)  ##fig.2

## conclude: from fig.1 & fig.2, through P-value, the influence of Abdomen.Cir 
## to Density has a little bit drop, but still strong.
## the Thigh.Cir affect Density is still significant.
## F-statistic is much larger than before and the p-value is so small, It indicates
## that the overall model is statistically significant, meaning that predictor variables 
## is associated with the response variable.
## so our multiple linear regression model is good.

## Step 4 diagnoise the linear regression

pairs(newdata[,c(4,6,7,8,9,12,14)]) ##plot.2

par(mfrow=c(2,2))
plot(fit1) ## Plot.3

## conclusion:the Residuals plot like a curve, so the model doesn't fit very well 
## we plan to change the variables, remove Adiposity index since Adiposity index 
## was defined by the Weight and Height


## Step 5, modify the model
fit2 <- lm(Density ~ Weight+Height+Fat.Free.Weight+
             Abdomen.cir+Thigh.cir, data=newdata)
summary(fit2) ## Fig.3 
plot(fit2) ##Plot.4

##Conclusion: F-statistic and p-value is really good, that means the model is good

cor_matrix <- cor(newdata[,c("Weight", "Height", "Abdomen.cir",
                             "Thigh.cir", "Fat.Free.Weight")])
cor_matrix

fit3 <- lm(Density ~ Weight+Fat.Free.Weight+
             Abdomen.cir+Thigh.cir+Weight:Abdomen.cir+Weight:Thigh.cir, data=newdata)
summary(fit3)
plot(fit3, col="blue") ##Plot.5

## Conclusion: residuals plot and scale plot still looks like curves, but the dots
## randomly scatter around the line, it's good. the model is useful.
## final model is
## Density ~ Weight+Fat.Free.Weight+Abdomen.cir+Thigh.cir+Weight:Abdomen.cir+Weight:Thigh.cir

## Step 6
## check Density ~ Age
par(mfrow=c(1,1))
fit4 <- lm(Density ~ Age, data=newdata)
plot(newdata$Age, newdata$Density,
     xlab="Age",ylab="Density", col="blue")
abline(fit4, col="red") ##Plot.6

## Conclusion, the dots randomly scattered, so there is no any relationship between
## Density and Age just like we see before from the P-value.

