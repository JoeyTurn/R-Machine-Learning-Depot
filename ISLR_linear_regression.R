#install.packages("MASS")
#install.packages("ISLR")
library(MASS)
library(ISLR)

fix(Boston)
#names(Boston)
lm.fit = lm(medv~lstat, data = Boston) #medv = median house value,
                                       #lstat = % low socioec stat households
lm.fit; summary(lm.fit)
coef(lm.fit)
confint(lm.fit) #confidence interval
predict(lm.fit, data.frame(lstat=c(5, 10, 15)), interval = "confidence")
predict(lm.fit, data.frame(lstat=c(5, 10, 15)), interval = "prediction")

attach(Boston)
plot(lstat, medv)
abline(lm.fit)

abline(lm.fit, lwd=3, col = "red") #lwd = line width
plot(lstat, medv, pch = 20) #pch changes shape

par(mfrow = c(2, 2))
plot(lm.fit)

#multiple lin reg

lm.fit = lm(medv~., data = Boston)
summary(lm.fit)

#install.packages("car")
library(car)
vif(lm.fit)# variance inflation factors

#nonlinear transforms
lm.fit2 = lm(medv ~ lstat+I(lstat^2)) #can also use poly(lstat, 2)
summary(lm.fit2)
lm.fit = lm(medv~lstat)
anova(lm.fit, lm.fit2)

par(mfrow=c(2, 2))
plot(lm.fit2)
