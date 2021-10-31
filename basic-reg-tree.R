#adding regression to trees

#standard deviation reduction
#SDR = sd(T) - sum( |T_i|/|T| *sd(T_i) )
tee <- c(1, 1, 1, 2, 2, 3, 4, 5, 5, 6, 6, 7, 7, 7, 7)
at1 <- tee[1:9]
at2 <- tee[10:15]
bt1 <- tee[1:7]
bt2 <- tee[8:15]
sdr_a <- sd(tee) - (length(at1) / length(tee) * sd(at1) + length(at2)/length(tee)*sd(at2)) #1.202
sdr_b <- sd(tee) - (length(bt1) / length(tee) * sd(bt1) + length(bt2)/length(tee)*sd(bt2)) #1.392
#since sdr_b > sdr_a, the decision tree would use B before A

wine <- read.csv("https://raw.githubusercontent.com/PacktPublishing/Machine-Learning-with-R-Third-Edition/master/Chapter06/whitewines.csv", stringsAsFactors = FALSE)
str(wine)
hist(wine$quality)
wine_train <- wine[1:3750, ]
wine_test <- wine[3751:4898, ]
#install.packages("rpart")
library(rpart)
m.rpart <- rpart(quality ~ ., data = wine_train)
m.rpart #* nodes are terminal/leaf nodes, which means they predict
p.rpart <- predict(m.rpart, wine_test)
summary(p.rpart)
summary(wine_test$quality)
#not identifying extreme cases
cor(p.rpart, wine_test$quality)
#MAE = mean absolute error = 1/n sum(|e_i|)
MAE <- function(actual, predicted) {
  mean(abs(actual-predicted))
}
MAE(p.rpart, wine_test$quality) #on avg, the diff between prediction and true was only .59
library(RWeka)
m.m5p <- M5P(quality ~ ., data = wine_train)
m.m5p
summary(m.m5p)
p.m5p <- predict(m.m5p, wine_test)
summary(p.m5p)
cor(p.m5p, wine_test$quality)
MAE(wine_test$quality, p.m5p)
