#support vector machine (SVM) aka hyperplanes between data (classification), highly complex
#maximum margin hyperplane = greatest seperation between classes
#support vectors = points closest to MMH, convex hull = boundary of points encircling a group
#MMH = perpendicular bisector of shortest line between convex hulls, sophisticaed algorithms use quadratic optimization
#non-linearly separable data -> use const function: min(1/2||w||^2+C sum(xi)) where xi is all points outside of boundary

#kernal trick: uses high dimensionality to extract linearity out of non-linear data
#linear kernel: K(x_i, x_j) = x_i*x_j
#polynomial kernel: K(x_i, x_j) = (x_i*x_j + 1)^d
#sigmoid kernel: K(x_i, x_j) = tanh(kx_i*x_j-delta)
#gaussian RBF kernal: K(x_i, x_j) = e*-||x_i-x_j||^2/(2 sigma^2)

#doing something similar to optical character recognition (OCR)
letters <- read.csv("https://raw.githubusercontent.com/PacktPublishing/Machine-Learning-with-R-Third-Edition/master/Chapter07/letterdata.csv", stringsAsFactors = FALSE)
str(letters)
letters_train <- letters[1:16000, ]
letters_test <- letters[16001:20000, ]
library(e1071)
#install.packages("kernlab")
library(kernlab)
letters_classifier <- ksvm(letter ~ ., data = letters_train, kernel = "vanilladot")
letters_classifier
letter_predictions <- predict(letters_classifier, letters_test)
table(letter_predictions, letters_test$letter) #top = real, vert = pred
prop.table(table(letter_predictions == letters_test$letter))

#improving model performance
letters_classifier_rbf <- ksvm(letter ~ ., data = letters_train, kernel = "rbfdot")
letter_predictions_rbf <- predict(letters_classifier_rbf, letters_test)
prop.table(table(letter_predictions_rbf == letters_test$letter))
