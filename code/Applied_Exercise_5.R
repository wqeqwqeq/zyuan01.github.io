"This question covers the study of non-linear decision boundaries by performing logistic regression using 
non-linear transformations of the features listed in the data set generated below."

"Start by loading the Mass and e1071 packages"
rm(list=ls())
####################################################
####           Load required packages           ####
####################################################

installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    require(thispackage, character.only = T)
  }
}

needed <- c('Mass', 'e1071')  
installIfAbsentAndLoad(needed)

## (a)##
"Our first step is to generate sample data to use during the simulation"
set.seed(1)

n <- 5000
p <- 2

x1 <- runif(n) - 0.5
x2 <- runif(n) - 0.5
y <- 1 * (x1^2 - x2^2 > 0)

DF <- data.frame(x1 = x1, x2 = x2, y = as.factor(y))

## (b) ## Plot the observations, colored according to their class labels
plot(x1, x2, col = (y + 1), pch = 19, cex = 1.05, xlab = "x1", ylab = "x2", main = "initial data")

#Grid function plots dotted lines at each label along the x and y axis 
grid()

## (c) ## Fit a logistic regression model to the data, using X1 and X2 as predictors.
m <- glm(y ~ x1 + x2, data = DF, family = binomial)

## (d) ## Apply this model to the training data in order to obtain a predicted class label for each training observation.
y_hat <- predict(m, newdata = data.frame(x1 = x1, x2 = x2), type = "response")
predicted_class <- 1 * (y_hat > 0.5)

# Plot the observations, colored according to the predicted class labels. The
# decision boundary should be linear.
plot(x1, x2, col = (predicted_class + 1), pch = 19, cex = 1.05, xlab = "x1", ylab = "x2", main = "logistic regression: y ~ x1 + x2")
grid()

## (e) ## Now fit a logistic regression model to the data using non-linear functions of X1 and X2 as predictors 
m <- glm(y ~ x1 + x2 + I(x1^2) + I(x2^2) + I(x1 * x2), data = DF, family = "binomial")
summary(m)

## (f) ## Apply this model to the training data in order to obtain a predicted class label for each training observation.
y_hat <- predict(m, newdata = data.frame(x1 = x1, x2 = x2), type = "response")
predicted_class <- 1 * (y_hat > 0.5)

plot(x1, x2, col = (predicted_class + 1), pch = 19, cex = 1.05, xlab = "x1", ylab = "x2")
grid()

## (g) ## Fit a support vector classifier to the data with X1 and X2 as predictors
dat <- data.frame(x1 = x1, x2 = x2, y = as.factor(y))
#takes a while to run 
tune.out <- tune(svm, y ~ ., data = dat, kernel = "linear", ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))

summary(tune.out)

tune.out$best.model

# Obtain a class prediction for each training observation
y_hat <- predict(tune.out$best.model, newdata = data.frame(x1 = x1, x2 = x2))
y_hat <- as.numeric(as.character(y_hat))

#Plot the observations, colored according to the predicted class labels
plot(x1, x2, col = (y_hat + 1), pch = 19, cex = 1.05, xlab = "x1", ylab = "x2")
grid()

'This support vector classifier (even with low cost) classifies all points to a single class'

## (h) ##
#takes a while to run 
tune.out <- tune(svm, y ~ ., data = dat, kernel = "radial", 
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100), gamma = c(0.5,1, 2, 3, 4)))

summary(tune.out)

tune.out$best.model

y_hat <- predict(tune.out$best.model, newdata = data.frame(x1 = x1, x2 = x2))
y_hat <- as.numeric(as.character(y_hat)) 

plot(x1, x2, col = (y_hat + 1), pch = 19, cex = 1.05, xlab = "x1", ylab = "x2")
grid()

## (i) ##
'Based on the results above, SVMs with non-linear kernel are extremely powerful in finding non-linear boundary.
Without any interaction terms, Both, logistic regression with non-interactions and SVMs with linear kernels fail
to find the decision boundary. Adding interaction terms to logistic regression seems to give them the same power as 
radial-basis kernels. However, there is some manual effort and tuning involved when picking the right interaction 
terms. This effort can become excessive when many features are involved. Radial basis kernels, on the other hand, 
only require tuning one parameter, gamma, which can be easily done using cross-validation.'
