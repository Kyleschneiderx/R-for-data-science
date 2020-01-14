#Chapter 29, 30 Homework
#DATA 320
#Kyle Schneider
#11/19/2019
library(caret)

#Exercises 29.5 #3
library(broom)
library(dslabs)
library(tidyverse)
data("mnist_27")
mnist_27$train %>% 
  glm(y ~ x_2, family = "binomial", data = .) %>% 
  tidy()

qplot(x_2, y, data = mnist_27$train)

fit <- loess(as.numeric(y) ~ x_2, degree=1, span = 0.12, data=mnist_27$train) 
mnist_27$train %>% mutate(smooth = fit$fitted)%>% ggplot(aes(x_2, y)) +
  geom_point(size = 2) +
  geom_line(aes(x_2, smooth), color="red")


#Exercises 30.6 #3-5

#3)Generate a random dataset like this:
y <- rnorm(100, 0, 1)
#Estimate the 75th quantile, which we know is:
qnorm(0.75)
#with the sample quantile:
quantile(y, 0.75)
#Run a Monte Carlo simulation to learn the expected value and standard error of this random variable.
B <- 10000
M <- replicate(B, {
  y <- rnorm(100, 0, 1)
  qnorm(0.75)
  quantile(y, 0.75)
})
mean(M)
se <- sd(M)/sqrt(length(M))

#4)In practice, we can’t run a Monte Carlo simulation because we don’t know if rnorm is being used
#to simulate the data. Use the bootstrap to estimate the standard error using just the initial
#sample y. Use 10 bootstrap samples.
N <- 10
X <- sample(y, N) 
mean(X)
se_1 <- sd(X)/sqrt(N)
se_1
#5)Redo exercise 4, but with 10,000 bootstrap samples.
N <- 10000
X <- sample(y, N) 
mean(X)
se_2 <- sd(X)/sqrt(N)
se_2


#Exercises 30.4 #1-6
set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n * p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

#1)Because x and y are completely independent, you should not be able to predict y using x with
#accuracy larger than 0.5. Confirm this by running cross validation using logistic regression
#to fit the model. Because we have so many predictors, we selected a random sample x_subset.
#Use the subset when training the model. Hint: use the caret train function. The results component
#of the output of train shows you the accuracy. Ignore the warnings.

modelLookup("regLogistic") #lists the parameters
train_glm <- train(y ~ ., method="regLogistic", data=x_subset)


#2)Now, instead of a random selection of predictors, we are going to search for those that are
#most predictive of the outcome. We can do this by comparing the values for the y = 1 group to 
#those in the y = 0 group, for each predictor, using a t-test. You can perform this step like this:

devtools::install_bioc("genefilter")
install.packages("genefilter")
library(genefilter)
install.packages("BiocManager")
library("BiocManager")
BiocManager::install("genefilter")
library(genefilter)
tt <- colttests(x, y)

#Create a vector of the p-values and call it pvals.

pvals = c(tt)

#3)Create an index ind with the column numbers of the predictors that were “statistically 
#significantly” associated with y. Use a p-value cutoff of 0.01 to define “statistically 
#significant”. How many predictors survive this cutoff?


#4)Re-run the cross validation but after redefining x_subset to be the subset of x defined
#by the columns showing “statistically significant” association with y. What is the accuracy now?

#5)Re-run the cross validation again, but this time using kNN. Try out the following grid of
#tuning parameters: k = seq(101, 301, 25). Make a plot of the resulting accuracy.


#6)In exercises 3 and 4, we see that despite the fact that x and y are completely independent,
#we were able to predict y with accuracy higher than 70%. We must be doing something wrong then.
#What is it?



