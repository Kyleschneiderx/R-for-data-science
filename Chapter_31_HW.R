#Chapter 31 Homework
#DATA 320
#Kyle Schneider
#12/4/2019

#1. 32.6 (31.6 on github) Exercise #2
library(caret)
library(tidyverse)
library(dslabs)
data("mnist_27")
view(mnist_27)
#2) Load the following dataset:
data("tissue_gene_expression")
#This dataset includes a matrix x:

dim(tissue_gene_expression$x)
view(tissue_gene_expression$x)

#with the gene expression measured on 500 genes for 189 biological samples representing
#seven different tissues. The tissue type is stored in y:
  
table(tissue_gene_expression$y)
view(tissue_gene_expression$y)
y = tissue_gene_expression$y
#Split the data in training and test sets, then use kNN to predict tissue type and see 
#what accuracy you obtain. Try it for  k = 1,3,…,11.



#Combine the data set
disease = data.frame(tissue_gene_expression$x,y = as.factor(y))
view(disease)

test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- disease %>% slice(-test_index)
test_set <- disease %>% slice(test_index)

train_knn <- train(y ~ ., method = "knn", 
                   data = train_set,
                   tuneGrid = data.frame(k = seq(1, 11, 2)))
ggplot(train_knn)

confusionMatrix(predict(train_knn, test_set, type = "raw"),
                test_set$y)

#2. Complete Case Study: more than 3 classes (32.7.6 or 31.8 on github)
if(!exists("mnist")) mnist <- read_mnist()
set.seed(3456)
index_127 <- sample(which(mnist$train$labels %in% c(1,2,7)), 2000)
y <- mnist$train$labels[index_127] 
x <- mnist$train$images[index_127,]
index_train <- createDataPartition(y, p=0.8, list = FALSE)
## get the quadrants
row_column <- expand.grid(row=1:28, col=1:28) 
upper_left_ind <- which(row_column$col <= 14 & row_column$row <= 14)
lower_right_ind <- which(row_column$col > 14 & row_column$row > 14)
## binarize the values. Above 200 is ink, below is no ink
x <- x > 200 
## proportion of pixels in lower right quadrant
x <- cbind(rowSums(x[ ,upper_left_ind])/rowSums(x), 
           rowSums(x[ ,lower_right_ind])/rowSums(x)) 
##save data
train_set <- data.frame(y = factor(y[index_train]),
                        x_1 = x[index_train,1], x_2 = x[index_train,2])
test_set <- data.frame(y = factor(y[-index_train]),
                       x_1 = x[-index_train,1], x_2 = x[-index_train,2])

train_set %>% ggplot(aes(x_1, x_2, color=y)) + geom_point()

train_qda <- train(y ~ ., method = "qda", data = train_set)

predict(train_qda, test_set, type = "prob") %>% head()

predict(train_qda, test_set) %>% head()

confusionMatrix(predict(train_qda, test_set), test_set$y)$table

GS <- 150
new_x <- expand.grid(x_1 = seq(min(train_set$x_1), max(train_set$x_1), len=GS),
                     x_2 = seq(min(train_set$x_2), max(train_set$x_2), len=GS))
new_x %>% mutate(y_hat = predict(train_qda, new_x)) %>%
  ggplot(aes(x_1, x_2, color = y_hat, z = as.numeric(y_hat))) +
  geom_point(size = 0.5, pch = 16) + 
  stat_contour(breaks=c(1.5, 2.5),color="black") + 
  guides(colour = guide_legend(override.aes = list(size=2)))

train_lda <- train(y ~ ., method = "lda", data = train_set)

new_x %>% mutate(y_hat = predict(train_lda, new_x)) %>%
  ggplot(aes(x_1, x_2, color = y_hat, z = as.numeric(y_hat))) +
  geom_point(size = 0.5, pch = 16) + 
  stat_contour(breaks=c(1.5, 2.5),color="black") + 
  guides(colour = guide_legend(override.aes = list(size=2)))

train_knn <- train(y ~ ., method = "knn", data = train_set,
                   tuneGrid = data.frame(k = seq(15, 51, 2)))

new_x %>% mutate(y_hat = predict(train_knn, new_x)) %>%
  ggplot(aes(x_1, x_2, color = y_hat, z = as.numeric(y_hat))) +
  geom_point(size = 0.5, pch = 16) + 
  stat_contour(breaks=c(1.5, 2.5),color="black") + 
  guides(colour = guide_legend(override.aes = list(size=2)))

train_set %>% mutate(y = factor(y)) %>% 
  ggplot(aes(x_1, x_2, fill = y, color=y)) + 
  geom_point(show.legend = FALSE) + 
  stat_ellipse(type="norm")


#3. 32.8 (31.9 on github) Exercises #1-6


#1)Create a dataset with just the classes “cerebellum” and “hippocampus”
#(two parts of the brain) and a predictor matrix with 10 randomly selected columns.

set.seed(1993)
data("tissue_gene_expression")
tissues <- c("cerebellum", "hippocampus")
ind <- which(tissue_gene_expression$y %in% tissues)
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

kyle = data.frame(x,y)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- kyle %>% slice(-test_index)
test_set <- kyle %>% slice(test_index)


view(train_set)

#Use the train function to estimate the accuracy of LDA.

train_lda <- train(y ~ ., method="lda", data = train_set)
y_hat <- predict(train_lda, test_set)
confusionMatrix(y_hat, test_set$y)


#2)n this case, LDA fits two 10-dimensional normal distributions. Look at the fitted model
#by looking at the finalModel component of the result of train. Notice there is a component
#called means that includes the estimate means of both distributions. Plot the mean vectors
#against each other and determine which predictors (genes) appear to be driving the algorithm.
train_lda$finalModel$means
qplot(train_lda$finalModel$mean[1,],train_lda$finalModel$means[2,])

#3) Repeat exercises 1 with QDA. Does it have a higher accuracy than LDA?

train_qda <- train(y ~ ., method="qda", data= train_set)
y_hat <- predict(train_qda, test_set)
confusionMatrix(y_hat, test_set$y)

#4) Are the same predictors (genes) driving the algorithm? Make a plot as in exercise 2.

train_qda$finalModel$means
qplot(train_qda$finalModel$mean[1,],train_qda$finalModel$means[2,])


#5)One thing we see in the previous plot is that the value of predictors correlate in both
#groups: some predictors are low in both groups while others are high in both groups. The mean value of each predictor, colMeans(x), is not informative or useful for prediction, and often for interpretation purposes it is useful to center or scale each column. This can be achieved with the preProcessing argument in train. Re-run LDA with preProcessing = "scale". Note that accuracy does not change but see how it is easier to identify the predictors that differ more between groups in the plot made in exercise 4.
train_lda <- train(y ~ ., method="lda", data = train_set, preProcess = 'scale')
y_hat <- predict(train_lda, test_set)
confusionMatrix(y_hat, test_set$y)

train_lda$finalModel$means
qplot(train_lda$finalModel$mean[1,],train_lda$finalModel$means[2,])

#6)In the previous exercises we saw that both approaches worked well. Plot the predictor values
#for the two genes with the largest differences between the two groups in a scatterplot to see 
#how they appear to follow a bivariate distribution as assumed by the LDA and QDA approaches.
#Color the points by the outcome.

train_set %>% ggplot(aes(TGFBR3,F11R, fill = y, color=y)) + 
  geom_point(show.legend = FALSE) + 
  stat_ellipse(type="norm")

