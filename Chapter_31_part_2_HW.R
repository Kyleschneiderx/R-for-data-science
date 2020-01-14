#Chapter 31 part 2 Homework
#DATA 320
#Kyle Schneider
#12/5/2019

#31.12 9-15
library(caret)
library(tidyverse)
library(dslabs)
#9) Use the rpart function to fit a classification tree to the tissue_gene_expression dataset.
#Use the train function to estimate the accuracy. Try out cp values of seq(0, 0.05, 0.01).
#Plot the accuracy to report the results of the best model.
data("tissue_gene_expression")
y = tissue_gene_expression$y
disease = data.frame(tissue_gene_expression$x,y = as.factor(y))
#view(disease)

test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- disease %>% slice(-test_index)
test_set <- disease %>% slice(test_index)

train_rpart <- train(y ~ ., method="rpart",
                     tuneGrid=data.frame(cp=seq(0,0.05,0.01)),
                     data=train_set)
plot(train_rpart)
train_rpart$bestTune
y_hat <- predict(train_rpart,test_set)
confusionMatrix(y_hat, test_set$y)
fit <- rpart(y ~.,
             data=train_set,control=rpart.control(cp=0.00833))
rpart.plot(fit)
train_rpart$finalModel

#10. Study the confusion matrix for the best fitting classification tree.
#What do you observe happening for placenta?
# its about a fifty percent chance of actually predicting it much lower then other organs

#11)Notice that placentas are called endometrium more often than placenta. Note also that
#the number of placentas is just six, and that, by default, `rpart` requires 20 observations
#before splitting a node. Thus it is not possible with these parameters to have a node in 
#which placentas are the majority. Rerun the above analysis but this time permit `rpart`
#to split any node by using the argument `control = rpart.control(minsplit = 0)`. Does
#the accuracy increase? Look at the confusion matrix again.

train_rpart_2 <- train(y ~ ., method="rpart",
                     tuneGrid=data.frame(cp=seq(0,0.05,0.01)),
                     control = rpart.control(minsplit = 0),
                     data=train_set)
plot(train_rpart_2)
train_rpart$bestTune
y_hat_2 <- predict(train_rpart_2,test_set)
confusionMatrix(y_hat_2, test_set$y)
fit_2 <- rpart(y ~.,
             data=train_set,control=rpart.control(cp=0.00833))
rpart.plot(fit_2)
train_rpart_2$finalModel

# it increases the accuracy to .8333

#12)Plot the tree from the best fitting model obtained in exercise 11.
fit_2 <- rpart(y ~.,
               data=train_set,control=rpart.control(cp=0.00833))
rpart.plot(fit_2)


#13)We can see that with just six genes, we are able to predict the tissue type.
#Now let's see if we can do even better with a random forest. Use the `train` function
#and the `rf` method to train a random forest. Try out values of `mtry` ranging from, 
#at least, `seq(50, 200, 25)`. What `mtry` value maximizes accuracy? To permit small
#`nodesize` to grow as we did with the classification trees, use the following argument:
#`nodesize = 1`.  This will take several seconds to run. If you want to test it out, 
#try using smaller values with `ntree`. Set the seed to 1990.

train_rf <- train(y ~., method="rf",
                  tuneGrid=data.frame(mtry=seq(50,200,25)),
                  data=train_set, nodesize=1)
ggplot(train_rf)

y_hat_rf = predict(train_rf, test_set)
confusionMatrix(y_hat_rf,test_set$y)


#14) Use the function `varImp` on the output of `train` and save it to an object called `imp`.

imp = varImp(train_rf)
imp
#15)The `rpart` model we ran above produced a tree that used just six predictors. Extracting
#the predictor names is not straightforward, but can be done. If the output of the call to 
#train was `fit_rpart`, we can extract the names like this:

ind <- !(train_rpart$finalModel$frame$var == "<leaf>")
ind
tree_terms <- 
  train_rpart$finalModel$frame$var[ind] %>%
  unique() %>%
  as.character()
tree_terms

#What is the variable importance in the random forest call for these predictors?
#Where do they rank?
#"GPA33:51.61"   "RELB: 100.00"    "SLC25A5: 43.40" "GREM2: 42.71"   "MAML1: 24.85"
varImp(train_rpart)





