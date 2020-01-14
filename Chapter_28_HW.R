#Chapter 28 Homework
#DATA 320
#Kyle Schneider
#11/15/2019


#28.5 (27.5 on github) #1-6
#Let’s talk about #2: Instead of using height to predict sex, use the type
#variable. 
library(lubridate)
library(tidyverse)
library(caret)
library(dslabs)
data("reported_heights")
dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & 
           date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & 
                         between(minute(date_time), 15, 30),
                       "inclass", "online")) %>% select(sex, type)
x <- dat$type
y <- factor(dat$sex, c("Female", "Male"))
dat
#1) Show summary statistics that indicate that the type is predictive of sex.
fit <- prop.table(table(y, x), 2)
summary(fit)
#2. Instead of using height to predict sex, use the type variable.
table(predicted = x, actual = y)

#3. Show the confusion matrix.

cm <- confusionMatrix(table(predicted = x, actual = y))
cm
#4. Use the confusionMatrix function in the caret package to report accuracy.

cm$overall["Accuracy"]

#5. Now use the sensitivity and specificity functions to report specificity and sensitivity.

cm$byClass[c("Sensitivity","Specificity", "Prevalence")]

#6. What is the prevalence (% of females) in the dat dataset defined above?
#Prevalence is defined by the amount of females in the data set

#28.7 (27.7 on github) #1-2

#1)Compute conditional probabilities for being Male for the heights dataset. Round the#
#heights to the closest inch. Plot the estimated conditional probability  
#P(x)= Pr (Male | height = x) for each x.
view(heights)


heights %>% 
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prob = mean(sex == "Male")) %>%
  ggplot(aes(x, prob)) +
  geom_point()


#2) In the plot we just made, we see high variability for low values of height. This
#is because we have few data points in these strata. This time use the quantile function
#for quantiles  0.1 , 0.2 , … , 0.9 and the cut function to assure each group has the
#same number of points. Hint: for any numeric vector x, you can create groups based on
#quantiles like this:
x <- heights$height

heights %>% 
  mutate(x = cut(x, quantile(x, seq(0, 1, 0.1)), include.lowest = TRUE))%>%
  group_by(x) %>% 
  filter(n() >= 10) %>%
  summarize(prob = mean(sex == "Male")) %>%
  ggplot(aes(x, prob)) +
  geom_point()






