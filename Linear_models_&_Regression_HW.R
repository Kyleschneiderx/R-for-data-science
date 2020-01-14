#Chapter 18 & 19 Homework
#DATA 320
#Kyle Schneider
#11/5/2019
library(Lahman)
library(dslabs)
library(broom)
library(tidyverse)
library(HistData)

data(GaltonFamilies)

view(GaltonFamilies)

galton_heights

# Chapter 18
#1. Using the GaltonFamilies data from the HistData , select 1 daughter for each
#mother and predict daughter’s height from mother’s height. 

galton_heights <- GaltonFamilies %>% filter(gender == "female") %>% group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>% 
  select(mother,father, childHeight) %>% rename(daughter=childHeight)
galton_heights


galton_heights %>% ggplot(aes(mother, daughter)) + geom_point(alpha=0.5)
galton_heights %>% summarize(r=cor(mother,daughter)) %>% pull(r)
galton_heights

#2. What is the best fit line of the data?

mu_x <- mean(galton_heights$mother)
mu_y <- mean(galton_heights$daughter)
s_x <- sd(galton_heights$mother)
s_y <- sd(galton_heights$daughter)
r <- cor(galton_heights$mother, galton_heights$daughter)
m <- r*s_y / s_x
b <- mu_y - m * mu_x

galton_heights %>% ggplot(aes(mother, daughter)) + geom_point(alpha = 0.5) + geom_abline(intercept=b, slope = m)
galton_heights %>% ggplot(aes(scale(mother), scale(daughter))) + geom_point(alpha=0.5) + geom_abline(intercept =
                                                                                                  0, slope = r)
#3. If a mother is 65 inches tall, what is the expected height of her daughter?

galton_heights %>% mutate(mother_strata = factor(round(mother))) %>%
  ggplot(aes(mother_strata, daughter)) + geom_boxplot() + geom_point()
# The Expected height would be around 65 inches

#4. Is mother’s height a significant predictor of daughter’s height?
model1 <- lm(daughter ~ mother, data = galton_heights)
summary(model1)

#5. What is the R 2 value?  How would we interpret this?
#R-squared is 0.0748. How I interpret this is that about 7% of the time the data follows the trendline

#6. Does the model meet the assumptions of regression?  Why or why not?
plot(model1)
max(cooks.distance(model1))

#I dont believe the model meets assumptions of regressions, because there is some very large outliers that
# I believe effect the out comes

# Chapter 19 (Linear Models)

#7. Using the data from above on Galton Families, fit a multiple regression model examining the role of
#mother’s height and father’s height on daughter’s height.
model1 <- lm(daughter ~ mother + father, data = galton_heights)
summary(model1)

#8. Check to see if the slopes of father’s height on daugther’s height seem approximately the same for
#different ‘strata’ of mother’s height

galton_heights %>% mutate(mother_strata = factor(round(mother))) %>%
  ggplot(aes(father, daughter)) +
  geom_point(alpha=0.5) + geom_smooth(method="lm") +
  facet_wrap(~ mother_strata)

galton_heights %>% mutate(father_strata = factor(round(father))) %>%
  ggplot(aes(mother, daughter)) +
  geom_point(alpha=0.5) + geom_smooth(method="lm") +
  facet_wrap(~ father_strata)

#9. Interpret the results.  Is mother’s height a significant predictor of daughter’s height after
#controlling for father’s height?  What about father’s height (after controlling for mother’s height)?

# A mothers height isn't a significant predictor of a daughters height compared to the significants of
#the fathers hieght playing a role.

#10. TEXTBOOK QUESTIONS: Section 19.6 #5-7 (or 18.6 #5-7 on github)

#5. Fit a linear regression model to obtain the effects of BB and HR on Runs (at the team level)
#in 1971. Use the tidy function in the broom package to obtain the results in a data frame.
dat <- Teams %>% filter(yearID == 1971)
fit <- lm(R ~ BB + HR, data = dat)
tidy(fit) #returns a tibble
  
#6. Now let’s repeat the above for each year since 1961 and make a plot. Use do and the broom
#package to fit this model for every year since 1961.
data = Teams %>% filter(yearID %in% 1961:2001) %>%
  group_by(yearID) %>%
  do(tidy(lm(R ~ BB, data = .), conf.int = TRUE))

#7. Use the results of the previous exercise to plot the estimated effects of BB on runs.

data = Teams %>% filter(yearID %in% 1961:2001) %>%
  group_by(yearID) %>%
  do(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) %>%
  ggplot(aes(BB, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point()


    
    
    