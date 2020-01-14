#Multiple Regression & Confounding Variables Homework 
#DATA 320
#Kyle Schneider
#11/6/2019

library(Lahman)
library(dslabs)
library(broom)
library(tidyverse)
library(HistData)

data(GaltonFamilies)
view(GaltonFamilies)
galton_heights

#Coming back to HW from last class, you: used the data from above on Galton Families to
#fit a multiple regression model examining the role of mother’s height and father’s height
#on daughter’s height.

daughter_heights <- GaltonFamilies %>% filter(gender == "female") %>% group_by(family) %>%
  sample_n(1) %>% ungroup() %>% select(father, mother, childHeight) %>% rename(daughter =
                                                                                 childHeight)

mod <- lm(daughter ~ mother + father, data=daughter_heights)
summary(mod)

#Now:
#1. Discuss whether the assumptions of multiple regression are met.
plot(mod)

#The variance of the residuals is contant across the predictor variable.
#The residual is constant across the preditors

#Normally distributed errors (residuals)
#It seems the distribution of the erros is Normal there is a slight difference at the tail edges

#No outliers in our residuals
#There are no outliers in our residual

#No influential points (Cook’s distance should be less than 1)
# There are no influential points
  
#2. Plot the predicted value of daughter’s height by her actual height.  How well does the model
#predict these values ?

daughter_heights %>%
mutate(daughter_height_hat = predict(mod, newdata = .)) %>%
  ggplot(aes(daughter_height_hat, daughter, label = mother )) +
  geom_point() +
  geom_text(nudge_x=0.1, cex = 2) +
  geom_abline()
 
# I dont think the model predict the values extremely well it actually seems quiet far off 

#3. Exercises 20.6 #5 (Ch. 19.6 #5 on github )
#To complete this, you will need to read through 1-4.  We have done 1-3 in class previously 
#(see Ch. 16 Statistical Inference section on Chi-square).
library(dslabs)
data("research_funding_rates")
research_funding_rates
totals <- research_funding_rates %>%
  summarize(yes_men = sum(awards_men), no_men = sum(applications_men - awards_men), yes_women =
              sum(awards_women), no_women = sum(applications_women - awards_women))
#what is the funding rate for men and women?
totals %>% summarize(percent_men = yes_men /(yes_men + no_men), percent_women = yes_women/(yes_women
                                                                                           +no_women))
#what is the overall rate?
funding_rate <- totals %>%
  summarize(percent_total = (yes_men + yes_women) / (yes_men + yes_women + no_men + no_women)) %>%
  pull(percent_total)

funding_rate

#create a continguency table
c_table <- data.frame(funded = c("no", "yes"), men = c(totals$no_men, totals$yes_men),
                      women = c(totals$no_women, totals$yes_women)) 

#To complete #4, you can use this code:

new_data <- research_funding_rates %>% mutate(discipline = reorder(discipline, success_rates_total)) %>%
  select(discipline, awards_men, awards_women, applications_men, applications_women) %>%
  gather(awards, applications, -discipline) %>% separate(awards, c("name", "gender")) %>%
  group_by(gender) %>%
  spread(name, applications) %>% ungroup()

#5)To check if this is a case of Simpson’s paradox, plot the success rates versus disciplines, which
#have been ordered by overall success, with colors to denote the genders and size to denote the number
#of applications.
k = new_data %>% mutate(success = awards/applications)
k %>% 
  ggplot(aes(discipline, success, size = applications, color = gender, label = applications)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_point()

#4. Conduct a chi-square test on the discipline Medical sciences to determine if there is a significant
#difference in the proportion of men and women who have their grants funded.  Discuss your findings.

k %>% select(awards, applications) %>% chisq.test() %>% tidy

#I dont know if I did it right but if we used these numbers we have a high chi-squared of 30.5 and
# a p-value less than 0.05, so we'd regect the null hypothesis and conclude that number of applications
# and Awarded funds are related.


