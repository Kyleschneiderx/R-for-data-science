# Chapter 16 Homework
#DATA-320
#Kyle Schneider
#10/17/2019



#1. Exercises 16.3 #1-2

#1) Suppose you poll a population in which a proportion p of voters are Democrats
#and 1− p are Republicans. Your sample size is N = 25. Consider the random variable
#S which is the total number of Democrats in your sample. What is the expected value
#of this random variable? Hint: it’s a function of p.
N=25

total = N*p


#2)What is the standard error of S ? Hint: it’s a function of p.

se = sqrt(p*(1-p))/sqrt(N)

#2. Exercises 16.7 #1-5
library(tidyverse)
polls <- polls_us_election_2016 %>% 
  filter(enddate >= "2016-10-31" & state == "U.S.") 


#1) For the first poll, you can obtain the samples size and estimated Clinton percentage with:

N <- polls$samplesize[1]
x_hat <- polls$rawpoll_clinton[1]/100

#Assume there are only two candidates and construct a 95% confidence interval for the election
# night proportion p.

se_hat = sqrt(x_hat*(1-x_hat)/N)
x_hat + c(-1,1)*pnorm(0.975)*se_hat

#2) Now use dplyr to add a confidence interval as two columns, call them lower and upper, to 
#the object poll. Then use select to show the pollster, enddate, x_hat,lower, upper variables.
#Hint: define temporary columns x_hat and se_hat.

polls %>% mutate(x_hat = polls$rawpoll_clinton/100, se_hat = sqrt(x_hat*(1-x_hat)/samplesize),
                 lower = x_hat - pnorm(0.975)*se_hat, upper = x_hat + pnorm(0.975)*se_hat) %>%
  select(pollster, enddate, x_hat, lower, upper)


#3)The final tally for the popular vote was Clinton 48.2% and Trump 46.1%. Add a column, call
#it hit, to the previous table stating if the confidence interval included the true proportion
#p = 0.482 or not.

polls %>% mutate(x_hat = polls$rawpoll_clinton/100, se_hat = sqrt(x_hat*(1-x_hat)/samplesize),
                 lower = x_hat - pnorm(0.975)*se_hat, upper = x_hat + pnorm(0.975)*se_hat, 
                 hit = lower<=0.482 & upper>=0.482) %>%
  select(pollster, enddate, x_hat, lower, upper, hit) 


#4)For the table you just created, what proportion of confidence intervals included p?

polls %>% mutate(x_hat = polls$rawpoll_clinton/100, se_hat = sqrt(x_hat*(1-x_hat)/samplesize),
                 lower = x_hat - pnorm(0.975)*se_hat, upper = x_hat + pnorm(0.975)*se_hat, 
                 hit = lower<=0.482 & upper>=0.482) %>%
  select(pollster, enddate, x_hat, lower, upper, hit) %>% summarize(mean(hit))

#5) If these confidence intervals are constructed correctly, and the theory holds up, what
#proportion should include p?

#0.95 or 95%


