# Chapter 17 & 18 Homework
#DATA 320
#Kyle Schneider
#10/30/2019


#1. Exercises 17.9 #1-3

#1)Create this table: 

library(tidyverse)
library(dslabs)
data("polls_us_election_2016")
polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

#Now for each poll use the CLT to create a 95% confidence interval for the spread
#reported by each poll. Call the resulting object cis with columns lower and upper 
#for the limits of the confidence intervals. Use the `select` function to keep the 
#columns `state, startdate, end date, pollster, grade, spread, lower, upper`.

cis = polls %>% mutate(X_hat = (spread+1)/2, se = 2*sqrt(X_hat*(1-X_hat)/samplesize), 
                        lower = spread - qnorm(0.975)*se, upper = spread + qnorm(0.975)*se) %>%
  select(state, startdate, enddate, pollster, grade, spread, lower, upper)


#2) You can add the final result to the cis table you just created using the right_join
#function like this:
add <- results_us_election_2016 %>% 
  mutate(actual_spread = clinton/100 - trump/100) %>% 
  select(state, actual_spread)
cis <- cis %>% 
  mutate(state = as.character(state)) %>% 
  left_join(add, by = "state")

#Now determine how often the 95% confidence interval includes the actual result.

cis %>% mutate(hit = lower <= actual_spread & upper >= actual_spread) %>% summarize(proportion_hits = mean(hit))

#3)Repeat this, but show the proportion of hits for each pollster. Show only pollsters
#with more than 5 polls and order them from best to worst. Show the number of polls
#conducted by each pollster and the FiveThirtyEight grade of each pollster. Hint:
#use n=n(), grade = grade[1] in the call to summarize.

cis %>% mutate(hit = lower <= actual_spread & upper >= actual_spread) %>% 
  group_by(pollster) %>%
  filter(n() >=  5) %>%
  summarize(proportion_hits = mean(hit), n = n(), grade = grade[1]) %>%
  arrange(desc(proportion_hits))





