# Chapter 17 Homework
#DATA-320
#Kyle Schneider
#10/24/2019

#1. Suppose a cystic fibrosis test has accuracy of 99%.  If we select a random person
#and they test positive, what is the probability that they have the disease?
#Use a simulation to verify that the probability is approximately 0.02.
#Note: See 17.4.1 and 17.5 in your textbook

prev <- 0.00025
N <- 100000
outcome <- sample(c("Disease", "Healthy"), N, replace = TRUE, prob = c(prev, 1 - prev))
N_D <- sum(outcome == "Disease")
N_D
N_H <- sum(outcome == "Healthy")
N_H
accuracy = 0.99
test <- vector("character", N)
test[outcome == "Disease"] <- sample(c("+", "-"), N_D, replace=TRUE, prob =
                                       c(accuracy, 1-accuracy))
test[outcome=="Healthy"] <- sample(c("-", "+"), N_H, replace = TRUE, prob =
                                     c(accuracy, 1-accuracy))
table(outcome, test)


#2. Exercises 17.7 #6-11

#6)Florida is one of the most closely watched states in the U.S. election
#because it has many electoral votes, and the election is generally close,
#and Florida tends to be a swing state that can vote either way. Create the
#following table with the polls taken during the last two weeks:

library(tidyverse)
library(dslabs)
data(polls_us_election_2016)
polls <- polls_us_election_2016 %>% 
  filter(state == "Florida" & enddate >= "2016-11-04" ) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

#Take the average spread of these polls. The CLT tells us this average is
#approximately normal. Calculate an average and provide an estimate of the
#standard error. Save your results in an object called results.

results <- polls %>% 
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread)))
results


#7) Now assume a Bayesian model that sets the prior distribution for Florida’s election night spread d to
#be Normal with expected value µ and standard deviation τ . What are the interpretations of µ and τ?

#B. µ and τ summarize what we would predict for Florida before seeing any polls. Based on past
#elections, we would set µ close to 0 because both Republicans and Democrats have won, and τ at
#about 0.02, because these elections tend to be close.


#8) The CLT tells us that our estimate of the spread ˆd has normal distribution with expected value d and
#standard deviation σ calculated in problem 6. Use the formulas we showed for the posterior distribution
#to calculate the expected value of the posterior distribution if we set µ = 0 and τ = 0.01.

mu <- 0
tau <- 0.01
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)
posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))
posterior_mean
posterior_se

#9)Now compute the standard deviation of the posterior distribution.
sqrt( 1/ (1/sigma^2 + 1/tau^2))

#10)Using the fact that the posterior distribution is normal, create an interval
#that has a 95% probability of occurring centered at the posterior expected value.
#Note that we call these credible intervals.

CI= posterior_mean + c(-1.96, 1.96)*posterior_se
CI

#11) According to this analysis, what was the probability that Trump wins Florida?
pnorm (0, posterior_mean, posterior_se)

#Theres a 32% chance that trump wins florida

#12)Now use sapply function to change the prior variance from seq(0.05, 0.05, len = 100) and observe
#how the probability changes by making a plot.

mu = 0
taus = seq(0.005, 0.05, len = 100)
sigma = results$se
Y = results$avg
ps <- sapply(taus, function(tau){
  B = sigma^2 / (sigma^2 + tau^2)
  pnorm(0, posterior_mean, posterior_se)
})
plot(taus, ps)








