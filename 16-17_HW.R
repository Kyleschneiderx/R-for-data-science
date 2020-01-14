#Kyle Schneider
# Chapter 16 & 17 Homework 
# DATA 320
# 10/23/2019


# Exercises 16.11 #1, 3

#1) A famous athlete has an impressive career, winning 70% of her 500 career matches. However,
#this athlete gets criticized because in important events, such as the Olympics, she has a 
#losing record of 8 wins and 9 losses. Perform a Chi squared test to determine if this losing
#record can be simply due to chance as opposed to not performing well under pressure.

wins = 500*.7
wins
losses = 500*.3

two_by_two <- data.frame(success = c("Win", "Loss"),
                         olympicCount = c(8,9),
                         nonOlympics = c(wins, losses))
two_by_two
chisq_test <- two_by_two %>% 
  select(-success) %>%
  chisq.test()
chisq_test

# theres an 8% chances that it was obtained by chance alone, so she does preform worse during
# important matches

#3) Compute the odds ratio of “losing under pressure” along with a confidence interval.

odds_important <- (two_by_two$olympicCount[1] / sum(two_by_two$olympicCount)) / 
  (two_by_two$olympicCount[2] / sum(two_by_two$olympicCount))
odds_important

odds_normal <- (two_by_two$nonOlympics[1] / sum(two_by_two$nonOlympics)) / 
  (two_by_two$nonOlympics[2] / sum(two_by_two$nonOlympics))
odds_normal

log_or <- log( odds_important / odds_normal )
se <- two_by_two %>% 
  select(-success) %>%
  summarize(se = sqrt(sum(1/olympicCount) + sum(1/nonOlympics))) %>%
  pull(se)
ci <- log_or + c(-1,1) * qnorm(0.975) * se
ci
exp(ci)

# Exercises 17.3 #1-5
library(dslabs)
data(heights)
x <- heights %>% filter(sex == "Male") %>%
  pull(height)

#1)Mathematically speaking, x is our population. Using the urn analogy, we have an
#urn with the values of x in it. What are the average and standard deviation of our
#population?

mean(x)
sd(x)


#2)Call the population average computed above μ and the standard deviation σ. Now
#take a sample of size 50, with replacement, and construct an estimate for μ and σ.

N = 50
X = sample(x, N, replace = TRUE)
mean(X)
sd(X)

#3)What does the theory tell us about the sample average ¯X and how it is related to μ?

#B. It is a random variable with expected value μ and standard error σ/√N.




#4)So how is this useful? We are going to use an oversimplified yet illustrative example.
#Suppose we want to know the average height of our male students, but we only get to
#measure 50 of the 708. We will use ¯X as our estimate. We know from the answer to exercise
#3 that the standard estimate of our error ¯X−μ is σ/√N. We want to compute this, but we
#don’t know σ. Based on what is described in this section, show your estimate of σ.

sd(X)


#5)Now that we have an estimate of  σ, let’s call our estimate s. Construct a 95% confidence
#interval for μ.

mean(X) + c(-1, 1)*qnorm(1 - 0.05/2) * sd(X) / sqrt(N)

