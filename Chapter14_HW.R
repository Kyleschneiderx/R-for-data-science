# Homework Chapter 14
#Data-320
#10/15/2019

#1.Use a Monte Carlo simulation (10,000 simulations) to determine the likelihood that you are
#dealt 2 Aces when playing blackjack. 
install.packages("gtools")
library(gtools)
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K")
deck <- expand.grid(number=numbers, suit=suits)
deck <- paste(deck$number, deck$suit) 

aces <- paste("A", suits)
facecard <- c("K", "Q", "J", "10")
facecard <- expand.grid(number = facecard, suit=suits)
facecard <- paste(facecard$number, facecard$suit)
facecard
hands <- combinations(52,2,v=deck) 

blackjack <- function() {
  hand <- sample(deck,2)
  (hand[1] %in% aces & hand[2] %in% aces) | (hand[2] %in% aces & hand[1] %in%
                                                   aces)
}
B <- 10000
results <- replicate(B, blackjack())
mean(results)


#2.Determine the ‘exact probability’ of getting 2 Aces when playing blackjack.

4/52*3/51

#0.0039

#3.Ch. 14 Exercises 14.14 #8

#The distribution of IQ scores is approximately normally distributed. The average 
#is 100 and the standard deviation is 15. Suppose you want to know the distribution
#of the highest IQ across all graduating classes if 10,000 people are born each in
#your school district. Run a Monte Carlo simulation with B=1000 generating 10,000
#IQ scores and keeping the highest. Make a histogram.

m = 100
sd = 15

B <- 1000
HighestIQ <- replicate(B, {
  simulated_data <- rnorm(10000, m, sd)
  max(simulated_data)
})
hist(HighestIQ)


