#DATA-320
#Chapter 9 Homework
#Kyle Schneider
#9/24/2019
library(tidyverse)

# 9.15 Exercises: #1-9, 11-14

#1)Define variables containing the heights of males and females like this:
library(dslabs)
data(heights)
male <- heights$height[heights$sex=="Male"]
female <- heights$height[heights$sex=="Female"]
#How many measurements do we have for each?
heights %>% count(sex)

# There is 238 female measurements
# and 812 male measurements

#2)Suppose we can’t make a plot and want to compare the distributions side by side.
#We can’t just list all the numbers. Instead, we will look at the percentiles. 
#Create a five row table showing female_percentiles and male_percentiles with the
#10th, 30th, 50th, …, 90th percentiles for each sex. Then create a data frame with
#these two as columns.

male = heights$height[heights$sex=="Male"]
female = heights$height[heights$sex=="Female"]

female_percentiles = quantile(female, seq(0.1, 0.9, 0.2))
male_percentiles = quantile(male, seq(0.1, 0.9, 0.2))
data.frame(female_percentiles, male_percentiles)   


#3)Study the following boxplots showing population sizes by country:
#Which continent has the country with the biggest population size?

#Asia has the greatest population

#4)What continent has the largest median population size?

#Africa has the largest median population size.

#5)What is median population size for Africa to the nearest million?


ds_theme_set()
data(gapminder)
tab = gapminder %>% filter(year == 2010) %>% group_by(continent) %>% select(continent, population)  
tab %>% ggplot(aes(x=continent, y=population/10^6)) + geom_boxplot() + scale_y_continuous(trans = "log10", breaks = c(1,10,100,1000)) + ylab("Population in millions")
round(median(tab$population[tab$continent=="Africa"]/10^6,),-1)

# the median population size in Africa to the nearest million is 10.

#6)What proportion of countries in Europe have populations below 14 million?

#B. 0.75

#7)If we use a log transformation, which continent shown above has the largest interquartile range?

# Americas have the largest interquartile range

#8)Load the height data set and create a vector x with just the male heights:
library(dslabs)
data(heights)
x = heights$height[heights$sex=="Male"]
#What proportion of the data is between 69 and 72 inches (taller than 69, but
#shorter or equal to 72)? Hint: use a logical operator and mean.

mean(x>69 & x<=71)

#9)Suppose all you know about the data is the average and the standard deviation.
#Use the normal approximation to estimate the proportion you just calculated. 
#Hint: start by computing the average and standard deviation. Then use the pnorm
#function to predict the proportions.

avg = mean(x)
stdev = sd(x)
pnorm(71, avg, stdev) - pnorm(69, avg, stdev)

#11)Approximate the distribution of adult men in the world as normally distributed
#with an average of 69 inches and a standard deviation of 3 inches. Using this 
#approximation, estimate the proportion of adult men that are 7 feet tall or taller,
#referred to as seven footers. Hint: use the pnorm function.

1 - pnorm(7*12, 68, 3)

#12)There are about 1 billion men between the ages of 18 and 40 in the world. Use 
#your answer to the previous question to estimate how many of these men (18-40 year olds)
#are seven feet tall or taller in the world?

p = 1 - pnorm(7*12, 68, 3)
r = round(p * 1.5*10^9)

#13)There are about 10 National Basketball Association (NBA) players that are 7 feet tall
#or higher. Using the answer to the previous two questions, what proportion of the world’s
#18 to 40 year old seven footers are in the NBA?

10/r

#14) Repeat the calculations performed in the previous question for Lebron James’ height:
#6 feet 8 inches. There are about 150 players that are at least that tall.

p = 1 - pnorm(6*12 + 8, 68, 3)
rLebron = round(p * 1.5*10^9)
150/rLebron


# Create a histogram of female heights (use binwidth = 1, fill color = blue, x
#                                          label = “Female heights in inches”)

heights %>% filter(sex=="Female") %>% ggplot(aes(height), ylabel = "Amount of Females") +
  geom_histogram(binwidth=1, fill="blue", color="black") +
  ggtitle("Female heights in inches")

# Create a density plot for female heights (fill color = red)
heights %>% filter(sex == "Female") %>% ggplot(aes(height)) +
  geom_density(fill="red", adjust = 2)                                         
                                          
# Create a qqplot to determine if female heights follows the normal curve
#(include the line y=x). Interpret your findings. (Hint: you may want to read section 9.14)

heights %>% filter(sex =="Female") %>% ggplot(aes(sample = scale(height)))+ geom_qq()+geom_abline()







