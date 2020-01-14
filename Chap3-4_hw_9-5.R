# Kyle Schneider
# DATA-LA 320
# 09/4/2019

# 3.13 Exercise

# 1) 

temp <- c(35, 88, 42, 84, 81, 30)
city <- c("Beijing", "Lagos", "Paris", "Rio de Janeiro", "San Juan", "Toronto")
city_temps <- data.frame(name = city, temperature = temp)


# Remake the data frame using the code above, but add a line that converts 
#the temperature from Fahrenheit to Celsius. The conversion is  C =5/9×(F − 32).

k= 5/9*(temp-32)
k

# 2)What is the following sum 1+ 1/2^2+ 1/3^2+…1/100^2? Hint: thanks to Euler,
#we know it should be close to π^2/6.
sum(pi^(2/6))
x <- seq(1, 100)
sum(1/x^2)


#3.17 Excercise

library(dslabs)
data(murders)
population_in_millions = murders$population/10^6
total_gun_murders = murders$total
plot(population_in_millions, total_gun_murders)
log10_population = log10(murders$population)
log10_murders_total = log10(murders$total)
plot(log10_population, log10_murders_total)

