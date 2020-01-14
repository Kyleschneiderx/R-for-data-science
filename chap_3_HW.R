# Kyle Schneider
# DATA-LA 320
# 9/2/2019
# Homework #1

# 3.3 Exercises

# 1.What is the sum of the first 100 positive integers? The formula
# for the sum of integers 1 through n is n(n+1)/2. Define n = 100 
# and then use R to compute the sum of 1 through 100 using the formula. What is the sum?

n1 = 100
k1 = n1*(n1+1)/2
print(k1)
# Sum = 5050

# 2.Now use the same formula to compute the sum of the integers from 1 through 1,000.

n2 = 1000
k2 = n2*(n2+1)/2
print(k2)
# sum = 500500

# 3.Look at the result of typing the following code into R:
n <- 1000
x <- seq(1, n)
sum(x)

#Based on the result, what do you think the functions seq and sum do?
# You can use the help system: B. seq creates a list of numbers and sum adds them up.


# 3.9 Exercises

# 1.Use the function c to create a vector with the average high temperatures in January
# for Beijing, Lagos, Paris, Rio de Janeiro, San Juan and Toronto, which are
# 35, 88, 42, 84, 81, and 30 degrees Fahrenheit. Call the object temp.


temp = c(35, 88, 42, 84, 81, 30)




# 2. Now create a vector with the city names and call the object city.

city = c('Beijing', 'Lagos', 'Paris', 'Rio de Janeiro', 'San Juan', 'Toronto')

# 3. Use the names function and the objects defined in the previous exercises
# to associate the temperature data with its corresponding city.

names(temp) <- city
temp
# 4. Use the [ and : operators to access the temperature of the first three cities on the list.
temp[1:3]






