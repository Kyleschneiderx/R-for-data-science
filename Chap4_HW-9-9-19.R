# Exercise 4.6 #3-9,11-12


#3) The function nchar tells you how many characters long a character vector is.
# Write a line of code that assigns to the object new_names the state abbreviation
# when the state name is longer than 8 characters.

new_names = ifelse(nchar(murders$state)>8, murders$abb, murders$state)

#4)Create a function sum_n that for any given value, say n,computes the
#sum of the integers from 1 to n (inclusive). Use the function to determine
# the sum of integers from 1 to 5,000.

sum_n = function(n){
  x = 1:n
  sum(x)
}

#5)Create a function altman_plot that takes two arguments, x and y, and 
#plots the difference against the sum
altman_plot = function(x,y){
  plot(x+y, y-x)
}

#6)After running the code below, what is the value of x?
x <- 3
my_func <- function(y){
  x <- 5
  y+5
}
my_func(5)
print(x)
# x= 3 if you run the function my_func


#7)Write a function compute_s_n that for any given n computes the 
#sum S n=1^2+2^2+3^2+…n^2. Report the value of the sum when n=10.

compute_s_n = function(n){
  x = 1:n
  sum(x^2)
}


#8)Define an empty numerical vector s_n of size 25 using 
#s_n <- vector("numeric", 25) and store in the results of
#1,S2,…S 25 using a for-loop.

s_n = vector("numeric", 25)
for(i in 1:25){
  s_n[i] = compute_s_n(i)
}
s_n


#9) Repeat exercise 8, but this time use sapply
s_n = sapply(1:25, compute_s_n)
s_n


#11)Plot Sn versus n. Use points defined by n= 1,…,25.
n= 1:25
plot(n, s_n)

#12) Confirm that the formula for this sum is Sn=n(n+1)(2n+1)/6
identical(s_n, n*(n+1)*(2*n+1)/6)
