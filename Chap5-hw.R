#Kyle Schneider
#DATA-LA 320
#Home Work #4
#9/17/2019
install.packages("tidyverse")
install.packages("dplyr")
install.packages("dslabs")
library(dplyr)
library(tidyverse)
library(dslabs)
library(NHANES)
data(murders)
data(NHANES)
#(pg. 78) Exercises 5.2 #4

#4)Which of the following built-in datasets is tidy (you can pick more than one):
# DNase, Formaldehyde, and orange are the tidy datasets
data("DNase")
head(DNase)
data("Formaldehyde")
head(Formaldehyde)
data("Orange")
head(Orange)


#(pg. 79-80) Exercises 5.4 #2, 4, 7

#2) If rank(x) gives you the ranks of x from lowest to highest, rank(-x) gives you
#the ranks from highest to lowest. Use the function mutate to add a column rank
#containing the rank, from highest to lowest murder rate. Make sure you redefine 
#murders so we can keep using this variable.
murders = mutate(murders, rank = rank(-rate))

#4)The dplyr function filter is used to choose specific rows of the data frame to
#keep. Unlike select which is for columns, filter is for rows. For example, you can
#show just the New York row like this: filter(murders, state == "New York")

filter(murders, state == "New York")

#Use filter to show the top 5 states with the highest murder rates. After we add 
#murder rate and rank, do not change the murders dataset, just show the result.
#Remember that you can filter based on the rank column.

#add murder rate
murders = mutate(murders, rate = total/population * 100000, rank = rank(-rate))

# Filter by top five
filter(murders, rank <=5)


#7) Suppose you want to live in the Northeast or West and want the murder rate to
#be less than 1. We want to see the data for the states satisfying these options.
#Note that you can use logical operators with filter. Here is an example in which
#we filter to keep only small states in the Northeast region.

filter(murders, population < 5000000 & region == "Northeast")

#Make sure murders has been defined with rate and rank and still has all states.
#Create a table called my_states that contains rows for states satisfying both
#the conditions: it is in the Northeast or West and the murder rate is less than
# 1. Use select to show only the state name, the rate and the rank.

my_states = filter(murders, region %in% c("Northeast", "West") & rate < 1)
select(my_states, state, rate, rank)


#(pg. 82) Exercises 5.6 #2

#2)Reset murders to the original table by using data(murders). Use a pipe to
#create a new data frame called my_states that considers only states in the 
#Northeast or West which have a murder rate lower than 1, and contains only 
#the state, rate and rank columns. The pipe should also have four components 
#separated by three %>%. The code should look something like this:

my_states = murders %>%
  mutate(rate =  total / population * 100000, rank = rank(-rate)) %>%
  filter(region %in% c("Northeast", "West") & rate < 1) %>% 
  select(state, rate, rank)


#(pg. 86-87) Exercises 5.9 #1-7
install.packages("NHANES")
library(NHANES)
data(NHANES)

#1)We will provide some basic facts about blood pressure. First let’s select
#a group to set the standard. We will use 20-29 year old females. AgeDecade 
#is a categorical variable with these ages. Note that the category is coded 
#like " 20-29“, with a space in front! What is the average and standard deviation
#of systolic blood pressure as saved in the BPSysAve variable? Save it to a variable
#called ref.

ref = NHANES %>% filter(AgeDecade == " 20-29" & Gender == "female") %>% summarize(average=mean(BPSysAve,na.rm=TRUE), standard_deviation=sd(BPSysAve,na.rm=TRUE))


#2)Using a pipe, assign the average to a numeric variable ref_avg.
# Hint: Use the code similar to above and then pull.
ref_avg = NHANES %>%
  filter(AgeDecade == " 20-29" & Gender == "female") %>%
  summarize(average = mean(BPSysAve, na.rm = TRUE), 
            standard_deviation = sd(BPSysAve, na.rm=TRUE)) %>% .$average

#3)Now report the min and max values for the same group.
NHANES %>%
  filter(AgeDecade == " 20-29"  & Gender == "female") %>%
  summarize(min=min(BPSysAve,na.rm=TRUE),
            max=max(BPSysAve,na.rm=TRUE))

#4)Compute the average and standard deviation for females,
#but for each age group separately rather than a selected 
#decade as in question 1. Note that the age groups are 
#defined by AgeDecade. Hint: rather than filtering by age
#and gender, filter by Gender and then use group_by.

NHANES %>%
  filter(Gender == "female") %>% group_by(AgeDecade) %>%
  summarize(average=mean(BPSysAve,na.rm=TRUE),
            standard_deviation = sd(BPSysAve,na.rm=TRUE))

#5)Repeat exercise 4 for males.
NHANES %>%
  filter(Gender == "male") %>% group_by(AgeDecade) %>%
  summarize(average=mean(BPSysAve,na.rm=TRUE),
            standard_deviation = sd(BPSysAve,na.rm=TRUE))


#6)We can actually combine both summaries for exercises 
#4 and 5 into one line of code. This is because group_by
#permits us to group by more than one variable. Obtain 
#one big summary table using group_by(AgeDecade, Gender).


NHANES %>% group_by(AgeDecade, Gender) %>%
  summarize(average=mean(BPSysAve,na.rm=TRUE),
            standard_deviation = sd(BPSysAve,na.rm=TRUE))

#7)For males between the ages of 40-49, compare systolic
#blood pressure across race as reported in the Race1 variable.
#Order the resulting table from lowest to highest average 
#systolic blood pressure.

NHANES %>% filter(Gender == "male" & AgeDecade =="40-49") %>% group_by(Race1)

NHANES %>%
  filter(AgeDecade ==" 40-49" & Gender == "male")  %>%
  group_by(Race1) %>% arrange(desc(BPSysAve))


#(pg. 94) Exercises 5.15 #1-4

#1) Load the murders dataset. Which of the following is true?
data(murders)
class(murders)
#B. murders is in tidy format and is stored in a data frame.

#2) Use as_tibble to covert the murders data table into a 
#tibble and save it in an object called murders_tibble.

murders_tibble = as_tibble(murders)

#3)Use the group_by function to convert murders into a tibble
#that is grouped by region

murders %>% group_by(region) %>% head()

#4)Write tidyverse code that is equivalent to this code:
#exp(mean(log(murders$population))). Write it using the 
#pipe so that each function is called without arguments.
#Use the dot operator to access the population. Hint: 
#The code should start with murders %>%.
exp(mean(log(murders$population)))

murders %>% 
  pull(population) %>% 
  log %>%
  mean %>% 
  exp




