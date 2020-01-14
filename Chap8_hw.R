#Chapter 8 Homework
#DATA - 320
#Kyle Schneider
#9/19/2019

library(NHANES)
data(NHANES)

#Using the NHANES dataset, create a scatter geplot of age (x-axis) vs. BMI (y-axis).Make the
#size of the points = 0.5. Indicate women and men in different colors. Include a title, “BMI by
#Age for Americans”. Put a line on the graph for BMI = 30 (obesity level)

NHANES %>% ggplot(aes(Age, BMI)) +
  geom_point(aes(col=Gender), size = 0.5) +
  xlab("Age") +
  ylab("BMI") +
  ggtitle("BMI by Age for Americans") +
  geom_abline(intercept = 30, slope = 0, lty=2, color="darkgrey")


#Take the data that you downloaded for last class and create a chart of your data by
#population for US state. (If you don’t have population in your spreadsheet, add it using the
#murders data from 2010). Then, include a label for each state (you can use the abb from the
#murders dataset). Finally, add appropriate x-axis labels, y-axis labels, and title.

library(readr)
dat = read_csv("VGLI_Enrollment_by_State_as_of_103112.csv")

dat = dat[-c(52),]
dat = dat[-c(52),]

dat %>% ggplot(aes(MEMBERS, murders$population/10^6, label = murders$abb)) +
  geom_point(size= 0.5)+
  xlab("Thousand Veteran Insured") +
  ylab("Population in millions")+
  ggtitle("Veterans Insured per State")

  





