#Chapter 22, 23, 24 Homework
#DATA 320
#Kyle Schneider
#11/8/2019

library(tidyverse)
library(dplyr)

#1. EXERCISES 22.5 #1-4 (OR 21.5 #1-4 IN GITHUB)
#1) Run the following command to define the co2_wide object:
co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
setNames(1:12) %>%
  mutate(year = as.character(1959:1997))
#Use the gather function to wrangle this into a tidy dataset. Call
#the column with the CO2 measurements co2 and call the month column 
#month. Call the resulting object co2_tidy.
head(co2_wide)
view(co2_wide)
co2_tidy <- co2_wide %>% gather(month, co2, -year)
view(co2_tidy)
#2) Plot CO2 versus month with a different curve for each year using this code:

co2_tidy %>% ggplot(aes(month, co2, color = year)) + geom_line()

#If the expected plot is not made, it is probably because co2_tidy$month is not numeric:

class(co2_tidy$month)

#Rewrite the call to gather using an argument that assures the month column will be numeric.
#Then make the plot.

co2_tidy <- co2_wide %>% gather(month, co2, -year, convert = TRUE)
co2_tidy %>% ggplot(aes(month, co2, color = year)) + geom_line()

#3) What do we learn from this plot?

# b.CO2 measures are higher in the summer and the yearly average increased from 1959 to 1997.

#4)Now load the admissions data set, which contains admission information for men and women
#across six majors and keep only the admitted percentage column:

load(admissions)
dat <- admissions %>% select(-applicants)
dat

#If we think of an observation as a major, and that each observation has two variables
#(men admitted percentage and women admitted percentage) then this is not tidy. Use the
#spread function to wrangle into tidy shape: one row for each major.

new_wide_dat <- dat %>%
  spread(gender, admitted)
view(new_wide_dat)

#2. EXERCISES 23.4 #1-2 (OR 22.4 #1-2 IN GITHUB)

#1)1. Install and load the Lahman library. This database includes data related to baseball
#teams. It includes summary statistics about how the players performed on offense and defense
#for several years. It also includes personal information about the players.

#The "Batting" data frame contains the offensive statistics for all players for many years.
#You can see, for example, the top 10 hitters by running this code:

library(Lahman)

top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%
  slice(1:10)

top %>% as_tibble()
view(top)
top

#But who are these players? We see an ID, but not the names. The player names are in this table

Master %>% as_tibble()
view(Master)

#We can see column names nameFirst and nameLast. Use the left_join function to create a table
#of the top home run hitters. The table should have playerID, first name, last name, and number
#of home runs (HR). Rewrite the object top with this new table.

top_homerun_hitters <- left_join(top, Master, by="playerID")  %>% select(playerID, nameFirst, nameLast, HR )
top_homerun_hitters

#2) Now use the Salaries data frame to add each player’s salary to the table you created in
#exercise 1. Note that salaries are different every year so make sure to filter for the year
#2016, then use right_join. This time show first name, last name, team, HR, and salary.
Salaries

top_homerun_hitters <- Salaries %>% filter(yearID == 2016) %>%  right_join(top_homerun_hitters ,Salaries, by = "playerID") %>%
  select(playerID, nameFirst, nameLast, teamID, HR, salary )
top_homerun_hitters

#3. EXERCISES 24.5 #1-2 (OR 23.5 #1-2 IN GITHUB)

#1)1. Visit the following web page: https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm

#Notice there are several tables. Say we are interested in comparing the payrolls of teams across
#the years. The next few exercises take us through the steps needed to do this.

#Start by applying what you learned to read in the website into an object called h.#

library(rvest)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)
class(h)

#2) Note that, although not very useful, we can actually see the content of the page by typing:

html_text(h)

#The next step is to extract the tables. For this, we can use the html_nodes function. 
#We learned that tables in html are associated with the table node. Use the html_nodes
#function and the table node to extract the first table. Store it in an object nodes.

tab <- h %>% html_nodes("table") 
tab <- tab[[1]] %>% html_table


 