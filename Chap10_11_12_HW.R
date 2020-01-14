#Chapter 10 Homework
#Kyle Schneider
#10/2/2019
library(dslabs)
library(tidyverse)
library(gapminder)




#Create a plot comparing life expectancy for the United Kingdom
#and Chile over time (years included in the survey). Make these
#lines and include labels (while also excluding the legend). 

countries = c("United Kingdom", "Chile")
labels = data.frame(country = countries, x=c(1970, 1965), y=c(70, 57))
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year, life_expectancy, col = country)) +
  geom_line()+
  geom_text(data = labels, aes(x,y,label=country), size = 3)+
  theme(legend.position = "none")



#Create the plot shown in 10.8.2 (represents data from year =
#2010). You will have to use trans=“logit”. Don’t worry about
#getting country labels. 
gapminder <- gapminder %>% 
  mutate(group = case_when(
    region %in% c("Western Europe", "Northern Europe","Southern Europe", 
                  "Northern America","Eastern Europe", "Australia and New Zealand") ~ "West",
    region %in% "Northern Africa" ~ "Northern Africa",
    region %in% c("Eastern Asia","Central Asia", "Western Asia", "South-Eastern Asia") ~ "East Asia",
    region == "Southern Asia"~ "Southern Asia",
    region %in% c("Central America", "South America", "Caribbean") ~ "Latin America",
    continent == "Africa" & region != "Northern Africa" ~ "Sub-Saharan Africa",
    region %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacific Islands"))

table(gapminder$region, gapminder$group == "NA")

gapminder %>% filter(year == 2010 & !is.na(dollars_per_day)) %>%
  ggplot(aes(dollars_per_day, 1-infant_mortality/1000, col = group)) +
  geom_point()+
  scale_x_continuous(trans = "log2")+
  scale_y_continuous(trans = "logit")





#2. Create this plot: also shown at the end of
#section 11.6.3

gapminder %>% mutate(population_in_mil = population/1000000)

p <- gapminder %>% filter(year == 2015) %>%
  ggplot(aes(group,dollars_per_day)) +
  geom_boxplot()

gapminder %>% filter(year == 2015) %>% mutate(population_in_millions = population/1000000)%>%
  ggplot(aes(continent, population_in_millions))+
  geom_boxplot()+
  geom_point()+
  geom_jitter(width = 0.1, alpha = 0.5)+
  scale_y_continuous(trans = "log2")

  

#3. Recreate the slope chart presented in section
west <- c("Western Europe","Northern Europe","Southern Europe",
          "Northern America","Australia and New Zealand")

dat <- gapminder %>% 
  filter(year%in% c(2010, 2015) & region %in% west & 
           !is.na(life_expectancy) & population > 10^7) 

dat %>%
  mutate(location = ifelse(year == 2010, 1, 2), 
         location = ifelse(year == 2015 & 
                             country %in% c("United Kingdom","Portugal"),
                           location+0.33, location),
         hjust = ifelse(year == 2010, 1, 0)) %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(year, life_expectancy, group = country)) +
  geom_line(aes(color = country), show.legend = FALSE) +
  geom_text(aes(x = location, label = country, hjust = hjust), 
            show.legend = FALSE) +
  xlab("") + 
  ylab("Life Expectancy")
#11.8.1. What does the following code do?
location = ifelse(year == 2015 & country %in%
  c("United Kingdom", "Portugal"), location +0.22, location)

# The code moves the label to the right more for United Kingdom and Protugal so they dont cover
#Greece or Germany

#4. Rd section 11.14 Case study: impact of
#vaccines on battling infectious diseases.
#Complete 11.15 #1 & #4
data(us_contagious_diseases)
#1)Reproduce the image plot we previously made but for smallpox. For this plot, do not
#include years in which cases were not reported in 10 or more weeks.
the_disease <- "Smallpox"
dat <- us_contagious_diseases %>%
  filter(!state%in%c("Hawaii","Alaska") & disease == the_disease & !year %in% weeks_reporting >=10) %>%
  mutate(rate = count / population * 10000 * 52 / weeks_reporting) %>% 
  mutate(state = reorder(state, rate))

dat %>% ggplot(aes(year, state, fill = rate)) +
  geom_tile(color = "grey50") +
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds"), trans = "sqrt") +
  geom_vline(xintercept=1963, col = "blue") +
  theme_minimal() +  theme(panel.grid = element_blank()) +
  ggtitle(the_disease) + 
  ylab("") + 
  xlab("")


#4)Now do the same for the rates for the US. Hint: compute the US rate by using
#summarize: the total divided by total population.

avg <- us_contagious_diseases %>%
  filter(disease==the_disease) %>% group_by(year) %>%
  summarize(us_rate = sum(count, na.rm=TRUE)/sum(population, na.rm=TRUE)*10000)

dat %>% 
  filter(!is.na(rate)) %>%
  ggplot() +
  geom_line(aes(year, rate, group = state),  color = "grey50", 
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate),  data = avg, size = 1, color = "black") +
  scale_y_continuous(trans = "sqrt", breaks = c(5, 25, 125, 300)) + 
  ggtitle("Cases per 10,000 by state") + 
  xlab("") + 
  ylab("") +
  geom_text(data = data.frame(x = 1955, y = 50), 
            mapping = aes(x, y, label="US average"), 
            color="black") + 
  geom_vline(xintercept=1963, col = "blue")

#5. Section 12.6 Exercises 1-7
library(HistData)
data(Galton)
x <- Galton$child

#1)Compute the average and median of these data.
mean(x)

#2)Compute the median and median absolute deviation of these data.
median(x)
mad(x)

#3)Now suppose Galton made a mistake when entering the first value
#and forgot to use the decimal point. You can imitate this error by typing:

x_with_error <- x
x_with_error[1] <- x_with_error[1]*10
#How many inches does the average grow after this mistake?
mean(x_with_error)
# it makes about an inche difference


#4)How many inches does the SD grow after this mistake?
sd(x)
sd(x_with_error)
# the mistake adds 21 extra inches

#5)How many inches does the median grow after this mistake?

median(x)
median(x_with_error)

# the median didnt change with the mistake

#6)How many inches does the MAD grow after this mistake?

mad(x)
mad(x_with_error)

# the mad doesnt change the with the mistake

#How could you use exploratory data analysis to detect that an error was made?
#C. A boxplot, histogram, or qq-plot would reveal a clear outlier.


