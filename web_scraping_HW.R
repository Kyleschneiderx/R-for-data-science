#Webscraping Homework
#DATA 320
#Kyle Schneider
#11/14/2019


#The website is:  https://www.imdb.com/search/title/?count=100&&release_date=2018,2018&&title_type=feature
library(rvest)
library(tidyverse)
url <- "https://www.imdb.com/search/title/?count=100&&release_date=2018,2018&&title_type=feature"
webpage <- read_html(url)
rank_data_html <- html_nodes(webpage, '.text-primary')

#1.Extract the data from the top 100 movies of 2018 to include the rank, title,
#runtime, genre, rating, gross Earnings (per millions)

rank_data <- html_text(rank_data_html)
rank_data <- as.numeric(rank_data)
head(rank_data)

title_data_html <- html_nodes(webpage,'.lister-item-header a')
title_data <- html_text(title_data_html)
head(title_data)


runtime_html <- html_nodes(webpage, '.runtime')
runtime <- html_text(runtime_html)
head(runtime)
runtime <- as.numeric(gsub(" min", "", runtime))

genre_data_html <- html_nodes(webpage, ".genre")
genre_data <- html_text(genre_data_html)
head(genre_data)


genre_data<-gsub("\n","",genre_data)
genre_data<-gsub(" ","",genre_data)
genre_data<-gsub(",.*","",genre_data)
genre_data<-as.factor(genre_data)
head(genre_data)

metascore_data_html <- html_nodes(webpage,'.metascore')
metascore_data <- html_text(metascore_data_html)
head(metascore_data)

metascore_data<-as.numeric(gsub(" ","",metascore_data))

length(metascore_data)

for(i in c(40,68,74,93,95)){
  a <- metascore_data[1:(i-1)]
  b <- metascore_data[i:length(metascore_data)]
  metascore_data <- append(a, NA)
  metascore_data <- append(metascore_data, b)
} 

gross_data_html <- html_nodes(webpage, '.ghost~ .text-muted+ span')
gross_data <- html_text(gross_data_html)
gross_data <- gsub("[$]","",gross_data)
gross_data<- as.numeric(gsub("M", "", gross_data))

for(i in c(44,68,69,70,74,80,81,82,89,94,95,98)){
  a <- gross_data[1:(i-1)]
  b <- gross_data[i:length(gross_data)]
  gross_data <- append(a, NA)
  gross_data <- append(gross_data, b)
} 

head(gross_data)
gross_data

movies_df <- data.frame(Rank=rank_data, Title = title_data,
                        Runtime = runtime, Genre = genre_data,
                        Metascore = metascore_data, Gross = gross_data) 
movies_df


#2.Plot Gross Earnings by Runtime with different colors for different genres

movies_df %>% ggplot(aes(Gross, Runtime, color = Genre)) + geom_point()


#3.Plot gross earnings by rating with different colors for different genres.  
movies_df %>% ggplot(aes(Gross, Metascore, color = Genre)) + geom_point()


#Is there a relationship between “good movies” and how much they earn?
# no there is not relationship

