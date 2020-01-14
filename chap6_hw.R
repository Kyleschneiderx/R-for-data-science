#DATA-320
#Homework Chapter 6
#Kyle Schneider
#9/19/2019
library(tidyverse)
library(readr)
#Exercise 6.3
#1)Use the read_csv function to read each of the files
#that the following code saves in the files object:
path <- system.file("extdata", package = "dslabs")
files <- list.files(path)
files
fullpath <- file.path(path, "olive.csv")
read_csv(fullpath)

#2)Note that the last one, the olive file, gives us a warning.
#This is because the first line of the file is missing the header
#for the first column.
#Read the help file for read_csv to figure out how to read in the
#file without reading this header. If you skip the header, you 
#should not get this warning. Save the result to an object called dat.

#sorry at the moment im very confused. Help() has been the least helpful thing
# ive ever used. I do apologize for the awful work ive been doing on this assignment.
# i have no idea why im struggling with this so much.
help(read_csv)
dat = read_csv(fullpath,skip =1)
               
#3)Use the readLines function to read in just the first line (we later learn
#how to extract values from the output).
names(dat)
read_lines(fullpath, n_max = 3)

# Bring in my own data section

url = "http://www.ntsb.gov/investigations/data/Documents/datafiles/table4_2014.csv"
dat = read_csv(url)
head(dat)
class(dat$X6)
a=dat$X6
b =as.numeric(a)
class(b)
mean(b)
sd(b)

# Im super sorry about this work im just frustrated from the first three and very tired.
# sorry again


