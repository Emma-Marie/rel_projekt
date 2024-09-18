### Workshop 1
## ~ ("tilde") is made like this: alt + ^

# Prelims
setwd("/Users/Emma-Marie/Documents/AU/ka_2.semester/rel_projekt/rel_projekt") # set working directory
#install.packages("psych") # load package - only needed the first time installing this package
library("psych") # open package

## Get to work

x<-"What's up?" # create an object
y<-2

str(x) # tells us the content of the variable e.g. that x is characters (chr) and y is numeric (num). 
str(y) 

x<-c(1, 2, 3, 4, 5) # concatenate function - OBS: here the five numbers is made into ONE unit
str(x) # x is "num"

x<-1:5
str(x) # x is "int"
class(x) # another way to show the class which is "integer"

# do stuff to the objects
x*2
t(x) # transpose - data set is turned by 90 degrees - we noq have one row and five columns of information [row,column]

y<-6:10
z<-c(x,y) # concatenate
z<-rbind(x,y) # bind as rows
z<-cbind(x,y) # bind as columns

z<-data.frame(x,y) # bind the columns into a data frame
z$x # $ = sub -> gives all the x values
z$y
