#### Workshop 2 ####

## Prelims
setwd("/Users/Emma-Marie/Documents/AU/ka_2.semester/rel_projekt/rel_projekt") # set working directory
library("psych") # open package

x<-1:5
y<- 6:10
z<- data.frame(x,y)
str(z)
z$x # check values in the x column
View(z)
View(t(z)) # transposes the data frame z (but we normally don't really use this)

## Indexing - looking at different coordinates in the data frame (row, column)
z[1,1]
z[1,2]
z[4,2]
z[4,1]
z[1,] #show the whole row 1 (across all columns)
z[,1]# show the whole column 1 (across all rows)

### Working with data

## Intuitive basic operations
plot(z, pch= 8, col = "blue", xlab = "Column 1", ylab = "Column 2", main = "Test:-)")
plot(z, type = "l") # makes graph into a line ("l")

corxy<- cor.test(z$x, z$y) # check correlation of x and y
corxy # the correlation is 1 meaning when x increases with 1 y also increases with 1.
str(corxy) # shows the structure of the corxy object

# Basic operations
obs <- c(2, 5, 5, 6, 10, 10, 12, 18, 29, 29)
sum(obs)
sum(obs)/length(obs)
mean(obs)
sd(obs) # standard deviation - the standard amount each observation deviates from the average
obs -mean(obs)
(obs-mean(obs))^2

hist(obs, main = "Histogram :-)", col = "pink") # create histogram
hist(obs/length(obs), main = "Histogram!", col = "pink")

## Draw from distribution --> simulating data
n <- 100 # number of observations/the sample size
set.seed(666) # instructs R how to randomly sample the sample, so that other people using the scripts gets the same random sample.

normal1 <- rnorm(n, 5, 1) # 5 is the mean, 1 is the standard deviation
normal1
hist(normal1)
mean(normal1) # gives mean close to 5, as we wished for

binom1 <- rbinom(n, 1, 0.7) # 1 is the index of success (here binary 0 and 1), 0.7 is the likelihood of getting the success (here 70% change of getting 1)
table(binom1) # shows the distribution of 0s and 1s
table(binom1)/n

## Prepackaged data
data("mtcars")
str(mtcars)
hist(mtcars$cyl, xlab = "number of cylenders", ylab = "number of cars", main = "Destribution of number of cylinders")
