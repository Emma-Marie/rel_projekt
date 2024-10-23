#########################################################
################### ASSIGNMENT 5 #######################
#########################################################
### Describe your sample and sampling strategy like a normal empirical study would (see readings from earlier in the 
### semester). Then, describe the dependent variable (in most cases, religiosity) and focal predictor of your study. 
### Use R for the quantitative and graphical aspects of these descriptions.

# prelims
setwd("/Users/Emma-Marie/Documents/AU/ka_2.semester/rel_projekt/rel_projekt") # set working directory

library("psych") # open package
library("devtools")
library(AnthroTools)

library(readxl) 

# load raw data
d <- read.table("data/RawProjektData.txt")

View(d)


# create function for information about observations with numeric values
statinfo <- function(x){
  numobs <- sum(table(x)) # number of inputs
  minobs <- min(x, na.rm = T) # minimum value
  meanobs <- round(mean(x, na.rm = T), 2) # mean values (two decimals)
  maxobs <- max(x, na.rm = T) # max value
  sdobs <- round(sd(x, na.rm = T), 2) # standard deviation (two decimals)
  df <- data.frame(n = numobs, min = minobs, 
                   M = meanobs, max = maxobs, SD = sdobs) # create data frame
  return(df)
}

### Investigate AGE - the independent variable ###
statinfo(d$ALDER)

### Investigate RELIGIOUSITY - the dependent variable ###

### other descriptions of sample ###

d.r <- d

## Education 
str(d$UDDANN) # values are characters, but needs to me numerical
unique(d.r$UDDANN) # look for the errors

# replace words etc. with correct numbers
d.r$UDDANN[which(d.r$UDDANN == "18 folkeskole/gymnasium/universitet")] <- 18
d.r$UDDANN[which(d.r$UDDANN == "12 aar mindst")] <- 12
d.r$UDDANN[which(d.r$UDDANN == "18,5")] <- 18.5
d.r$UDDANN[which(d.r$UDDANN == "16,5")] <- 16.5
d.r$UDDANN[which(d.r$UDDANN == "10+")] <- 10
d.r$UDDANN[which(d.r$UDDANN == "18-20")] <- 18

d.r$UDDANN <- as.numeric(d.r$UDDANN) # turn numeric

statinfo(d.r$UDDANN) # describe years of education --> relevant?

## Years in big city
d.r$BY <- as.numeric(d.r$BY) # turn numeric

statinfo(d.r$BY) # describe years living in city with more than XX citizens --> relevant?

## Gender

