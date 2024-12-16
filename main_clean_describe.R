###############################################
#############Cleaning and describing ##########
###############################################

# prelims
setwd("/Users/Emma-Marie/Documents/AU/ka_2.semester/rel_projekt/rel_projekt") # set working directory

library("psych")
library("devtools")
library(AnthroTools)

# load raw data
d <- read.table("data/RawProjektData.txt")
d.r <- d # create new object containing data in order to be able to clean data

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
summary(d$ALDER) # median at age 28

pdf(file = "fig_output/age_distribution.pdf", width = 4, height = 4)
barplot(table(d$ALDER), main = "Age distribution", xlab = "Age", ylim = c(0,20), col = "cadetblue") # plot age
dev.off()

####### investigate sample generally

### Education 
str(d.r$UDDANN) # values are characters, but needs to me numerical
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

## Religious upbringing
no <- sum(d.r$OPREL == 0, na.rm = TRUE)
yes <- sum(d.r$OPREL == 1, na.rm = TRUE)
df_oprel <- data.frame(upbringrel = c("no", "yes"),
                       Count = c(no, yes)) # create data frame
print(df_oprel)

## Years in big city 10,000+ citizens
d.r$BY <- as.numeric(d.r$BY) # turn numeric
statinfo(d.r$BY) # use function to describe

## Gender
female <- sum(d.r$KON == 0)
male <- sum(d$KON == 1)
other <- sum(d$KON == 3)
df_kon <- data.frame(Kon = c("Female", "Male", "Other"),
                     Count = c(female, male, other)) # create data frame
print(df_kon)


### Investigate RELIGIOUSITY - the dependent variable ###

# function to calculate the Cronbach's alpha
alpha.fun <- function(data, interval){
  k <- ncol(data)
  n <- nrow(data)
  p <- 1 - interval
  cormat <- cor(data)[lower.tri(cor(data))] # takes the lower triangle of R scores in the correlation matrix, to avoid having the same scores twice
  rhat <- mean(cormat)
  alpha <- (k*rhat)/(1 + (k - 1)*rhat) # the correlation score is weighted based on the size of the data
  df1 <- n - 1
  df2 <- df1*(k - 1)
  ffl <- qf(p/2, df1 = df1, df2 = df2)
  ffu <- qf(1 - (p/2), df1 = df1, df2 = df2)
  upper <- 1 - (1 - alpha)*ffl # lower value in confidence interval
  lower <- 1 - (1 - alpha)*ffu # upper value in confidence interval
  return(data.frame(alpha = alpha, lower = lower, upper = upper)) # return df with Cronbach's alpha and confidence interval
}

# Cronsbach's alpha on religiosity questions
rel_labs <- c("REL72", "REL85", "REL94", "REL106", "REL108", "REL113") # our chosen religiosity questions
relscor <- d[rel_labs]
alpha(relscor)

d.r$RELSCOR <- d$REL72 + d$REL85 + d$REL94 + d$REL106 + d$REL108 + d$REL113 # create new column with REL-scores

# plot histogram
pdf(file = "fig_output/rel_distribution.pdf", width = 4, height = 4)
hist(d.r$RELSCOR, xlab = "Religiosity score", ylab = "Number of participants", main = "Religiosity distribution", xlim = c(-15,15), ylim = c(0,80), col = "cadetblue") # plot rel scores
dev.off()

##### other descriptions of sample and variables #####

## Mental health
table(d.r$HELB) # check values
d.r$HELB[which(d.r$HELB == "0.2")] <- 0 # replaces invalid value

labs_ment <- c("BEKYMFYS", "BEKYMMENT", "LSTAND", "ALSYG", "HELB") # create object of variable names to capture the mental health questions
mentsub <- d.r[labs_ment] # subtract the religiosity data
mentsubs <- mentsub[complete.cases(mentsub),] # delete missing values

# Calculate Cronbach's alpha on mental health questions
alpha.fun(mentsubs, 0.95) # run our function calculating Cronbach's alpha 
alpha(mentsub) # check reliability if items are dropped 

# Investigate connection between health and religiosity

plot(jitter(HELSCOR, factor = 1) ~ RELSCOR, data = d.r,
     xlab = "relscore", ylab = "helscore",
     pch = 21, col = "darkblue")

labs_ment <- c("BEKYMFYS", "BEKYMMENT", "LSTAND", "ALSYG", "HELB") # create object of variable names to capture the mental health questions
mentsub <- d.r[labs_ment] # subtract the religiosity data
mentsubs <- mentsub[complete.cases(mentsub),] # delete missing values

## Income

labs_eco <- c("BEKYMJOB", "BEKYMLIV") # create object of variable names to capture the income questions
ecosub <- d.r[labs_eco] # subtract the religiosity data
ecosubs <- ecosub[complete.cases(ecosub),] # delete missing values

# Cronbach's alpha on income questions
alpha.fun(ecosubs, 0.95) # run our function calculating Cronbach's alpha 
alpha(ecosub) # check reliability if items are dropped 

# plot economy
par(mar = c(4, 4, 2, 1)) # set margins for plot
d.r$ECO <- d.r$BEKYMJOB + d.r$BEKYMLIV # create column of income scores
hist(d.r$ECO, main  ="Distribution of worry about economy", xlab = "Ecomony worry", ylab = "Number of participants", col = "lightskyblue2", ylim = c(0,50)) # plot distribution

# Investigate connection between income (worry) and religiosity
par(mar = c(4, 4, 2, 1)) # set margins for plot
plot(jitter(ECO, factor = 1) ~ RELSCOR, data = d.r,
     xlab = "relscore", ylab = "income worry",
     pch = 21, col = "darkblue")

########################################################
##### For the free list script ####
# create new age variable to differentiate between "young" and "older"
d.r$UNG[d.r$ALDER <= 28] <- 0
d.r$UNG[d.r$ALDER > 28] <- 1

########################################################

# Save cleaned data in data frame as .txt
write.table(d.r, file = paste0("data/dat_survey_cleaned.txt"), sep = "\t", row.names = TRUE, col.names = NA)
