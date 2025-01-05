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

## Religious upbringing
no <- sum(d.r$OPREL == 0, na.rm = TRUE)
yes <- sum(d.r$OPREL == 1, na.rm = TRUE)
df_oprel <- data.frame(upbringrel = c("no", "yes"),
                       Count = c(no, yes)) # create data frame
print(df_oprel)

## Gender
female <- sum(d.r$KON == 0)
male <- sum(d$KON == 1)
other <- sum(d$KON == 3)
df_kon <- data.frame(Kon = c("Female", "Male", "Other"),
                     Count = c(female, male, other)) # create data frame
print(df_kon)


### Investigate RELIGIOUSITY (the dependent variable) ###

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
alpha(relscor) # Cronbachs alpha
relscor <- na.omit(relscor) # remove rows with NA
alpha.fun(relscor, 0.95) # get 95% CI for Cronbachs alpha

# create new column with religiosity scores
d.r$RELSCOR <- d$REL72 + d$REL85 + d$REL94 + d$REL106 + d$REL108 + d$REL113 

# plot histogram
pdf(file = "fig_output/rel_distribution.pdf", width = 4, height = 4)
hist(d.r$RELSCOR, xlab = "Religiosity score", ylab = "Number of participants", main = "Religiosity distribution", xlim = c(-15,15), ylim = c(0,80), col = "cadetblue") # plot rel scores
dev.off()

#######################################################
##### other descriptions of sample and variables #####

### Mental health
table(d.r$HELB) # check values
d.r$HELB[which(d.r$HELB == "0.2")] <- 0 # replaces invalid value

labs_ment <- c("BEKYMFYS", "BEKYMMENT", "DEPRI", "ALSYG", "HELB") # create object of variable names to capture the mental health questions
mentsub <- d.r[labs_ment] # subtract the health data
mentsubs <- mentsub[complete.cases(mentsub),] # delete missing values

# Calculate Cronbach's alpha on mental health questions
alpha.fun(mentsubs, 0.95) # run our function calculating Cronbach's alpha 
alpha(mentsub) # check reliability if items are dropped 

d.r$HELSCOR <- d.r$BEKYMFYS + d.r$BEKYMMENT + d.r$DEPRI + d.r$ALSYG + d.r$HELB # create new column with health insecurity scores

### Material insecurity
labs_mat <- c("BEKYMJOB", "BEKYMLIV", "LSTAND") # create object of variable names to capture material insecurity
matsub <- d.r[labs_mat] # subtract the material insecurity data
matsubs <- matsub[complete.cases(matsub),] # delete missing values

# Cronbach's alpha on material insecurity 
alpha.fun(matsubs, 0.95) # run our function calculating Cronbach's alpha 
alpha(matsub) # check reliability if items are dropped 

# plot material insecurity
par(mar = c(4, 4, 2, 1)) # set margins for plot
d.r$MAT <- d.r$BEKYMJOB + d.r$BEKYMLIV + d.r$LSTAND# create column of income scores
hist(d.r$MAT, main  ="Material insecurity of participants", xlab = "Material insecurity", ylab = "Number of participants", col = "lightskyblue2", ylim = c(0,50)) # plot distribution

########################################################
##### For the free list script ####
# create new age variable to differentiate between "young" and "older"
d.r$UNG[d.r$ALDER <= 28] <- 0
d.r$UNG[d.r$ALDER > 28] <- 1

########################################################

# Save cleaned data in data frame as .txt
write.table(d.r, file = paste0("data/dat_survey_cleaned.txt"), sep = "\t", row.names = TRUE, col.names = NA)
