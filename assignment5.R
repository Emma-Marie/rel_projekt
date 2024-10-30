#########################################################
################### ASSIGNMENT 5 #######################
#########################################################
### Describe your sample and sampling strategy like a normal empirical study would (see readings from earlier in the 
### semester). Then, describe the dependent variable (in most cases, religiosity) and focal predictor of your study. 
### Use R for the quantitative and graphical aspects of these descriptions.

# prelims
setwd("/Users/Emma-Marie/Documents/AU/ka_2.semester/rel_projekt/rel_projekt") # set working directory

library("psych")
library("devtools")
library(AnthroTools)
library(readxl) 

# load raw data
d <- read.table("data/RawProjektData.txt")

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

### investigate min and max number of participants each student have found
name_counts <- table(d$STUDENT) # count occurrence of each student name
print(name_counts)
total_sum <- sum(name_counts) # ass all occurrences together
print(total_sum)

### Investigate AGE - the independent variable ###
statinfo(d$ALDER)

### Investigate RELIGIOUSITY - the dependent variable ###

# function which calculates the Cronbach's alpha
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

# Calculate Cronbach's alpha on religiosity questions
str(d, list.len = ncol(d)) # line necessary?
values <- c(69:133)
labs <- paste0("REL", values) # create object of variable names to capture the religiosity questions
relsub <- d[labs] # subtract the religiosity data
relsubs <- relsub[complete.cases(relsub),] # delete missing values
alpha.fun(relsubs, 0.95) # run our function calculating Cronbach's alpha 
alpha(relsub) # check reliability if items are dropped
alpha(relsub, check.keys = T) # reverse code items with negative loadings, so 2 becomes -2 and 1 becomes -1 (right?)

# investigating the distribution of religiosity scores on selected data
d$relig <- d$REL76 + d$REL100 + d$REL133 # example --> add the scores of the scale questions, you need for your project
par(mar = c(3, 4, 2, 1)) # set margins for plot
hist(d$relig, main  ="Distribution of religiousity", xlab = "Religiosity score") # plot distribution


### other descriptions of sample and variables ###

d.r <- d # create new object containing data in order to be able to clean data

## Education 
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
barplot(df_oprel$Count, names.arg = df_oprel$upbringrel,
        main = "Religios upbringing",
        xlab = "religios upbringing", ylab = "number",
        col = c("coral", "coral2", "coral3"),
        ylim = c(0, 160))

## Years in big city 10,000+ citizens --> relevant?
d.r$BY <- as.numeric(d.r$BY) # turn numeric
statinfo(d.r$BY) # use function to describe

## Income

result_list <- list() # empty list for results
for (x in unique(d.r$INDKOM)) {
  # check that x isn't NA
  if (!is.na(x)) {
    # calculate sum of unique values
    sum_var <- sum(d$INDKOM == x, na.rm = TRUE)
    # save in list
    result_list[[as.character(x)]] <- sum_var
  }
}

result_df <- data.frame(Income = names(result_list), Count = unlist(result_list)) # data frame with results
print(result_df)
  
# Plot
dist_plot <- barplot(result_df$Count, names.arg = result_df$Income,
                      main = "Distribution INDKOM",
                      xlab = "Income", ylab = "Number",
                      col = c("coral", "coral2", "coral3"),
                      ylim = c(0, max(result_df$Count) + 10))  # Juster y-aksen dynamisk

## Gender

female <- sum(d.r$KON == 0)
male <- sum(d$KON == 1)
other <- sum(d$KON == 3)
df_kon <- data.frame(Kon = c("Female", "Male", "Other"),
                     Count = c(female, male, other)) # create data frame
print(df_kon)

## Mental health
# Calculate Cronbach's alpha on mental health questions
str(d.r, list.len = ncol(d.r)) # line necessary?
labs_ment <- c("BEKYMFYS", "BEKYMMENT", "PLADSFREM", "HELB", "LSTAND", "BESVAER", "RASTLOS", "ENSOM", "ALSYG", "POSITIV", "DEPRI") # create object of variable names to capture the mental health questions
mentsub <- d.r[labs_ment] # subtract the religiosity data
mentsubs <- mentsub[complete.cases(mentsub),] # delete missing values
alpha.fun(mentsubs, 0.95) # run our function calculating Cronbach's alpha 
alpha(mentsub) # check reliability if items are dropped 

# plot mental health scores
par(mar = c(3, 4, 2, 1)) # set margins for plot
d.r$mental <- d.r$BEKYMFYS + d.r$BEKYMMENT + d.r$PLADSFREM + d.r$HELB + d.r$LSTAND + d.r$BESVAER + d.r$RASTLOS + d.r$ENSOM + d.r$ALSYG + d.r$POSITIV + d.r$DEPRI
#d.r$mental <- d.r$BEKYMMENT + d.r$BESVAER + d.r$RASTLOS + d.r$ENSOM + d.r$ALSYG + d.r$POSITIV + d.r$DEPRI
hist(d.r$mental, main  ="Distribution of mental health", xlab = "Mental health score", col = "pink") # plot distribution

# more descriptions on mental health
statinfo(d.r$mental)

# Save cleaned data in data frame as .txt
write.table(d.r, file = paste0("data/dat_survey_cleaned.txt"), sep = "\t", row.names = TRUE, col.names = NA)

