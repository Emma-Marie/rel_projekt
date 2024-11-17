### The Beauty of the Mean:  The Mean as Model
## Day 1 Recap
obs <- c(2, 5, 5, 6, 10, 10, 12, 18, 29, 29)
count <- c(1, 2, NA, 1, 2, NA, 1, 1, 2, NA)
d <- data.frame(cbind(obs, count)) # create data frame
n <- length(d$obs)
d$cntdn <- count/n # divide each obs. occurrence with total number of obs. -> gets percentage (in decimal) -> the weights
d$timesobs <- d$cntdn*d$obs # each obs. multiplied by its weight
d$dev <- d$obs - mean(d$obs) # the deviation of each observation from the mean -> the error (sum of all errors should be 0).
d$dev.sq <- d$dev^2 # distribution of deviance. The sum of these is the SS value. 
d # show complete data frame


##############################################
### Figure 5.1: Accounting for deviance from the mean

par(mar = c(1, 4, 1, 1)) # set parameters for plot
plot(obs, ylim = c(0, 30),
     ylab = "value", xlab = NA, axes = F, frame.plot = T, pch = 16) # plot obs.
Axis(side = 2, labels = T)
abline(h = mean(obs), lty = 2) # draw the mean as a dashed line
# Connect each obs. with a line to the mean line. 
arrows(1, 2, 1, mean(obs), length = 0) # x0, y0, x1, y1
arrows(2, 5, 2, mean(obs), length = 0) # x0, y0, x1, y1
arrows(3, 5, 3, mean(obs), length = 0) # x0, y0, x1, y1
arrows(4, 6, 4, mean(obs), length = 0) # x0, y0, x1, y1
arrows(5, 10, 5, mean(obs), length = 0) # x0, y0, x1, y1
arrows(6, 10, 6, mean(obs), length = 0) # x0, y0, x1, y1
arrows(7, 12, 7, mean(obs), length = 0) # x0, y0, x1, y1
arrows(8, 18, 8, mean(obs), length = 0) # x0, y0, x1, y1
arrows(9, 29, 9, mean(obs), length = 0) # x0, y0, x1, y1
arrows(10, 29, 10, mean(obs), length = 0) # x0, y0, x1, y1

## standard deviation
(variance <- (sum(d$dev.sq))/(n - 1)) # var with Bessel-Gauss' correction
(sqrt(variance)) # standard deviation
sd(d$obs) # another method to calculate SD

### Day 2
## z-scores (z = (obs - mean)/sd)
d$z <- (d$obs - mean(d$obs))/sd(d$obs) # (observation - mean)/sd(variable)

(12.6 - mean(d$obs))/sd(d$obs) #z-score of mean = 0
sd(d$z) # SD of z-score = 1

##############################################
### Figure 5.2: z-distribution curve

par(mar = c(3, 3.5, 1, 1)) # set plot parameters
curve(dnorm(x, 0, 1), xlim = c(-4, 4), # set mean = 0 and SD = 1 --> we get the normal destribution
      main = NULL, xlab = NA, ylab = NA)
cord.x <- c(-1.96, seq(-1.96, 1.96, 0.01), 1.96) # +/-1.96 on a z-distribution
cord.y <- c(0, dnorm(seq(-1.96, 1.96, 0.01)), 0) 
polygon(cord.x, cord.y, col = 'coral') # color the 95% confidence interval (the area between the z- values -1.96 and 1.96)
mtext(expression(italic(z)-score), side = 1, padj = 4) # label x-axis
mtext("Probability", side = 2, padj = -4) # label y-axis
lines(c(0, 0), c(0, dnorm(0, 0, 1)), lty = 5, lwd = 2) # the mean!

# probability ("sandsynlighed")
lines(c(-1.09, -1.09), c(0, dnorm(-1.09, 0, 1)), lty = 3, lwd = 2) # draw a line at the z-value corresponding to the observation 2.
# --> the line shows that there is 14% chance of getting an obs of 2 or below. 
lines(c(-0.06, -0.06), c(0, dnorm(-0.06, 0, 1)), lty = 3, lwd = 2) # draw a line at the z-value corresponding to the observation 12.
# --> the line shows that there is 46% chance of getting an obs of 12 or below.
lines(c(1.69, 1.69), c(0, dnorm(1.69, 0, 1)), lty = 3, lwd = 2) # draw a line at the z-value corresponding to the observation 29.
# --> the line shows that there is 96% chance of getting an obs of 29 or below.

# shaded area is 95% of the curve which is +/- 1.96 standard
# errors above and below the mean
pnorm(1.96, mean = 0, sd = 1) -
pnorm(-1.96, mean = 0, sd = 1)

## Standard error and confidence intervals
(se <- sd(d$obs)/sqrt(n)) # standard error

(upper <- mean(d$obs) + se * 1.96) # upper bound of CI
(lower <- mean(d$obs) - se * 1.96) # lower bound of CI

# see what happens to SE as n goes up!

# it is written line this: M = 12.6, 95% CI = [6.57, 18.63] 

## Confidence interval of the mean function

CI.mean <- function(variable){
  mean <- mean(variable, na.rm = T)
  stdev <- sd(variable, na.rm = T)
  n <- length(which(!is.na(variable)))
  se <- stdev/sqrt(n)
  lower <- mean - se * 1.96
  upper <- mean + se * 1.96
  df1 <- data.frame(mean, 
                    round(stdev, digits = 2), 
                    round(lower, digits = 2), 
                    round(upper, digits = 2))
  colnames(df1) <- c("mean", "sd", "lower", "upper")
  rownames(df1) <- c("normal") # calculated assuming normality
  return(df1)
}

mean(d$obs)
sd(d$obs)
CI.mean(d$obs)

CI.mean(rnorm(10, mean(d$obs), sd(d$obs))) # run a bunch of times!
CI.mean(rnorm(100, mean(d$obs), sd(d$obs))) # compare the 95% confidence intervals
CI.mean(rnorm(1000, mean(d$obs), sd(d$obs))) # compare the 95% confidence intervals
CI.mean(rnorm(10000, mean(d$obs), sd(d$obs))) # compare the 95% confidence intervals
CI.mean(rnorm(100000, mean(d$obs), sd(d$obs))) # compare the 95% confidence intervals
CI.mean(rnorm(1000000, mean(d$obs), sd(d$obs))) # compare the 95% confidence intervals

# 95% of any repeated observations will yield intervals
# that contain the population or "true" mean. So, if we replicated this study
# 100 times, 95 of the intervals will contain the population mean. The wider
# the interval then, the less precise the mean is as an estimate--
# or model of the true mean. Very narrow intervals represent very
# precise estimates of the mean. 

simdat <- rnorm(1000, mean = 12.6, sd = 9.73) # simulate data with specific mean and SD
CI.mean(simdat) # compare the confidence intervals

simdat2 <- rnorm(1000, mean = 12.6, sd = 1) # SD!
CI.mean(simdat2) # compare the confidence intervals --> a smaller SD gives a more narrow interval 

## Graph it!

par(mfrow = c(2, 1)) # splits the graph panels
 # simdat
hist(simdat, xlab = NA, ylab = NA, prob = TRUE,
     main = "Simulated Data I (M = 12.6, SD = 9.73)", 
     xlim = c(-20, 50), breaks = 15)
curve(dnorm(x, 12.6, 9.73), xlim = c(-20, 50), 
      main = NULL, add = TRUE) 
# simdat2
hist(simdat2, xlab = NA, ylab = NA, prob = TRUE,
     main = "Simulated Data II (M = 12.6, SD = 1)", 
     xlim = c(-20, 50), breaks = 15)
curve(dnorm(x, 12.6, 1), xlim = c(-20, 50), 
      main = NULL, add = TRUE) 

### Real data!
setwd("/Users/Emma-Marie/Documents/AU/ka_2.semester/rel_projekt/rel_projekt")
d <- read.table("data/RawProjektData.txt") # for next session!

CI.mean(d$ALDER) # our data: calculate mean, sd and interval at a confidence interval of 95%. 
# my own question: what can we interpret, when sd is 17.21 (relatively high, right?) but interval is narrow [34.07, 38.6]???

simage <- rnorm(1000, mean(d$ALDER), sd(d$ALDER)) # simulate data with same meand and SD as our data
agez <- (d$ALDER-mean(d$ALDER))/sd(d$ALDER) # each obs. minus the mean divided by the SD (17,21) --> z-values for ages in our sample

par(mfrow = c(3, 1), mar = c(4, 4, 0, 0))
hist(simage, xlim = c(-20, 100), main = NULL,
     xlab = "simulated") # plot simlated data = the normal distribution of age data with same meand and sd as our data
hist(d$ALDER, xlim = c(-20, 100), main = NULL,
     xlab = "sampled") # plot our age observations
hist(agez, xlim = c(-2, 4), main = NULL,
     xlab = "zscored") # plot z- values of out data
