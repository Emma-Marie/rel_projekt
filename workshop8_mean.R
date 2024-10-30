# prelims
setwd("/Users/Emma-Marie/Documents/AU/ka_2.semester/rel_projekt/rel_projekt") # set working directory

## The Mean as Model
obs <- c(2, 5, 5, 6, 10, 10, 12, 18, 29, 29)
barplot(table(obs)) 
count <- c(1, 2, NA, 1, 2, NA, 1, 1, 2, NA)
d <- data.frame(cbind(obs, count)) # the occurrence of each observation are counted and put into a df
n <- length(d$obs) # occurrences are summed --> the sum is the number of obs.
d$cntdn <- count/n # count is divided by number of obs (which is 10) --> gives the weights of each obs
d$timesobs <- d$cntdn*d$obs # each obs are multiplied by its weight
d$dev <- d$obs - mean(d$obs) # the error of each obs are calculated by: obs - mean
d$dev.sq <- d$dev^2 # take the square root of each error value from above to remove negative numbers
d # table with all the values from above

SS <- sum(d$dev.sq)
variance <- SS/(n-1)
SD <- sqrt(variance)

##############################################
### Figure 5.1: Accounting for deviance from the mean

par(mar = c(1, 4, 1, 1)) # set margins for plot
plot(obs, ylim = c(0, 30),
     ylab = "value", xlab = NA, axes = F, frame.plot = T, pch = 16) # plot the obs from above
Axis(side = 2, labels = T) # add values to y-axis
abline(h = mean(obs), lty = 2) # add horizontal line in plot marking the mean value
arrows(1, 2, 1, mean(obs), length = 0) # x0, y0, x1, y1 # add vertical line from obs to the mean line
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
sd(d$obs) # standard deviation again (just another approach)
