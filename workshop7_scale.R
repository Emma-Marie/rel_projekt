### Scale Design
### 24-10-24

### Prelims
rm(list = ls()) # wipe out session
library(psych)

setwd("/Users/Emma-Marie/Documents/AU/ka_2.semester/rel_projekt/rel_projekt")
d <- read.table("data/RawProjektData.txt")

##############################################
### from book: R Code Box 2.5

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
  upper <- 1 - (1 - alpha)*ffl
  lower <- 1 - (1 - alpha)*ffu
  return(data.frame(alpha = alpha, lower = lower, upper = upper))
}

eat1 <- c(0, 1, 1, 1, 0, 1, 1, 1, 0, 0)
eat2 <- c(0, 0, 1, 1, 0, 1, 1, 1, 0, 0)
eat3 <- c(0, 1, 1, 1, 0, 0, 1, 1, 0, 0)
eat4 <- c(1, 1, 1, 1, 0, 1, 1, 1, 0, 0)
eat5 <- c(0, 0, 1, 1, 0, 1, 1, 1, 0, 0)

eating <- data.frame(eat1, eat2, eat3, eat4, eat5)

alpha.fun(eating, 0.95)

random1 <- rbinom(10, 1, 0.5)
random2 <- rbinom(10, 1, 0.5)
random3 <- rbinom(10, 1, 0.5)
random4 <- rbinom(10, 1, 0.5)
random5 <- rbinom(10, 1, 0.5)
dran <- data.frame(random1, random2, random3, random4, random5)

alpha.fun(dran, 0.95)
alpha.fun(eating, 0.95)

### Religiosity vars
str(d, list.len = ncol(d))
values <- c(69:133) 
labs <- paste0("REL", values) # create object of variable names to capture the religiousity questions
relsub <- d[labs]
relsubs <- relsub[complete.cases(relsub),]
alpha.fun(relsubs, 0.95)
alpha(relsub)
alpha(relsub, check.keys = T) # reverse code items?

d$relig <- d$REL76 + d$REL76 + d$REL100 # example
hist(d$relig) # plotting the distribution of religiousity scores
