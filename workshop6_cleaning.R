############################################################
### Data Management, Quality Checking, Etc., 9th of October
############################################################

### Prelims
setwd("/Users/Emma-Marie/Documents/AU/ka_2.semester/rel_projekt/rel_projekt")
d <- data.frame(read_excel("data/DATA_24_cleaning.xlsx", sheet = "Main", na = "NA"))
str(d)

### Quality Checks
table(d$KON)
table(d$BY)

### Using NA and apply()
apply(d, 2, max, na.rm = T)
apply(d, 2, min, na.rm = T)

# apply the min and mean values of the columns with data about religiosity
apply(d[,76:140], 2, min, na.rm = T)
apply(d[,76:140], 2, mean, na.rm = T)
par(mfrow = c(8, 8))
# create histogram of 
apply(d[,76:140], 2, hist, na.rm = T)

# plot with dots
par(mar = c(5, 5, 5, 5))
sojler <- apply(d[,76:140], 2, mean, na.rm = T)
plot(sojler)

#density plot
d102 <- density(d$REL102, na.rm = T)
plot(d102)

## Describing sample
d$ALDER # see all ages
mean(d$ALDER)
sd(d$ALDER)
length(d$ALDER)
# plot destribution of ages
barplot(table(d$ALDER))

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

statinfo(d$ALDER)

statinfo(d$REL69)

statinfo(d$REL97) # aliens

statinfo(d$REL74) # rel. leaders and politics

# investigate correlation (yes?) between belief in aliens and wanting religious leaders to influence politics
par(mar = c(0, 0,0, 0))
plot(jitter(REL97, factor = 1) ~ ALDER, data = d,
     xlab = "Alder", ylab = "Aliens",
     pch = 8, col = "darkgreen")


