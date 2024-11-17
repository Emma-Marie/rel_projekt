##############################################
### Regression

x <- c(1, 28, 16, 40, 42, 30, 4, 25, 7, 19, 33, 35, 40, 10, 43, 10, 45, 19, 18, 28) # data independent variable
y <- c(9, 20, 17, 26, 33, 24, 10, 23, 10, 15, 23, 25, 21, 12, 35, 15, 32, 15, 18, 23) # data dependent variable
d <- data.frame(y, x) # create data frame

(m1 <- lm(y ~ x, data = d)) # calculate intercept and slope (the coefficients) for linear regression
summary(m1)
confint(m1) # calculate 95% confidence intervals for both intercept and slope.

## Calculations step-by-step
d$xd <- x - mean(x) # deviance of x
d$yd <- y - mean(y) # deviance of y
d$xdsq <- d$xd^2 # squared dev. of x --> to get rid of negative values
d$xy <- d$xd*d$yd # dev. of x AND y

## just shows how intercept and slope were actually calculated by lm() in line 8.
beta <- (sum(d$xy))/(sum(d$xdsq)) # prop. of x's dev. to ALL
alpha <- mean(y) - beta*mean(x) # avg. y without weighted avg. x --> the 

## Uncertainty
# Residual standard error
d$ypred <- alpha + beta * x # get predictions for y --> the y-values on the regression line
d$preddiff <- y - d$ypred # get prediction error --> take the actual y values from data and subtracts tge predicted y-values --> get how much actual y-values deviates from line.
d$preddiffsq <- d$preddiff^2 # square errors to get rid of negative values
SSE <- sum(d$preddiffsq) # sum of squared errors --> the bigger the sample size, the smaller the SSE values, because the total amount of deviation decreases --> sample looks more like population.
var.e <- SSE/(nrow(d) - 2) # variance
resstderr <- sqrt(var.e) # residual standard error (SE)

# Std. error of intercept
alpha.se <- resstderr * sqrt((1/nrow(d)) + ((mean(x)^2) / sum(d$xdsq)))

# Std. error of slope
beta.se <- sqrt(var.e / sum(d$xdsq))

# 95% CIs
beta.up <- beta + qt(0.975, nrow(d) - 2)*beta.se
beta.lo <- beta - qt(0.975, nrow(d) - 2)*beta.se
alpha.up <- alpha + qt(0.975, nrow(d) - 2)*alpha.se
alpha.lo <- alpha - qt(0.975, nrow(d) - 2)*alpha.se

# t-statistics
#t.alpha <- alpha/alpha.se
#t.beta <- beta/beta.se

# create output showing koefficients, their se and their lower and upper bouderies for the confidence intervals. 
outtab <- round(data.frame(est = c(alpha, beta), se = c(alpha.se, beta.se), 
                           #tstat = c(t.alpha, t.beta), 
                           lower = c(alpha.lo, beta.lo),
                           upper = c(alpha.up, beta.up), row.names = c("alpha", "beta_age")), 2)

### Plot
m1 <- lm(y ~ x, data = d)

# plot the linear regression
par(mar = c(4, 4, 1, 1), mfrow = c(1, 1))
plot(y ~ x, data = d,
     ylab = "# of items listed", xlab = "age",
     xlim = c(0, 50), ylim = c(0, 40),
     pch = 16)
abline(m1)

# adjust the Se to chunks of data
newxs <- seq(min(x), max(x), length.out = 100)
preds <- predict(m1, newdata = data.frame(x = newxs), 
                 interval = "confidence")

polygon(c(rev(newxs), newxs), 
        c(rev(preds[ ,3]), 
          preds[ ,2]), col = rgb(0, 0, 0, 0.3), border = NA) # rgb: 0's for grey, 0.3 for transparency

### Uncertainty interval demo

mycol <- rgb(128, 128, 128, max = 255, alpha = 25, names = "vapor")
mycol1 <- rgb(224, 224, 224, max = 255, alpha = 100, names = "lightgray") 

# create random data
par(mfrow = c(1, 2), mar = c(4, 4, 1, 1))
n <- 100 # sample size
x <- rnorm(n, 0, 10) # predictor
y <- 0.5*x + rnorm(n, 0, 10) # outcome
# plot 
plot(y ~ x, xlim = c(-30, 30), ylim = c(-30, 30),
     xlab = expression(italic("x")), 
     ylab = expression(italic("y")), pch = 16) # plot data points
abline(lm(y ~ x)) # plot regression line
# plot SE adjusted to chunks of data
newx <- seq(min(x), max(x), length.out = 100)
preds <- predict(lm(y ~ x), newdata = data.frame(x = newx), 
                 interval = "confidence")
polygon(c(rev(newx), newx), 
        c(rev(preds[ ,3]), 
          preds[ ,2]), col = rgb(0, 0, 0, 0.3), border = NA) # rgb: 0's for grey, 0.3 for transparency

regsim <- function(n, xbar, sd, beta){ # function to resample
  x <- rnorm(n, xbar, sd)
  y <- beta * x + rnorm(n, xbar, sd)
  abline(lm(y ~ x), col = mycol1)
}
# plot the re sampled data as regression lines
plot(NA, xlim = c(-30, 30), ylim = c(-30, 30),
     xlab = expression(italic("x")), ylab = expression(italic("y")))
replicate(1000, regsim(100, 0, 10, 0.5)) # replicate the data 1000 times and draw line

### Multiple predictors

n <- 1000
alpha <- 5
betax1 <- 0.7
betax2 <- -0.7
noiser <- rnorm(n, 0, 1)
x1 <- rnorm(n, 0, 1)
x2 <- rbinom(n, 1, 0.5)
y <- alpha + betax1 * x1 + betax2 * x2 + noiser

m3 <- lm(y ~ x1 + x2)
m3
confint(m3)

labs <- c("intercept", "x1", "x2")
x <- seq(1, length(labs), by = 1)

est <- c(coef(m3)[1], coef(m3)[2], coef(m3)[3])
LL <- confint(m3)[,1]
UL <- confint(m3)[,2]
LS <- est-LL
US <- UL-est

par(mar = c(4, 5, 1, 1))
plot(est, x, pch = 16, xlim = c(-2, 7), ylim = c(0.5, 4), 
     xlab = NA, 
     ylab = NA, yaxt = "n", frame.plot = F)
arrows(x0 = est-LS, y0 = x, x1 = US + est, y1 = x, code = 3, angle = 90, length = 0.05)
abline(v = 0, lty = 2)
axis(2, at = x, labels = labs, las = 2, cex = 0.8)
mtext("estimate", 1, padj = 4, cex = 0.8)
