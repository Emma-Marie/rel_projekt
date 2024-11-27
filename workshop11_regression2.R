###################
### Regression 2
### Multiple predictors

library(dagitty)

# Theoretical model
# age OR sex predict religiosity, they are independent (no confounds, mediators, or colliders)

plot(dagitty('

dag { 
  bb = "0,0,1,1"
  age [exposure, pos = "0.266, 0.358"]
  religiosity [outcome, pos = "0.453,0.357"]
  sex [pos = "0.354, 0.216"]
  age -> religiosity
  sex -> religiosity
}

'))

# Simulation
set.seed(777) # set seed so the random data drawn by others will be the same
n <- 100 # sample size
alpha <- 10 # intercept
bage <- 5 # effect of age
bsex <- -10 # effect of sex
noise <- rnorm(n, 0, 1) # for some realism

age <- rnorm(n, 0, 1) # set as normalized age in the real analysis, if wanted
sex <- rbinom(n, 1, 0.5) 
rel <- alpha + bage * age + bsex * sex + noise # religiosity or whatever

m1 <- lm(rel ~ age) # model for the effect of age on religiosity
m2 <- lm(rel ~ age + sex) # model for the effect of age AND sex on religiosity

coef(m1)  # coefficient close to five, which we defined (but not precisely five because of the noise)
coef(m2) #coefficient close to ten, which we defined (but not precisely ten because of the noise)

# Scatter plot (not "controlling" for sex!!!)
par(mar = c(4, 4, 1, 1))
plot(rel ~ age, pch = sex) 
abline(m2) # "controlling for sex" --> this means controlling for "being male" by setting sex to 0 = female
abline(m1, lty = 2) # ignoring it" --> which is the clever choise
legend(-3, 25, legend = c("female", "male"), pch = c(0, 1))

# Interval plot
labs <- c("intercept", "age", "sex")
x <- seq(1, length(labs), by = 1)

est <- c(coef(m2)[1], coef(m2)[2], coef(m2)[3])
LL <- confint(m2)[,1] # lower values of CI
UL <- confint(m2)[,2] # upper value of CI
LS <- est - LL
US <- UL - est

par(mar = c(4, 4, 1, 1))
plot(est, x, pch = 16, xlim = c(-10, 12), ylim = c(0.5, 4), 
     xlab = NA, 
     ylab = NA, yaxt = "n", frame.plot = F)
arrows(x0 = est-LS, y0 = x, x1 = US + est, y1 = x, code = 3, angle = 90, length = 0.05)
abline(v = 0, lty = 2)
axis(2, at = x, labels = labs, las = 2, cex = 0.8)
mtext("estimate", 1, padj = 4, cex = 0.8)

# Mess with n and SDs!
