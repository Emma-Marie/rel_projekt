############################
### Project Template
rm(list = ls())

### Prelims
setwd("C:/Users/au624473/Dropbox/2. Aarhus/Courses/Projects Courses/Demographics and Causal Inference/2024 Fall/Project")
d <- read.delim("~/Dropbox/2. Aarhus/Courses/Projects Courses/Demographics and Causal Inference/2024 Fall/Project/Oct10data.txt", stringsAsFactors = TRUE)

library(psych)
library(dagitty)
library(AnthroTools)

### Theoretical Model (Causal System)
plot(dagitty('dag {
bb="0,0,1,1"
police [pos="0.398,0.505"]
religiosity [pos="0.592,0.505"]
sex [pos="0.499,0.258"]
police -> religiosity
sex -> police
sex -> religiosity
}
'))

#### Simulation
### Simulate Theoretical Model
set.seed(666)
n <- 1000
bSP <- -2
bPR <- -2
bSR <- -5
sex <- rbinom(n, 1, 0.5)
police <- bSP * sex + rnorm(n, 0, 1)
religiosity <- bPR * police + bSR * sex + rnorm(n, 0, 1)

### Analyze Simulated Outcomes
m0 <- lm(religiosity ~ 1)
m1 <- lm(religiosity ~ police)  
m2 <- lm(religiosity ~ police + sex)

### Plot Simulated Outcomes

#### Application
### Necessary Data Transformations
# remember your transformations! (e.g, KON.r)

### Religiosity Scale
labs <- c("REL69", "REL70", "REL71", "REL72", "REL73",
          "REL74", "REL75", "REL76", "REL77", "REL78")
relscale <- d[labs]
alpha(relscale)

labs <- c("REL70", "REL71", "REL72", "REL73",
          "REL74", "REL75", "REL76", "REL77", "REL78")
relscale <- d[labs]
alpha(relscale)

d$religiosity <- d$REL70 + d$REL71 + d$REL72 + d$REL73 +
  d$REL74 + d$REL75 + d$REL76 + d$REL77 + d$REL78

### Statistical model
m0 <- lm(religiosity ~ 1, data = d)
m1 <- lm(religiosity ~ TPOL, data = d)
m2 <- lm(religiosity ~ TPOL + KON.r, data = d)

### Examine differences
m0
m1
m2

coef(m2)[1] - 2 * coef(m2)[2] + 0 * coef(m2)[3]
coef(m2)[1] - 1 * coef(m2)[2] + 0 * coef(m2)[3]
coef(m2)[1] + 0 * coef(m2)[2] + 0 * coef(m2)[3]
coef(m2)[1] + 1 * coef(m2)[2] + 0 * coef(m2)[3]
coef(m2)[1] + 2 * coef(m2)[2] + 0 * coef(m2)[3]

coef(m2)[1] + 2 * coef(m2)[2] + 0 * coef(m2)[3] - 0.3296634

### Plots
## Regression plot (2 vars)
plot(religiosity ~ jitter(TPOL, factor = 2), data = d,
     pch = 16, xlab = "Tilfreds med politiets efft.")
abline(m1)
newxs <- seq(min(d$TPOL, na.rm = T), 
             max(d$TPOL, na.rm = T), length.out = 100)
preds <- predict(m1, newdata = data.frame(TPOL = newxs), 
                 interval = "confidence")
polygon(c(rev(newxs), newxs), 
        c(rev(preds[ ,3]), 
          preds[ ,2]), col = rgb(0, 0, 0, 0.3), border = NA) # rgb: 0's for grey, 0.3 for transparency
abline(m2, lty = 3)
legend(-1.5, 5, lty = c(1, 3), legend = c("model 1", "model 2"),
       cex = .7)

## Uncertainty interval plot (>2 vars)
labs <- c("intercept", "TPOL", "KON.r")
x <- seq(1, length(labs), by = 1)

est <- c(coef(m2)[1], coef(m2)[2], coef(m2)[3])
LL <- confint(m2)[,1]
UL <- confint(m2)[,2]
LS <- est-LL
US <- UL-est

par(mar = c(4, 5, 1, 1))
plot(est, x, pch = 16, xlim = c(-15, 7), ylim = c(0.5, 4), 
     xlab = NA, 
     ylab = NA, yaxt = "n", frame.plot = F)
arrows(x0 = est-LS, y0 = x, x1 = US + est, y1 = x, code = 3, angle = 90, length = 0.05)
abline(v = 0, lty = 2)
axis(2, at = x, labels = labs, las = 2, cex = 0.8)
mtext("estimate", 1, padj = 4, cex = 0.8)
