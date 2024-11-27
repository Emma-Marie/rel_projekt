####################################################
############### Main script statistics #############
####################################################

# prelims
setwd("/Users/Emma-Marie/Documents/AU/ka_2.semester/rel_projekt/rel_projekt") # set working directory

library(dagitty)

# load cleaned data
d.r <- read.delim("data/dat_survey_cleaned.txt")
d.r[1] <- NULL # remove first column with row number


###### DAG ########
####################

pdf(file = "fig_output/DAG.pdf", width = 4, height = 4)

plot(dagitty('dag {
bb="0,0,1,1"
AGE [exposure,pos="0.262,0.346"]
ECO [adjusted,pos="0.458,0.136"]
HEL [adjusted,pos="0.459,0.579"]
REL [outcome,pos="0.672,0.346"]
SEX [adjusted,pos="0.653,0.097"]
AGE -> ECO
AGE -> HEL
AGE -> REL
ECO -> HEL
ECO -> REL
HEL -> REL
SEX -> ECO
SEX -> REL
}
           ')) 

dev.off()

###### Simulation ########
###########################

set.seed(666)
# set colour scheme
mycol1 <-  ("white")
mycol2 <- ("khaki") 
mycol3 <- ("lavender")
mycol4 <- ("cadetblue3")

fd <- function(n, beta) {
  e_rel <- rnorm(n, 0, 1) # noise
  e_hel <- rnorm(n, 0, 1)
  e_eco <- rnorm(n, 0, 1)
  e_sex <- rnorm(n, 0, 1)
  e_age <- rnorm(n, 0, 1) 
  SEX <- rbinom(n, 1, .5) # sex
  AGE <- rnorm(n, 0, 1)
  ECO <- AGE * beta + SEX * beta + e_eco
  HEL <- AGE * beta + ECO * beta + e_hel
  REL <- AGE * beta + HEL * beta + SEX * beta + ECO * beta + e_rel # religiosity
  
  df <- data.frame(AGE, ECO, SEX, HEL, REL)
  open0 <- coef(lm(REL ~ AGE, dat = df))[2]
  open1 <- coef(lm(REL ~ AGE + SEX, dat = df))[2]
  open2 <- coef(lm(REL ~ AGE + SEX + HEL, dat = df))[2]
  closed <- coef(lm(REL ~ AGE + SEX + HEL + ECO, dat = df))[2]
  return(c(open0, open1, open2, closed))
}

sim1 <- data.frame(t(replicate(1000, fd(1000, .5))))
names(sim1) <- c("open0", "open1", "open2", "controlled")

densop0 <- density(sim1$open0)
densop1 <- density(sim1$open1)
densop2 <- density(sim1$open2)
densco1 <- density(sim1$controlled)

par(mfrow = c(2, 1), mar = c(2, 1, 1, 1))

pdf(file = "fig_output/simulation.pdf", width = 5, height = 3)

plot(NA, xlab = NA, ylab = "", 
     xlim = c(0.3, 1.3), 
     ylim = c(0, 13), 
     cex.lab = 1.3, yaxt = 'n')
polygon(densop0, col = mycol1) # open0
polygon(densop1, col = mycol2) # open1
polygon(densop2, col = mycol3) # open2
polygon(densco1, col = mycol4) # closed
abline(v = 0.5, lty = 2)
legend(1.1, 13, legend = c("~ Age", "+ Sex", "+ Eco", "+ Hel"), 
       fill = c(mycol1, mycol2, mycol3, mycol4), 
       cex = .8, horiz = F, bty = T, inset = c(0.03, 0.15))

dev.off()

###### Linear regression #####
#############################

### Linear regression
y <- d.r$RELSCOR
x1 <- d.r$ALDER
x1.c <- d.r$ALDER-mean(d.r$ALDER) # centralized age
x2 <- d.r$KON
x3 <- d.r$HELSCOR
x4 <- d.r$ECO
d1 <- data.frame(y, x1, x2, x3, x4)

# the models
(m0 <- lm(y ~ x1, data = d.r)) # rel ~ age
(m1 <- lm(y ~ x1 + x2, data = d.r)) # rel ~ age + sex
(m2 <- lm(y ~ x1 + x2 + x3, data = d.r)) # rel ~ age + sex + health
(mc <- lm(y ~ x1 + x2 + x3 + x4, data = d.r)) # rel ~ age + sex + health + income

# the same models but with centralized age
(m0.c <- lm(y ~ x1.c, data = d.r))
(m1.c <- lm(y ~ x1.c + x2, data = d.r))
(m2.c <- lm(y ~ x1.c + x2 + x3, data = d.r))
(mc.c <- lm(y ~ x1.c + x2 + x3 + x4, data = d.r))

summary(mc)
confint(mc)

# plot effect of age on religiosity
par(mar = c(4, 4, 1, 1), mfrow = c(1, 1))
plot(y ~ x1, data = d1, main = "Religiosity ~ age",
     ylab = "Religiosity score", xlab = "Age",
     xlim = c(10, 90), ylim = c(-13, 13),
     pch = 21, col = "skyblue4" )
abline(m0) # draw linear model rel ~ age

## UI-Plot for the variables (uncertainty interval)
labs <- c("(Intercept)", "ALDER", "KON", "HELSCO", "ECO")
x <- seq(1, length(labs), by = 1)

est <- coef(mc)  # coefficients
LL <- confint(mc)[, 1]  # lower boundary
UL <- confint(mc)[, 2]  # upper boundary
LS <- est - LL  # length to the left
US <- UL - est  # length to the right

pdf(file = "fig_output/boxplot.pdf", width = 4, height = 4)

par(mar = c(4, 5, 2, 1))  # plot margins
plot(est, x, pch = 16, 
     xlim = c(min(LL) - 1, max(UL) + 1),  # dynamic x-axis
     ylim = c(0.5, length(labs) + 0.5),  # dynamic y-axis
     xlab = "Estimate", 
     ylab = "", yaxt = "n", frame.plot = FALSE)
arrows(x0 = est - LS, y0 = x, x1 = est + US, y1 = x, 
       code = 3, angle = 90, length = 0.05)
abline(v = 0, lty = 2)  # Lodret linje ved 0
axis(2, at = x, labels = labs, las = 2, cex.axis = 0.8)
#mtext("Estimate", 1, padj = 3, cex = 0.8)

dev.off()
