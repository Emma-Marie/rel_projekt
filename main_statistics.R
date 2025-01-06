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
  "Health insecurity" [pos="0.481,0.593"]
  "Material insecurity" [pos="0.491,0.230"]
  Age [exposure,pos="0.347,0.429"]
  Religiosity [outcome,pos="0.646,0.417"]
  Sex [pos="0.649,0.232"]
  "Health insecurity" -> "Material insecurity"
  "Health insecurity" -> Religiosity
  "Material insecurity" -> Religiosity
  Age -> "Health insecurity"
  Age -> "Material insecurity"
  Age -> Religiosity
  Sex -> "Material insecurity"
  Sex -> Religiosity
}
'))


dev.off()

###### Simulation ########
###########################

# set color scheme
mycol1 <-  ("white")
mycol2 <- ("cadetblue2") 
mycol3 <- ("lightblue2")
mycol4 <- ("skyblue4")

set.seed(666)
fd <- function(n, beta) {
  e_rel <- rnorm(n, 0, 1) # noise
  e_hel <- rnorm(n, 0, 1)
  e_mat <- rnorm(n, 0, 1)
  e_sex <- rnorm(n, 0, 1)
  e_age <- rnorm(n, 0, 1) 
  SEX <- rbinom(n, 1, .5)
  AGE <- rnorm(n, 0, 1)
  
  HEL <- AGE * beta + e_hel
  MAT <- - AGE * beta - SEX * beta + HEL * beta + e_mat 
  REL <- AGE * beta + HEL * beta - SEX * beta + MAT * beta + e_rel
  
  df <- data.frame(AGE, MAT, SEX, HEL, REL)
  # models
  open0 <- coef(lm(REL ~ AGE, dat = df))[2] # age as only variable
  open1 <- coef(lm(REL ~ AGE + SEX, dat = df))[2] # sex added as control variable
  open2 <- coef(lm(REL ~ AGE + SEX + HEL, dat = df))[2] # sex and hel added
  closed <- coef(lm(REL ~ AGE + SEX + HEL + MAT, dat = df))[2] # sex, mat and hel added
  return(c(open0, open1, open2, closed))
}

# the process is repeated 1000 times and the koefficients saved
sim1 <- data.frame(t(replicate(1000, fd(1000, .5))))
names(sim1) <- c("open0", "open1", "open2", "controlled")

densop0 <- density(sim1$open0)
densop1 <- density(sim1$open1)
densop2 <- density(sim1$open2)
densco1 <- density(sim1$controlled)

par(mfrow = c(2, 1), mar = c(2, 1, 1, 1))

pdf(file = "fig_output/simulation.pdf", width = 5, height = 3)

plot(NA, xlab = NA, ylab = "", 
     xlim = c(0, 1.4), 
     ylim = c(0, 13), 
     cex.lab = 1.3, yaxt = 'n')
polygon(densop0, col = mycol1) # open0
polygon(densop1, col = mycol2) # open1
polygon(densop2, col = mycol3) # open2
polygon(densco1, col = mycol4) # closed
abline(v = 0.5, lty = 2)
legend(1.1, 13, legend = c("~ Age", "+ Sex", "+ hel", "+ mat"), 
       fill = c(mycol1, mycol2, mycol3, mycol4), 
       cex = .8, horiz = F, bty = T, inset = c(0.03, 0.15))

dev.off()

###### Linear regression ######
###############################

### Linear regression
y <- d.r$RELSCOR
x1.c <- d.r$ALDER-mean(d.r$ALDER) # centralized age
x2 <- d.r$KON.r
x3 <- d.r$HELSCOR # health insecurity
x4 <- d.r$MAT # material insecurity
d1 <- data.frame(y, x1, x2, x3, x4)

# the models
(m0 <- lm(y ~ x1.c, data = d.r))
(m1 <- lm(y ~ x1.c + x2, data = d.r))
(m2 <- lm(y ~ x1.c + x2 + x3, data = d.r))
(mc <- lm(y ~ x1.c + x2 + x3 + x4, data = d.r))

summary(mc)

# get confidence intervals
confint(m0)
confint(m1)
confint(m2)
confint(mc)

# plot effect of age on religiosity
par(mar = c(4, 4, 1, 1), mfrow = c(1, 1))
plot(y ~ x1, data = d1, main = "Religiosity ~ age",
     ylab = "Religiosity score", xlab = "Age",
     xlim = c(10, 90), ylim = c(-13, 13),
     pch = 21, col = "skyblue4" )
abline(mc) # draw linear model rel ~ age


## UI-Plot for the variables (uncertainty interval)
labs <- c("(Intercept)", "ALDER", "KON", "HELSCO", "MAT")
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

dev.off()

##############################################

# Discussion: correlation between age and religious upbringing?
x <- d.r$ALDER
y <- d.r$OPREL

# the models
(model1 <- lm(y ~ x, data = d.r)) # rel ~ age

# plot effect of age on religiosity
par(mar = c(4, 4, 1, 1), mfrow = c(1, 1))
plot(y ~ x, data = d.r, main = "age ~ religios upbringing",
     ylab = "Religiosity score", xlab = "Age",
     xlim = c(10, 90), ylim = c(-0.5, 1.5),
     pch = 21, col = "skyblue4" )
abline(model1) # draw linear model rel ~ age
