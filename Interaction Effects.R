###########################
### Interaction effects

library(dagitty)
set.seed(777)

### DAG
plot(dagitty('dag {
bb="0,0,1,1"
PILLAxPILLB [pos="0.398,0.505"]
religiosity [pos="0.592,0.505"]
PILLAxPILLB -> religiosity
}
'))

### Simple
n <- 1000
pillA <- rbinom(n, 1, 0.5)
pillB <- rbinom(n, 1, 0.5)
beta <- 5
Y <- beta * pillA * pillB + rnorm(n, 0, 1)

(m1 <- lm(Y ~ pillA * pillB))

coef(m1)[1] + coef(m1)[2] * 1 + coef(m1)[3] * 1 + coef(m1)[4] * 1 * 1 # pill A AND B
coef(m1)[1] + coef(m1)[2] * 1 + coef(m1)[3] * 0 + coef(m1)[4] * 1 * 0 # pill A, holding B and the interaction constant
coef(m1)[1] + coef(m1)[2] * 0 + coef(m1)[3] * 1 + coef(m1)[4] * 0 * 1 # pill B, holding A and the interaction constant
coef(m1)[1] + coef(m1)[2] * 0 + coef(m1)[3] * 0 + coef(m1)[4] * 0 * 0 # no pills; holding everything constant

### Specified
n <- 1000
pillA <- rbinom(n, 1, 0.5)
pillB <- rbinom(n, 1, 0.5)
betaA <- 5
betaB <- -5
betaAB <- 25

Y <- betaA * pillA + betaB * pillB + betaAB * pillA * pillB + rnorm(n, 0, 1)

(m2 <- lm(Y ~ pillA * pillB))

coef(m2)[1] + coef(m2)[2] * 1 + coef(m2)[3] * 1 + coef(m2)[4] * 1 * 1 # pill A AND B
coef(m2)[1] + coef(m2)[2] * 1 + coef(m2)[3] * 0 + coef(m2)[4] * 1 * 0 # pill A, holding B and the interaction constant
coef(m2)[1] + coef(m2)[2] * 0 + coef(m2)[3] * 1 + coef(m2)[4] * 0 * 1 # pill B, holding A and the interaction constant
coef(m2)[1] + coef(m2)[2] * 0 + coef(m2)[3] * 0 + coef(m2)[4] * 0 * 0 # no pills; holding everything constant

# Plot (ideal for binary predictors)
exposures <- expand.grid(pillA = c(0, 1), pillB = c(0, 1)) # combs of exposure as above
exposures$ypred <- predict(m2, newdata = exposures)

plot(exposures$pillB[exposures$pillA == 0], exposures$ypred[exposures$pillA == 0], 
     type = "b", col = "black", pch = 16, xlab = NA, ylab = "Predicted Y", 
     main = NA, 
     xaxt = "n", xlim = c(-0.1, 1.1), ylim = c(min(exposures$ypred), max(exposures$ypred)))
axis(1, at = c(0, 1), labels = c("Pill B = 0", "Pill B = 1"))
lines(exposures$pillB[exposures$pillA == 1], exposures$ypred[exposures$pillA == 1], 
      type = "b", col = "gray", pch = 16, lwd = 2)
legend(0.5, 11, legend = c("Pill A = 0", "Pill A = 1"), 
       col = c("black", "gray"), pch = 16, lwd = 2,
       cex = 0.9)
