
N <- 500

set.seed(4)
u <- rnorm(N, 0, sqrt(3))
v <- rnorm(N, 0, sqrt(1))
w <- rnorm(N, 0, sqrt(2))
  
y <- cumsum(u + 0.5)
x <- cumsum(v + 0.3)
z <- 6 + 0.5*x + w

# PLot all simulated series
plot(y, type = "l", main = "Simulated Series", lwd = 2)
grid()
lines(x, col = "red", lwd = 2)
lines(z, col = "blue", lwd = 2)
abline(a = 6, b = 0.15, lty = "dashed")
abline(a = 0, b = 0.3, lty = "dashed")
abline(a = 0, b = 0.5, lty = "dashed")
legend("topleft", legend = c("y", "x", "z"), 
       col = c("black", "red", "blue"), lty = "dashed")


# Cointegration
plot(x[1:500], type = "l", main = "Simulated Series", col = "red", 
     ylim = c(min(x[1:500]), max(x[1:500])), lwd = 2)
grid()
lines(z[1:500], col = "blue", lwd = 2)
lines(z[1:500] - 0.5*x[1:500], col = "black", lwd = 2)
abline(a = 6, b = 0.15, lty = "dashed")
abline(a = 0, b = 0.3, lty = "dashed")
abline(a = 6, b = 0, lty = "dashed")
legend("topleft", legend = c("x", "z", "z - 0.5*x"), 
       col = c("red", "blue", "black"), lty = 1, lwd = 1)



 
# Scatterplots
par(mfrow = c(1, 2))
plot(x, y, main = "Scatterplot of x and y")
grid()
plot(x, z, main = "Scatterplot of x and z") 
grid()


# Residual Plots and ACFs

# Regressions
linMod1 <- lm(y ~ x)

linMod2 <- lm(z ~ x)

# Plot Residuals and ACF
epsilon1 <- linMod1$residuals
plot(epsilon1, type = "l", main = "Epsilon 1")
grid()
acf(epsilon1, main = "ACF")

# Plot Residuals and ACF  
epsilon2 <- linMod2$residuals
plot(epsilon2, type = "l", main = "Epsilon2")
grid()
acf(epsilon2, main = "ACF")

# Coefficients are close to the values of z and epsilon is similar to wt
linMod2$coefficients
mean(epsilon2)
var(epsilon2)


# Simulation

sampleN <- c(50, 100, 500, 1000)
M <- 1000

tStats1 <- list()
tStats2 <- list()
rSquared1 <- list()
rSquared2 <- list()

# Simulation for each sample size
for (i in 1:4){
  N <- sampleN[i]
  
  t1 <- numeric(length = M)
  t2 <- numeric(length = M)
  r21 <- numeric(length = M)
  r22 <- numeric(length = M)
  
  for (j in 1:M){
    set.seed(j)
    u <- rnorm(N, 0, sqrt(3))
    v <- rnorm(N, 0, sqrt(1))
    w <- rnorm(N, 0, sqrt(2))
    
    y <- cumsum(u + 0.5)
    x <- cumsum(v + 0.3)
    z <- 6 + 0.5*x + w
    
    m1 <- lm(y ~ x)
    t1[j] <- summary(m1)[["coefficients"]][2, 3]
    r21[j] <- summary(m1)$r.squared
    
    # Regression (b) 
    
    m2 <- lm(z ~ x)
    t2[j] <- summary(m2)[["coefficients"]][2, 3]
    r22[j] <- summary(m2)$r.squared
  }
  
  tStats1[[i]] <- t1
  tStats2[[i]] <- t2
  
  rSquared1[[i]] <- r21
  rSquared2[[i]] <- r22
}


# Plot the Results
par(mfrow = c(2,2))
for (i in 1:4){
  hist(tStats1[[i]], 
       main = paste("Histogram of T-Statistics (N = ", sampleN[i], ")", sep =""),
       xlab = "T-Statistic")
}

for (i in 1:4){
  hist(tStats2[[i]], 
       main = paste("Histogram of T-Statistics (N = ", sampleN[i], ")", sep =""),
       xlab = "T-Statistics")
}

for (i in 1:4){
  hist(rSquared1[[i]], 
       main = paste("Histogram of R-Squared (N = ", sampleN[i], ")", sep =""),
       xlab = "R^2")
}
for (i in 1:4){
  hist(rSquared2[[i]], 
       main = paste("Histogram of R_Squared (N = ", sampleN[i], ")", sep =""),
       xlab  = "R^2")
}
par(mfrow = c(1,1))





