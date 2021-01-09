library(readr)
library(urca)


# Stationary vs non-stationary time series

# AR(1) vs Random Walk
nObs <- 1000
set.seed(1)
ar1 <- arima.sim(n = nObs, model = list(ar = c(0.95)))
set.seed(2)
randomWalk <- cumsum(rnorm(nObs))

limits <- c(min(ar1, randomWalk), max(ar1, randomWalk))
plot(ar1, main = "AR(1) vs. Random Walk", ylim =limits, col = "blue", 
     lwd = 2, ylab = "x_t", xlab = "Index")
lines(randomWalk, col = "red", lwd = 2)
legend("bottomright", legend = c("AR(1)", "Random Walk"), 
       col = c("blue", "red"), lwd = 2)
grid()

# Trend Stationary vs. Random Walk with drift
# AR(1) vs Random Walk
nObs <- 1000
set.seed(1)
ar1Trend <- 0.5*c(1:nObs) + arima.sim(n = nObs, model = list(ar = c(0.95)))
set.seed(2)
randomWalkDrift <- cumsum(rnorm(nObs) + 0.5)

limits <- c(min(ar1Trend, randomWalkDrift), max(ar1Trend, randomWalkDrift))
plot(ar1Trend, main = "Trend Stationary AR(1) vs. Random Walk with Drift", 
     ylim =limits, col = "blue", 
     lwd = 2, ylab = "x_t", xlab = "Index")
lines(randomWalkDrift, col = "red", lwd = 2)
legend("bottomright", legend = c("AR(1)", "Random Walk"), 
       col = c("blue", "red"), lwd = 2)
grid()

# Simulation of the Distribution of the AR-Coefficient with a unit root
paramEstimRw <- numeric(length = 10000)
paramEstimAr <- numeric(length = 10000)


for (i in 1:10000){
  set.seed(i)
  # Simulate Random Walk
  randomWalk <- cumsum(rnorm(500)) 
  ar1 <- arima.sim(n = 500, model = list(ar = c(0.5)))
  
  # Esitmate Coefficient
  modEstRw <- ar(randomWalk, order.max = 1, method = "ols") 
  modEstAr <- ar(ar1, order.max = 1, method = "ols")
  
  # Store coefficient in vector
  paramEstimRw[i] <- modEstRw$ar
  paramEstimAr[i] <- modEstAr$ar
}

# Calculate a smoothed empirical densities
coefDensRw <- density(paramEstimRw) 
coefDensAr <- density(paramEstimAr)

# Plot the empirical Density of the estimated Coefficient of the AR(1) - Process
plot(coefDensAr, col = "blue", lwd = 2, 
     main = "Estimated Density of the AR-Coefficient", 
     xlab = "Estimated AR Coefficient")
xRange <- seq(min(coefDensAr$x),
              max(coefDensAr$x),  by = 0.001)
lines(xRange, dnorm(xRange, mean = 0.5, sd = sd(paramEstimAr)),
      lwd = 2, col = c("red"))  # add a normal density with same variance
grid()
legend("topleft", legend = c("Smoothed empirical Density", 
                             "Normal Distribution"),
       col = c("blue", "red"), lwd = 2)



# Plot of the empirical Density of the estimated Coefficent of the Random Walk
plot(coefDensRw, col = "blue", lwd = 2, 
     main = "Estimated Density of the AR-Coefficient", 
     xlab = "Estimated AR Coefficient")
xRange <- seq(min(coefDensRw$x),
              max(coefDensRw$x),  by = 0.001)
lines(xRange, dnorm(xRange, mean = 1, sd = sd(paramEstimRw)),
      lwd = 2, col = c("red"))  # add a normal density with same variance
grid()
legend("topleft", legend = c("Smoothed empirical Density", 
                             "Normal Distribution"),
       col = c("blue", "red"), lwd = 2)


# The emipircal densities of the simulation shows that the distribution 
# of the estimated coefficient does not converge to a normal distribution 
# if the process is a random walk. 


# Simulated density function of the DF-Distribution based on the
# formula given in Hamilton (1994) p. 489
N <- 500
nSim <- 10000

dfVals <- numeric(length = nSim)

for (i in 1:nSim){
  set.seed(i)
  u <- rnorm(N)
  W <- 1/sqrt(N)*cumsum(u)
  dfVals[i] <- 1/2*(W[N]^2-1)/(sqrt(mean(W^2)))
}


# Create Plot Elements for Dickey-Fuller Distribution
tCritDf <- sort(dfVals)[c(0.05)*nSim]
dfDensity <- density(dfVals)
xShadeDF <- dfDensity$x[dfDensity$x <= tCritDf]
yShadeDF <- dfDensity$y[dfDensity$x <= tCritDf]

# Create Plot Elements for T-Distribution
tDensity <- dt(seq(-4, 4, 0.01), df = N-1) 
tCritT <- qt(0.05, df = (N-1))
xShadeT <- seq(-4, 4, 0.01)[seq(-4, 4, 0.01) <= tCritT]
yShadeT <- tDensity[seq(-4, 4, 0.01) <= tCritT]

# Plot both distributions and critical regions
plot(tDensity ~ seq(-4, 4, 0.01), col = "red", lwd = 2, type = "l",
     main = "Dickey Fuller Distribution vs. T-Distribution", xlab = "x", 
     ylab = "f(x)", ylim = c(0, max(dfDensity$y)))
lines(dfDensity, col= "blue", lwd = 2)  # adds DF density
polygon(c(xShadeDF[1] , xShadeDF, tCritDf), c(0, yShadeDF, 0), 
        col="lightblue", border = "blue", lwd = 2)  # adds DF crit. region
polygon(c(xShadeT[1] , xShadeT, tCritT), c(0, yShadeT, 0), 
        col="mistyrose", border = "red", lwd=2)  # adds T crit. region
legend("topleft", legend = c("Dickey-Fuller Distribution", "T-Distribution"),
       col = c("blue", "red"), lwd = 2, cex = 0.8)
grid()


# Load the data
setwd("...")
prices <- read_delim("prices.csv", ";", escape_double = FALSE, trim_ws = TRUE)

date <- seq(as.Date("2000/12/01"), as.Date("2007/12/01"), "month")

prices <- prices$CPI
prices <- na.omit(prices)


plot(prices ~ date, type="l", main = "Prices", xlab = "Date", ylab = "Prices")
grid()

# Test Trend Stationary vs. Random Walk with Drift
adf1 <- ur.df(prices, "trend", selectlags = "BIC")
summary(adf1)
# Test is one-sided and we can't reject the null hypothesis if the 
# Test Statistic is larger than the critical value. 
# This is the case here, so we conclude there is a unit root.
# Yet, Phi 3 indicates that trend might not be significant. 

# Test AR with non-zero mean vs. Random Walk with Drift
adf2 <- ur.df(prices, "drift", selectlags = "BIC")
summary(adf2)
# Testing for a random walk with drift against a stationary process with mean
# unequal to zero also does not let us reject the null hypothesis of a unit root
# We have to reject the joint hypothesis of mean zero and a unit root. 

# Take first differences 
deltaPrices <- diff(prices)
plot(deltaPrices ~ date[2:85], type = "l", main = "Prices - Absolute Change", 
     xlab = "Date", ylab = "Prices")
grid()

# Test AR with non-zero mean vs. Random Walk with Drift
adf3 <- ur.df(deltaPrices, "drift", selectlags = "BIC")
summary(adf3)
# This time, the test statistic is smaller than the critical values and we 
# have to reject the Null Hypothesis of a unit root at every significance level
# Hence, there is no unit root but the joint hypothesis of a zero mean and a 
# unit root has to be rejected. The process is I(1).


