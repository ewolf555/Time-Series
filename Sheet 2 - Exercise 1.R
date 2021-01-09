# Tutorial 2: ARMA models
# Simulaions of different stationary time series models and their respective
# Autocorrelation and Partial Autocorrelation Functions.

#define length of simulated series
N <- 150

# (a) White Noise
wn <- rnorm(N, 0, (0.5^2));


# Plot the processes
dev.new()
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
plot(wn, type = "l")
grid()
title(main = "White Noise Process")
acf(wn)
pacf(wn)

# (b) AR(2)
ar <- arima.sim(list(ar = c(0.5, 0.3)), n = N)
ar2 <- arima.sim(list(ar = c(-0.5, 0.3)), n = N)
ar3 <- arima.sim(list(ar = c(0.65, 0.3)), n = N)

# Plot the processes
dev.new()
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
plot(ar, main = "AR(2), a1=0.5, a2=0.3")
grid()
acf(ar)
pacf(ar)

dev.new()
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
plot(ar2, main = "AR(2), a1=-0.5, a2=0.3")
grid()
acf(ar2)
pacf(ar2)

dev.new()
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
plot(ar3, main = "AR(2), a1 = 0.65, a2 = 0.3")
grid()
acf(ar3)
pacf(ar3)

# (c) MA(1)
ma <- 2 + arima.sim(list(ma = 0.35), n = N)
ma2 <- 2 + arima.sim(list(ma = 0.9), n = N)
ma3 <- 2 + arima.sim(list(ma = -0.9), n = N)

# Plot the processes
dev.new()
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
plot(ma, main = "MA(1), theta1=0.35")
grid()
acf(ma)
pacf(ma)

dev.new()
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
plot(ma2, main = "MA(1), theta1=0.9")
grid()
acf(ma2)
pacf(ma2)

dev.new()
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
plot(ma3, main = "MA(1), theta1=-0.9")
grid()
acf(ma3)
pacf(ma3)

# (d) ARMA(1,1)
arma <- 0.2 + arima.sim(list(ar = 0.3, ma = 0.1), n = N)
arma2 <- 0.2 + arima.sim(list(ar = 0.9, ma = 0.1), n = N)
arma3 <- 0.2 + arima.sim(list(ar = -0.9, ma = 0.1), n = N)

# Plot the processes
dev.new()
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
plot(arma, main = "ARMA(1,1), alpha = 0.3, theta = 0.1")
grid()
acf(arma)
pacf(arma)

dev.new()
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
plot(arma2, main="ARMA(1,1), alpha = 0.9, theta = 0.1")
grid()
acf(arma2)
pacf(arma2)

dev.new()
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
plot(arma3, main = "ARMA(1,1), alpha = -0.9, theta = 0.1")
grid()
acf(arma3)
pacf(arma3)


