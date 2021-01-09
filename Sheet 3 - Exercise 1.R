# Tutorial 3, part 2
library(readr)
library(forecast)
library(caret)

setwd("...")
data <- read_csv("mom.csv", col_names = FALSE)
momHicp <- data$X1

dev.new()
layout(matrix(c(1,1,2,3),2,2, byrow = TRUE));
plot.ts(momHicp)
grid()
title(main= "Month-on-Month Inflation Rate of the Euro Area")
acf(momHicp)
pacf(momHicp)


# Information criteria
lags <- expand.grid(x = 1:5, y = 1:5)
names(lags) <- c("p", "q")

aic <- numeric(length = nrow(lags))
ll <- numeric(length = nrow(lags))
bic <- numeric(length = nrow(lags))

for (i in 1:nrow(lags)){
  p <- lags[i, 1]
  q <- lags[i, 2]
    
  armaMod <- arima(momHicp[1:195], order = c(p,0,q), 
                   method = "ML" , include.mean = TRUE, 
                   optim.control = list(maxit = 500))
  
  aic[i] <- AIC(armaMod)
  ll[i] <- armaMod$loglik
  bic[i] <- BIC(armaMod)
}

aicBest <- which.min(aic)
bicBest <- which.min(bic)
llBest <- which.max(ll)

print(c(aicBest, aic[aicBest]))
print(c(bicBest, bic[bicBest]))
print(c(llBest, ll[llBest]))

# choose the best model 
bestParams <- min(aicBest, bicBest)

bestMod <- lags[bestParams, ]

armaModBest <- arima(momHicp[1:195], order = c(bestMod$p, 0, bestMod$q), 
                  method = "ML", include.mean = TRUE, 
                  optim.control = list(maxit = 1000))


# 1-step ahead forecast
forecast1 <- numeric(length = 12)

for (i in 1:12){
  armaModExp <- Arima(momHicp[1:(194+i)], model = armaModBest)
  forecast1[i] <- predict(armaModExp, n.ahead = 1)$pred
}

# Plot 1-forecast and original series
par(mfrow = c(1,1))
plot(momHicp[196:207], type = "l", main = "One-step forecasts")
lines(forecast1, col = "red")
grid()
legend("bottomleft", legend = c("Hicp", "Forecast (1 Period)"), 
       col=c("black", "red"), lty = 1, cex = 0.75)


# 12-step ahead forecast
forecast12 <- predict(armaModBest, n.ahead = 12)$pred

# no change forecast
ncf <- rep(momHicp[185], 12)

# Plot all Series
par(mfrow = c(1,1))
plot(momHicp[196:207], type = "l", main = "Forecasts and true Series", 
     xlab = "Period", ylab = "momHicp")
lines(forecast1, col = "red", type = "l")
lines(as.numeric(forecast12), col = "blue", type = "l")
lines(ncf, col = "green")
lines(rep(mean(momHicp), 12), lty = "dotted")
grid()
legend("bottomleft", legend = c("Hicp", "Forecast (1 Period)", "Forecast (12 Period)", "No Change Forecast"),
       col = c("black", "red", "blue", "green"), lty = 1, cex = 0.75)

      
msfeArma1 = mean((forecast1 - momHicp[196:207])^2)
msfeArma12 = mean((forecast12 - momHicp[196:207])^2)
msfeNcf = mean((ncf - momHicp[196:207])^2)
print(msfeArma1)
print(msfeArma12)
print(msfeNcf)





