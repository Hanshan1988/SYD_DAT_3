library(forecast)
library(fpp)

# Time Series Components
fit <- stl(elecequip, s.window=5)

plot(elecequip)

plot(elecequip, col="gray", 
     main="Electrical equipment manufacturing",
     ylab="New orders index", xlab="")
lines(fit$time.series[, 2], col="red", ylab="Trend")

plot(fit)

# Exponential Smoothing Model
fit1 <- ses(oil, alpha=0.2, initial="simple", h=3)
fit2 <- ses(oil, alpha=0.6, initial="simple", h=3)
fit3 <- ses(oil, h=3)

plot(fit1, plot.conf=F, ylab="Oil (millions of tonnes)",
     xlab="Year", main="", fcol="white", type="o")

lines(fitted(fit1), col="blue", type="o")
lines(fitted(fit2), col="red", type="o")
lines(fitted(fit3), col="green", type="o")

plot(fit1, plot.conf=F, ylab="Oil (millions of tonnes)",
     xlab="Year", main="", fcol="white", type="o")
lines(fit1$mean, col="blue", type="o")
lines(fit2$mean, col="red", type="o")
lines(fit3$mean, col="green", type="o")

legend("topleft", lty=1, col=c(1,"blue", "red", "green"),
       c("data", expression(alpha==.2),
         expression(alpha == .6),
         expression(alpha == .99)), pch=1)

# Holt Winters
aust <- window(austourists, start=2005)
plot(aust)

fit1 <- hw(aust, seasonal="additive")
fit2 <- hw(aust, seasonal="multiplicative")

plot(fit2, ylab="International visitor night in Australia (millions)",
     plot.conf=F, type="o", fcol="white", xlab="Year")
lines(fitted(fit1), col="red", lty=2)
lines(fitted(fit2), col="green", lty=2)

lines(fit1$mean, type="o", col="red")
lines(fit2$mean, type="o", col="green")

# Seasonal Differencing
plot(a10)
plot(log(a10))

plot(diff(log(a10), 12), xlab="Year",
     ylab="Annual change in monthly log A10 sales")

plot(stl(a10, s.window = 12))

plot(stl(log(a10), s.window = 12))

# ARIMA Model
tsdisplay(diff(WWWusage), main="") #PACF, Partial Auto Correlation Function
fit <- Arima(WWWusage, order=c(3,1,1))
summary(fit)
plot(forecast(fit))

fit1 <- auto.arima(WWWusage)
summary(fit1)
plot(forecast(fit1))

fit2 <- auto.arima(a10)
summary(fit2)
plot(forecast(fit2))

# Evaluating a model
beer2 <- window(ausbeer, start=1992, end=2006-.1)

beerfit1 <- meanf(beer2, h=11)
beerfit2 <- rwf(beer2, h=11)
beerfit3 <- snaive(beer2, h=11)

plot(beerfit1, plot.conf=F,
     main="Forecasts for quarterly beer production")
lines(beerfit2$mean, col=2)
lines(beerfit3$mean, col=3)
lines(ausbeer)
legend("topright", lty=1, col=c(4, 2, 3),
       legend=c("Mean method", "Naive method", "Seasonal naive method"))


