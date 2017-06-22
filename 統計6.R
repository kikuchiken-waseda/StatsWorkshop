d <- read.csv("data4a.csv")
head(d)
summary(d)
plot(d$x, d$y, pch = c(21, 19)[d$f])

N <- 8
y <- 0:N

plot(y, dbinom(y, N, prob = 0.1), type = "b", lty = 2, pch = 21, ylab = "prob")
lines(y, dbinom(y, N, prob = 0.3), type = "b", lty = 2, pch = 23)
lines(y, dbinom(y, N, prob = 0.6), type = "b", lty = 2, pch = 24)
legend("topright", legend = c(0.1, 0.3, 0.6), pch = c(21, 23, 24), title = "q", cex = 0.7)

logistic <- function(z) 1 / (1 + exp(-z))
z <- seq(-6, 6, 0.1)
plot(z, logistic(z), type = "l", lwd = 3, ylim = c(0, 1), yaxs = "i", ylab = "q")
abline(v = 0, lty = 3)


xx <- seq(-3, 3, 0.1)

plot(xx, logistic(0 + 2 * xx), type = "l", lwd = 3, ylim = c(0, 1), yaxs = "i", ylab = "q", main = expression(beta[2]==2))
lines(xx, logistic(2 + 2 * xx), lwd = 3, lty = 2)
lines(xx, logistic(-3 + 2 * xx), lwd = 3, lty = 4)
legend("topleft", legend = c(0, 2, -3), lty = c(1, 2, 4), title = expression(beta[1]))

plot(xx, logistic(0 + 2 * xx), type = "l", lwd = 3, ylim = c(0, 1), yaxs = "i", ylab = "q", main = expression(beta[1]==0))
lines(xx, logistic(0 + 4 * xx), lwd = 3, lty = 2)
lines(xx, logistic(0 - 1 * xx), lwd = 3, lty = 4)
legend("left", legend = c(2, 4, -1), lty = c(1, 2, 4), title = expression(beta[2]))

fit.xf <- glm(cbind(y, N - y) ~ x + f, data = d, family = binomial)
fit.xf
plot(d$x, d$y, pch = c(21, 19)[d$f])
xx <- seq(min(d$x), max(d$x), length = 50)
ff <- factor("C", levels = c("C", "T"))
q <- predict(fit.xf, newdata = data.frame(x = xx, f = ff), type = "response")
lines(xx, q * 8, lwd = 3)
ff <- factor("T", levels = c("C", "T"))
q <- predict(fit.xf, newdata = data.frame(x = xx, f = ff), type = "response")
lines(xx, q * 8, col = "gray", lwd = 3)
legend("topleft", legend = c("C", "T"), pch = c(21, 19))

library(MASS)
stepAIC(fit.xf)



