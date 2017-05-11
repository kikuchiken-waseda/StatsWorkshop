load("/Users/yusuke/Downloads/data.RData")
data
logL <- function(m) sum(dpois(data, m, log = TRUE))
lambda <- seq(2, 5, 0.1)
plot(lambda, sapply(lambda, logL), type = "l")
sum(data)/50
#最尤推定量を求めた

datar <- rpois(50, 3.56)
#新しい観測データを乱数から求めた

hist(datar, breaks = seq(-0.5, 12, 1))
y <- 0:10
prob <- dpois(y, lambda = 3.56)
lines(y, 50*prob, type = "b", lty = 2)
plot(y, prob, type = "b", lty = 2)
logLr <-function(m) sum(dpois(datar, m , log = TRUE))
lambda <- seq(2, 5, 0.1)
cbind(lambda, sapply(lambda, logLr))
plot(lambda, sapply(lambda, logLr), type = "l")
sum(dpois(datar, 3.56 , log = TRUE))
#観測データから推定されたλがどれだけ新しい観測データに当てはまるか

