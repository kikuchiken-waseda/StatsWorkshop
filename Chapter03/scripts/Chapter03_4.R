#図3.4
poisson1 <- function(x){exp(-1 + 0.4*x)}
poisson2 <- function(x){exp(-2 - 0.8*x)}
plot(poisson1, xlim=c(-4, 5), ylim=c(0, 2.5), lty=1, ann=F,)
text(4, 2.4, expression(paste("{", β[1],",", β[2], "}")), pos=2)
text(3, 2.4, "={-1, 0.4}", pos=1)
text(-1, 2.4,expression(paste("{", β[1], ",", β[2], "}")), pos=2)
text(-2, 2.4, "={-2, -0.8}", pos=1)
par(new=T)
plot(poisson2, xlim=c(-4, 5),ylim=c(0, 2.5), lty=2, xlab=expression(個体iの体サイズx[i]), ylab=expression(個体iのλ[i]), main=expression(paste(λ[i], "=", exp(paste(β[1], "+", β[2], x[i])), のグラフ)))
abline(v=0, lty=2)

#体サイズによる平均種子数の予測
fit <- glm(y ~ x, data=d, family = poisson)
summary(fit)

#最大対数尤度 
logLik(fit)


#データ可視化 図3.7
plot(d$x, d$y, pch = c(21, 19)[d$f],  main="λ = exp(1.29 + 0.0757x)")
xx <- seq(min(d$x), max(d$x), length = 100)
lines(xx, exp(1.29 + 0.0757 * xx), lwd = 2)

yy <- predict(fit, newdata = data.frame(x = xx), type="response")
lines(xx, yy, lwd = 2)


#説明変数が因子型の統計モデル
fit.f <- glm(y ~ f, data=d, family = poisson)

logLik(fit.f)
#前回のモデルより当てはまりが悪い