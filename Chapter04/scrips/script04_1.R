setwd("~/Desktop/")
d <- read.csv("data4.csv")


# Nullモデルの残差逸脱度
fit.null <- glm(y ~ 1, data = d, family = poisson)
fit.null
logLik(fit.null)

# fモデルの残差逸脱度
fit.f <- glm(y ~ f, data = d, family = poisson)
fit.f
logLik(fit.f)

#xモデルの残差逸脱度
fit.x <- glm(y ~ x, data = d, family = poisson)
fit.x
logLik(fit.x)

#x+fモデルの残差逸脱度
fit.xf <- glm(y ~ x + f, data = d, family = poisson)
fit.xf
logLik(fit.xf)

#フルモデルは、データ一つ一つに対し、その観測値をλとする。
#なので、フルモデルの最大対数尤度は、
#pi(yi|λi) [i=1,2,3...50, かつλi=yiとする] の積と等しい
log.full <- sum(log(dpois(d$y, lambda=d$y)))
log.full
-2*log.full  #逸脱度(これがデータセットにとっての最小逸脱度)
