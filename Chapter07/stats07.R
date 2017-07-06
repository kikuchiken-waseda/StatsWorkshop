# localのデータを使用（自分用）
#setwd("~/Desktop/StatsWorkshop/Chapter07")
#d <- read.csv("data.csv")

# Web上のデータをダウンロードして使用
d <- read.csv("http://hosho.ees.hokudai.ac.jp/~kubo/stat/iwanamibook/fig/glmm/data.csv",
              as.is = TRUE)


### データの可視化 ###


# plotで重なった点を表現するのに必要なライブラリのインポート
install.packages("psych")
library(psych)

# 重なった点を表現できるplot
par(family = "HiraKakuProN-W3") # 日本語対応
plot(jitter(d$x,0.3),jitter(d$y,0.3), xlab = "葉数", ylab = "生存種子数")

# 通常のplot
#plot(d$x,d$y)


### 真のモデルの描画（オリジナル）###


# 線形混合モデルを用いたモデリングに必要なライブラリのインポート
install.packages("glmmML")
library("glmmML")

anal.glmm <- glmmML(cbind(y, N - y) ~ x, data = d, family = binomial, cluster = id)

# 生存確率の算出
calcProb <- function(x, b, r){
  1.0 / (1.0 + exp(-1 * (b[1] + b[2] * x + r)))
}

# x の範囲（2 ～ 6）を 100分割
xx <- seq(min(d$x), max(d$x), length = 100)

coef.glmm <- anal.glmm$coefficients

# 個体差 r = 0 の葉数と生存種子数との予測線
lines(xx, max(d$N) * calcProb(xx, coef.glmm, 0))


### GLMによるモデリング（6章の手法） ###

# logit(種子の生存確率) = 切片 + 係数 * 葉の数

anal.glm <- glm(data = d, cbind(y, N - y) ~ x, family = "binomial")

summary(anal.glm)

coef.glm = anal.glm$coefficients

lines(xx, max(d$N) * calcProb(xx, coef.glm, 0)) # 真のモデルとは異なる

### なぜこのようなことが起きるのか？ ###

### 生存種子数ごとの個体数の分布 ###

N = 8
y <- 0:N

plot(y, table(d[d$x == 4,]$y), xlab="生存種子数", ylab="個体数") # 二項分布ではなさそう

# スライドに戻る





