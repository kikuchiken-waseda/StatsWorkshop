setwd("~/Downloads")
d <- read.csv("data3a.csv")

# 肥料なしを赤、肥料ありを青でプロット
plot(d$x, d$y, col = c("red", "blue")[d$f])

# モデル作成
fit.all <- glm(y ~ x + f, data = d, family = poisson)
# family にpoissonを指定すると、自動でリンク関数にlogが選ばれる
fit.all
logLik(fit.all)


# 下記のようにリンク関数を明記すれば別のリンク関数も使用できる
fit.all.id <- glm(y ~ x + f, data = d, family = poisson(link = identity))
fit.all.id

# 未知のx（花のサイズ）に対する予測を描画する
# 下準備1・・・5から25までの範囲で0.1刻みに数列を作成
# ・・・これが未知のxとなる
x.range <- seq(5, 25, by = 0.1)
x.range

# 下準備2・・・因子TとCをそれぞれ格納
ft <- factor("T", levels = levels(d$f))
fc <- factor("C", levels = levels(d$f))

# 未知のxに対する予測（リンク関数がlogの場合）
p.log.t <- predict(fit.all, data.frame(x = x.range, f = ft ), type="response")
p.log.t
p.log.c <- predict(fit.all, data.frame(x = x.range, f = fc ), type="response")

# 未知のxに対する予測（リンク関数がidentityの場合）
p.id.t <- predict(fit.all.id, data.frame(x = x.range, f = ft ), type="response")
p.id.c <- predict(fit.all.id, data.frame(x = x.range, f = fc ), type="response")

plot(d$x, d$y, xlim = c(5, 20), ylim = c(5, 20), col = c("red", "blue")[d$f])

# 対数リンク関数 & 肥料ありの予測値を 緑 で描画
lines(x.range, p.log.t, col = "green")
# 対数リンク関数 & 肥料なしの予測値を 黄 で描画
lines(x.range, p.log.c, col = "gray")

# 恒等リンク関数 & 肥料ありの予測値を 桃 で描画
lines(x.range, p.id.t, col = "pink")
# 恒等リンク関数 & 肥料なしの予測値を 紫 で描画
lines(x.range, p.id.c, col = "purple")

