# 3.1~3.3
d <- read.csv("http://hosho.ees.hokudai.ac.jp/~kubo/stat/iwanamibook/fig/poisson/data3a.csv")

#列の取得
d$x
d$y
d$f

#クラスの確認
class(d)
class(d$y)
class(d$x)
class(d$f)

#データフレームの概要を調べる
summary(d)

#散布図の作成(pchはプロット文字指定)
plot(d$x, d$y, pch = c(21, 19)[d$f])

#凡例の挿入
legend("topleft", legend = c("C","T"), pch = c(21, 19))
 
#箱ひげ図の作成
plot(d$f, d$y)
