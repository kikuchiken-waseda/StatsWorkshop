####基礎的な操作を確認する####
setwd("~/Download")
load("data.Rdata")
print(data)
length(data)
#↑格納されているデータの数を表示
summary(data)
#↑四分位数、最小値、最大値、平均値を表示
hist(data, breaks = seq(-0.5, 9.5, 1))
#↑ヒストグラムを表示。seq(-0.5, 9.5, 1)によって、
# {-0.5, 0.5, 1.5, 2.5, ..., 8.5, 9.5}という数列を作る。
# break引数にこの数列を与えると、「-0.5より大きく0.5以下」
# 「0.5より大きく1.5以下」etcの区間ごとの頻度を積み上げた
# ヒストグラムが出力される。
var(data)
#↑標本分散を表示
sd(data)
#↑標本標準偏差を表示
sqrt(var(data))
#↑標本標準偏差を計算

####ポアソン分布のグラフを出力する####
y <- 0:9
prob <- dpois(y, lambda = 3.56)
#↑probオブジェクトに、λ=3.56のポワソン分布(確率変数yが、0<=y<=9の範囲)を格納
cbind(y, prob)
#↑表の形式でポアソン分布を出力
plot(y, prob, type="b", lty=2)
#↑グラフの形式でポアソン分布を出力
#plot(横軸, 縦軸, グラフの形式, 折線の書式)
#グラフの形式は、 type="b"だと「折れ線+丸」、p「丸」、l「線」、
# o「折れ線+丸(重なる)」、s「階段状」、h「ヒストグラム風」
#折線の書式は、lty=1で実線、2で破線、3で点線、4で一点鎖線etc
hist(data, breaks = seq(-0.5, 9.5, 1))
lines(y, 50*prob, type ="b", lty=2)
#↑上記二行を連続して実行すると、ヒストグラムとポワソン分布のグラフが重なる

####対数尤度とλの関係を調べる####
logL <- function(m) sum(dpois(data, m, log =TRUE))
#一変数関数を作成。変数mはλを入力するための変数。
# log=TRUEで、対数値での表示を指示。
#dpois(data, m, log =TRUE)はlog(p(yi|λ))の計算(i=1,2,...50)
lambda <- seq(2, 5, 0.1)
#lambdaに、{2, 2.1, 2.2, 2.3, ...4.9, 5.0}という数列を格納
plot(lambda, sapply(lambda, logL), type = "l")
#sapply(リストデータA,関数B)は、関数BにリストデータAを一括適用し適用し、
#結果を行列に返す(変換する)。logLの関数の引数mにlambdaを入力していった。

####乱数を発生させる####
data02 <- rpois(50, 3.5)
hist(data02, breaks = seq(-0.5, 9.5, 1))
data03 <- rpois(50, 3.5)
hist(data03, breaks = seq(-0.5, 9.5, 1))
#rpois関数では完全なランダムで乱数が発生させられているので、
# data02とdata03で手順は同じでも、格納されるデータは全く異なる
# ＝ヒストグラムの形状も異なる