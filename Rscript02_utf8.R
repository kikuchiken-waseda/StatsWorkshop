setwd("~/Desktop")
data01 <- rpois(50,3.5)
  #平均3.5でであるポアソン乱数を発生させた、50個からなるデータを作成
print(data01)
summary(data01)
hist(data01, breaks = seq(-0.5, 9.5, 1))
  #data01のヒストグラムを作成 (人・試行により異なる)
  #何回か試すと、試行ごとの「ばらつき」の加減が視覚化されてよく分かる


##教科書 2.2 p18-21##
y <- 0:9
prob3.5 <- dpois(y, lambda = 3.5)
cbind(y, prob3.5)
lines(y, 50*prob3.5, type ="b", lty=1)
  #data01のヒストグラムに、λ=3.5のポアソン分布を重ねる


##教科書 2.4 p24-26 最尤推定##
sum(dpois(data01, 3.5, log = TRUE))
#data01に対する、λ=3.5のポアソン分布の対数尤度を計算する

prob2 <- dpois(y, lambda = 2)
lines(y, 50*prob2, type ="l", lty=2, col="red")
  #data01のヒストグラムに、λ=2のポアソン分布を重ねる
sum(dpois(data01, 2, log = TRUE))
  #data01に対する、λ=2のポアソン分布の対数尤度を計算する

prob2.5 <- dpois(y, lambda = 2.5)
lines(y, 50*prob2.5, type ="l", lty=2, col="orange")
sum(dpois(data01, 2.5, log = TRUE))

prob3 <- dpois(y, lambda = 3)
lines(y, 50*prob3, type ="l", lty=2, col="yellow")
sum(dpois(data01, 3, log = TRUE))

prob4 <- dpois(y, lambda = 4)
lines(y, 50*prob4, type ="l", lty=2, col="green")
sum(dpois(data01, 4, log = TRUE))

prob4.5 <- dpois(y, lambda = 4.5)
lines(y, 50*prob4.5, type ="l", lty=2, col="blue")
sum(dpois(data01, 4.5, log = TRUE))

prob5 <- dpois(y, lambda = 5)
lines(y, 50*prob5, type ="l", lty=2, col="purple")
sum(dpois(data01, 5, log = TRUE))
#おそらく、λ=3~4くらいがヒストグラムと一致しそうに見える
#その「一致しそう」な度合いをを定量的に評価するための指標が対数尤度


##教科書 2.4 最尤推定量の計算・推定 p27-28##
logL <- function(m) sum(dpois(data01, m, log = TRUE))
  #logLは、変数mにλを代入すると、そのλのdata01に対する対数尤度を計算できる関数
lambda <- seq(2, 6, 0.5)
cbind(lambda, sapply(lambda, logL))
plot(lambda, sapply(lambda, logL), type = "b")
  #様々なλの値を取った場合の、data01に対する対数尤度がわかる

data02 <- rpois(50,4.5)
logL02 <- function(m) sum(dpois(data02, m, log = TRUE))
lines(lambda, sapply(lambda, logL02), type = "b", lty=2)
  #別の平均値をもつ別のデータでは、対数尤度のカーブは異なる

lambda <- seq(2, 6, 0.02)
  #lambdaは、0.5刻みでは荒すぎるので、もっと細かい目盛りに直す
max_est <- lambda[sapply(lambda, logL) == max(sapply(lambda, logL))]
print(max_est)  #data01の最尤推定値を計算し、出力する
#57行目で出力したグラフと見比べると、max_estは極大値のように見える

max_est02 <- lambda[sapply(lambda, logL02) == max(sapply(lambda, logL02))]
print(max_est02)  #data02の最尤推定値を計算し、出力する


##教科書 2.4.1 p29-30 データによる最尤推定値のばらつき##
loop01 <- function(n){
  x <- c()
  for(i in 1:n){
    data01 <- rpois(50,3.5)
    logL <- function(m) sum(dpois(data01, m, log = TRUE))
    lambda <- seq(2, 6, 0.1)
    max_est <- lambda[sapply(lambda, logL) == max(sapply(lambda, logL))]
    x <- c(x, max_est)
  }
  return(x)
}
#ポアソン乱数により、50個の整数からなるデータをnセット作成する
#それぞれのデータセットに対し、最尤推定値を計算し、出力する
#一つの引数nをもち、上記のプロセスを実行する関数がloop01(n)である

loop_result_10 <- loop01(10)
print(loop_result_10)
  #10セット作った場合の、それぞれのデータセットに対する最尤推定値を表示する
hist(loop_result_10, breaks = seq(1.95,5.05, 0.1))
  #10個の最尤推定値のヒストグラム
  #91・93行目を何度か実行すると、その都度ヒストグラムの形は大きく変わる
  #意外と、3.5(ポアソン乱数で指定した平均値)にはならないケースが多いと分かる
  #いちデータセットの抱えるずれは、かなり大きい可能性があると分かる

loop_result_100 <- loop01(100)
hist(loop_result_100, breaks = seq(1.95,5.05, 0.1))
  #100セットの場合

loop_result_1000 <- loop01(1000)
hist(loop_result_1000, breaks = seq(1.95,5.05, 0.1))
  #1000セットの場合
  #何度か試行すると、その度にヒストグラムの形状が変わる
  #最頻値は3.3-3.8くらいだが、試行のたびのばらつきが大きい

loop_result_5000 <- loop01(5000)
hist(loop_result_5000, breaks = seq(1.95,5.05, 0.1))
  #5000セットの場合
  #何度か試行しても、ヒストグラムの形状に大きな変化はあまりなく、釣鐘型を保つ
  #最頻値は3.4-3.6あたりに収束する

