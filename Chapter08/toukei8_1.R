N <- 8
data <- c(4,3,4,5,5,2,3,1,4,0,1,5,5,6,5,4,4,5,3,4)
logL <- function(m) sum(dbinom(data, N, m, log = TRUE))

qm <- sum(data)/(N*length(data))
qm

grid <- (0:100)/100
grid.logL <- sapply(grid,logL)
grid.map <- list(x = grid, y = grid.logL)
plot(grid.map)

q <- seq(0.01, 0.99, by=0.01) 
lv <- sapply(q,logL)
plot(q,lv)

furafura <- function(qp0){
  pv <- numeric(100)
  for(i in 1:100){
    pv[i] <- q[qp0]
    lv0 <- lv[qp0]
    qp1 <- qp0 +sample(c(1,-1),1)
    lv1 <- lv[qp1]
    if(lv1 > lv0){
      qp0 <- qp1
    }
  }
  pv
}

pv1 <- furafura(30)
pv2 <- furafura(60)
matplot(cbind(pv1,pv2),type="l",lwd=2)

MH <- function(iter){
  pv <- numeric(iter)
  for(i in 1:iter){
    pv[i] <- q[qp0]
    lv0 <- lv[qp0]
    qp1 <- qp0 +sample(c(1,-1),1)
    lv1 <- lv[qp1]
    if(lv1 > lv0){
      qp0 <- qp1
    }else{
      r <- exp(lv1-lv0) #対数尤度比を尤度比に
      if(runif(1) < r) qp0 <- qp1
    }
  }
  pv
}
qp0 <- 30
pv1 <- MH(100)
plot(pv1,type="l")

plotfunc <- function(pv1){
  #デフォルトを保存
  def.par <- par(no.readonly = TRUE)
  nf <- layout(matrix(c(1,2),2,2,byrow = TRUE), c(3,1), c(1,3), TRUE)
  
  # plot1
  plot(pv1, type="l",xlab="MCMC step 数", ylab="成功確率 p")
  
  #plot2
  par(mai = c(0.85, 0, 0.68, 0.35))
  hist1 <- hist(pv1, plot=FALSE)
  barplot(hist1$count, horiz=TRUE, space=0)
  
  #もとにもどす
  par(def.par)
}
pv1 <- MH(100)
plotfunc(pv1)

pv1 <- MH(1000)
plotfunc(pv1)

pv1 <- MH(10000)
plotfunc(pv1)
