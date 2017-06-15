bias <- function(lambda.true, sample.size){
  sample.rpois <- rpois(sample.size, lambda.true)
  fit <- glm(sample.rpois~1, family=poisson) 
  lambda.estimated <- exp(coef(fit))
  likelihood.mean <- mean(sapply(1:200, function(i){sum(log(dpois(rpois(N, lambda.true), lambda.estimated)))}))
  logLik(fit) - likelihood.mean
}

N <- 100
lambda.true <- 2
bias.sampled <- sapply(1:1000, function(i) bias(lambda.true, N))
mean(bias.sampled)
hist(bias.sampled, breaks = 40)


d <- read.csv("data3a.csv")
fit1 <- glm(y~1,data = d, family = poisson)
fit2 <- glm(y~x,data = d, family = poisson)
fit3 <- glm(y~x+f,data = d, family = poisson)
fit1
fit2
fit3
  sample.rpois <- rpois(50, 8)
fit <- glm(sample.rpois~1, family=poisson) 
lambda.estimated <- exp(coef(fit))
lambda.estimated
