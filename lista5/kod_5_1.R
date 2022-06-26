library(mvtnorm)
library(nlme)
corr<- corCompSymm(form=~1|id)
wei  <-  varIdent(form=~1)

generate_X <- function(n, k, p){
  matrix(rnorm(n*k*(p-1), 0, 1/sqrt(n*k)),  n*k, p-1)
}

generate_sigma <- function(n, k, p){
  sigma <- matrix(rep(ro, k^2), k, k)
  diag(sigma) <- rep(1, k)
  sigma <- sigma  * gama^2
}

generate_Y <-  function(n, k, p,  X, sigma){
  Y <- as.vector(sapply(seq(n), function(i){
    Xbeta <-  matrix(1, k,  p)
    Xbeta[,2:p] <- X[seq((i-1)*k+1,i*k), ]
    Xbeta <-  Xbeta%*%beta
    rmvnorm(1, Xbeta, sigma)}))
  id <-  as.vector(sapply(seq(n), function(i)(rep(i, k))))
  Tvec <-  rep(seq(k), n)
  data.frame(Y, id, Tvec, X0=rep(1,  n*k), X)
}

cov_beta <- function(sigma_est1, n=20, k=3, p=4, dataf){
  id <- seq(n)
  res1 <- matrix(rep(0), p, p)
  for(i in id){
    x <- dataf[which(dataf$id==i),4:(p+3)]
    res1 <- res1 + t(as.matrix(x))%*%sigma_est1%*%as.matrix(x)
  }
  solve(res1)
}

generate_estimators <- function(n, k, p,  X, sigma){
  dataf <- generate_Y(n, k, p,  X, sigma)
  mod1 <- gls(Y~.-id -Tvec -X0, dataf, corr, wei, method="REML")
  sigma_est <-  getVarCov(mod1)
  s <- solve(sigma_est)
  res2 <- rep(0, p)
  for(i in seq(n)){
    x <- dataf[dataf$id==i,4:(p+3)]
    res2 <- res2 + t(x)%*%s%*%dataf$Y[dataf$id==i]
  }
  cov_beta_est <-  cov_beta(s, n, k, p, dataf)
  beta_est <- cov_beta_est%*%res2
  est_gamma  <- sqrt(diag(sigma_est)[1])
  est_ro <-  sigma_est[1,2]/diag(sigma_est)[1]
  real_beta_cov <- cov_beta(solve(sigma), n, k, p, dataf)
  c(beta_est, est_gamma, est_ro, real_beta_cov[1,1], real_beta_cov[2,2])
}

generate_results <- function(n, k, p, times=500){
  X <-  generate_X(n=n, k=k, p=p)
  sigma <-  generate_sigma(n, k, p)
  sapply(rep(n, times), generate_estimators, k, p,  X, sigma)
}

set.seed(128)
ro <- 0.3
gama <- 2
beta <- c(0, 3, 3, 0)


# Zad 2
par(mfrow=c(2,2))

z2 <- generate_results(n=20, k=3, p=4)
hist(z2[1,], main = "Histogram b0", freq=F, ylab = 'gęstość', xlab = "estymatory b0 dla n=20, k=3, p=4, method = 'REML'", ylim=c(0, 1.5), breaks = seq(-1.4, 1.2, 0.1))
lines(seq(-1.5, 1.2, 0.001), dnorm(seq(-1.5, 1.2, 0.001), 0, sqrt(z2[7,1])), col=2)
hist(z2[2,], main = "Histogram b1", freq=F, ylab = 'gęstość', xlab = "estymatory b1 dla n=20, k=3, p=4, method = 'REML'", breaks = seq(-2, 9, .2))
lines(seq(-2, 9, 0.01), dnorm(seq(-2, 9, 0.01), 3, sqrt(z2[8,1])), col=2)
hist(z2[5,], main = "Histogram gamma", freq=F, ylab = 'gęstość', xlab = "estymatory gamma dla n=20, k=3, p=4, method = 'ML'", breaks = seq(1.4, 2.7,  0.05))
hist(z2[6,], main = "Histogram ro", freq=F, ylab = 'gęstość', xlab = "estymatory ro dla n=20, k=3, p=4, method = 'ML'", breaks = seq(-.3, .9, 0.05))

# Zad 3
z3 <-  generate_results(n=100, k=3, p=4)
hist(z3[1,], main = "Histogram b0", freq=F, ylab = 'gęstość', xlab = "estymatory b0 dla n=100, k=3, p=4, method = 'REML'", breaks = seq(-.6, .6, 0.05))
lines(seq(-1.2, 1.2, 0.001), dnorm(seq(-1.2, 1.2, 0.001), 0, sqrt(z3[7,1])), col=2)
hist(z3[2,], main = "Histogram b1", freq=F, ylab = 'gęstość', xlab = "estymatory b1 dla n=100, k=3, p=4, method = 'REML'", breaks = seq(-5, 10, .2))
lines(seq(-5, 10, .01), dnorm(seq(-5, 10, .01), 3, sqrt(z3[8,1])), col=2)
hist(z3[5,], main = "Histogram gamma", freq=F, ylab = 'gęstość', xlab = "estymatory gamma dla n=100, k=3, p=4, method = 'REML'", breaks = seq(1.7, 2.3,  0.01))
hist(z3[6,], main = "Histogram ro", freq=F, ylab = 'gęstość', xlab = "estymatory ro dla n=100, k=3, p=4, method = 'REML'", breaks = seq(0.05, .5, 0.01))

# Zad 4
z4 <- generate_results(n=100, k=15, p=4)
hist(z4[1,], main = "Histogram b0", freq=F, ylab = 'gęstość', xlab = "estymatory b0 dla n=100, k=15, p=4, method = 'REML'", breaks = seq(-.4, .4, 0.01))
lines(seq(-1.2, 1.2, 0.001), dnorm(seq(-1.2, 1.2, 0.001), 0, sqrt(z4[7,1])), col=2)
hist(z4[2,], main = "Histogram b1", freq=F, ylab = 'gęstość', xlab = "estymatory b1 dla n=100, k=15, p=4, method = 'REML'", breaks = seq(-3, 10, .2))
lines(seq(-3, 10, .01), dnorm(seq(-3, 10, .01), 3, sqrt(z4[8,1])), col=2)
hist(z4[5,], main = "Histogram gamma", freq=F, ylab = 'gęstość', xlab = "estymatory gamma dla n=100, k=15, p=4, method = 'REML'", breaks = seq(1.8, 2.2,  0.01))
hist(z4[6,], main = "Histogram ro", freq=F, ylab = 'gęstość', xlab = "estymatory ro dla n=100, k=15, p=4, method = 'REML'", breaks = seq(.2, .45, 0.01))

# Zad 5 
z5 <- generate_results(n=100, k=20, p=4)
hist(z5[1,], main = "Histogram b0", freq=F, ylab = 'gęstość', xlab = "estymatory b0 dla n=100, k=20, p=4, method = 'REML'", breaks = seq(-.37, .37, 0.01))
lines(seq(-1.2, 1.2, 0.001), dnorm(seq(-1.2, 1.2, 0.001), 0, sqrt(z5[7,1])), col=2)
hist(z5[2,], main = "Histogram b1", freq=F, ylab = 'gęstość', xlab = "estymatory b1 dla n=100, k=20, p=4, method = 'REML'", breaks = seq(-4, 9, .2))
lines(seq(-4, 10, .01), dnorm(seq(-4, 10, .01), 3, sqrt(z5[8,1])), col=2)
hist(z5[5,], main = "Histogram gamma", freq=F, ylab = 'gęstość', xlab = "estymatory gamma dla n=100, k=3, p=4, method = 'REML'", breaks = seq(1.8, 2.2,  0.01))
hist(z5[6,], main = "Histogram ro", freq=F, ylab = 'gęstość', xlab = "estymatory ro dla n=100, k=20, p=4, method = 'REML'", breaks = seq(.148, .42, 0.01))

#####
#  Zadanie 6

generate_estimators6 <- function(n, k, p,  X, sigma){
  dataf <- generate_Y(n, k, p,  X, sigma)
  mod1 <- gls(Y~.-id -Tvec -X0, dataf, corr, wei, method="ML")
  sigma_est <-  getVarCov(mod1)
  s <- solve(sigma_est)
  res2 <- rep(0, p)
  for(i in seq(n)){
    x <- dataf[dataf$id==i,4:(p+3)]
    res2 <- res2 + t(x)%*%s%*%dataf$Y[dataf$id==i]
  }
  cov_beta_est <-  cov_beta(s, n, k, p, dataf)
  beta_est <- cov_beta_est%*%res2
  est_gamma  <- sqrt(diag(sigma_est)[1])
  est_ro <-  sigma_est[1,2]/diag(sigma_est)[1]
  real_beta_cov <- cov_beta(solve(sigma), n, k, p, dataf)
  c(beta_est, est_gamma, est_ro, real_beta_cov[1,1], real_beta_cov[2,2])
}

generate_results6 <- function(n, k, p, times=500){
  X <-  generate_X(n=n, k=k, p=p)
  sigma <-  generate_sigma(n, k, p)
  sapply(rep(n, times), generate_estimators6, k, p,  X, sigma)
}

# Zad 2
zz2 <-  generate_results6(n=20, k=3, p=4)
hist(zz2[1,], main = "Histogram b0", freq=F, ylab = 'gęstość', xlab = "estymatory b0 dla n=20, k=3, p=4, method = 'ML'", ylim=c(0, 1.5), breaks = seq(-1.2, 1.2, 0.1))
lines(seq(-1.2, 1.2, 0.001), dnorm(seq(-1.2, 1.2, 0.001), 0, sqrt(zz2[7,1])), col=2)
hist(zz2[2,], main = "Histogram b1", freq=F, ylab = 'gęstość', xlab = "estymatory b1 dla n=20, k=3, p=4, method = 'ML'", breaks = seq(-2, 8, .2))
lines(seq(-2, 8, 0.01), dnorm(seq(-2, 8, 0.01), 3, sqrt(zz2[8,1])), col=2)
hist(zz2[5,], main = "Histogram gamma", freq=F, ylab = 'gęstość', xlab = "estymatory gamma dla n=20, k=3, p=4, method = 'ML'", breaks = seq(1.4, 2.7,  0.05))
hist(zz2[6,], main = "Histogram ro", freq=F, ylab = 'gęstość', xlab = "estymatory ro dla n=20, k=3, p=4, method = 'ML'", breaks = seq(-.3, .9, 0.05))

# Zad 3
zz3 <-generate_results6(n=100, k=3, p=4)
hist(zz3[1,], main = "Histogram b0", freq=F, ylab = 'gęstość', xlab = "estymatory b0 dla n=100, k=3, p=4, method = 'ML'", ylim=c(0, 3), breaks = seq(-.55, .5, 0.05))
lines(seq(-1.2, 1.2, 0.001), dnorm(seq(-1.2, 1.2, 0.001), 0, sqrt(zz3[7,1])), col=2)
hist(zz3[2,], main = "Histogram b1", freq=F, ylab = 'gęstość', xlab = "estymatory b1 dla n=100, k=3, p=4, method = 'ML'", breaks = seq(-3, 10, .2))
lines(seq(-3, 10, .01), dnorm(seq(-3, 10, .01), 3, sqrt(zz3[8,1])), col=2)
hist(zz3[5,], main = "Histogram gamma", freq=F, ylab = 'gęstość', xlab = "estymatory gamma dla n=100, k=3, p=4, method = 'ML'", breaks = seq(1.7, 2.3,  0.01))
hist(zz3[6,], main = "Histogram ro", freq=F, ylab = 'gęstość', xlab = "estymatory ro dla n=100, k=3, p=4, method = 'ML'", breaks = seq(.1, .5, 0.01))


# Zad 4
zz4 <-generate_results6(n=100, k=15, p=4)
hist(zz4[1,], main = "Histogram b0", freq=F, ylab = 'gęstość', xlab = "estymatory b0 dla n=100, k=15, p=4, method = 'ML'", breaks = seq(-.5, .5, 0.01))
lines(seq(-1.2, 1.2, 0.001), dnorm(seq(-1.2, 1.2, 0.001), 0, sqrt(zz4[7,1])), col=2)
hist(zz4[2,], main = "Histogram b1", freq=F, ylab = 'gęstość', xlab = "estymatory b1 dla n=100, k=15, p=4, method = 'ML'", breaks = seq(-3, 10, .2))
lines(seq(-3, 10, .01), dnorm(seq(-3, 10, .01), 3, sqrt(zz4[8,1])), col=2)
hist(zz4[5,], main = "Histogram gamma", freq=F, ylab = 'gęstość', xlab = "estymatory gamma dla n=100, k=15, p=4, method = 'ML'", breaks = seq(1.8, 2.2,  0.01))
hist(zz4[6,], main = "Histogram ro", freq=F, ylab = 'gęstość', xlab = "estymatory ro dla n=100, k=15, p=4, method = 'ML'", breaks = seq(.2, .48, 0.01))

# Zad 5 
zz5 <-generate_results6(n=100, k=20, p=4)
hist(zz5[1,], main = "Histogram b0", freq=F, ylab = 'gęstość', xlab = "estymatory b0 dla n=100, k=20, p=4, method = 'ML'", breaks = seq(-.4, .4, 0.01))
lines(seq(-1.2, 1.2, 0.001), dnorm(seq(-1.2, 1.2, 0.001), 0, sqrt(zz5[7,1])), col=2)
hist(zz5[2,], main = "Histogram b1", freq=F, ylab = 'gęstość', xlab = "estymatory b1 dla n=100, k=20, p=4, method = 'ML'", breaks = seq(-3, 10, .2))
lines(seq(-3, 10, .01), dnorm(seq(-3, 10, .01), 3, sqrt(zz5[8,1])), col=2)
hist(zz5[5,], main = "Histogram gamma", freq=F, ylab = 'gęstość', xlab = "estymatory gamma dla n=100, k=3, p=4, method = 'ML'", breaks = seq(1.8, 2.2,  0.01))
hist(zz5[6,], main = "Histogram ro", freq=F, ylab = 'gęstość', xlab = "estymatory ro dla n=100, k=20, p=4, method = 'ML'", breaks = seq(.2, .4, 0.01))

biases  <-  function(z){
  vec <- abs(z[1:6,]-c(0, 3, 3, 0, 2,  0.3))
  mvec<-sapply(seq(500), function(i){max(vec[1:4,  i])})
  c(b0_bias = mean(vec[1,]), b1_bias = mean(vec[2,]), b2_bias = mean(vec[3,]), b3_bias = mean(vec[4,]), gamma_bias = mean(vec[5,]), ro_bias = mean(vec[6,]), beta_sup = mean(mvec))
}

biases(z2)
biases(z3)
biases(z4)
biases(z5)
biases(zz2)
biases(zz3)
biases(zz4)
biases(zz5)
