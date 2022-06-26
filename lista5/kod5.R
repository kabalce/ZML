library(mvtnorm)
generate <-  function(n, k, p, beta = c(0, 3, 3, 0), ro = .3, gama= 2){
  X  <-  matrix(rnorm(n*k*(p-1), 0, 1/sqrt(n*k)),  n*k, p-1)
  sigma <- matrix(rep(ro, k^2), k, k)
  diag(sigma) <- rep(1, k)
  sigma <- sigma  * gama^2
  Y <- as.vector(sapply(seq(n), function(i){
    Xbeta <-  matrix(1, k,  p)
    Xbeta[,2:p] <- X[seq((i-1)*k+1,i*k), ]
    Xbeta <-  Xbeta%*%  beta
    rmvnorm(1, Xbeta, sigma)}))
  id <-  as.vector(sapply(seq(n), function(i)(rep(i, k))))
  Tvec <-  rep(seq(k), n)
  data.frame(Y, id, Tvec, X0=rep(1,  n*k), X)
}
library(nlme)
corr<- corCompSymm(form=~1|id)
wei  <-  varIdent(form=~1)
dataf <- generate(20,3,4)
mod1 <- gls(Y~.-id -Tvec -X0, dataf, corr, wei, method="REML")
sigma <- getVarCov(mod1)
beta <- function(X, sigma, n=20, k=3, p=4){
  id <- unique(X$id)
  s <- solve(sigma)
  res1 <- matrix(rep(0), p, p)
  res2 <- rep(0, p)
  for(i in id){
    x <- X[which(X$id==i),4:(p+3)]
    res1 <- res1 + t(as.matrix(x))%*%s%*%as.matrix(x)
    res2 <- res2 + t(x)%*%s%*%X$Y[which(X$id==i)]
  }
  solve(res1)%*%res2
}
cov_beta <- function(X, sigma, n=20, k=3, p=4){
  id <- unique(X$id)
  s <- solve(sigma)
  res1 <- matrix(rep(0), p, p)
  for(i in id){
    x <- X[which(X$id==i),4:(p+3)]
    res1 <- res1 + t(as.matrix(x))%*%s%*%as.matrix(x)
  }
  solve(res1)
}
estimated_beta <- beta(dataf, sigma)
est_cov_from_model <-  vcov(mod1)
est_cov <-  cov_beta(dataf, sigma)
est_gamma  <- sqrt(mean(diag(sigma)))
est_ro <-  mean(as.vector(sigma/(est_gamma^2))[c(2:4, 6:8)])

sup_ro <-  max(abs(as.vector(sigma/(est_gamma^2))[c(2:4, 6:8)] - .3))
sup_gamma <-  max(abs(sqrt(diag(sigma)) - 2))

sss <-  matrix(rep(0.3, 3^2), 3, 3)
diag(sss) <- rep(1, 3)
sss <- sss  *2^2
cov_beta_realSigma <-  cov_beta(dataf, sss)

max(abs(c(0, 3,  3, 0) - estimated_beta))
