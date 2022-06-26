library(nlme)

#testy
#pomiary wielokrotne
#modele mieszane

#####
#Testy

test_single_beta <- function(beta, variance, alpha=0.05){
  statistic <- beta/variance
  half_interval <- qnorm(1-alpha/2)*variance
  p_value <- 2*(1 - pnorm(abs(statistic)))
  result <- statistic>= qnorm(1-alpha/2)
  round(c(stat = statistic, res = result, p_val = p_value, int_length =2*half_interval, int_from = beta-half_interval, int_to = beta + half_interval), 3)
}

test_mult_beta <- function(mod1, mod2, alpha = 0.05){
  statistic <- 2*abs(logLik(mod1) - loglin(mod2))
  df <- abs(length(mod1$coefficients) - length(mod2$coefficients))
  critical_value <- qchisq(1-alpha, df)
  p_value <-  1 -  pchisq(statistic, df)
  round(c(stat = statistic, res = statistic>=critical_value,  crit =  critical_value, p_val = p_value), 3)
}

Wald_test <-  function(L, beta, sigma, alpha=0.05){
  statistic <- t(L%*%beta)%*%solve(L%*%sigma%*%t(L))%*%(L%*%beta)
  df  <- length(L[,1])
  critical <- qchisq(1-alpha, df)
  p_value <-  1-pchisq(statistic, df)
  round(c(stat = statistic, res = statistic>=critical,  crit =  critical, p_val = p_value), 3)
}

single_Wald <- function(betai, Jii, alpha=0.05){
  statistic <- betai^2/Jii
  df  <- 1
  critical <- qchisq(1-alpha, df)
  p_value <-  1-pchisq(statistic, df)
  round(c(stat = statistic, res = statistic>=critical_value,  crit =  critical_value, p_val = p_value), 3)
}

#####
# GLS

#Przykład: 
gls(Y~.-id -Tvec -X0, dataf, corr, wei, method="REML")

cov_beta <- function(X, sigma, n=20, k=3, p=4){ #zczytaj z danych n k p 
  id <- unique(X$id)
  s <- solve(sigma)
  res1 <- matrix(rep(0), p, p)
  for(i in id){
    x <- X[which(X$id==i),4:(p+3)]
    res1 <- res1 + t(as.matrix(x))%*%s%*%as.matrix(x)
  }
  solve(res1)
}

beta <- function(X, sigma, n=20, k=3, p=4){ #zczytaj z danych n k p 
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

#####
# modele  mieszane

# lsoowy intercept - przykład:
m1 = lme(y~time,
         random = ~1|id,
         data = dane)
# losowy intercept i slope
m2 = lme(y~time,
         random = ~time|id, #~1+time|id
         data = dane)
#
random_intercept_test <- function(mod1, mod2,  alpha=0.05){
  l1 <- logLik(mod1)
  l2 <-  logLik(mod2)
  statistic <-  2*abs(l1-l2)
  crit <- 1-qchisq(1-2*alpha, 1)
  result <-  statistic>=crit
  p_value <- 0.5*(1-pchisq(statistic, 1)) + 0.5 * as.numeric(statistic>=0)
  round(c(stat =  statistic, crit = crit, result=result, p_value = p_value), 3)
}

random_slope_test <-  function(mod1, mod2,  alpha=0.05){
  l1 <- logLik(mod1)
  l2 <-  logLik(mod2)
  statistic <-  2*abs(l1-l2)
  p_value <- 0.5*(1-pchisq(statistic, 2)) + 0.5*(1-pchisq(statistic, 1))
  result <-  p_value<=alpha
  round(c(stat =  statistic, result=result, p_value = p_value), 3)
}
