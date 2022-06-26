set.seed(132)
n  <- 100
p  <- 3
Sigma <- matrix(rep(.3, p^2), p, p)
for(i in 1:p){
  Sigma[i,  i] <- 1}
X <- mvrnorm(n, rep(0, p), Sigma)
pi0 <- sapply(seq(n), function(i){
  y <- 3*X[i, 1] + 3*X[i, 2] + 3*X[i, 3];
  return(exp(y)/(1+exp(y)))
})
Xx  <-  matrix(rep(1, n*(p+1)), n, (p+1))
Xx[, 2:4] <- X
#ponizej element losowy w zadniu:
Y <- rbinom(n, 1, pi0)

# asymptotyczna macierz kowariancji: 
pp0 <- exp(pi0)/(1+exp(pi0));
S0<-diag(pp0*(1-pp0))
kow1 <- t(Xx)%*%S0%*%Xx
#Fisher 
kow1
#macierz Fishera ^
kow <- solve(kow1)
kow

# 2 a

Y500<- t(sapply(seq(n), function(i){
  rbinom(500, 1, pi0[i])
}))
#Y500
i<-14
beta_matrix <- sapply(seq(500), function(i){
  model <- glm(Y500[,i]~X[,1]+X[,2]+X[,3], family = "binomial") #intercept = FALSE
  c(model$coefficients[2], model$coefficients[3], model$coefficients[4], model$deviance, model$coefficients[1])
})
mediana <- sapply(seq(5), function(i){ median(beta_matrix[i, ])})
err <- sapply(seq(5), function(i){beta_matrix[i, ]-mediana})
to_be_deleted <- sort(c(which(err[,1]>quantile(err[,1], .98)), which(err[,2]>quantile(err[,2], .98)), which(err[,3]>quantile(err[,3], .98))))
res <- c()
for(i in 1:500){
  control <- TRUE
  for(j in 1:length(to_be_deleted)){
    if(i==to_be_deleted[j]){
      control <- FALSE
    }
  }
  if(control){res <- c(res, i)}
}

beta_matrix_improved <- beta_matrix[, res]

hist(beta_matrix_improved[1,], main = "beta1", freq = F, breaks = seq(-4, 10, 1), ylim = c(0, .18)) #asympt normal
lines(seq(-4, 10, .1), dnorm(seq(-4, 10, .1), 3, sqrt(kow[2, 2])), type = "l")
hist(beta_matrix_improved[2,], main = "beta2", freq = F, breaks =  seq(-4, 10, 1), ylim = c(0, .23))
lines(seq(-4, 10, .1), dnorm(seq(-4, 10, .1), 3, sqrt(kow[3,3])), type = "l")
hist(beta_matrix_improved[3,], main = "beta3", freq = F, breaks = seq(-6, 15, 1), ylim = c(0, .2))
lines(seq(-6, 15, .1), dnorm(seq(-6, 15, .1), 3, sqrt(kow[4,4])), type = "l")
hist(beta_matrix_improved[4,], main = "deviance", freq = F, breaks = seq(105, 140, 1)) 
#lines(seq(520, 555, .5), dchisq(seq(520, 555, .5),  (500-p-1)))
#dorysuj gęstości!

# b - obciążenie:

mean(beta_matrix[1,])-3
mean(beta_matrix[2,])-3
mean(beta_matrix[3,])-3

# 2 c 
b0 <- beta_matrix[5,]
b1 <- beta_matrix[1,]
b2 <- beta_matrix[2,]
b3 <- beta_matrix[3,]
pi <- sapply(seq(n), function(i){mean(Xx[i, ]%*%t(matrix(c(b0, b1, b2, b3), byrow = F, ncol= p+1)))})
pp <- exp(pi)/(1+exp(pi));
S<-diag(pp*(1-pp))
J <- t(Xx)%*%S%*%Xx #macierz inf Fishera

#wyestymowana macierz kow3ariancji:
solve(J)
