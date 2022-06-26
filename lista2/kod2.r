#####
A <- structure(list(numeracy = c(6.6, 7.1, 7.3, 7.5, 7.9, 7.9, 8,
                                 8.2, 8.3, 8.3, 8.4, 8.4, 8.6, 8.7, 8.8, 8.8, 9.1, 9.1, 9.1, 9.3,
                                 9.5, 9.8, 10.1, 10.5, 10.6, 10.6, 10.6, 10.7, 10.8, 11, 11.1,
                                 11.2, 11.3, 12, 12.3, 12.4, 12.8, 12.8, 12.9, 13.4, 13.5, 13.6,
                                 13.8, 14.2, 14.3, 14.5, 14.6, 15, 15.1, 15.7), anxiety = c(13.8,
                                                                                            14.6, 17.4, 14.9, 13.4, 13.5, 13.8, 16.6, 13.5, 15.7, 13.6, 14,
                                                                                            16.1, 10.5, 16.9, 17.4, 13.9, 15.8, 16.4, 14.7, 15, 13.3, 10.9,
                                                                                            12.4, 12.9, 16.6, 16.9, 15.4, 13.1, 17.3, 13.1, 14, 17.7, 10.6,
                                                                                            14.7, 10.1, 11.6, 14.2, 12.1, 13.9, 11.4, 15.1, 13, 11.3, 11.4,
                                                                                            10.4, 14.4, 11, 14, 13.4), success = c(0L, 0L, 0L, 1L, 0L, 1L,
                                                                                                                                   0L, 0L, 1L, 0L, 1L, 1L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L,
                                                                                                                                   1L, 1L, 1L, 0L, 0L, 0L, 1L, 0L, 1L, 0L, 0L, 1L, 1L, 1L, 1L, 1L,
                                                                                                                                   1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L)), .Names = c("numeracy",
                                                                                                                                                                                                "anxiety", "success"), row.names = c(NA, -50L), class = "data.frame")
library(stats)
model <- glm(success~numeracy+anxiety, data=A, family="binomial")

# Zad 1 a
eta <- predict.glm(model, type="response")
eta <- eta*(1-eta)
S <- matrix(0*seq(length(eta)^2), length(eta), length(eta))
S<- sapply(seq(length(eta)), function(i){S[i, i]<-eta[i]
S[,i]
})
B<- matrix(seq(150), ncol = 3)
B[,1]<- rep(1, length(A[,1]))
B[,2]<- A$numeracy
B[,3] <- A$anxiety

solve(t(B)%*%S%*%B)

summary(model)$deviance
qchisq(0.95, 47)
qchisq(0.95, 2)



# Zad 1 b

print(model)

# Null deviance??

# Zad 1c

# 1d

summary(model)
model1 <- glm(success~numeracy+anxiety, data=A, family="binomial", method = "glm.fit", control =  list(epsilon = 0.1))
summary(model1)
model2 <- glm(success~numeracy+anxiety, data=A, family="binomial", method = "glm.fit", control =  list(epsilon = 0.01))
summary(model2)
model3 <- glm(success~numeracy+anxiety, data=A, family="binomial", method = "glm.fit", control =  list(epsilon = 0.001))
summary(model3)
model4 <- glm(success~numeracy+anxiety, data=A, family="binomial", method = "glm.fit", control =  list(epsilon = 0.000001))
summary(model4)
#####Zadanie 2
set.seed(132)
n  <- 400
p  <- 3
X <- matrix(rnorm(n*p, 0, 1/sqrt(n)), n, p)
pi0 <- sapply(seq(n), function(i){
  y <- 3*X[i, 1] + 3*X[i, 2] + 3*X[i, 3];
  return(exp(y)/(1+exp(y)))
})
Xx  <-  matrix(rep(1, n*(p+1)), n, (p+1))
Xx[, 2:(p+1)] <- X
#ponizej element losowy w zadniu:
Y <- rbinom(n, 1, pi0)

# asymptotyczna macierz kowariancji: 
pp0 <- exp(pi0)/(1+exp(pi0));
S0<-diag(pp0*(1-pp0))
kow1 <- t(Xx)%*%S0%*%Xx
kow1
#macierz Fishera ^
kow <- solve(kow1)
kow2t <- kow
# 2 a

Y500<- t(sapply(seq(n), function(i){
  rbinom(500, 1, pi0[i])
}))
i<-14
beta_matrix <- sapply(seq(500), function(i){
model <- glm(Y500[,i]~X[,1]+X[,2]+X[,3], family = "binomial") #intercept = FALSE
c(model$coefficients[2], model$coefficients[3], model$coefficients[4], model$deviance, model$coefficients[1])
})

hist(beta_matrix[1,], main = "beta1", freq = F, breaks = seq(-4, 10, 0.2), ylim = c(0, 0.3)) #asympt normal
lines(seq(-4, 10, 0.1), dnorm(seq(-4, 10, 0.1), 3, sqrt(kow[2, 2])), type = "l")
hist(beta_matrix[2,], main = "beta2", freq = F, breaks = seq(-4, 10, 0.2), ylim = c(0, 0.32))
lines(seq(-4, 10, 0.1), dnorm(seq(-4, 10, 0.1), 3, sqrt(kow[3,3])), type = "l")
hist(beta_matrix[3,], main = "beta3", freq = F, breaks = seq(-4, 10, 0.2), ylim = c(0, 0.33))
lines(seq(-4, 10, 0.1), dnorm(seq(-4, 10, 0.1), 3, sqrt(kow[4,4])), type = "l")
hist(beta_matrix[4,], main = "deviance", freq = F, breaks = seq(520, 555, 1)) 
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
kow2e <- solve(J)


#####Zadanie 3
set.seed(132)
n  <- 100
p  <- 3
X <- matrix(rnorm(n*p, 0, 1/sqrt(n)), n, p)
pi0 <- sapply(seq(n), function(i){
  y <- 3*X[i, 1] + 3*X[i, 2] + 3*X[i, 3];
  return(exp(y)/(1+exp(y)))
})
Xx  <-  matrix(rep(1, n*(p+1)), n, (p+1))
Xx[, 2:(p+1)] <- X
#ponizej element losowy w zadniu:
Y <- rbinom(n, 1, pi0)

# asymptotyczna macierz kowariancji: 
pp0 <- exp(pi0)/(1+exp(pi0));
S0<-diag(pp0*(1-pp0))
kow1 <- t(Xx)%*%S0%*%Xx
kow1
#macierz Fishera ^
kow <- solve(kow1)
kow3t <- kow
# 3a

Y500<- t(sapply(seq(n), function(i){
  rbinom(500, 1, pi0[i])
}))
i<-14
beta_matrix <- sapply(seq(500), function(i){
  model <- glm(Y500[,i]~X[,1]+X[,2]+X[,3], family = "binomial") #intercept = FALSE
  c(model$coefficients[2], model$coefficients[3], model$coefficients[4], model$deviance, model$coefficients[1])
})
c(min(beta_matrix[4,]), max(beta_matrix[4,]))
hist(beta_matrix[1,], main = "beta1", freq = F, breaks = seq(-3.8, 12.2, 0.2), ylim = c(0, 0.25), xlim = c(-4, 13)) #asympt normal
lines(seq(-3.8, 12.2, 0.1), dnorm(seq(-3.8, 12.2, 0.1), 3, sqrt(kow[2, 2])), type = "l")
hist(beta_matrix[2,], main = "beta2", freq = F, breaks = seq(-2.7, 10.1, 0.2), ylim = c(0, 0.28))
lines(seq(-2.7, 10.1, 0.2), dnorm(seq(-2.7, 10.1, 0.2), 3, sqrt(kow[3,3])), type = "l")
hist(beta_matrix[3,], main = "beta3", freq = F, breaks = seq(-4.7, 16.7, 0.2), ylim = c(0, 0.25))
lines(seq(-4.7, 16.7, 0.1), dnorm(seq(-4.7, 16.7, 0.1), 3, sqrt(kow[4,4])), type = "l")
hist(beta_matrix[4,], main = "deviance", freq = F, breaks = seq(92, 139, 1)) 
#lines(seq(520, 555, .5), dchisq(seq(520, 555, .5),  (500-p-1)))
#dorysuj gęstości!

# b - obciążenie:

mean(beta_matrix[1,])-3
mean(beta_matrix[2,])-3
mean(beta_matrix[3,])-3

# 3 c 
b0 <- beta_matrix[5,]
b1 <- beta_matrix[1,]
b2 <- beta_matrix[2,]
b3 <- beta_matrix[3,]
pi <- sapply(seq(n), function(i){mean(Xx[i, ]%*%t(matrix(c(b0, b1, b2, b3), byrow = F, ncol= p+1)))})
pp <- exp(pi)/(1+exp(pi));
S<-diag(pp*(1-pp))
J <- t(Xx)%*%S%*%Xx #macierz inf Fishera

#wyestymowana macierz kow3ariancji:
kow3e <- solve(J)
abs(diag(kow3e-kow3t))
abs(diag(kow2e-kow2t))
#####
# Zadanie 4
library(MASS)
Sigma <- matrix(rep(.3, p^2), p, p)
for(i in 1:p){
  Sigma[i,  i] <- 1}
#X <- mvrnorm(n, rep(0, p), Sigma)

#####Zadanie 4 - 2
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
c(min(beta_matrix[4,]), max(beta_matrix[4,]))
hist(beta_matrix[1,], main = "beta1", freq = F, breaks = seq(0, 2000, 50))#, ylim = c(0, 4)) #asympt normal
#lines(seq(-3, 6, 0.01), dnorm(seq(-3, 6, 0.01), 3, sqrt(kow[2, 2])), type = "l")
hist(beta_matrix[2,], main = "beta2", freq = F)#, breaks = seq(1, 5.5, 0.2), ylim = c(0, 3.55))
#lines(seq(1, 5.5, 0.01), dnorm(seq(1, 5.5, 0.01), 3, sqrt(kow[3,3])), type = "l")
hist(beta_matrix[3,], main = "beta3", freq = F)#, breaks = seq(2, 6, 0.2), ylim = c(0, 3.55))
#lines(seq(2, 6, 0.01), dnorm(seq(2, 6, 0.01), 3, sqrt(kow[4,4])), type = "l")
hist(beta_matrix[4,], main = "deviance", freq = F)#, breaks = seq(520, 555, 1)) 
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
kow

#####
#Zad 5

set.seed(132)
n  <- 400
p  <- 20
X <- matrix(rnorm(n*p, 0, 1/sqrt(n)), n, p)
pi0 <- sapply(seq(n), function(i){
  y <- 3*X[i, 1] + 3*X[i, 2] + 3*X[i, 3];
  return(exp(y)/(1+exp(y)))
})
Xx  <-  matrix(rep(1, n*(p+1)), n, (p+1))
Xx[, 2:(p+1)] <- X
#ponizej element losowy w zadniu:
Y <- rbinom(n, 1, pi0)

# asymptotyczna macierz kowariancji: 
pp0 <- exp(pi0)/(1+exp(pi0));
S0<-diag(pp0*(1-pp0))
kow1 <- t(Xx)%*%S0%*%Xx
kow1[1:6,  1:6]
#macierz Fishera ^
kow <- solve(kow1)
kow[1:6,  1:6]
# 2 a

Y500<- t(sapply(seq(n), function(i){
  rbinom(500, 1, pi0[i])
}))
i<-14
beta_matrix <- sapply(seq(500), function(i){
  model <- glm(Y500[,i]~X[,1]+X[,2]+X[,3]+X[,4]+X[,5]+X[,6]+X[,7]+X[,8]+X[,9]+X[,10]+X[,11]+X[,12]+X[,13]+X[,14]+X[,15]+X[,16]+X[,17]+X[,18]+X[,19]+X[,20], family = "binomial") #intercept = FALSE
  c(model$coefficients[2], model$coefficients[3], model$coefficients[4], model$deviance, model$coefficients[1])
})

hist(beta_matrix[1,], main = "beta1", freq = F, breaks = seq(-6, 12, 0.5), ylim = c(0, 0.25)) #asympt normal
lines(seq(-6, 12, 0.1), dnorm(seq(-6, 12, 0.1), 3, sqrt(kow[2, 2])), type = "l")
hist(beta_matrix[2,], main = "beta2", freq = F, breaks = seq(-6, 12, 0.5), ylim = c(0, 0.25))
lines(seq(-6, 12, 0.1), dnorm(seq(-6, 12, 0.1), 3, sqrt(kow[3,3])), type = "l")
hist(beta_matrix[3,], main = "beta3", freq = F, breaks = seq(-6, 12, 0.5), ylim = c(0, 0.25))
lines(seq(-6, 12, 0.1), dnorm(seq(-6, 12, 0.1), 3, sqrt(kow[4,4])), type = "l")
hist(beta_matrix[4,], main = "deviance", freq = F, breaks = seq(485, 545, 1)) 
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
solve(J)[1:6,  1:6]


#####Zadanie 3
set.seed(132)
n  <- 100
p  <- 20
X <- matrix(rnorm(n*p, 0, 1/sqrt(n)), n, p)
pi0 <- sapply(seq(n), function(i){
  y <- 3*X[i, 1] + 3*X[i, 2] + 3*X[i, 3];
  return(exp(y)/(1+exp(y)))
})
Xx  <-  matrix(rep(1, n*(p+1)), n, (p+1))
Xx[, 2:(p+1)] <- X
#ponizej element losowy w zadniu:
Y <- rbinom(n, 1, pi0)

# asymptotyczna macierz kowariancji: 
pp0 <- exp(pi0)/(1+exp(pi0));
S0<-diag(pp0*(1-pp0))
kow1 <- t(Xx)%*%S0%*%Xx
kow1[1:6,  1:6]
#macierz Fishera ^
kow <- solve(kow1)
kow[1:6,  1:6]
# 2 a

Y500<- t(sapply(seq(n), function(i){
  rbinom(500, 1, pi0[i])
}))
i<-14
beta_matrix <- sapply(seq(500), function(i){
  model <- glm(Y500[,i]~X[,1]+X[,2]+X[,3]+X[,4]+X[,5]+X[,6]+X[,7]+X[,8]+X[,9]+X[,10]+X[,11]+X[,12]+X[,13]+X[,14]+X[,15]+X[,16]+X[,17]+X[,18]+X[,19]+X[,20], family = "binomial") #intercept = FALSE
  c(model$coefficients[2], model$coefficients[3], model$coefficients[4], model$deviance, model$coefficients[1])
})
#c(min(beta_matrix[4,]), max(beta_matrix[4,]))
hist(beta_matrix[1,], main = "beta1", freq = F, breaks = seq(-7, 20, 0.5), ylim = c(0, 0.2)) #asympt normal
lines(seq(-7, 20, 0.1), dnorm(seq(-7, 20, 0.1), 3, sqrt(kow[2, 2])), type = "l")
hist(beta_matrix[2,], main = "beta2", freq = F, breaks = seq(-5, 15, 0.5), ylim = c(0, 0.2))
lines(seq(-5, 15, 0.1), dnorm(seq(-5, 15, 0.1), 3, sqrt(kow[3,3])), type = "l")
hist(beta_matrix[3,], main = "beta3", freq = F, breaks = seq(-8, 20, 0.5), ylim = c(0, 0.17))
lines(seq(-8, 20, 0.1), dnorm(seq(-8, 20, 0.1), 3, sqrt(kow[4,4])), type = "l")
hist(beta_matrix[4,], main = "deviance", freq = F, breaks = seq(75, 130, 1)) 
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
solve(J)[1:6,  1:6]