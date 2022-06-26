# 1


# 2
G <- matrix(c(1.0, 0.1, 0.1, 1.0), 2, 2, byrow = T)
beta <- c(-0.3,2.1)
Yi <- c(1.47,3.05)
Xi <- matrix(c(1.0, -0.47, 1.0, 1.11), 2, 2, byrow=T)
gamma <- matrix(c(3.9, 0, 0, 3.9), 2, 2)

mu<- G%*%t(Xi)%*%gamma%*%(Yi-Xi%*%beta)
mu <-  solve(Xi)%*%(Yi-Xi%*%beta)
#3
beta <- c(0.91, 0.12,  -0.23)
cov_b <- matrix(c(1.851,  0.8263,  0.1376, 
0.8263,  2.2614,  -0.4968,
0.1376,  -0.4968,  1.8876),3,3)
L <- matrix(c(0, 1,  0,  0, 1, 1), 2, 3, byrow=T)

Wald_test(L, beta, cov_b)
#4



#5
X1 <- matrix(c(0.01,	0.06,-0.54,	-0.34), byrow=T,2,2)
X2 <- matrix(c(-0.05,  	-0.8  ,
               -0.09,  	-2.59  ),2, 2, byrow=T)
Y1 <- c(1.96,
        0.43)
Y2 <- c(0.81,
        0.86)
sigma <- matrix(c(2,0, 0, 2), 2,2)
G <- matrix(c(1, 0, 0, 1), 2, 2)

s <- solve(X1%*%G%*%t(X1) + sigma)
res1 <- matrix(rep(0), 2, 2)
res2 <- rep(0, 2)

  x <- X1
  res1 <- res1 + t(as.matrix(x))%*%s%*%as.matrix(x)
  res2 <- res2 + t(x)%*%s%*%Y1
  x <- X2
  s <- solve(X2%*%G%*%t(X2) + sigma)
  res1 <- res1 + t(as.matrix(x))%*%s%*%as.matrix(x)
  res2 <- res2 + t(x)%*%s%*%Y2

round(solve(res1)%*%res2,3)
#6
stat <-  2*( 13.92  - 	10.98)
p_value <- 0.5*(1-pchisq(stat, 3)) + 0.5*(1-pchisq(stat, 2))



#7




#8




#9
kor <- matrix(c(1, 0.4, 0.4, 1), 2, 2)
X1 <- matrix(c(-0.74 ,  	-0.01  ,
               1.11  , 	0.11  ), 2, 2, byrow=T)
X2 <- matrix(c(1.52 , 	-0.46  ,
               0.99,  	-0.05  ), 2, 2, byrow=T)
Y1  <- c(-0.01,
         1.21)
Y2 <- c(0.27,
        0.94)

  s <- solve(kor)
  res1 <- matrix(rep(0), 2, 2)
  res2 <- rep(0, 2)
  
    x <- X1
    res1 <- res1 + t(as.matrix(x))%*%s%*%as.matrix(x)
    res2 <- res2 + t(x)%*%s%*%Y1
    x <- X2
    res1 <- res1 + t(as.matrix(x))%*%s%*%as.matrix(x)
    res2 <- res2 + t(x)%*%s%*%Y2

 round( solve(res1)%*%res2, 3)


#10
stat <- 2*(84.52  -  	55.79)
1-pchisq(stat, 34)


#11




#12



#13



