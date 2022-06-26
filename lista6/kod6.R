library(nlme)
tab <- read.csv2('/home/kabalcer/Dokumenty/ZML/lista6/swinie2.csv', sep=',')
dane <- tab[which(tab$gat==1),2:4]
head(dane)
m1 <- lme(y~time,
         random = ~1|id,
         data = dane)

m2 <- lme(y~time,
         random = ~time|id, #~1+time|id
         data = dane)

y_hat1 = predict(m1)
pred1 = cbind(y  = y_hat1,
             dane[,-1], 
             fit = rep("fit",nrow(dane)))

y_hat2 = predict(m2)
pred2 = cbind(y  = y_hat2,
             dane[,-1], 
             fit = rep("fit",nrow(dane)))


interaction.plot(as.numeric(dane[,2]),
                 dane[,3], dane[,1],
                 xlab="time", ylab="waga ?wi?", col=c(1:10), legend=F, main="model z losowym interceptem") 
abline(m1$coefficients$fixed, col= 'red', lwd = 3)

interaction.plot(as.numeric(dane[,2]),
                 dane[,3], dane[,1],
                 xlab="time", ylab="waga ?wi?", col=c(1:10), legend=F, main="model z losowym interceptem i slopem") 
abline(m2$coefficients$fixed, col= 'blue', lwd = 3)

dp1 = rbind(cbind(dane, 
                 fit=rep("real",nrow(dane)))
           , pred1)
colnames(dp1) = c("y",'time','id',"fit")
qplot(time,y, facets = ~id, color = fit, data = dp, main="model z losowym interceptem")

dp2 = rbind(cbind(dane, 
                 fit=rep("real",nrow(dane)))
           ,pred2)
colnames(dp) = c("y",'time','id',"fit")
qplot(time,y, facets = ~id, color = fit, data = dp, main="model z losowym interceptem i slopem")

