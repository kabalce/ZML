require("nlme")
require("ggplot2")
dane<- read.csv("C:/Users/Michal Kos/Dropbox/moje/Dydaktyka/Zaawansowane modele liniowe/dane/swinie.csv")[,-1]
colnames(dane) = c("y",'time','id')

head(dane)

interaction.plot(as.numeric(dane[,2]),
                 dane[,3], dane[,1],
                 xlab="time", ylab="waga świń", col=c(1:10), legend=F) 

#dopasowanie modelu z losowym interceptem - funkcja lme()
m1 = lme(y~time,
         random = ~1|id,
         data = dane)
m1
summary(m1)

#Jak dobrze model jest dopasowany do danych
#�rednia dla populacji
abline(m1$coefficients$fixed, col= 'red', lwd = 3)

#�rednia dla ka�dej swini osobno
y_hat = predict(m1)
pred = cbind(y  = y_hat,
             dane[,-1], 
             fit = rep("fit",nrow(dane)))

interaction.plot(as.numeric(pred[,2]),
                 pred[,3], pred[,1],
                 xlab="time", ylab="waga świń", col=c(1:10), legend=F) 
abline(m1$coefficients$fixed, col= 'red', lwd = 3)


#�rednia dla ka�dej swini osobno ggplot2
dp = rbind(cbind(dane, 
                 fit=rep("real",nrow(dane)))
           ,pred)

colnames(dp) = c("y",'time','id',"fit")
qplot(time,y, facets = ~id, color = fit, data = dp)





#dopasowanie modelu z losowym interceptem i slopem - funkcja lme()
m2 = lme(y~time,
         random = ~time|id, #~1+time|id
         data = dane)
m2
summary(m2)

#Jak dobrze model jest dopasowany do danych
#�rednia dla populacji
interaction.plot(as.numeric(dane[,2]),
                 dane[,3], dane[,1],
                 xlab="time", ylab="waga �wi�", col=c(1:10), legend=F) 
abline(m2$coefficients$fixed, col= 'blue', lwd = 3)

#�rednia dla ka�dej swini osobno
y_hat = predict(m2)
pred = cbind(y  = y_hat,
             dane[,-1], 
             fit = rep("fit",nrow(dane)))

interaction.plot(as.numeric(pred[,2]),
                 pred[,3], pred[,1],
                 xlab="time", ylab="waga �wi�", col=c(1:10), legend=F) 
abline(m2$coefficients$fixed, col= 'blue', lwd = 3)


#�rednia dla ka�dej swini osobno ggplot2
dp = rbind(cbind(dane, 
                 fit=rep("real",nrow(dane)))
           ,pred)

colnames(dp) = c("y",'time','id',"fit")
qplot(time,y, facets = ~id, color = fit, data = dp)
