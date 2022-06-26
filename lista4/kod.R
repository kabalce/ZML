## Zadanie 1

tab <- read.csv2('/home/kabalcer/Dokumenty/ZML/lista4/DebTrivedi.csv', sep=',')

## Zadanie 2:

f_function <- function(vec){log(.5+vec)}

hosp <- (tab$hosp>=3)*3 + tab$hosp*(tab$hosp<3)
numchrom <- (tab$hosp>=5)*5 + tab$hosp*(tab$hosp<5)
school <- as.numeric(tab$school<7) + as.numeric(tab$school==7)*2 + as.numeric(tab$school<11 & tab$school>7)*3 + as.numeric(tab$school==11)*4 + as.numeric(tab$school>11)*5

hist(tab$ofp, main = "histogram zm zależnej ofp", freq=F, breaks = seq(-1, 89, 1), ylim=c(0, 0.18))
points(seq(0, 80, 1), dpois(seq(0, 80, 1), mean(tab$ofp)), col="red", type="h")
#hist(f_function(tab$ofp))
boxplot(sapply(0:3, function(i){f_function(tab$ofp[which(hosp==i)])}), main =  "boxploty wg zmiennej hosp", names = c('0', '1', '2', '3 lub więcej'))
boxplot(sapply(0:5, function(i){f_function(tab$ofp[which(numchrom==i)])}), main = 'boxploty wg zmiennej numchron', names = c('0', '1', '2', '3', '4', '5 lub więcej'))
boxplot(sapply(1:5, function(i){f_function(tab$ofp[which(school==i)])}), main = 'boxploty wg zmiennej school', names = c('< 7', '7', '>11', '11', '> 11'))
boxplot(sapply(levels(tab$gender), function(gen){f_function(tab$ofp[which(tab$gender==gen)])}), main = 'boxploty wg zmiennej gender', names = levels(tab$gender))
boxplot(sapply(levels(tab$privins), function(gen){f_function(tab$ofp[which(tab$privins==gen)])}), main = 'boxploty wg zmiennej privins', names = levels(tab$privins))
boxplot(sapply(levels(tab$health), function(gen){f_function(tab$ofp[which(tab$health==gen)])}),  main = 'boxploty wg zmiennej health', names  = levels(tab$health))
#boxplot(sapply(1:5, function(i){f_function(tab$ofp[which(school==i)])}), main = 'boxploty dla Carycy', names = c('< 7', '7', '>11', '11', '> 11'))

## Zadanie 3:
library(MASS)
library(pscl)
poiModel <- glm(ofp~hosp+health+numchron+gender+school+privins, data = tab, family = "poisson")
negbinModel <- glm.nb(ofp~hosp+health+numchron+gender+school+privins, data = tab)
zipr <- zeroinfl(ofp~hosp+health+numchron+gender+school+privins, data = tab, dist="poisson")
zinbr <- zeroinfl(ofp~hosp+health+numchron+gender+school+privins, data = tab, dist="negbin")
poiHurdle <- hurdle(ofp~hosp+health+numchron+gender+school+privins, data = tab, dist="poisson")
negbinHurdle <- hurdle(ofp~hosp+health+numchron+gender+school+privins, data = tab, dist="negbin")

r_zipr <- zeroinfl(ofp~hosp+health+numchron+gender+school+privins|hosp+numchron+gender+school+privins, data = tab, dist="poisson")
r_zinbr <- zeroinfl(ofp~hosp+health+numchron+gender+school+privins|hosp+numchron+gender+school+privins, data = tab, dist="negbin")
r_poiHurdle <- hurdle(ofp~hosp+health+numchron+gender+school+privins|hosp+numchron+gender+school+privins, data = tab, dist="poisson")
r_negbinHurdle <- hurdle(ofp~hosp+health+numchron+gender+school+privins|hosp+numchron+gender+school+privins, data = tab, dist="negbin")
r_r_zinbr <- zeroinfl(ofp~hosp+health+numchron+gender+school+privins|numchron+gender+school+privins, data = tab, dist="negbin")

library('caret')
library('leaps')
stepPoi <- stepAIC(glm(ofp~hosp*health*numchron*gender*school*privins, data = tab, family = "poisson"), direction = "backward", steps  = 1000)
#stepPoi_int <- stepAIC(glm(ofp~hosp*health*numchron*gender*school*privins, data = tab, family = "poisson"), direction = "backward", trace = TRUE,  steps = 1000)
stepNegBin <- stepAIC(glm.nb(ofp~hosp*health*numchron*gender*school*privins, data = tab), direction = "backword", trace = TRUE,  steps = 1000)
#stepNegBin_int <- stepAIC(glm.nb(ofp~hosp*health*numchron*gender*school*privins, data = tab), direction = "both", trace = TRUE,  steps = 1000)
stepZipr <-  stepAIC(zeroinfl(ofp~hosp+health+numchron+gender+school+privins, data = tab, dist="poisson"), direction = "backward", trace = TRUE,  steps = 1000)
stepZinbr <-  stepAIC(zeroinfl(ofp~hosp*health*numchron*gender*school*privins, data = tab, dist="negbin"), direction = "both", trace = TRUE,  steps = 1000)
stepPHurdle <- stepAIC(hurdle(ofp~hosp+health+numchron+gender+school+privins, data = tab, dist="poisson"),  direction = "backward", trace = TRUE,  steps = 1000)
#stepNBHurdle <- stepAIC(hurdle(ofp~hosp*health*numchron*gender*school*privins, data = tab, dist="negbin"),  direction = "backward", trace = TRUE,  steps = 1000)

summary(poiModel)
summary(negbinModel)
summary(zipr)
summary(zinbr)
summary(poiHurdle)
summary(negbinHurdle)

summary(r_zipr)
summary(r_r_zinbr)
summary(r_poiHurdle)
summary(r_negbinHurdle)

#testy
-2 *(logLik(zipr) -  logLik(r_zipr)) < qchisq(0.9, 2)
-2 *(logLik(zinbr) -  logLik(r_r_zinbr)) < qchisq(0.9, 1)
-2 *(logLik(poiHurdle) -  logLik(r_poiHurdle)) < qchisq(0.9, 2)
-2 *(logLik(negbinHurdle) -  logLik(r_negbinHurdle)) < qchisq(0.9, 2)


poiModel$aic
negbinModel$aic
AIC(r_zipr)
AIC(r_r_zinbr)
AIC(r_poiHurdle)
AIC(r_negbinHurdle)

BIC(poiModel)
BIC(negbinModel)
BIC(r_zipr)
BIC(r_r_zinbr)
BIC(r_poiHurdle)
BIC(r_negbinHurdle)

logLik(poiModel)
logLik(negbinModel)
logLik(r_zipr)
logLik(r_r_zinbr)
logLik(r_poiHurdle)
logLik(r_negbinHurdle)

2 * (logLik(negbinHurdle) -logLik(r_r_zinbr)) < qchisq(0.95, 1)

summary(stepPoi)
#summary(stepPoi_int)
summary(stepNegBin)
-2 * (logLik(stepPoi) - logLik(poiModel)) < qchisq(0.95, 92-8)
2 * (logLik(zinbr) - logLik(negbinModel)) < qchisq(0.95, 8)
AIC(stepNegBin)
AIC(stepPoi)
summary(stepNegBin_int)
summary(stepZipr)
AIC(stepZipr)
BIC(stepZipr)
summary(stepZinbr)
AIC(stepZinbr)
BIC(stepZinbr)
summary(stepPHurdle)
AIC(stepPHurdle)
BIC(stepPHurdle)
summary(stepPHurdle)
AIC(stepNBHurdle)
BIC(stepNBHurdle)

logLik(stepPoi_int)
logLik(stepNegBin_int)

predict(poiModel, tab, type = "response")
predict(negbinModel, type = "response")
predict(zipr, tab, type = "response")
predict(zinbr, tab, type = "response")
predict(poiHurdle, tab, type = "response")
predict(negbinHurdle, tab, type = "response")

round(sum(dpois(0, predict(poiModel, tab, type = "response"))))
round(sum(dnbinom(0, negbinModel$theta, mu=predict(negbinModel, tab, type = "response"))))
round(sum(predict(r_zipr, tab, type = "prob")[,1]))
round(sum(predict(r_r_zinbr, tab, type = "prob")[,1]))
round(sum(predict(r_poiHurdle, tab, type = "prob")[,1]))
round(sum(predict(r_negbinHurdle, tab, type = "prob")[,1]))

## Zadanie 4

X <- matrix(rnorm(2*1000,  0,  sqrt(1/1000)), 1000, 2)
beta <- c(3,3)
eta <-  X%*%beta
Y <- sapply(seq(10000), function(j){sapply(seq(1000),function(i){rpois(1, exp(eta)[i])})})

stats <- function(y){
  mp <- glm(y~X[,1]+X[,2], family = "poisson")
  mnb <- glm.nb(y~X[,1]+X[,2])
  llp <- logLik(mp)
  llnb <- logLik(mnb)
  c('chisq' =  -2*(llp - llnb), 'T' = 1/mnb$theta/mnb$SE.theta, 'alpha' = 1/mnb$theta)
}

statistics <- sapply(seq(10000), function(i){stats(Y[,i])})

## chisq plots
hist(statistics[1,], main = 'Rozkład statystyki chisq', freq = F,  xlab = 'wartości statystyki',  ylab = 'prawdopodobieństwo', breaks = seq(-.5, 15,  0.5))
lines(seq(0, 15, 0.01), dchisq(seq(0, 15, 0.01), 1))
qqplot(rchisq(100000, 1), statistics[1,], type = "l", main = "QQ plot dla rozkładu Chi^2_0")
hist(statistics[1,order(statistics[1,])[5001:10000]], main = 'Rozkład statystyki chisq0', freq = F,  xlab = 'wartości statystyki',  ylab = 'prawdopodobieństwo', breaks = seq(-.5, 15,  0.5))
lines(seq(0, 15, 0.01), dchisq(seq(0, 15, 0.01), 1))
qqplot(rchisq(100000, 1), statistics[1,order(statistics[1,])[5001:10000]], type = "l", main = "QQ plot dla rozkładu Chi^2_1")
#smooth.spline(rchisq(5000, 1), statistics[1,order(statistics[1,])[5001:10000]])
## T plots -- chujoza
hist(statistics[2,], freq = F, main = 'Rozkład statystyki T', xlab = 'wartości statystyki',  ylab = 'prawdopodobieństwo', breaks = seq(-.05, 0.15, 0.01))
lines(seq(-.05, 0.15, 0.001), dnorm(seq(-.05, 0.15, 0.001), 0,  1))

hist(statistics[2,], main = 'Rozkład statystyki T', freq = F,  xlab = 'wartości statystyki',  ylab = 'prawdopodobieństwo', breaks = seq(-.05, 0.15, 0.001))#, ylim = c(0, 1), xlim=c(-1, 1))
lines(seq(-1, 1, 0.001), dnorm(seq(-1, 1, 0.001), 0,  1))
order(statistics[2,])[5001:10000]
qqnorm(sort(statistics[3,]), main =  "qqplot dla wszystkich T", type =  "l")

## ^aplha plots
hist(statistics[3,],  main = 'Rozkład alpha z daszkiem', freq = F,  xlab = 'wartości estymatora',  ylab = 'prawdopodobieństwo')
lines(seq(-.05, 0.20, 0.001), dnorm(seq(-.05, 0.20, 0.001), 0,  quantile(statistics[3,], .75)/qnorm(.75)))
qqnorm(sort(statistics[3,]), main =  "qqplot dla wszystkich alf", type =  "l")

hist(statistics[3,order(statistics[3,])[5001:10000]],  main = 'Rozkład alpha z daszkiem', freq = F,  xlab = 'wartości estymatora',  ylab = 'prawdopodobieństwo', breaks = seq(-.005, 0.20, 0.005))
lines(seq(-.05, 0.20, 0.001), dnorm(seq(-.05, 0.20, 0.001), 0,  quantile(statistics[3,], .75)/qnorm(.75)))
lines(seq(-.05, 0.20, 0.001), 2*dnorm(seq(-.05, 0.20, 0.001), 0,  quantile(statistics[3,order(statistics[3,])], .75)/qnorm(.75)), col =  "red")
#lines(seq(-.05, 0.20, 0.001), dnorm(seq(-.05, 0.20, 0.001), 0,  quantile(statistics[3,order(statistics[3,])[5001:10000]], .75)/qnorm(.75)), col =  "green")
qqnorm(statistics[3,order(statistics[3,])[5001:10000]], main = "QQ plot z połową większych alf", type = "l")
