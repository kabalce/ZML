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
model <- glm(success~numeracy+anxiety, data=A, family="binomial")
summary(model)
summary(glm(success~numeracy*anxiety, data=A, family="binomial"))
b <- model$coefficients
exp(b[0]+b[1]*10+b[2]*13)/(1+exp(b[0]+b[1]*10+b[2]*13))
#Zadanie 1 - czy bierzemy pod uwagę iloczyn X1*X2? Przeciez to psuje model. Czy więc należało ozważyć dwa modele z i bez interakcji??

# wyniki testów istotności - czyli co? wartosc statystyki testowej p-wartosc?

new <- data.frame(numeracy = c(10), anxiety = c(13))
predict.glm(model, new, type = "response")
b <- model$coefficients
exp(b[1]+b[2]*10+b[3]*13)/(1+exp(b[1]+b[2]*10+b[3]*13))

#krzywa ROC
library("ROCR")

ROCRPred=prediction(model$fitted.values, model$y)
ROCRPerf=performance(ROCRPred, "tpr", "fpr")
plot(ROCRPerf, colorize=TRUE, print.cutoffs.at=seq(0,0.8,0.1), text.adj=c(-0.2, 1.7), main = "Krzywa ROC modelu z funkcją linkującą logit")

#####

model <- glm(success~numeracy+anxiety, data=A, family=binomial(link = "probit"))
summary(model)

new <- data.frame(numeracy = c(10), anxiety = c(13))
predict.glm(model, new, type = "response")
b <- model$coefficients
exp(b[1]+b[2]*10+b[3]*13)/(1+exp(b[1]+b[2]*10+b[3]*13))

ROCRPred=prediction(model$fitted.values, model$y)
ROCRPerf=performance(ROCRPred, "tpr", "fpr")
plot(ROCRPerf, colorize=TRUE, print.cutoffs.at=seq(0,0.8,0.1), text.adj=c(-0.2, 1.7), main = "Krzywa ROC modelu z funkcją linkującą probit")


#####

model <- glm(success~numeracy+anxiety, data=A, family=binomial(link = "cauchit"))
summary(model)


new <- data.frame(numeracy = c(10), anxiety = c(13))
predict.glm(model, new, type = "response")
b <- model$coefficients
exp(b[1]+b[2]*10+b[3]*13)/(1+exp(b[1]+b[2]*10+b[3]*13))

ROCRPred=prediction(model$fitted.values, model$y)
ROCRPerf=performance(ROCRPred, "tpr", "fpr")
plot(ROCRPerf, colorize=TRUE, print.cutoffs.at=seq(0,0.8,0.1), text.adj=c(-0.2, 1.7), main = "Krzywa ROC modelu z funkcją linkującą cauchit")

#####

model <- glm(success~numeracy+anxiety, data=A, family=binomial(link = "cloglog"))
summary(model)

new <- data.frame(numeracy = c(10), anxiety = c(13))
predict.glm(model, new, type = "response")
b <- model$coefficients
exp(b[1]+b[2]*10+b[3]*13)/(1+exp(b[1]+b[2]*10+b[3]*13))

ROCRPred=prediction(model$fitted.values, model$y)
ROCRPerf=performance(ROCRPred, "tpr", "fpr")
plot(ROCRPerf, colorize=TRUE, print.cutoffs.at=seq(0,0.8,0.1), text.adj=c(-0.2, 1.7), main = "Krzywa ROC modelu z funkcją linkującą cloglog")
