library(ggplot2)
sklep <- data.frame(read.csv2("/home/kabalcer/Dokumenty/ZML/lista3/sklep.txt", sep = ","))

model <- glm(no.klients~day+factor(hour)+factor(events), poisson, sklep)

boxplot(sklep$no.klients[which(sklep$day=="Monday")], sklep$no.klients[which(sklep$day=="Tuesday")], sklep$no.klients[which(sklep$day=="Wednesday")], sklep$no.klients[which(sklep$day=="Thursday")], sklep$no.klients[which(sklep$day=="Friday")], sklep$no.klients[which(sklep$day=="Saturday")],  sklep$no.klients[which(sklep$day=="Sunday")], names =  c("mon", "tues", "wedn", "thurd", "fri", "satur", "sun"))
boxplot(sklep$no.klients[which(sklep$events==0)], sklep$no.klients[which(sklep$events==1)], names = c("no event", "event"))
boxplot(sapply(seq(8, 23), function(i){sklep$no.klients[which(sklep$hour==i)]}), names = seq(8,  23))

qplot(hour,no.klients, shape = as.factor(events),col = day, data = sklep)
qplot(hour,no.klients, facets = events~day, data = sklep)
qplot(hour,no.klients, color = day, data = sklep)
qplot(hour,no.klients, facets =~day, data = sklep)

model_interaction <- glm(no.klients~day*factor(hour)*factor(events), poisson, sklep)

vec_day <- sapply(sklep$day, function(day){ifelse(((day=="Saturday") | (day)=="Sunday"), "weekend","working-day")}) 
vec_hour <- sapply(sklep$hour, function(hour){
  if(hour<12) return("morning")
  if(hour<16) return("afternoon")
  if(hour<20) return("evening")
  return("night")
})

model_intervals <- glm(sklep$no.klients~vec_day*vec_hour, poisson, x = TRUE)

summary(model_interaction)
#(model_interaction$null.deviance - model_interaction$deviance)/(model_interaction$df.null - model_interaction$df.residual) < qchisq(.95, model_interaction$df.null - model_interaction$df.residual)
summary(model_intervals)
  #(model_intervals$null.deviance - /(model_intervals$df.null - model_intervals$df.residual) < qchisq(.95, model_intervals$df.null - model_intervals$df.residual)
(model_intervals$deviance - model_interaction$deviance) <  qchisq(.95, model_intervals$df.residual - model_interaction$df.residual)

#istotność interakcji:
anova(glm(sklep$no.klients~vec_day+vec_hour, poisson), model_intervals, test =  "Chisq")

X <- model_intervals$x
beta <- model_intervals$coefficients
for(i in c("working-day", "weekend")){
  for(j in c("morning", "afternoon", "evening", "night")){
    print(c(i, j, mean(sklep$no.klients[which((vec_day==i) & (vec_hour==j))]), mean((X%*%beta)[which((vec_day==i) & (vec_hour==j))]), mean(exp((X%*%beta)[which((vec_day==i) & (vec_hour==j))]))))
  }
}

Cov_matrix <- vcov(model_intervals)[3:5, 3:5]
matrix(beta[3:5], ncol = 3)%*%Cov_matrix%*%beta[3:5] < qchisq(.95, 3)
