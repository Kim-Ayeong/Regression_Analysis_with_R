# [Example 11.1] Comparing glm() and lm()

library(regbook)

x1 = rep(1:10, 2) ; x2 = rchisq(20, df=2)
y = rnorm(20, mean=x1+2*x2, sd=2)
fit.lm = lm(y ~ x1 + x2)
summary(fit.lm)

fit.glm = glm(y ~ x1 + x2, family=gaussian)
summary(fit.glm)

# [Example 11.2] Dose-response model

conc = c(1.6907, 1.7242, 1.7552, 1.7842, 1.8113, 1.8369, 1.8610, 1.8839)
nbug = c(59, 60, 62, 56, 63, 59, 62, 60)
ndead = c(6, 13, 18, 28, 52, 53, 61, 60)
beetle = cbind(ndead, nalive=nbug-ndead) # "beetle" is a matrix
rate = ndead / nbug
plot(rate ~ conc)
bug.logit = glm( beetle ~ conc, family=binomial(link="logit") ) # logistic model
points(conc, fitted(bug.logit), type="l")
summary(bug.logit)

deviance(bug.logit)

bug.probit = glm( beetle ~ conc, family=binomial(link="probit") ) # probit model
summary(bug.probit)
bug.cloglog = glm( beetle ~ conc, family=binomial(link="cloglog") ) # cloglog model
summary(bug.cloglog)

anova(bug.logit, test="Chisq")
anova(bug.logit, bug.probit, bug.cloglog, test="Chisq")

predict(bug.logit, type="link") # estimated values of linear predictor
predict(bug.logit, type="response") # estimated values of probability

resid(bug.logit, type="response")
resid(bug.logit, type="deviance") # default

coef1 = coef(bug.logit) # estimated coefficients

# plotting the fitted equations
conc2 = seq(1.6, 1.95, 0.01)
coef1 = coef(bug.logit)
coef2 = coef(bug.probit)
coef3 = coef(bug.cloglog)
linpre1 = coef1[1] + coef1[2]*conc2
linpre2 = coef2[1] + coef2[2]*conc2
linpre3 = coef3[1] + coef3[2]*conc2
fitted1 = exp(linpre1) / ( 1 + exp(linpre1) )
fitted2 = pnorm(linpre2)
fitted3 = 1 - exp( -exp(linpre3) )
plot(rate ~ conc)
points(conc2, fitted1, type="l", lty=1, col=4)
points(conc2, fitted2, type="l", lty=2, col=6)
points(conc2, fitted3, type="l", lty=3, col=1)
legend(1.7, 0.9, c("logistic", "probit", "cloglog"), col = c(4, 6, 1), lty = c(1, 2, 3))

# [Example 11.3] Dose-response model

# [Step 1] plot of the observed data
dose = rep(c(1, 2, 4, 8, 16, 32), 2)
ndead = c(1, 4, 9, 13, 18, 20, 0, 2, 6, 10, 12, 16)
sex = rep(c("M", "F"), c(6, 6))
MOTH = cbind(ndead, nalive=20-ndead)
plot( c(1, 32), c(0, 1), type="n", xlab="dose", ylab="prob")
text( dose, ndead/20, labels=as.character(sex))

# [Step 2] fitting models
moth.m1 = glm( MOTH ~ dose, family=binomial(link="logit") ) # model1
summary(moth.m1)
deviance(moth.m1)

moth.m2 = glm( MOTH ~ dose + factor(sex), family=binomial(link="logit") ) # model2
summary(moth.m2)
deviance(moth.m2)

moth.m3 = glm( MOTH ~ dose + factor(sex) + dose:factor(sex), family=binomial(link="logit") )
summary(moth.m3)
deviance(moth.m3)

anova(moth.m1, moth.m2, moth.m3, test="Chisq")

# Plot of fitted model
plot( c(1, 32), c(0, 1), type="n", xlab="dose", ylab="prob") # vacant plot
text( dose, ndead/20, labels=as.character(sex) )
dose2 = seq(0, 32, 0.1)
fittedM = predict(moth.m3, data.frame(dose=dose2, sex=rep("M", length(dose2))), type="response")
fittedF = predict(moth.m3, data.frame(dose=dose2, sex=rep("F", length(dose2))), type="response")
lines(dose2, fittedM, col=3)
lines(dose2, fittedF, lty=2, col=2)

# [Example 11.3] 꽃씨 발아 예제

nseed = c(76, 81, 90, 102, 99, 108)
nsucceed = c(55, 50, 50, 55, 52, 57)
force = c(40, 150, 350, 40, 150, 350)
trt = c("treat", "treat", "treat", "control", "control", "control")
seed = cbind(nsucceed, nseed - nsucceed)

seed.m3 = glm( seed ~ log(force), family=binomial(link="logit") ) # model1
summary(seed.m3)
deviance(seed.m3)

seed.m2 = glm( seed ~ log(force) + factor(trt), family=binomial(link="logit") ) # model2
summary(seed.m2)
deviance(seed.m2)

seed.m1= glm( seed ~ log(force) + factor(trt) + log(force):factor(trt), family=binomial(link="logit"))
summary(seed.m1)
deviance(seed.m1)
anova(seed.m3, seed.m2, seed.m1)

# plotting the fitted equations
lforce = seq(3.5, 6.5, 0.01) ; coef1 = coef(seed.m1)
linpre1 = coef1[1] + coef1[2]*lforce
linpre2 = coef1[1] + coef1[3] + (coef1[2] + coef1[4])*lforce
fitted1 = exp(linpre1) / ( 1 + exp(linpre1) )
fitted2 = exp(linpre2) / ( 1 + exp(linpre2) )
plot(nsucceed/nseed ~ log(force), pch=c("T", "T", "T", "C", "C", "C"), xlim=c(3.5, 6.5))
points(lforce, fitted1, type="l", lty=1, col=4)
points(lforce, fitted2, type="l", lty=2, col=6)

# [Example 11.4] Survey data of the class 2017

survey2017 = read.table(file="c:/survey2017-2.txt", head=T) # gender, S, year, pol
survey2017 = within(survey2017, {year[year<2015]=1; year[year>=2015]=2
pol2=pol; pol2[pol2<=2]=0; pol2[pol2>2]=1;
})
attach(survey2017)
table(year, pol, S, gender) # table(gender); table(year, gender)

# (1) Logistic model for pol2(보수+중도, 진보)
survey.m1 = glm( pol2 ~ gender + S + year, family=binomial(link="logit"), data=survey2017)
summary(survey.m1)
survey.m2 = glm( pol2 ~ S + year, family=binomial(link="logit"), data=survey2017)
summary(survey.m2)
survey.m3 = glm( pol2 ~ S, family=binomial(link="logit"), data=survey2017)
summary(survey.m3)

# (2) Baseline-category logit
library(nnet)
survey.mult1 <- multinom(pol ~ gender + S + factor(year), data=survey2017)
summary(survey.mult1)
survey.mult2 <- multinom(pol ~ S, data=survey2017)
summary(survey.mult2)

# (3) Proportional odds model
survey.pr1 = polr(factor(pol) ~ S, data=survey2017) # MASS, response must be a factor
summary(survey.pr1)

# Model 1 : models with only main effects.
house.mult1 <- multinom(Sat ~ Infl + Type + Cont, weights=Freq, data=housing)
summary(house.mult1)

# check for the interaction terms
addterm(house.mult1, ~. + (Infl+Type+Cont)^2, test="Chisq")

# Comparison with the model containing 3-way interactions.
house.mult2 <- multinom(Sat ~ Infl*Type*Cont, weights=Freq, data=housing)
anova(house.mult1, house.mult2, test="Chisq")

# [Example 11.5(Cont.)] Copenhagen housing satisfaction data

# Model 1 : models with only main effects.
house.pr1 <- polr(Sat ~ Infl + Type + Cont, weights=Freq, data=housing)
summary(house.pr1)

# [Example 11.6] 광부들의 근무기간에 따른 진폐증(pneumoconiosis) 감염여부 자료

Period = rep(c(5.8, 15.0, 21.5, 27.5, 33.5, 39.5, 46.0, 51.5), each=3)
Pdegree = factor(rep(1:3, 8)) # response must be a factor
Freq=c(98, 0, 0, 51, 2, 1, 34, 6, 3, 35, 5, 8, 32, 10, 9, 23, 7, 8, 12, 6, 10, 4, 2, 5)
pnsis = data.frame(Period, Pdegree, Freq)
pnsis.pr1 = polr(Pdegree ~ log(Period), weights=Freq, data=pnsis)
summary(pnsis.pr1)

# [Example 11.7] 치즈 맛 평가문제 (homework)

Cheese = rep(c("A", "B", "C", "D"), each=9)
Cheese = factor(Cheese); Cheese = relevel(Cheese, ref="D")

Score = factor(rep(1:9, 4)) # response must be a factor
Freq = c(0,0,1,7,8,8,19,8,1,6,9,12,11,7,6,1,0,0,1,1,6,8,23,7,5,1,0,0,0,0,1,3,7,14,16,11)
cheese = data.frame(Cheese, Score, Freq)

cheese.pr1 = polr(Score~Cheese, weights = Freq, data=cheese)
summary(cheese.pr1)
