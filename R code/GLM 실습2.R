# [Example 12.0] Teen smoking and gender

library(regbook)
library(UsingR)
tbl = xtabs( ~ gender + amt.smoke, subset= amt.smoke<98 & gender !=7, data=samhda)
tbl

chisq.test(tbl)

tbl2 = as.data.frame(tbl) ; tbl2
# independent model
tbl2fit1 = glm(Freq ~ gender + amt.smoke, family=poisson, data=tbl2)
summary(tbl2fit1)

1-pchisq(deviance(tbl2fit1), tbl2fit1$df.resid) # 0.6578

# saturated model
tbl2fit2 = glm(Freq ~ gender * amt.smoke, family=poisson, data=tbl2)
summary(tbl2fit2)

# model comparison
anova(tbl2fit1, tbl2fit2, test="Chisq")

# [Example 12.1] 악성종양 예제
# Note : The total frequency is fixed at 400.

Site = rep(c("Head", "Trunk", "Extreme"), 4)
Site = factor(Site) ; Site = relevel(Site, ref="Extreme")
Tumour = rep(c("Freckle", "Super", "Nodular", "Indeter"), each=3)
Tumour = factor(Tumour) ; Tumour = relevel(Tumour, ref="Indeter")
Freq = c(22, 2, 10, 16, 54, 115, 19, 33, 73, 11, 17, 28)
melanoma = data.frame(Site, Tumour, Freq)
tbl = xtabs( Freq ~ Site + Tumour, data=melanoma)
tbl

chisq.test(tbl)

# minimal model
mel.fit0 = glm(Freq ~ 1, family=poisson, data=melanoma)
summary(mel.fit0)

# independent model
mel.fit1 = glm(Freq ~ Site + Tumour, family=poisson, data=melanoma)
summary(mel.fit1)

# saturated model
mel.fit2 = glm(Freq ~ Site * Tumour, family=poisson, data=melanoma)
summary(mel.fit2)

# model comparison
anova(mel.fit0, mel.fit1, mel.fit2, test="Chisq")

# [Example 12.3] 아스피린 예제
# Note : row total frequency is fixed.

Freq = c(39, 25, 62, 6, 49, 8, 53, 8)
Group = rep(c("case", "case", "control", "control"), 2)
Group = factor(Group) ; Group = relevel(Group, ref="control")
Type = rep(c("위궤양", "십이지궤양"), each=4)
Aspirin = rep(c(0, 1), 4)
aspirin = data.frame(Group, Type, Aspirin, Freq)
tbl = xtabs( Freq ~ Group + Aspirin + Type, data=aspirin)
tbl

# minimal model
asp.fit0 = glm(Freq ~ 1, family=poisson, data=aspirin)
summary(asp.fit0)

# independent model
asp.fit1 = glm(Freq ~ Type*Group + Aspirin, family=poisson, data=aspirin)
summary(asp.fit1)

asp.fit2 = glm(Freq ~ Type*Group + Aspirin*Type, family=poisson, data=aspirin)
asp.fit3 = glm(Freq ~ Type*Group + Aspirin*Group, family=poisson, data=aspirin)
asp.fit4 = glm(Freq ~ Type*Group + Aspirin*Group + Aspirin:Type, family=poisson, data=aspirin)

anova(asp.fit1, asp.fit2, asp.fit3, asp.fit4, test="Chisq")
anova(asp.fit2, asp.fit4, test="Chisq")
anova(asp.fit3, asp.fit4, test="Chisq")

summary(asp.fit4)

# Logistic model 1
aspirin.logit1 = glm(Aspirin ~ Group + Type, weights=Freq, family=binomial, data=aspirin)
summary(aspirin.logit1)

# Logistic model 2
Pa0 = c(39, 62, 49, 53)
PaA = c(25, 6, 8, 8)
Pa = cbind(PaA, Pa0)
Group = rep(c("case", "control"), 2)
Group = factor(Group) ; Group = relevel(Group, ref="control")
Type = rep(c("위궤양", "십이지궤양"), each=2)
aspirin.logit2 = glm( Pa ~ Group + Type, family=binomial(link="logit") )
summary(aspirin.logit2)

# [Example 12.2] 백신 예제 (homework)
# Note : row total frequency is fixed.

Freq = c(25, 8, 5, 6, 18, 11)
Group = rep(c("vaccine", "control"), each=3)
HIA = rep(c("low", "medium", "xhigh"), 2)
vaccine = data.frame(Group, HIA, Freq)
tbl = xtabs( Freq ~ Group + HIA, data=vaccine)
tbl

# minimal model
vac.fit0 = glm(Freq ~ 1, family=poisson, data=vaccine)
summary(vac.fit0)

# independent model
vac.fit1 = glm(Freq ~ Group + HIA, family=poisson, data=vaccine)
summary(vac.fit1)

# saturated model
vac.fit2 = glm(Freq ~ Group * HIA, family=poisson, data=vaccine)

# model comparison
anova(vac.fit1, vac.fit2, test="Chisq")
summary(vac.fit2)
