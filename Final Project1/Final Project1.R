rfd = read.csv("어린이 행복지수.csv")
head(rfd)
D = rfd[-1]

# 반응변수와 설명변수들 간의 산점도
pairs(D)
par(mfrow=c(3,3))
plot(death ~ ., D, log="xy")

# 분산분석표
anova(fit)

# 회귀계수 추정치와 표준오차, 결정계수
fit=lm(death~.,D)
summary(fit)

# 잔차 대 설명변수들
par(mfrow=c(3,3))
plot(resid(fit)~.,D[-1])

# 잔차 대 예측값, 잔차 대 관측순서, 정규확률도
par(mfrow=c(2,2))
plot(fit,which=2)
plot(fit,which=1)
plot(rstandard(fit))

# 다중공선성 탐색을 위한 측도
summary(vif(fit))

# 영향력 관측치 들을 위한 측도
influence.measures(fit)

# (1)쿡의 거리 통계량
plot(fit,which=4)
abline(h=0.1668,lty=3)

# (2)DFFITS
plot(dffits(fit))
abline(h=1.0328,lty=3)
abline(h=-1.0328,lty=3)

# (3)DFBETAS
par(mfrow=c(3,3))
plot(dfbetas(fit)[,2])
abline(h=0.3651,lty=4); abline(h=-0.3651,lty=4)

plot(dfbetas(fit)[,3])
abline(h=0.3651,lty=4); abline(h=-0.3651,lty=4)

plot(dfbetas(fit)[,4])
abline(h=0.3651,lty=4); abline(h=-0.3651,lty=4)

plot(dfbetas(fit)[,5])
abline(h=0.3651,lty=4); abline(h=-0.3651,lty=4)

plot(dfbetas(fit)[,6])
abline(h=0.3651,lty=4); abline(h=-0.3651,lty=4)

plot(dfbetas(fit)[,7])
abline(h=0.3651,lty=4); abline(h=-0.3651,lty=4)

plot(dfbetas(fit)[,8])
abline(h=0.3651,lty=4); abline(h=-0.3651,lty=4)

# 변수선택의 기준
D.rgs = regsubsets(death ~.,D,nbest=3)
summaryf(D.rgs)

# 모형 적합
fit2 = lm(death~money+NEET+birth,D)
summary(fit2)
