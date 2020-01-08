# [Example 8.1] 6~12세까지의 나이별 키의 자료(‘elementheight’) : meanheight, sdheight, age.

library(regbook)
fit.wls <- lm(meanheight ~ age, data=elementheight, weights=1/sdheight^2)

# [Example 8.2] 자료 ‘restaurant’ : 비슷한 업종의 음식점 30개의 매출액(Y)과 광고비(X)

# (1) 그래프를 이용한 이분산성 여부 조사
fit.ols <- lm(Y ~ X, restaurant)
plot(resid(fit.ols) ~ fitted(fit.ols))

# (2) HC0를 이용한 이분산성 여부 조사
vcov(fit.ols)
library(sandwich)
( HC0 <- sandwich::vcovHC(fit.ols, type="HC0") )

# 참고) 회귀계수에 대한 유의성검정 : lmtest 패키지의 coeftest() 함수를 이용
lmtest::coeftest(fit.ols, vcov=HC0)
lmtest::coeftest(fit.ols)

# (3) Breusch-Pagan검정을 이용한 이분산성 여부 조사
lmtest::bptest(fit.ols)

# [Example 8.3] 음식점 매출액 자료

( agg <- aggregate(Y ~ X, restaurant, sd) ) # 수준별 평균과 표준편차 계산
plot(Y ~ X, agg)

( sdfit <- lm(Y ~ X + I(X^2), agg) )
s <- predict(sdfit, newdata=data.frame(X=restaurant$X))
( w <- 1/s^2 )

fit.wls <- lm(Y ~ X, restaurant, weights=w)
summary(fit.wls)

# 가중최소제곱추정량에 대한 White의 HC0 공식으로 계산된 표준오차
HC0 <- sandwich::vcovHC(fit.wls, type="HC0")
lmtest::coeftest(fit.wls, vcov=HC0)


# [Example 8.4] ‘hald’ 자료(4장)에 대한 능형회귀

# MASS 패키지의 lm.ridge() 함수를 이용
library(MASS)
fit <- lm.ridge(y ~ x1 + x2 + x3 + x4, hald, lambda=seq(0, 0.5, 0.01))
plot(fit)

# GCV 방법
select(fit)
lm.ridge(y ~ x1 + x2 + x3 + x4, hald, lambda=seq(0, 0.4, 0.1))
lm.ridge(y ~ x1 + x2 + x3 + x4, hald, lambda=0.32)

# [Example 8.5] ‘hald’ 자료에 대한 주성분회귀

# 상관계수행렬에 대한 고유값 분해
( R <- cor(hald[2:5]) )
eigen(R)

# 주성분분석
pr <- princomp(hald[2:5], cor=TRUE)
summary(pr)
plot(pr, type="l")

# 주성분회귀
hald.pc <- data.frame(pr$scores)
hald.pc$y <- hald$y
fit <- lm(y ~ ., hald.pc)
summary(fit)

# [Example 8.6] ‘hald’ 자료에 대한 불완전 주성분회귀

regbook::pcr(y ~ ., hald, ncomp=4)
regbook::pcr(y ~ ., hald, ncomp=3)

# 주성분회귀분석의 결과를 원래 설명변수들로 표현한 모형
fit.pcr <- regbook::pcr(y ~ ., hald, ncomp=3)
coef(fit.pcr)

# [Example 8.7] 자료 ‘stackloss’ : 암모니아를 산화시켜 질산을 만드는 공정

lm(stack.loss ~ ., stackloss)
sapply(stackloss, sd)
sapply(stackloss, mad)

# M 추정량, 가중치함수=Tukey 함수인 psi.bisquare를 지정
fitM <- rlm(stack.loss ~ ., stackloss, method="M", psi=psi.bisquare)
summary(fitM)

plot(fitM, which=7:8)

# [Example 8.8] 자료 ‘cygnus’ : 백조자리 성운의 47개 별들의 밝기(Y)와 표면온도(X)

lm(light ~ temp, cygnus)
fitMM <- rlm(light ~ temp, cygnus, method="MM")
summary(fitMM)