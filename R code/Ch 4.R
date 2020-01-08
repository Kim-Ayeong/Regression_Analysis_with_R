# [Example 4.1] 완전모형

library(regbook)

# [Example 4.2] 중고차 가격 예제 ‘usedcars’

# (1) 회귀제곱합을 추가제곱합으로 분해
fit <- lm(price ~ year + mileage + cc + automatic, usedcars)
anova(fit)

drop1(fit, test="F") # 편제곱합 : drop1() 함수 사용

# (2) 가설 H0에 대한 부분 F 검정
# anova() 함수를 이용한 부분 F 검정

# [Example 4.3] 중고차 가격 예제

# (1) 가설 H0:beta3=0
mod0 <- lm(price ~ year + mileage + automatic, usedcars)
mod1 <- lm(price ~ year + mileage + cc + automatic, usedcars)
anova(mod0, mod1)

# (2) 가설 H0:beta3=beta4=0
mod0 <- lm(price ~ year + mileage, usedcars)
mod1 <- lm(price ~ year + mileage + cc + automatic, usedcars)
anova(mod0, mod1)

# [Example 4.4] 자료 ‘suneung’. 수학능력점수(Y), X1:국어, X2:영어, X3:수학, X4:과학

mod0 <- lm(suneung ~ kor + I(2.5*math + sci), suneung) # I() 함수 사용
mod1 <- lm(suneung ~ kor + eng + math + sci, suneung)
anova(mod0, mod1)

# [Example 4.5] 중고차 예제에서 X1과 X2의 편회귀그림

fit <- lm(price ~ year + mileage, usedcars)
y.x1 <- resid(lm(price ~ year, usedcars))
x2.x1 <- resid(lm(mileage ~ year, usedcars))
plot(y.x1 ~ x2.x1)
abline(lm(y.x1 ~ x2.x1))

# [Example 4.6] 중고차 예제에서 각 변수들의 편회귀그림

# 예) year의 편회귀그림
y.others <- resid(lm(price ~ mileage + cc + automatic, usedcars))
x.others <- resid(lm(year ~ mileage + cc + automatic, usedcars))
plot(y.others ~ x.others, main="year")
abline(lm(y.others ~ x.others))
grid()

# [Example 4.7] 자료 ‘expdata1’

# (1) y의 x1에 대한 단순선형회귀모형의 적합결여검정
fit.lm <- lm(y ~ x1, expdata1)
anova(fit.lm) # SSE=811.3

fit.pe <- lm(y ~ factor(x1), expdata1)
anova(fit.pe) # SS_PE=467.3, MS_PE=66.76
anova(fit.lm, fit.pe)

# (2) y의 x1, x2에 대한 중회귀모형의 적합결여검정
fit.lm <- lm(y ~ x1 + x2, expdata1)
fit.pe <- lm(y ~ factor(x1) + factor(x2), expdata1)
anova(fit.lm, fit.pe)

# [Example 4.8] 자료 ‘bug’ : 살충제의 독성실험. 살충제에 노출된 벌레들의 생존개체 수. 변수: 생존벌레수, 시간(분). 지수쇠퇴함수의 변환모형 적합.

par(mfrow = c(1, 2))
plot(y ~ time, bug)

bug <- within(bug, logy <- log(y))
fit <- lm(logy ~ time, bug)
plot(logy ~ time, bug) ; abline(fit)

summary(fit)

# [Example 4.9] 자료 ‘restaurant’ : 음식점 30개의 매출액(X)과 광고비(Y). 매출액의 분산이 광고비 또는 매출액이 커짐에 따라 지속적으로 증가. 이 자료에 대한 분산안정화 변환?

Y.meanvar <- meanvar(Y ~ X, restaurant)
plot(Y.meanvar, sd ~ mean + I(mean^2))

# [Example 4.10] ‘aflength’ 예제에서의 박스-칵스변환

library(MASS)
boxcox(lm(foot ~ forearm, aflength))

# [Example 4.11] 자료 ‘wool’ : 양모의 강력(strength)에 대한 3^3 요인실험

boxcox(lm(cycle ~ length + amplitude + load, wool))

# [Example 4.12] 자료 ‘houseprice’: 주택 판매가격(Y,천만원), 세금(X1,만원), 대지평수(X2,평), 건물평수(X3,평), 주택연령(X4,년) : 표준화 회귀계수?

fit <- lm(price ~ ., houseprice)
summary(fit)

stdcoef(fit) # regbook 패키지의 stdcoef() 함수와 lmbeta() 함수

summary(lmbeta(fit))

# [Example 4.13] 설명변수들에 정확한 선형종속관계가 있는 경우. ‘배기주행(ccmile)’ = ‘배기량(cc)’ + ‘주행거리(mileage)’

usedcars <- within(usedcars, ccmile <- cc + mileage)
lm(price ~ year + mileage + cc + automatic + ccmile, usedcars)

# [Example 4.14] ‘hald’: 시멘트가 굳을 때 발생하는 열량에 시멘트의 네 가지 성분들이 미치는 영향을 분석. 다중공선성의 유무와 형태를 파악

# (1) 설명변수들의 표본상관계수값
(R <- cor(hald[2:5]))

# (2) VIF값 : VIF=표본상관계수행렬의 역행렬의 대각원소
diag(solve(R))

# (3) 선형종속관계의 개수는? 행렬 W′W의고유값
(eigen(R)$values)

eigenval <- eigen(R)$values
sqrt(max(eigenval) / eigenval)

# (5) 분산비(variance proportion) : 각 고유값이 회귀계수 추정량의 분산에 영향을 미치는 정도
summary(vif(lm(y ~ ., hald)))

# [Example 4.15] Hald 자료에서 변수 X2를 제외한 회귀분석

fit <- lm(y ~ x1 + x3 + x4, hald)
summary(vif(fit))

fit2 <- lm(y ~ x1 + x2 + x3 + x4, hald)
summary(fit)
summary(fit2)
