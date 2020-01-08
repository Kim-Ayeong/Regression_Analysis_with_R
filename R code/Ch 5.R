# [Example 5.1] 중고차 가격 자료 ‘usedcars’ : 잔차와 지렛값을 구하여 분석

library(regbook)
fit <- lm(price ~ ., usedcars)
rstandard(fit)
rstudent(fit)

hatvalues(fit) # 19번째 관측치가 높은 지렛점 ; 기준값=    
sum(resid(fit)^2) # SSE
sum((resid(fit) / (1 - hatvalues(fit)))^2) # PRESS

# [Example 5.2] [그림 5.2]의 자료

fit.AB <- lm(y ~ x, infludata[c(-1,-2),]) # A, B를 제외한 모형적합

# [Example 5.3] 중고차 가격 예제. 잔차와 지렛값, 여러 가지 영향력 측도들의 계산

fit <- lm(price ~ ., usedcars)
influence.measures(fit)

cooks.distance(fit) ; covratio(fit) ; dffits(fit) ; dfbetas(fit) # 영향력 측도 계산 함수들

# 그래프 이용한 분석
plot(fit, which=4) # which=1:6
plot(dffits(fit))
plot(dfbetas(fit)[,2]) ; plot(dfbetas(fit)[,3]) ; plot(dfbetas(fit)[,4]) ; plot(dfbetas(fit)[,5])

# [Example 5.4] ‘steel’ : 반응변수-강판의 가격, 설명변수-기계의 가격. 자기상관 조사

plot(steelprice ~ machineprice, steel)
fit <- lm(steelprice ~ machineprice, steel)

# 잔차의 산점도와 시차그림(lag plot) :
plot(resid(fit))
lag.plot(resid(fit), do.lines=FALSE)

# 일차 자기상관계수의 추정값 : 0.588
e <- resid(fit) ; n <- length(e) ; sum(e[1:(n-1)] * e[2:n]) / sum(e^2)

# 더빈-왓슨 검정통계량 값 : 0.7095
sum(diff(e)^2) / sum(e^2)

# lmtest 패키지의 dwtest() 함수
lmtest::dwtest(fit)

# [Example 5.5] 강판가격 자료 ‘steel’ : Cochrane-Orcutt 방법으로 자기회귀오차모형을 적합

# (1) 일차 자기상관 계수의 추정값 : r=0.588
fit <- lm(steelprice ~ machineprice, steel)
e <- resid(fit) ; n <- length(e)
( rho <- sum(e[1:(n-1)] * e[2:n]) / sum(e^2) )

# (2) 변환공식
y <- steel$steelprice ; x <- steel$machineprice ; n <- length(x)
y2 <- y[2:n] - rho * y[1:(n-1)]
x2 <- x[2:n] - rho * x[1:(n-1)]

# (3) 변환된 변수를 사용한 회귀분석
fit.ar <- lm(y2 ~ x2)
summary(fit.ar)

# (4) 변환된 모형의 독립성 검정
e <- resid(fit.ar) ; n <- length(e) ; sum(e[1:(n-1)] * e[2:n]) / sum(e^2)
lmtest::dwtest(fit.ar)

# 참고 : ar() 함수를 이용한 자기상관계수의 추정값
fit <- lm(steelprice ~ machineprice, steel) ; e <- resid(fit)
ar(e, aic=FALSE, order.max=1)
