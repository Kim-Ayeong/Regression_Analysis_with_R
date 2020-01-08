# [Example 2.1] 자료 ‘aflength’ : 남녀 각각 16명. 발길이와 앞팔길이 자료(단위:mm)

plot(foot ~ forearm, aflength)

# [Example 2.2] 자료 ‘aflength’. 절편과 기울기의 최소제곱추정값, 예측값과 잔차

# (1) 수작업을 이용한 계산

# (2) R을 사용한 통계값 직접 계산
x <- aflength$forearm ; y <- aflength$foot ; ( xbar <- mean(x) ) ; ( ybar <- mean(y) )
( sxx <- sum(x^2) ) ; ( syy <- sum(y^2) ) ; ( sxy <- sum(x*y) )

( n <- nrow(aflength) ) ; ( b1 <- ( sxy - n*xbar*ybar ) / ( sxx -n*xbar^2 ) )
( b0 <- ybar - b1*xbar )

# (2) R의 lm() 함수를 이용한 추정
lm(foot ~ forearm, aflength)

# (3) R을 이용한 적합값과 잔차의 계산
aflength.lm <- lm(foot ~ forearm, aflength)
fitted(aflength.lm)
resid(aflength.lm)
aflength$foot - fitted(aflength.lm)

# (4) 관측값들과 함께 표시된 추정회귀직선
plot(foot ~ forearm, aflength)
abline(aflength.lm)

# [Example 2.3] 갤튼의 키 자료

# [Example 2.4] 자료 ‘aflength’ : 오차분산 추정값 s^2

summary(aflength.lm)

e <- resid(aflength.lm) ; ( n <- length(e) ) ; ( sse <- sum(e^2) ) ; ( s2 <- sse / (n-2) )
( s <- sqrt(s2) )

# [Example 2.5] 자료 ‘aflength’

# (1) 분산분석표와 F 검정
aflength.lm <- lm(foot ~ forearm, aflength)
anova(aflength.lm)

with(aflength, sum((foot -mean(foot))^2))

# (2) s와 결정계수
summary(aflength.lm)

# (3) PRESS와 예측결정계수 계산 : regbook 패키지의 rpredict() 함수 이용
rpredict(aflength.lm)
r <- rpredict(aflength.lm) ; sum(r^2)

# [Example 2.6] 자료 ‘aflength’. 회귀계수에 대한 추론

summary(aflength.lm)

confint(aflength.lm)

# [Example 2.7] 자료 ‘aflength’. X=260에서 Y의 평균과 새로운 Y값에 대한 추론

predict(aflength.lm, se.fit=TRUE) # 예측값과 표준오차
predict(aflength.lm, interval="confidence") # 평균에 대한 신뢰구간
predict(aflength.lm, interval="prediction") # Y의 새로운 값에 대한 예측구간

newdata <- data.frame(forearm=260) # 주어진 X값이 현재의 자료에 없는 값인 경우
predict(aflength.lm, newdata, interval="confidence")
predict(aflength.lm, newdata, interval="prediction")

fitplot(aflength.lm) # 관측값의 산점도, 추정 회귀직선, 95% 신뢰대와 예측대: regbook 패키지의 fitplot() 함수

# [Example 2.8] 발길이와 앞팔길이 예제

# (1) 등분산성 검정 : ‘잔차 대 예측값’과 ‘표준화잔차 대 예측값’의 산점도

# (2) 정규성 검정 : 잔차의 정규확률도, 히스토그램, 상자그림
par(mfrow=c(2, 2))
plot(aflength.lm)

resid(aflength.lm) # 잔차
rstandard(aflength.lm) # 표준화잔차

histf(rstandard(aflength.lm), boxplot=TRUE, rug=TRUE) # 표준화잔차의 히스토그램과 상자그림

# (3) 적합의 정도 : 반응변수의 관측값( )과 예측값( )의 산점도, RF 그림
plot(foot ~ fitted(aflength.lm), aflength)
abline(0, 1, lty=3)

summary(aflength.lm)$r.squared
library(lattice)
rfs(aflength.lm)

# (4) 잔차 대 설명변수의 산점도, 표준화잔차 대 관측순서의 산점도
plot(rstandard(aflength.lm) ~ forearm, aflength) # 표준화잔차 대 설명변수
abline(h=0, lty=3)
plot(rstandard(aflength.lm)) # 표준화잔차 대 관측순서
abline(h=0, lty=3)

# [Example 2.9] 자료 ‘quadratic’. 모형설정이 잘못된 경우의 예제

# loess 평활곡선
plot(y ~ x, quadratic)
quadratic.lm1 <- lm(y ~ x, quadratic)
abline(quadratic.lm1)
with(quadratic, lines(lowess(x, y), lty = 2))
par(mfrow=c(2, 2))
plot(quadratic.lm1)

quadratic.lm2 <-lm(y ~ x + I(x^2), quadratic)
summary(quadratic.lm2)
plot(quadratic.lm2)

# [Example 2.10] 자료 ‘aflength’. 상수항이 없는 단순선형회귀모형

aflength.noint <- lm(foot ~ -1 + forearm, aflength)
summary(aflength.noint)
confint(aflength.noint)
anova(aflength.noint)
