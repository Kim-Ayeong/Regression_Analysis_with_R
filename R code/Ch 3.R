# [Example 3.1] 중고차 가격 예제. ‘usedcars’

library(regbook)
usedcars; head(usedcars)
pairs(usedcars)

# [Example 3.2] 중고차 가격 예제. 계수의 최소제곱추정값. 예측값과 잔차, s^2의 계산

usedcars.lm <- lm(price ~ year + mileage + cc + automatic, usedcars)
summary(usedcars.lm)
fitted(usedcars.lm) ; resid(usedcars.lm) # 예측값과 잔차

# [Example 3.3] 중고차 가격 예제. 분산분석표 작성과 F 검정. 잔차표준오차, 결정계수, PRESS, R^2의 값

# (1-1) 분산분석표 작성
usedcars.lm <- lm(price ~ year + mileage + cc + automatic, usedcars)
anova(usedcars.lm)

# (1-2) F 검정
summary(usedcars.lm)

# (2) 잔차표준오차와 결정계수

# (3) PRESS와 예측결정계수
sum(rpredict(usedcars.lm)^2) # regbook 패키지의 rpredict()를 이용

# [Example 3.4] 중고차 가격 예제

# (1) H0:betaj=0 에 대한 t 검정
summary(usedcars.lm)

# betaj에 대한 95% 신뢰구간
confint(usedcars.lm)

# [Example 3.5] 책의 가격(‘bookprice’) 예제, beta0, beta1에 대한 95% 공동신뢰영역과 각 계수에 대한 95% 신뢰구간

# install.packages("ellipse")
library(ellipse)
fit <- lm(price ~ pages, bookprice)
plot(ellipse(fit), type="l")
abline(v=confint(fit)[1,], lty=3, col="cyan")
abline(h=confint(fit)[2,], lty=3, col="cyan")
points(coef(fit)[1], coef(fit)[2], pch=3)

# [Example 3.6] 4개의 설명변수를 모두 사용하는 중고차 가격 예제에서 bata0=525, beta1=-5, beta2=-0.0023, beta3=0, beta4=165라는 가설에 대한 검정

# [Example 3.7] 중고차 가격예제. Y의 평균에 대한 신뢰구간과 새로운 Y값에 대한 예측구간

# (1) 연식=109개월, 주행거리=120,000km, 배기량=1800cc, 변속기=수동 인 경우
predict(usedcars.lm, interval = "confidence") # 신뢰구간
predict(usedcars.lm, interval = "confidence", se.fit=TRUE) # 표준오차를 따로 산출
predict(usedcars.lm, interval = "prediction") # 예측구간

# (2) 연식= 60개월, 주행거리=100,000km, 배기량=2000cc, 변속기=자동 인 경우
nw <- data.frame(year=60, mileage=100000, cc=2000, automatic=1)
nw
predict(usedcars.lm, newdata = nw, interval = "confidence")
predict(usedcars.lm, newdata = nw, interval = "prediction")

# [Example 3.8] 중고차 가격 예제. 잔차분석

par(mfrow = c(2, 2))
plot(usedcars.lm)

# 표준화잔차 대 설명변수의 그림
res <- rstandard(usedcars.lm)
plot(res ~ year, usedcars, ylab="Standardized Residuals")
abline(h=0, lty=3)

# 관측값 대 예측값의 산점도
plot(price ~ fitted(usedcars.lm), usedcars)
abline(a=0, b=1, lty=3)

# RF(Residual-Fit)그림
rfs(usedcars.lm)
