# [Example 9.1] 자료 ‘growth1’ : 식물의 성장 속도에 관한 실험 결과. 성장치(Y)와 시간(X)

library(regbook)
plot(y ~ x, growth1)

# [Example 9.2] 식물의 성장에 관한 예제.  과  의 초기값. 최소제곱추정값

growth1.nls <- nls(y ~ theta1*x/(theta2 + x), data=growth1, start=list(theta1=22, theta2=9), trace=TRUE)
summary(growth1.nls)

# 적합된 모형식의 그림
plot(y ~ x, growth1)
theta <- coef(growth1.nls)
curve(theta[1]*x / (theta[2] + x), add=TRUE, col=2)
# 초기값 자동 생성 함수 이용하는 방법
growth1.nls <- nls(y ~ SSmicmen(x, theta1, theta2), growth1, trace=TRUE)

# [Example 9.3] 식물의 성장에 관한 예제

# (1) 오차분산의 추정값 계산
deviance(growth1.nls) / df.residual(growth1.nls)

# (2) 모수에 대한 신뢰구간
regbook::waldint(growth1.nls)

# (3) 가설 H0:theta1=30 에 대한 t 검정

# (4) 잔차분석
r <- residuals(growth1.nls, type="pearson") # 비선형회귀분석에서의 표준화잔차

# [Example 9.4] 식물의 성장에 관한 예제 : 프로파일 t 함수를 이용한 신뢰구간과 곡률분석

growth1.nls <- update(growth1.nls, trace=FALSE)
confint(growth1.nls)

# 프로파일 t 함수의 그림
par(mfrow=c(1,2))
plot(profile(growth1.nls), absVal=FALSE, conf=0.95)

# [Example 9.5] 식물의 성장에 관한 예제 : 모수변환을 이용한 분석

fit <- nls(y ~ x/(beta1 + beta2 * x), growth1, start=list(beta1=12.5744/28.1370, beta2=1/28.1370))
summary(fit)

# confint()를 이용한 신뢰구간
confint(fit)
par(mfrow=c(1,2))
plot(profile(fit), absVal=FALSE, conf=0.95)

# 붓스트랩 신뢰구간
library(nlstools)
boot.ci <- nlstools::nlsBoot(fit)
summary(boot.ci)
