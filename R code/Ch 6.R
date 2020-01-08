# [Example 6.1] 반응변수: 주택 판매가격(Y , 천만원), 설명변수: 세금(X1,만원), 대지평수(X2,평), 건물평수(X3,평), 주택연령(X4,년)

library(regbook)
pairs(houseprice) # 예비분석
options(digits=3) ; cor(houseprice) # 표본상관계수행렬
fit <- lm(price ~ ., houseprice) ; summary(fit) # 예비모형
plot(fit) # 잔차분석

# [Example 6.2] 주택판매가격 자료. 단계별 회귀를 수행.

# (1) add1(), drop1() 함수를 이용한 대화식 방법
# 단계 1 : 상수항만 있는 모형(model0)을 적합하고, add1()함수를 이용하여 후보 대상군에서 변수를 하나씩만 추가한 4개의 모형을 적합한다.
model0 <- lm(price ~ 1, houseprice)
add1(model0, scope = ~ tax + ground + floor + year, test="F")

# 단계 2 : floor를 제외한 나머지 변수들에 대해 부분 F 검정 실시 ⇒ tax를 모형에 추가
model1 <- update(model0, . ~ . + floor) # 기존의 모형에 수정을 가한 후 다시 적합.
add1(model1, scope = ~ tax + ground + floor + year, test="F")

# 단계 3 : 현재모형에 있는 변수들의 타당성을 조사. ⇒ 미제거
model2 <- update(model1, . ~ . + tax)
drop1(model2, test="F")

# 단계 4 : X2와 X4의 추가 가능성을 위해 부분 F 검정을 실시 ⇒ 해당 변수 없음.
add1(model2, scope = ~ tax + ground + floor + year, test="F")

# (2) step() 함수를 이용한 자동화 방법
model0 <- lm(price ~ 1, houseprice)
step(model0, scope = ~ tax + ground + floor + year, direction = "both")

# [Example 6.3] 주택판매가격 자료. 전진선택법과 후진제거법으로 회귀모형을 적합.

# 전진선택법
model0 <- lm(price ~ 1, houseprice)
step(model0, scope = ~ tax + ground + floor + year, direction = "forward")

# 후진제거법
fit <- lm(price ~ tax + ground + floor + year, houseprice)
step(fit, direction = "backward")

# [Example 6.4] 주택판매가격 예제. X1, X2를 포함한 모형. PRESS을 이용한 모형확인 작업.

fit <- lm(price ~ tax + floor, houseprice)
sum(resid(fit)^2) ; deviance(fit) # SSE=95.2
sum((resid(fit) / (1 - hatvalues(fit)))^2) # PRESS=144

resid(fit) / (1 - hatvalues(fit))
