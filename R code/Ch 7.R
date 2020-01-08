# [Example 7.1] 광고비예제 : 광고비(ad, X)의 판매액(sale, Y). 광고매체별 차이에 대하여 분석

library(regbook)
adsale ; fit1 <- lm(sale ~ ad + media, adsale) # 문자형 변수인 경우 범주형으로 처리
model.matrix(fit1)
summary(fit1)

# [Example 7.2] 광고비예제. 기울기도 차이가 있을 것이라는 가정 하에 두 평균함수를 비교

fit2 <- lm(sale ~ ad + media + ad:media, adsale)
model.matrix(fit2)
summary(fit2)

# [Example 7.3] 어느 4년제 대학교의 학년 별 영어실력 비교.

english1 ; english1$grade <- factor(english1$grade, levels=c(4, 1:3))
fit <- lm(score ~ grade, english1)
summary(fit)

# [Example 7.4] 영어교육방법 A, B, C 비교. 6개월 동안 각 영어교육방법으로 교육을 받은 후 영어시험성적을 측정. 중학교 2학년 남학생 대상 ⇒ 나이와 성별은 고정. 교육대상자의 IQ는 랜덤화로 평준화. 교육 전 영어실력은 사전 영어시험으로 측정하여 모형에 포함. 교육 후 영어성적(postscore, Y), 교육 전 영어성적(prescore, X1), 교육방법종류(method, X2 : A, B, C)에 대한 공분산분석

english2$method <- relevel(english2$method, ref="C") # 기준범주를 C로 설정
fit <- lm(postscore ~ method + prescore, english2)
summary(fit)

# [Example 7.5] 자료 ‘enzyme’. 배양액의 농도에 따른 효소의 반응속도 측정. 다항회귀모형

fit <- lm(rate ~ conc + I(conc^2) + I(conc^3), enzyme)
summary(fit)

fit <- lm(rate ~ poly(conc, 3, raw=TRUE), enzyme) # raw=TRUE 옵션 : 원래 변수에 대한 다항식의 회귀계수를 계산.

# [Example 7.6] 자료 ‘yield’ : 화학공정에서 공정시간과 공정온도에 따른 효율을 조사

library(rsm) # rsm 패키지의 rsm() 함수 사용
yield.rsm <- rsm(yield ~ SO(time, temp), data=yield)
summary(yield.rsm)

xs(yield.rsm) # 정상점
nw <- data.frame(time=xs(yield.rsm)[1], temp=xs(yield.rsm)[2])
predict(yield.rsm, newdata=nw) # 정상점에서의 반응변수 적합값

# 표준화 후 반응표면분석
yieldcoded <- coded.data(yield, x1~(time-12)/8, x2~(temp-250)/30)
yieldcoded.rsm <- rsm(yield ~ SO(x1, x2), data=yieldcoded)
summary(yieldcoded.rsm)

# 반응표면분석 결과 : 등고선그림과 3차원그림
contour(yield.rsm, ~ time + temp, image=TRUE) # image 옵션 : 색을 추가
persp(yield.rsm, ~ time + temp, theta=120, col="lightcyan") # theta 옵션 : 각도 조정
