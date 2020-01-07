# [Example 1.3] 제5차 한국인 인체치수조사사업(Size Korea)

# install.packages("devtools") # ‘regbook’ 패키지 설치하기
library(devtools)
# install_github("regbook/regbook")
library(regbook) # ‘regbook’ 패키지 읽어오기
data(package="regbook") # ‘regbook’ 패키지에 포함된 예제 자료의 목록보기(각 자료에는 도움말이 제공)

hweight
male <- subset(hweight, gender=="M") # 남자만 뽑아 새로운 데이터프레임 만들기

hist(male$height) # 히스토그램 작성하기
hist(male$height, probability=TRUE) # 세로축이 비율인 히스토그램률과 추정된 확밀도함수 그리기
lines(density(male$height))

histf(male$height) # regbook 패키지의 histf() 함수 이용 : 평활곡선과 정규분포의 pdf 추가

# [Example 1.4] 자료 ‘hweight’. 남자들의 키에 대한 기본측도들 계산
male <- subset(hweight, gender=="M")
y <- male$height ; summary(y)
length(y) ; sum(y) ; mean(y) ; median(y) ; var(y) ; sd(y) ; IQR(y)
quantile(y) ; quantile(y, probs=c(0.1, 0.9))
min(y) ; max(y) ;
range(y) ; diff(range(y))

# [Example 1.5] 자료 ‘hweight’. 키에 대한 남녀별 상자그림 작성
boxplot(height ~ gender, hweight, horizontal=TRUE) # 남녀별 상자그림(가로 방향)

# [Example 1.6] 자료 ‘hweight’. 남자들의 평균 키에 대한 신뢰구간과 가설검정
# (1) 남자들의 평균 키에 대한 95% 신뢰구간
male <- subset(hweight, gender=="M")
y <- male$height
n <- length(y)
mean(y) + qt(c(0.025, 0.975), n-1) * sd(y) / sqrt(n)

# (2) 남자들의 평균 키가 173cm이라는 가설에 대한 양측 t 검정
( n <- length(y) )
( ybar <- mean(y) )
( s <- sd(y) )
( se <- s / sqrt(n))
qt(0.975, df=length(y)-1) # t(0.025; 343)
2*(1-pt(2.477, df=length(y)-1)) # p값 계산
t.test(male$height, mu=173, alternative="two.sided") # t.test() 함수를 이용한 검정

histf(y) # 정규성 가정의 타당성 조사 : 히스토그램과 Q-Q그림
qqnorm(y) ; qqline(y)

# [Example 1.7] 자료 ‘hweight’. 남자들의 몸무게와 키의 표본상관계수
# 두 변수간의 상관계수가 0.4라는 가설을 유의수준 0.05에서 양측검정
male <- subset(hweight, gender=="M")
with(male, cor(height, weight))
with(male, cor.test(height, weight))
# 피셔의 Z 변환 통계량값을 이용한 검정
male <- subset(hweight, gender=="M")
( n <- nrow(male) )
( r <- with(male, cor(height, weight)) )
rho <- 0.4
( z <- 1/2 * sqrt(n-3) * log( ((1+r)*(1-rho)) / ((1-r)*(1+rho)) ) )
( p.value <- 2 * (1 - pnorm(z)) )
