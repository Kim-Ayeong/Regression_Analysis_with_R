# (1) Load data
setwd('C:/workspace/R/회귀분석2')
rawdata = read.csv('train.csv')

# (2) EDA
head(rawdata)
dim(rawdata)
colSums(is.na(rawdata)) # Age에 missing value 177개 
summary(rawdata) # Embarked에 ""(공란) 2개

# (3) Pre-proccessing
# 3-1) Select feature
data = subset(rawdata, select=-c(PassengerId, Name, Ticket, Cabin))
head(data)
dim(data)

# 3-2) set type of variable
apply(data, 2, function(x) length(unique(x))) # value 범주 확인
str(data)
data$Pclass = as.factor(data$Pclass) # Survived(y)는 제외
str(data)

# 3-3) fill numeric missing value
summary(data$Age)
data$Age[is.na(data$Age)] = mean(data$Age, na.rm=T)
summary(data$Age)

# 3-4) fill categorical missing value
table(data$Embarked)
data$Embarked[data$Embarked==""] = "S" # 최빈값
data$Embarked = factor(data$Embarked, levels = c("C", "Q", "S"))
table(data$Embarked)

# 3-5) split X, y 
X = data[-data$Survived]; dim(X)
y = data$Survived; length(y)

# 3-6) data visualization
# categorical
Survive <- xtabs(Survived ~ Pclass + Sex + Embarked, data)
Survive
mosaicplot(Survive, color=T)

# numeric
plot(Survived~Age, pch=20, data); abline(lm(Survived~Age, data), col='red')
plot(Survived~SibSp, pch=20, data); abline(lm(Survived~SibSp, data), col='red')
plot(Survived~Parch, pch=20, data); abline(lm(Survived~Parch, data), col='red')
plot(Survived~Fare, pch=20, data); abline(lm(Survived~Fare, data), col='red')

pairs(X)

# Standardization 시행(?)

# (4) Model fitting
# 4-1) full logistic regression model
model_logis = glm(Survived~., family=binomial(link="logit"), data=data, trace=F)
summary(model_logis)
deviance(model_logis)

# 4-2) stepwise logistic regression
library(MASS)
model_step = stepAIC(model_logis, trace=F) 
model_step$anova
summary(model_step)
deviance(model_step)

# 4-3) probit model
model_probit = glm(Survived~., family=binomial(link="probit"), data=data, trace=F)
summary(model_probit)
deviance(model_probit)

# 4-4) cloglog model
model_cloglog = glm(Survived~., family=binomial(link="cloglog"), data=data, trace=F)
summary(model_cloglog)
deviance(model_cloglog)

# compare model
anova(model_step, model_probit, model_cloglog, test="Chisq")

# (5) Predict & Scoring
test = read.csv('test.csv')
head(test)
dim(test)

test$Pclass = as.factor(test$Pclass)
y_prob = predict(model_cloglog, newdata=test, type="response")

result = read.csv('gender_submission.csv')
head(result)
dim(result) # PassengerId 동일

table(result$Survived, y_prob>=0.5)
(197+115)/dim(result)[1] # Accuracy = 74.6%



