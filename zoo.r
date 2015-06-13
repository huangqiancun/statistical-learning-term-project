library(leaps)
library(MASS)
library(ISLR)
library(class)
library(tree)

set.seed(1)
# workspace
# mac
# setwd('/Users/Chris/Downloads/sl')
# windows
setwd('E:\\Chris\\Documents\\GitHub\\statistical-learning-term-project')

# bank-full.csv
data.bank = read.csv("bank/bank-full.csv", header = TRUE, sep = ";", quote = "\"")

train = sample(dim(data.bank)[1], dim(data.bank)[1]/2)
test = (-train)
data.bank.train = data.bank[train, ]
data.bank.test = data.bank[test, ]

# logistic regression

# yes = 1, no = 0
glm.fit = glm(y ~ ., data = data.bank, family = binomial, subset = train)
summary(glm.fit)
# test
glm.probs = predict(glm.fit, data.bank.test, type="response")
glm.pred = rep("no", dim(data.bank.test)[1])
glm.pred[glm.probs > .5] = "yes"
table(glm.pred, data.bank.test$y)
mean(glm.pred == data.bank.test$y)
# result = 0.9047598

# LDA

lda.fit = lda(y ~ ., data = data.bank, subset = train)
# test
lda.pred = predict(lda.fit, data.bank.test)
lda.class = lda.pred$class
table(lda.class, data.bank.test$y)
mean(lda.class ==  data.bank.test$y)
# result = 0.9036981

# QDA

qda.fit = qda(y ~ ., data = data.bank, subset = train)
# test
qda.pred = predict(qda.fit, data.bank.test)
qda.class = qda.pred$class
table(qda.class, data.bank.test$y)
mean(qda.class ==  data.bank.test$y)
# result = 0.8733964

# KNN?

# best subset
# 42 variables??
# nvmax

regfit.full = regsubsets(y ~ ., data = data.bank)
reg.full.summary = summary(regfit.full)
which.max(reg.full.summary$adjr2)
which.min(reg.full.summary$cp)
which.min(reg.full.summary$bic)
coef(regfit.full, 8)

# forward

regfit.fwd = regsubsets(y ~ ., data = data.bank, method = "forward")
reg.fwd.summary = summary(regfit.fwd)
which.max(reg.fwd.summary$adjr2)
which.min(reg.fwd.summary$cp)
which.min(reg.fwd.summary$bic)
coef(regfit.fwd, 8)

# backward

regfit.bwd = regsubsets(y ~ ., data = data.bank, method = "backward")
reg.bwd.summary = summary(regfit.bwd)
which.max(reg.bwd.summary$adjr2)
which.min(reg.bwd.summary$cp)
which.min(reg.bwd.summary$bic)
coef(regfit.bwd, 8)

# ridge regression & lasso?

# PCR & PLS?

# Decision Tree

tree.bank = tree(y ~ ., data = data.bank, subset = train)
summary(tree.bank)
# cross validation test
cv.bank.tree = cv.tree(tree.bank, FUN = prune.misclass)
prune.bank = prune.misclass(tree.bank, best = 4)
tree.pred = predict(prune.bank, data.bank.test, type = "class")
table(tree.pred, data.bank.test$y)

# SVM?