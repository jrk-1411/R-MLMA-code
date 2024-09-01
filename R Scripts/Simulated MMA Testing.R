library(dplyr)
set.seed(123)

n = 1000

X = rnorm(n)
M1 = 0.5 * X + rnorm(n)
M2 = 0.3 * X + rnorm(n)
M3 = 0.4 * X + rnorm(n)
Y = 0.2 * X + 0.4 * M1 + 0.3 * M2 + 0.5 * M3 + rnorm(n)

myData = data.frame(X, M1, M2, M3, Y)
plot(X, Y)
head(data)
summary(data)

model.0 = lm(Y ~ X, myData)
summary(model.Y)
moe.m = lm(Y ~ M1 + M2 + M3, myData)
model.M1 = lm(M1 ~ X, myData)
model.M2 = lm(M2 ~ X, myData)
model.M3 = lm(M3 ~ X, myData)
model.Y = lm(Y ~ X + M1 + M2 + M3, myData)

results = mediate(model.M1, model.Y, treat = 'X', mediator = 'M1', boot = TRUE, sims = 500)
summary(results)
sum(abs(residuals(model.0)))
sum(abs(residuals(model.Y)))


#############################
######--Using MMA GLM--######
#############################

XM = data.frame(X, M1, M2, M3)
M = data.frame(M1, M2, M3)

mmaAnalysisGLM = mma(x = M, y = Y, mediator = 1:ncol(M), pred = X, testtype = 1, alpha = 0.05, alpha2 = 0.05, n = 1)
summary(mmaAnalysisGLM)



#############################
########--Data Org Big--########
#############################
idenMediators = data.org.big(x = M, y = Y, mediator = 1:ncol(M), pred = X, testtype = 1, alpha = 0.05, alpha2 = 0.05)
summary(idenMediators)
mediationAnalysis = med.big(data = idenMediators)
summary(mediationAnalysis)
mediationAnalysis$coef.model[1:4]
mmaAnalysisGLM$a.contx$estimation
predict(mediationAnalysis$model, data = XM)
XM[1, ]
sum(XM[3, ] * mediationAnalysis$coef.model[1:4])
0.200 + 0.131 + 0.006 + 0.654
Y[3]
