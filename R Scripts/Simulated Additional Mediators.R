library(dplyr)
set.seed(123)

n = 1000

X = rnorm(n)
M1 = 0.5 * X + rnorm(n)
M2 = 0.3 * X + rnorm(n)
M3 = 0.4 * X + rnorm(n)
M4 = 0.5 * X + rnorm(n) + M1 + M2 + M3
M5 = 0.1 * X + rchisq(n, df = 1)

Y = 0.2 * X + 0.4 * M1 + 0.3 * M2 + 0.5 * M3 + 0.3 * M4 + rnorm(n)

myData = data.frame(X, M1, M2, M3, M4, M5, Y)

head(data)
summary(data)

model.0 = lm(Y ~ X, myData)

model.M1 = lm(M1 ~ X, myData)
model.M2 = lm(M2 ~ X, myData)
model.M3 = lm(M3 ~ X, myData)
model.M4 = lm(M4 ~ X, myData)
model.M5 = lm(M5 ~ X, myData)
model.Y = lm(Y ~ X + M1 + M2 + M3 + M4 + M5, myData)

results = mediate(model.M5, model.Y, treat = 'X', mediator = 'M5', boot = TRUE, sims = 500)
summary(results)
model.pre.M4 = lm(M4 ~ X + M1 + M2 + M3)
model.pred1.M4 = lm(M4 ~ M3)
sum(abs(residuals(model.pre.M4)))
sum(abs(residuals(model.pred1.M4)))


#############################
########--Using MMA--########
#############################
join = list(n = 1, c('M1', 'M2', 'M3', 'M4', 'M5'))
M = data.frame(M1, M2, M3, M4, M5)

mmaAnalysisMARTFullModel = mma(x = M, y = Y, mediator = 1:ncol(M), pred = X, testtype = 1, jointm = join)
summary(mmaAnalysisMARTFullModel)
mmaAnalysisMARTFullModel$a.contx$estimation
mmaAnalysisMART = mma(x = M, y = Y, mediator = 1:ncol(M), pred = X, testtype = 1, n = 1, nonlinear = TRUE, jointm = join)
summary(mmaAnalysisMART, RE = TRUE)

mmaAnalysisMARTnoJointm = mma(x = M, y = Y, mediator = 1:ncol(M), pred = X, testtype = 1, nonlinear = TRUE)
summary(mmaAnalysisMARTnoJointm, RE = TRUE)
mmaAnalysisMARTnoJointm$a.contx$estimation

mmaAnalysisGLMnoJointm = mma(x = M, y = Y, mediator = 1:ncol(M), pred = X, testtype = 1, n = 1)
summary(mmaAnalysisGLMnoJointm, RE = TRUE)
mmaAnalysisGLMnoJointm$a.contx$estimation

fullModelSummary = capture.output(summary(mmaAnalysisMARTFullModel, RE = TRUE))
martSummary = capture.output(summary(mmaAnalysisMART, RE = TRUE))
noJointmSummary = capture.output(summary(mmaAnalysisMARTnoJointm, RE = TRUE))

combinedSummary = c("Full Model Summary:", fullModelSummary, "\nMART Summary:", martSummary, "\nNo Jointm Summary:", noJointmSummary)
writeLines(combinedSummary, "SimulatedmmaAnalysisSummaries.txt")
XM = data.frame(X, M1, M2, M3)

XM[, 2]

#############################
########--Data Org Big--########
#############################
idenMediators = data.org.big(x = M, y = Y, mediator = 1:ncol(M), pred = X, testtype = 2, alpha = 0.05, alpha2 = 0.05)
summary(idenMediators)
mediationAnalysis = med.big(data = idenMediators)
summary(mediationAnalysis)

mediationAnalysis$coef.model[1:5]
mmaAnalysisGLM$a.contx$estimation
mmaAnalysisMARTnoJointm$a.contx$estimation
predict(mediationAnalysis$model, data = XM)
XM[1, ]
sum(XM[3, ] * mediationAnalysis$coef.model[1:4])
0.200 + 0.131 + 0.006 + 0.654
Y[3]
