library(mmabig)
library(mma)
###########################
#---Setup Data---#
###########################
data = read.csv("C:\\Users\\khato\\OneDrive\\Documents\\Thesis\\shared-work\\j\\data\\Master_Thesis.csv")
independantVariables = c("EA", "PA", "SA", "EN", "PN")
mediatorVariables = c(
  'cluster406824rest_DefaultModeLPl', 'cluster104814rest_DefaultModeLPl',
  'cluster564252rest_DefaultModeLPl', 'cluster208050rest_DefaultModeLPl',
  'cluster105846rest_DefaultModeLPl', 'cluster52832rest_DefaultModeLPl',
  'cluster54328rest_DefaultModeLPl', 'cluster62120rest_DefaultModeLPl',
  'cluster205652rest_DefaultModeLPl', 'cluster8444rest_DefaultModeLPl',
  'cluster145874rest_DefaultModeLPl', 'cluster465420rest_DefaultModeLPr',
  'cluster345824rest_DefaultModeLPr', 'cluster24620rest_DefaultModeLPr',
  'cluster0560rest_DefaultModeMPFC', 'cluster42222rest_DefaultModeMPFC',
  'cluster561618rest_DefaultModeMPFC', 'cluster45628rest_DefaultModePCC',
  'cluster242258rest_DefaultModePCC', 'cluster36462rest_DefaultModePCC'
)
# Multiply the mediator variables by 1000
data[mediatorVariables] = data[mediatorVariables] * 1000

dependantVariable = "IEDCompletedstageerrors"
###########################
#---Data Subsetting---#
###########################
X = data.frame(data[independantVariables])
Y = data.frame(data[dependantVariable])
M = data.frame(data[mediatorVariables])
XM = data.frame(X, M)
all_cluster = list(n = 1, j1 = c('cluster406824rest_DefaultModeLPl', 'cluster104814rest_DefaultModeLPl',
                                 'cluster564252rest_DefaultModeLPl', 'cluster208050rest_DefaultModeLPl',
                                 'cluster105846rest_DefaultModeLPl', 'cluster52832rest_DefaultModeLPl',
                                 'cluster54328rest_DefaultModeLPl', 'cluster62120rest_DefaultModeLPl',
                                 'cluster205652rest_DefaultModeLPl', 'cluster8444rest_DefaultModeLPl',
                                 'cluster145874rest_DefaultModeLPl', 'cluster465420rest_DefaultModeLPr',
                                 'cluster345824rest_DefaultModeLPr', 'cluster24620rest_DefaultModeLPr',
                                 'cluster0560rest_DefaultModeMPFC', 'cluster42222rest_DefaultModeMPFC',
                                 'cluster561618rest_DefaultModeMPFC', 'cluster45628rest_DefaultModePCC',
                                 'cluster242258rest_DefaultModePCC', 'cluster36462rest_DefaultModePCC'))

# GBM MODELS
mmaAnalysisMART = mma(x = M, y = Y, mediator = c(1, 1:ncol(M)), pred = X, testtype = 1, jointm = all_cluster, n2 = 50)
summary(mmaAnalysisMART)
mmaAnalysisMART$a.contx$estimation

summary_output = capture.output(summary(mmaAnalysisMART))
mmaAnalysis$model
# saving
summary_output = capture.output(summary(mmaAnalysis))

# Save the summary output to a text file
writeLines(summary_output, "mma_mediation_summary.txt")

# Optionally, you can read the summary output and convert it to a data frame
summary_text = readLines("mma_mediation_summary.txt")
summary_df = read.table(text = summary_text, header = TRUE, sep = "\t")
write.csv(summary_df, "mma_mediation_summary.csv", row.names = FALSE)
# til here----

# jointm not specified GBM
mmaAnalysis2 = mma(x = M, y = Y, mediator = 1:ncol(M), pred = X, nonlinear = TRUE, all.model = TRUE, contmed = 1:ncol(M), testtype = 2)
summary(mmaAnalysis2, RE = TRUE)
predict(mmaAnalysis2, X)
model(mmaAnalysis2)
mmaAnalysis$a.contx$model$model
# GLM MODELS
mmaAnalysisGLM = mma(x = M, y = Y, mediator = 1:ncol(M), pred = X, testtype = 1, all.model = TRUE, contmed = 1:ncol(M))
summary(mmaAnalysisGLM, bymed = TRUE)
mmaAnalysisGLM$a.contx$estimation
# One By One gives not viable Mediator - Use Full Model like above
mmaAnalysisGLM2 = mma(x = M, y = Y, mediator = 1:ncol(M), pred = X, testtype = 2, contmed = 1:ncol(M))
summary(mmaAnalysisGLM2, RE = TRUE)


# Full Model but with Jointm Condition
mmaAnalysisGLMJoint = mma(x = M, y = Y, mediator = 1:ncol(M), pred = X, testtype = 1, all.model = TRUE, contmed = 1:ncol(M), jointm = all_cluster)
summary(mmaAnalysisGLMJoint)
mmaAnalysisGLMJoint$a.contx$estimation


################################
# Manual Verification of mma----#
################################
# Y ~ X + M
my_lm = lm(IEDCompletedstageerrors ~ EA + EN + PA + PN + SA + cluster465420rest_DefaultModeLPr, data = data)
# M ~ X
mod1 = lm(cluster465420rest_DefaultModeLPr ~ EA + EN + PA + PN + SA, data = data)
# X ~ Y
xy_lm = lm(IEDCompletedstageerrors ~ EA + EN + PA + PN + SA, data = data)
summary(my_lm)
summary(mod1)
summary(xy_lm)

results = mediate(mod1, my_lm, treat = 'EN', mediator = 'cluster465420rest_DefaultModeLPr',
                  boot = TRUE, sims = 500, covariates = c("EA", "SA", "PA", "PN"))
summary(results)
output = capture.output(print(mma::mma))
write(output, file = "output_mma.txt")
boxplot(M)
help(boxplot)
