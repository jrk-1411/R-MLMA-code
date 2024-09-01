# Load necessary libraries
library(mma)
library(dplyr)

data = read.csv("C:\\Users\\khato\\OneDrive\\Documents\\Thesis\\shared-work\\j\\data\\Master_Thesis.csv")
independentVariables = c("patientgroup_binary")
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
dependentVariable = "SWMBetweenerrors"

mediatorVariables = c('cluster465420rest_DefaultModeLPr')
# Data 
X = data[independentVariables]
Y = data[dependentVariable]
M = data[mediatorVariables]
XM = data.frame(X, M)
all_cluster = list(n = 1, j1 = mediatorVariables)

# mma here
mmaAnalysisMART2 = mma(x = M, y = Y, mediator = c(1, 1:ncol(M)), pred = X, testtype = 1, nonlinear = TRUE, jointm = all_cluster, n2 = 50)
#GLM
mmaAnalysisGLM = mma(x = M, y = Y, mediator = c(1, 1:ncol(M)), pred = X, testtype = 1, n2 = 50, jointm = all_cluster)
# Summary of the analysis
summary(mmaAnalysisMART)
summary(mmaAnalysisMART2)
summary(mmaAnalysisGLM)
mmaAnalysisGLM$a.contx$model

independentVariable = "patientgroup_binary"
mediatorVariable = "cluster465420rest_DefaultModeLPr"
dependentVariable = "IEDTotalerrorsadjusted"
mediator_model = lm(cluster465420rest_DefaultModeLPr ~ patientgroup_binary, data = data)
outcome_model = lm(SWMBetweenerrors ~ patientgroup_binary + cluster465420rest_DefaultModeLPr, data = data)
mediation_analysis = mediate(mediator_model, outcome_model, treat = "patientgroup_binary", mediator = "cluster465420rest_DefaultModeLPr", boot = TRUE, sims = 500)
summary(mediation_analysis)

####################################################################
head(data)
boxplot(data$Neglect ~ data$patientgroup_binary)
lk = glm(patientgroup_binary ~ EA + EN + PA + PN + SA, data = data, family = 'binomial')
lk2 = glm(patientgroup_binary ~ Abuse + Neglect, data = data, family = 'binomial')
summary(lk)
summary(lk2)
library(nnet)
model = multinom(patientgroup ~ Neglect + Abuse, data = data)

summary(model)
# Make predictions on the training data
train_predictions = predict(model, type = "class")

# Create a confusion matrix
confusion_matrix = table(train_predictions, data$patientgroup)
print(confusion_matrix)

## new glm

lm = glm(SWMBetweenerrors ~ Neglect * factor(patientgroup), data = data)
summary(lm)

cluster406824rest_DefaultModeLPl, cluster104814rest_DefaultModeLPl, cluster145874rest_DefaultModeLPl, cluster465420rest_DefaultModeLPr
