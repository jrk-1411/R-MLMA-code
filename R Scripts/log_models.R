# Load necessary libraries
library(dplyr)
library(nnet)

# Read the dataset
data = read.csv("C:\\Users\\khato\\OneDrive\\Documents\\Thesis\\shared-work\\j\\data\\Master_Thesis.csv")
boxplot(data[M_vars], horizontal = TRUE)

# Define the columns to keep
X_vars = c("EA", "EN", "PA", "PN", "SA")
M_vars = c(
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
all_Y = c("IEDTotalerrors", "IEDTotalerrorsadjusted", "IEDCompletedstageerrors",
          "IEDStagescompleted", "SWMBetweenerrors", "SWMStrategy")

colnames(data)

boxplot(Neglect ~ patientgroup_binary, data = data, xlab = 'Patient Group', ylab = 'Neglect')
boxplot(Abuse ~ patientgroup_binary, data = data)

# Boxplot new
boxplot(Abuse ~ patientgroup, data = data)
boxplot(Neglect ~ patientgroup, data = data)
boxplot(Abuse + Neglect ~ patientgroup_binary, data = data)

independantVariables = c("patientgroup")
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

dependantVariable = "IEDCompletedstageerrors"

###########################
#---Data Subsetting---#
###########################
X = data.frame(data[independantVariables])
Y = data.frame(data[dependantVariable])
M = data.frame(data[mediatorVariables])
XM = data.frame(X, M)

mmaAnalysisGLM = mma(x = M, y = Y, mediator = c(1, 1:ncol(M)), pred = X, testtype = 1, jointm = all_cluster, n2 = 50)
summary(mmaAnalysisGLM)

logistic_model_1 = glm(patientgroup_binary ~ EA + EN + PA + PN + SA, data = data, family = binomial())
summary(logistic_model_1)
logistic_model_2 = glm(patientgroup_binary ~ Abuse + Neglect, data = data, family = binomial())
summary(logistic_model_2)

# Multinomial models here
multinom_model = multinom(patientgroup ~ EA + EN + PA + PN + SA, data = data)
multinom_model_2 = multinom(patientgroup ~ Abuse + Neglect, data = data)

coef_model1 = summary(multinom_model)$coefficients
se_model1 = summary(multinom_model)$standard.errors

coef_model2 = summary(multinom_model_2)$coefficients
se_model2 = summary(multinom_model_2)$standard.errors

z_values_model1 = coef_model1 / se_model1
z_values_model2 = coef_model2 / se_model2
p_values_model1 = 2 * (1 - pnorm(abs(z_values_model1)))
p_values_model2 = 2 * (1 - pnorm(abs(z_values_model2)))

print(p_values_model1)
print(p_values_model2)

# 'cluster465420rest_DefaultModeLPr'
# 'ied ~ neglect*diagnosis'

table(data$patientgroup)


