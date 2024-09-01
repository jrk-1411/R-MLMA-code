# Load necessary libraries
library(gbm)
library(mediation)
library(dplyr)

# Load data
dataAll = read.csv("C:\\Users\\khato\\OneDrive\\Documents\\Thesis\\shared-work\\j\\data\\Nona\\GRP_12345NONA.csv")

# Define variables
independentVariables = c("EA", "PA", "SA", "EN", "PN")
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
dependentVariable = "IEDCompletedstageerrors"

X = dataAll[independentVariables]
M = dataAll[mediatorVariables]
Y = dataAll[[dependentVariable]]

# Fit GBM models for each mediator
mediator_models = list()
for (i in 1:ncol(M)) {
  mediator_models[[i]] = gbm(M[[i]] ~ ., data = X, distribution = "gaussian", n.trees = 100, interaction.depth = 3)
}

# Predict mediators using fitted GBM models
mediator_preds = lapply(mediator_models, function(model) predict(model, newdata = X, n.trees = 100))
mediator_preds = as.data.frame(mediator_preds)
colnames(mediator_preds) = mediatorVariables

# Combine X with mediator predictions
X_with_mediators = cbind(X, mediator_preds)

# Fit the outcome model
outcome_model = lm(Y ~ ., data = X_with_mediators)
# Uncomment to use GBM for the outcome model
# outcome_gbm = gbm(Y ~ ., data = X_with_mediators, distribution = 'gaussian', n.trees = 100, interaction.depth = 3)

##############################
## Calculate direct effects ###
##############################
direct_effects = coef(outcome_model)[independentVariables]

##############################
# Calculate indirect effects ##
##############################
indirect_effects = numeric(length(mediatorVariables))
for (i in 1:length(mediatorVariables)) {
  # This part is complex with GBM models - coefficients are not directly available
  # Placeholder: You might need to adapt this part based on your needs
  # Example assumes effect_X_to_M and effect_M_to_Y are obtained differently
  # model_summary = summary(mediator_models[[i]])
  # effect_X_to_M = model_summary$coefficients[2:(length(independentVariables) + 1), "Estimate"]
  effect_M_to_Y = coef(outcome_model)[mediatorVariables[i]]
  indirect_effects[i] = effect_M_to_Y # Modify this line according to actual calculation
}

print(direct_effects)
print(indirect_effects)
# Uncomment to see GBM summary
# summary(outcome_gbm)
