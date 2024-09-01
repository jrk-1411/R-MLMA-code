# Load necessary libraries
library(gbm)
library(mediation)
library(dplyr)

# Load data
dataAll = read.csv("C:\\Users\\khato\\OneDrive\\Documents\\Thesis\\shared-work\\j\\data\\Nona\\GRP_12345NONA.csv")

# Define variables
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
dependantVariable = "IEDCompletedstageerrors"

# Prepare data
X = dataAll[independantVariables]
M = dataAll[mediatorVariables]
Y = dataAll[[dependantVariable]]

# Fit GBM models for each mediator
mediator_models = list()
for (i in 1:ncol(M)) {
  mediator_models[[i]] = gbm(M[[i]] ~ ., data = X, distribution = "gaussian", n.trees = 100, interaction.depth = 3)
}

# Predict mediator values using the fitted models
mediator_preds = as.data.frame(sapply(mediator_models, function(model) predict(model, newdata = X, n.trees = 100)))
colnames(mediator_preds) = mediatorVariables

# Fit the outcome model including both predictors and predicted mediators
outcome_model = lm(Y ~ ., data = cbind(X, mediator_preds))

# Function to calculate indirect effects
calculate_indirect_effects = function(X, M, mediator_models, outcome_model, mediatorVariables) {
  indirect_effects = numeric(ncol(M))
  for (i in 1:ncol(M)) {
    # Effect of X on M
    model_summary = summary(mediator_models[[i]])
    effect_X_to_M = model_summary$coefficients[2:(length(independantVariables) + 1), "Estimate"]
    
    # Effect of M on Y
    mediator_name = mediatorVariables[i]
    effect_M_to_Y = coef(outcome_model)[mediator_name]
    
    # Indirect effect
    indirect_effects[i] = sum(effect_X_to_M) * effect_M_to_Y
  }
  return(indirect_effects)
}

# Calculate indirect effects
indirect_effects = calculate_indirect_effects(X, M, mediator_models, outcome_model, mediatorVariables)

# Calculate direct effects
direct_effects = coef(outcome_model)[names(coef(outcome_model)) %in% independantVariables]

# Calculate total effects
total_effects = direct_effects + colSums(matrix(indirect_effects, nrow = length(independantVariables), ncol = length(mediatorVariables)))

# Print results
cat("Direct Effects:\n")
print(direct_effects)
cat("\nIndirect Effects:\n")
print(indirect_effects)
cat("\nTotal Effects:\n")
print(total_effects)

# Bootstrapping to get confidence intervals
bootstrapping = function(X, Y, M, B = 1000, independantVariables, mediatorVariables) {
  n = nrow(X)
  boot_indirect_effects = matrix(0, B, ncol(M))
  for (b in 1:B) {
    indices = sample(1:n, n, replace = TRUE)
    X_boot = X[indices, ]
    Y_boot = Y[indices]
    M_boot = M[indices, ]
    
    # Fit GBM models for each mediator
    mediator_models_boot = list()
    for (i in 1:ncol(M_boot)) {
      mediator_models_boot[[i]] = gbm(M_boot[, i] ~ ., data = X_boot, distribution = "gaussian", n.trees = 100, interaction.depth = 3)
    }
    
    # Predict mediator values using the fitted models
    mediator_preds_boot = as.data.frame(sapply(mediator_models_boot, function(model) predict(model, newdata = X_boot, n.trees = 100)))
    colnames(mediator_preds_boot) = mediatorVariables
    
    # Fit the outcome model including both predictors and predicted mediators
    outcome_model_boot = lm(Y_boot ~ ., data = cbind(X_boot, mediator_preds_boot))
    
    # Calculate indirect effects
    boot_indirect_effects[b, ] = calculate_indirect_effects(X_boot, M_boot, mediator_models_boot, outcome_model_boot, mediatorVariables)
  }
  
  return(boot_indirect_effects)
}

# Perform bootstrapping
set.seed(123)
boot_indirect_effects = bootstrapping(X, Y, M, B = 10, independantVariables, mediatorVariables)

# Calculate 95% confidence intervals for indirect effects
ci_lower = apply(boot_indirect_effects, 2, function(x) quantile(x, 0.025))
ci_upper = apply(boot_indirect_effects, 2, function(x) quantile(x, 0.975))

# Print confidence intervals
cat("\n95% Confidence Intervals for Indirect Effects:\n")
print(data.frame(Lower = ci_lower, Upper = ci_upper))
