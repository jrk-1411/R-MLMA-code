library(mediation)
library(glmnet)

data = read.csv("C:\\Users\\khato\\OneDrive\\Documents\\Thesis\\shared-work\\j\\data\\stratify_data.csv")

Y = c("IEDTotalerrors",
      "IEDTotalerrorsadjusted",
      "IEDCompletedstageerrors",
      "IEDStagescompleted",
      "SWMBetweenerrors",
      "SWMStrategy")
mediators = c(
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
column_name = "IEDTotalerrors"  
data = data[!is.na(data[[column_name]]), ]

# First check if X predicts Y ---- NOT ABLE TO ESTABLISH THIS
X = c("EA", "PA", "SA", "EN", "PN")

normalLinear = function(Y, X, d) {
  formula = as.formula(paste(Y, "~", paste(X, collapse = " + ")))
  new_lm = lm(formula, data = d)
  print(nrow(d))
  cat("Summary for Y variable", Y)
  print(summary(new_lm))
}

for (i in Y) {
  normalLinear(i, X, data)
}

# Second lets try to establish X and M effect replicating from ideas given before use regularisation for feature selection

fit_regularized_mediators = function(X, mediators, data) {
  for (each in mediators) {
    newdat = complete.cases(data[c(each, X)])
    clean_data = data[newdat, ]
    
    formula = as.formula(paste(each, "~", paste(X, collapse = " + ")))
    design_matrix = model.matrix(formula, clean_data)[, -1]  
    
    response = clean_data[[each]]
    
    ridge_model = cv.glmnet(design_matrix, response, alpha = 0)
    cat("Ridge Regression Model for mediator:", each, ":\n")
    print(coef(ridge_model, s = "lambda.min"))
    
    lasso_model = cv.glmnet(design_matrix, response, alpha = 1)
    cat("Lasso Regression Model for mediator:", each, ":\n")
    print(coef(lasso_model, s = "lambda.min"))
  }
}

fit_regularized_mediators(X, mediators, data)

# Mediators of interest from lasso regularisation above
interest = c('cluster62120rest_DefaultModeLPl', 'cluster105846rest_DefaultModeLPl', 'cluster564252rest_DefaultModeLPl')

new_mediators = c('cluster62120rest_DefaultModeLPl')
fit_regularized_mediators(X, new_mediators, data)
