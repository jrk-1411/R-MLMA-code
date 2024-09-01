#establish M~X
library(mediation)
library(glmnet)
data = read.csv("C:\\Users\\khato\\OneDrive\\Documents\\Thesis\\shared-work\\j\\data\\stratify_data.csv")

X = c("EA", "PA", "SA", "EN", "PN")

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
results= data.frame()

fit_regularized_mediators = function(X, M, d) {
  for (x in X) {
    for (m in M){
    formula = as.formula(paste(m, "~", x))
    new_lm = lm(formula, data = d)
    summary_lm = summary(new_lm)
    coefficients = summary_lm$coefficients
    #print(coefficients)
    for (i in 1:nrow(coefficients)) {
      result_row = data.frame(
        Mediator = m,
        X = x,
        Term = rownames(coefficients)[i],
        Estimate = coefficients[i, "Estimate"],
        StdError = coefficients[i, "Std. Error"],
        PValue = coefficients[i, "Pr(>|t|)"]
      )
      results <= rbind(results, result_row)
    }
  }
  }
}
fit_regularized_mediators(X, mediators, data)
write.csv(results, "mediator_univariate.csv", row.names = FALSE)
