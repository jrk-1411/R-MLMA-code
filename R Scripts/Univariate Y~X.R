#establish Y~X
library(mediation)
library(glmnet)
data = read.csv("C:\\Users\\khato\\OneDrive\\Documents\\Thesis\\shared-work\\j\\data\\stratify_data.csv")
#Y variables
Y = c("IEDTotalerrors",
       "IEDTotalerrorsadjusted",
       "IEDCompletedstageerrors",
       "IEDStagescompleted",
       "SWMBetweenerrors",
       "SWMStrategy")
#X variables
X = c("EA", "PA", "SA", "EN", "PN")

results = data.frame()

normalLinear = function(Y, X, d) {
  for (x in X) {
    formula = as.formula(paste(Y, "~", x))
    new_lm = lm(formula, data = d)
    
    summary_lm = summary(new_lm)
    coefficients = summary_lm$coefficients
    
    for (i in 1:nrow(coefficients)) {
      result_row = data.frame(
        Y = Y,
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
for (i in Y) {
  normalLinear(i, X, data)
}
write.csv(results, "linear_model_results.csv", row.names = FALSE)