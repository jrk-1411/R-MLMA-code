library(mma)
library(splines)

estimate_effects = function(sampled_x, x, y, M, k, a, N) {
  n = length(y)
  p = ncol(M)
  
  M1 = matrix(0, nrow=N, ncol=p)
  M2 = matrix(0, nrow=N, ncol=p)
  
  for (j in 1:N) {
    for (col in 1:p) {
      if (length(unique(x)) < 4) {
        M1[j, col] = mean(M[, col])
        M2[j, col] = mean(M[, col])
      } else {
        M1[j, col] = predict(smooth.spline(x, M[, col], tol=1e-8)$fit, sampled_x[j])$y
        M2[j, col] = predict(smooth.spline(x, M[, col], tol=1e-8)$fit, sampled_x[j] + a)$y
      }
    }
  }
  
  M_k_combined = c(M1[, k], M2[, k])
  M_k_permuted = sample(M_k_combined)
  
  data = data.frame(x, M)
  colnames(data) = c("x", paste0("M", 1:ncol(M)))
  formula = as.formula(paste("y ~", paste(colnames(data), collapse = "+")))
  glm_model = glm(formula, data=data, family=gaussian())
  
  model_predict = function(x_values, M_matrix) {
    x_values_repeated = matrix(rep(x_values, each=nrow(M_matrix)), nrow=nrow(M_matrix))
    data_pred = data.frame(x_values_repeated, M_matrix)
    colnames(data_pred) = c("x", paste0("M", 1:ncol(M_matrix)))
    predict(glm_model, newdata=data_pred)
  }
  
  total_effect = 0
  direct_effect = 0
  for (j in 1:N) {
    effect1_total = model_predict(sampled_x[j] + a, M2[j, , drop=FALSE])
    effect2_total = model_predict(sampled_x[j], M1[j, , drop=FALSE])
    total_effect = total_effect + (effect1_total - effect2_total)
    
    M1[j, k] = M_k_permuted[j]
    M2[j, k] = M_k_permuted[N + j]
    
    effect1_direct = model_predict(sampled_x[j] + a, M2[j, , drop=FALSE])
    effect2_direct = model_predict(sampled_x[j], M1[j, , drop=FALSE])
    direct_effect = direct_effect + (effect1_direct - effect2_direct)
  }
  
  total_effect = total_effect / (a * N)
  direct_effect = direct_effect / (a * N)
  indirect_effect = total_effect - direct_effect
  
  return(list(total_effect=total_effect, direct_effect=direct_effect, indirect_effect=indirect_effect))
}

n = 1000
set.seed(123)
n = 100
X = rnorm(n)
M1 = 0.6 * X + rnorm(n)
M2 = 0.2 * X + rnorm(n)
M3 = 0.5 * X + rnorm(n)
M4 = 0.3 * X + rnorm(n) + 0.4 * M1 + 0.2 * M2 + M3
M5 = 0.2 + rchisq(n, df=2)
Y = 0.1 * X + 0.1 * M1 + 0.2 * M2 + 0.4 * M3 + 0.7 * M4 + rnorm(n)

myData = data.frame(X, M1, M2, M3, M4, M5, Y)

x = myData$X
y = myData$Y
M = as.matrix(myData[, c("M1", "M2", "M3", "M4", "M5")])

a = 1
N = 100
B = 50
n=100

mma_res=
  bootstrap_results = list(total_effect=numeric(B),
                           direct_effect=list(),
                           indirect_effect=list())
for (k in 1:ncol(M)) {
  bootstrap_results$direct_effect[[paste0("M", k)]] = numeric(B)
  bootstrap_results$indirect_effect[[paste0("M", k)]] = numeric(B)
}

for (i in 1:B) {
  cat("Bootstrap Run:", i, "of", B, "...\n")
  sampled_indices = sample(1:n, size=N, replace=TRUE)
  sampled_x = x[sampled_indices]
  
  for (k in 1:ncol(M)) {
    effects = estimate_effects(sampled_x, x, y, M, k, a, N)
    bootstrap_results$total_effect[i] = effects$total_effect
    bootstrap_results$direct_effect[[paste0("M", k)]][i] = effects$direct_effect
    bootstrap_results$indirect_effect[[paste0("M", k)]][i] = effects$indirect_effect
  }
}

bootstrap_mean_total_effect = mean(bootstrap_results$total_effect)
bootstrap_ci_total_effect = quantile(bootstrap_results$total_effect, probs=c(0.025, 0.975))

cat("Total Effect:", bootstrap_mean_total_effect, "\n")
cat("Total Effect 95% CI:", bootstrap_ci_total_effect, "\n")

lis = length(5)
for (k in 1:ncol(M)) {
  mediator = paste0("M", k)
  bootstrap_samples_direct_effect = bootstrap_results$direct_effect[[mediator]]
  bootstrap_samples_indirect_effect = bootstrap_results$indirect_effect[[mediator]]
  
  bootstrap_mean_direct_effect = mean(bootstrap_samples_direct_effect)
  bootstrap_ci_direct_effect = quantile(bootstrap_samples_direct_effect, probs=c(0.025, 0.975))
  
  bootstrap_mean_indirect_effect = mean(bootstrap_samples_indirect_effect)
  bootstrap_ci_indirect_effect_quant = quantile(bootstrap_samples_indirect_effect, probs=c(0.025, 0.975))
  lis[k] = bootstrap_mean_indirect_effect
  bootstrap_ci_indirect_effect = quantile(bootstrap_samples_indirect_effect, probs=c(0.025, 0.975))
  cat("Indirect Effect Through", mediator, "Val:", bootstrap_mean_indirect_effect, "\n")
  cat("Quantile CI,",round(bootstrap_ci_indirect_effect_quant,3),"\n")
}
de=bootstrap_mean_total_effect-sum(lis)

lis = c(lis,de)
lis = data.frame(lis)
rownames(lis) = c("M1","M2","M3","M4","M5")
lis_sorted = lis[order(-lis$lis), , drop=FALSE]
barplot(lis_sorted$lis,
        horiz = TRUE,               
        col = "skyblue",            
        border = "white",           
        main = "Effects From Mediators/Predictors",   
        xlab = "Effect",         
        ylab = "Mediators",
        names.arg = rownames(lis_sorted), 
        cex.main = 1.2,             
        cex.lab = 1.0,             
        cex.axis = 0.8,             
        las = 1                     
)
