library(mma)
library(splines)
library(randomForest)
library(ipred)
library(e1071)
library(caret)

#estimate total,indirect effects
estimate_effects=function(sampled_x, x, y, M, k, a, N) {
  n=length(y)
  p=ncol(M)
  M1=matrix(0,nrow=N,ncol=p)
  M2=matrix(0,nrow=N,ncol=p)
  
  for (j in 1:N) {
    M1[j,]=sapply(1:p, function(col) {
      predict(smooth.spline(x, M[,col], tol=1e-8)$fit, sampled_x[j])$y
    })
    M2[j,]=sapply(1:p, function(col) {
      predict(smooth.spline(x, M[,col], tol=1e-8)$fit, sampled_x[j]+a)$y
    })
  }
  
  M_k_combined=c(M1[,k], M2[,k])
  M_k_permuted=sample(M_k_combined)
  
  data=data.frame(x, M)
  colnames(data)=c("x", paste0("M", 1:ncol(M)))
  formula=as.formula(paste("y ~", paste(colnames(data), collapse="+")))
  
  # this is yout f(xyz)..change f to whatever you wish. 
  glm_model=randomForest(formula, data=data)
  
  model_predict=function(x_values, M_matrix) {
    x_values_repeated=matrix(rep(x_values, each=nrow(M_matrix)), nrow=nrow(M_matrix))
    data_pred=data.frame(x_values_repeated, M_matrix)
    colnames(data_pred)=c("x", paste0("M", 1:ncol(M_matrix)))
    predict(glm_model, newdata=data_pred)
  }
  
  total_effect=0
  direct_effect=0
  for (j in 1:N) {
    effect1_total=model_predict(sampled_x[j]+a, M2[j,,drop=FALSE])
    effect2_total=model_predict(sampled_x[j], M1[j,,drop=FALSE])
    total_effect=total_effect+(effect1_total-effect2_total)
    
    M1[j,k]=M_k_permuted[j]
    M2[j,k]=M_k_permuted[N+j]
    
    effect1_direct=model_predict(sampled_x[j]+a, M2[j,,drop=FALSE])
    effect2_direct=model_predict(sampled_x[j], M1[j,,drop=FALSE])
    direct_effect=direct_effect+(effect1_direct-effect2_direct)
  }
  
  total_effect=total_effect/(a*N)
  direct_effect=direct_effect/(a*N)
  indirect_effect=total_effect-direct_effect
  
  return(list(total_effect=total_effect, direct_effect=direct_effect, indirect_effect=indirect_effect))
}

myData= # put file path here
x=myData$PN # predictor
y=myData$IEDCompletedstageerrors #outcome
M=as.matrix(myData[,c('cluster406824rest_DefaultModeLPl', 'cluster104814rest_DefaultModeLPl',
                      'cluster564252rest_DefaultModeLPl', 'cluster208050rest_DefaultModeLPl',
                      'cluster105846rest_DefaultModeLPl', 'cluster52832rest_DefaultModeLPl',
                      'cluster54328rest_DefaultModeLPl', 'cluster62120rest_DefaultModeLPl',
                      'cluster205652rest_DefaultModeLPl', 'cluster8444rest_DefaultModeLPl',
                      'cluster145874rest_DefaultModeLPl', 'cluster465420rest_DefaultModeLPr',
                      'cluster345824rest_DefaultModeLPr', 'cluster24620rest_DefaultModeLPr',
                      'cluster0560rest_DefaultModeMPFC', 'cluster42222rest_DefaultModeMPFC',
                      'cluster561618rest_DefaultModeMPFC', 'cluster45628rest_DefaultModePCC',
                      'cluster242258rest_DefaultModePCC', 'cluster36462rest_DefaultModePCC')])# mediators

a=mean(x) #increase by what unit for spline
N=nrow(myData) # num observations
B=50 #bootstrap resamples
bootstrap_results=list(total_effect=numeric(B),direct_effect=list(),indirect_effect=list())

for (k in 1:ncol(M)) {
  bootstrap_results$direct_effect[[paste0("M", k)]]=numeric(B)
  bootstrap_results$indirect_effect[[paste0("M", k)]]=numeric(B)
}

for (i in 1:B) {
  cat("BootStrap", i, "/", B, "...\n")
  sampled_indices=sample(1:N, size=N, replace=TRUE)
  sampled_x=x[sampled_indices]
  for (k in 1:ncol(M)) {
    effects=estimate_effects(sampled_x, x, y, M, k, a, N)
    bootstrap_results$total_effect[i]=effects$total_effect
    bootstrap_results$direct_effect[[paste0("M", k)]][i]=effects$direct_effect
    bootstrap_results$indirect_effect[[paste0("M", k)]][i]=effects$indirect_effect
  }
}

bootstrap_mean_total_effect=mean(bootstrap_results$total_effect)
bootstrap_ci_total_effect=quantile(bootstrap_results$total_effect, probs=c(0.025, 0.975))
cat("Total Effect:", bootstrap_mean_total_effect, "\n")
cat("Total Effect 95% CI:", bootstrap_ci_total_effect, "\n")
indirectEffects=length(ncol(M))
for (k in 1:ncol(M)) {
  mediator=paste0("M", k)
  bootstrap_samples_indirect_effect=bootstrap_results$indirect_effect[[mediator]]
  
  bootstrap_mean_indirect_effect=mean(bootstrap_samples_indirect_effect)
  bootstrap_ci_indirect_effect=quantile(bootstrap_samples_indirect_effect, probs=c(0.025, 0.975))
  
  indirectEffects[k]=bootstrap_mean_indirect_effect
  print(indirectEffects)
  print(bootstrap_ci_indirect_effect)
}
# calculate direct effect by predictor
directEffect=bootstrap_mean_total_effect-sum(indirectEffects)

#plots code removed
  