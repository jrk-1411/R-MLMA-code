#install.packages("pls")
library(pls)
data = read.csv("C:\\Users\\khato\\OneDrive\\Documents\\Thesis\\shared-work\\j\\data\\Nona\\ALL_GRP_Nona.csv")
X = data[, c("EA", "PN","PA","SA","EN")]
M = data[, c("cluster406824rest_DefaultModeLPl", "cluster104814rest_DefaultModeLPl", 
              "cluster564252rest_DefaultModeLPl", "cluster208050rest_DefaultModeLPl",
              "cluster105846rest_DefaultModeLPl", "cluster52832rest_DefaultModeLPl", 
              "cluster54328rest_DefaultModeLPl", "cluster62120rest_DefaultModeLPl", 
              "cluster205652rest_DefaultModeLPl", "cluster8444rest_DefaultModeLPl", 
              "cluster145874rest_DefaultModeLPl", "cluster465420rest_DefaultModeLPr", 
              "cluster345824rest_DefaultModeLPr", "cluster24620rest_DefaultModeLPr", 
              "cluster0560rest_DefaultModeMPFC", "cluster42222rest_DefaultModeMPFC", 
              "cluster561618rest_DefaultModeMPFC", "cluster45628rest_DefaultModePCC", 
              "cluster242258rest_DefaultModePCC", "cluster36462rest_DefaultModePCC")]
Y = data$IEDCompletedstageerrors
combined = cbind( M)

pls_model = plsr(Y ~ ., data = combined,validation = "CV")

summary(pls_model)

validationplot(pls_model, val.type = "MSEP")

scoreplot(pls_model, col='green', pch=16)
help(scoreplot)

loadingplot(pls_model)

coefficients(pls_model)
pls_scores = scores(pls_model)
dim(pls_scores)


independent_variable = X  

mediators = pls_scores[, 1:4]  
outcome = data$IEDCompletedstageerrors

mediation_data = data.frame(outcome, independent_variable, mediators)

# library(mma)
# 
# colnames(mediation_data) = c("Y", "EA","PN", paste0("M", 1:4))
# M = mediation_data[,c("M1","M2","M3","M4")]
# all_clus = list(n=1,j1=c( 'M1', 'M2',
#                           'M3', 'M4'))
# #mma_res=mma(x=M,y=Y,mediator=1:ncol(M),pred=X,nonlinear =FALSE,testtype=1,jointm = all_clus,contmed = 1:ncol(M))
# #summary(mma_res)
