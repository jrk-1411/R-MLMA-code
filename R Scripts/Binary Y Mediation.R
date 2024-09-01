myData = read.csv("C:\\Users\\khato\\OneDrive\\Documents\\Thesis\\shared-work\\j\\data\\Nona\\ALL_GRP_Nona_Categorical_Y.csv")

# Categorical Y mma
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
all_cluster = list(n = 1, j1 = c(
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
))
dependantVariable = "Y"

###########################
#---Data Subsetting---#
###########################
X = data.frame(myData[independantVariables])
Y = c(myData[dependantVariable])
M = data.frame(myData[mediatorVariables])
mmaAnalysisGLM = mma(x = M, y = Y, mediator = 1:ncol(M), pred = X, testtype = 1, all.model = TRUE, contmed = 1:ncol(M), jointm = all_cluster, refy = 0)
summary(mmaAnalysisGLM)
mmaAnalysisGLM$a.contx$estimation
mmaAnalysisGLM$a.contx$all_model
