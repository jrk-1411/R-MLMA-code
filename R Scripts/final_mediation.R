library(mediation)

# Read the data
data = read.csv("C:\\Users\\khato\\OneDrive\\Documents\\Thesis\\shared-work\\j\\data\\stratify_data.csv")

#--do on this
X = c("EA", "PA", "SA", "EN", "PN")
M = c(
  'cluster105846rest_DefaultModeLPl',
  'cluster54328rest_DefaultModeLPl',
  'cluster561618rest_DefaultModeMPFC',
  'cluster564252rest_DefaultModeLPl',
  'cluster208050rest_DefaultModeLPl',
  'cluster105846rest_DefaultModeLPl',
  'cluster561618rest_DefaultModeMPFC',
  'cluster8444rest_DefaultModeLPl',
  'cluster42222rest_DefaultModeMPFC'
)
Y = c("IEDTotalerrors",
      "IEDTotalerrorsadjusted",
      "SWMBetweenerrors",
      "SWMStrategy")

x = "PN"
m = "cluster104814rest_DefaultModeLPl"
y = "SWMStrategy"

subset_data = data[, c(x, m, y)]
subset_data = subset_data[complete.cases(subset_data), ]

model.0 = lm(SWMStrategy ~ PN, data = subset_data)
model.M = lm(cluster104814rest_DefaultModeLPl ~ PN, data = subset_data)
model.Y = lm(SWMStrategy ~ PN + cluster104814rest_DefaultModeLPl, data = subset_data)

results = mediate(model.M, model.Y, treat = x, mediator = m, boot = TRUE, sims = 500)

mediation_summary = summary(results)
names(results)
mediation_info = data.frame(
  X = x,
  M = m,
  Y = y,
  ACME_Effect = mediation_summary$d0,
  ACME_p_value = mediation_summary$d0.p
)
