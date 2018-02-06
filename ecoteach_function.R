# Simulating data for teaching

library(DHARMa)
library(lme4)
library(foreach)
set.seed(123)

dispValues = seq(0,1.2, len = 10)
replicates = 100




# No overdispersion -------------------------------------------------------

testData = createData(sampleSize = 200, overdispersion = 0, family = poisson())

fittedModel <- glmer(observedResponse ~ Environment1 + (1|group) , family = "poisson", data = testData)
summary(fittedModel)

sim = simulateResiduals(fittedModel)
plot(sim)

# Parametric test
testOverdispersionParametric(fittedModel)

# Omnibus test
testOverdispersionParametric(fittedModel)

# Non-parametric test

sim = simulateResiduals(fittedModel,refit = T)
plot(sim)
testOverdispersion(sim,  plot = T)









# test external env dat ---------------------------------------------------

X <- testData[, grepl("Environment", names(testData))]

testData = createData(sampleSize = 20000, extPredictors = X, fixedEffects = c(2, 2, 1), overdispersion = 0, family = gaussian())

cor(testData[, grepl("Environment", names(testData))])







# Overdispersion ----------------------------------------------------------



testData = createData(sampleSize = 200, overdispersion = 1, family = poisson())

fittedModel <- glmer(observedResponse ~ Environment1 + (1|group) , family = "poisson", data = testData)

sim = simulateResiduals(fittedModel)
plot(sim)

# Parametric test
testOverdispersionParametric(fittedModel)

# Omnibus test
testOverdispersionParametric(fittedModel)

# Non-parametric test

sim = simulateResiduals(fittedModel,refit = T)
plot(sim)
testOverdispersion(sim,  plot = T)





