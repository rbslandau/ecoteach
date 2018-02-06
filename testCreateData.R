# Simulating data for teaching

library(DHARMa)
library(lme4)
library(foreach)
set.seed(123)

source("../ecoteach@git/createData.R")


# No overdispersion with complex environment ------------------------------

testData = createData(sampleSize = 20000, intercept = 100, fixedEffects = c(2, 2, 0), cor= 0.5, overdispersion = 0, family = gaussian())

summary(testData)
cor(testData[, grepl("Environment", names(testData))])

fittedModel <- lmer(observedResponse ~ Environment1 + Environment2 + Environment3 + (1|group), data = testData)
summary(fittedModel)

sim = simulateResiduals(fittedModel)
plot(sim)

# Parametric test
testOverdispersionParametric(fittedModel)

# Omnibus test
testOverdispersionParametric(fittedModel)



# Climate data in R -------------------------------------------------------

library(rWBclimate)

temp = get_historical_temp("DEU", "year")$data
prec = get_historical_precip("DEU", "year")$data*10

# https://unstats.un.org/unsd/methodology/m49/



# Generate real world example ---------------------------------------------

testData = createData(sampleSize = 2000, intercept = 100, fixedEffects = c(2, 2, 0), cor= 0, numGroups = 1, overdispersion = 0, family = gaussian())

hist(testData$observedResponse)




# real range is hard to achieve (or fixed effects change....)
testData$temp = (sd(temp) * testData$Environment1) + mean(temp) 
testData$prec = (sd(prec) * testData$Environment2) + mean(prec)
testData$pH = testData$Environment3 + 6

pairs(testData)
hist(testData$temp)
hist(temp)

hist(testData$prec)
hist(prec)
# 
# hist(testData$pH)


fittedModel <- lm(observedResponse ~ temp + prec + pH, data = testData)
summary(fittedModel)

# fittedModel <- lmer(observedResponse ~ temp * prec + pH + (1|group), data = testData)
# summary(fittedModel)


X <- testData[, grepl("Environment", names(testData))]

testData = createData(sampleSize = 20000, extPredictors = X, fixedEffects = c(2, 2, 1), overdispersion = 0, family = gaussian())

cor(testData[, grepl("Environment", names(testData))])



# No overdispersion -------------------------------------------------------

testData = createData(sampleSize = 200, overdispersion = 0, family = binomial(), temporalAutocorrelation = 0.5)

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





testData = createData(sampleSize = 1000, family = gaussian(), temporalAutocorrelation = 1)

plot((observedResponse) ~ time, data = testData)
plot(log(observedResponse) ~ x, data = testData)


