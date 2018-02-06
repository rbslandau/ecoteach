require(lme4)


# complex environment

testData = createDataExt(sampleSize = 2000, intercept = 5, fixedEffects = c(2, 2, 0), cor = 0.5)

pairs(testData[, grepl("Environment", names(testData))])

fittedModel <- lmer(observedResponse ~ Environment1 + Environment2 + Environment3 + (1|group), data = testData)
summary(fittedModel)



# External predictors

# nvar <- 4
# mu <- sample(seq(0.01, 0.5, length.out = 50), nvar, replace = TRUE)
# data_sim <- corrEnv(n = 2000, nvar = nvar, ngrad = 3, mu = mu, rho = 0.9, rho.non.corr = 0)
#
# testData = createDataExt(sampleSize = 2000, extPredictors = data_sim[[2]], fixedEffects = c(2, 2, 1, 0))
#
# hist(testData$observedResponse)
# pairs(testData[, grepl("Environment", names(testData))])
#
# fittedModel <- lmer(observedResponse ~ Environment1 + Environment2 + Environment3 + Environment4 + (1|group), data = testData)
# summary(fittedModel)



### FROM DHARMa



# with zero-inflation

testData = createDataExt(sampleSize = 500, intercept = 2, fixedEffects = c(1),
                      overdispersion = 0, family = poisson(), quadraticFixedEffects = c(-3),
                      randomEffectVariance = 0, pZeroInflation = 0.6)

par(mfrow = c(1,2))
plot(testData$Environment1, testData$observedResponse)
hist(testData$observedResponse)


# binomial with multiple trials

testData = createDataExt(sampleSize = 40, intercept = 2, fixedEffects = c(1),
                      overdispersion = 0, family = binomial(), quadraticFixedEffects = c(-3),
                      randomEffectVariance = 0, binomialTrials = 20)

plot(observedResponse1 / observedResponse0 ~ Environment1, data = testData, ylab = "Proportion 1")


# spatial / temporal correlation

testData = createDataExt(sampleSize = 100, family = poisson(), spatialAutocorrelation = 3,
                      temporalAutocorrelation = 3)

par(mfrow = c(1,2))
plot(log(observedResponse) ~ time, data = testData)
plot(log(observedResponse) ~ x, data = testData)










