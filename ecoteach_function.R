# Simulating data for teaching



# No overdispersion -------------------------------------------------------



testData = createData(sampleSize = 200, overdispersion = 0, family = poisson())

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
