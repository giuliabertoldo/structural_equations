# Load the library and data
library(psych)
library(psychTools)
data(epi)

# (1) ZERO CORRELATION BETWEEN LATENT VARIABLES
# Specify a three-factor model with one correlation set to zero
epi.model <- 'extraversion =~ V1 + V3 + V5 + V8
neuroticism =~ V2 + V4 + V7 + V9
lying =~ V6 + V12 + V18 + V24
extraversion ~~ 0*neuroticism'

# Run the model
epi.fit <- cfa(model = epi.model, data = epi)

# Examine the output
summary(epi.fit, standardized = TRUE, fit.measures = TRUE)

# (2) DIRECT PATH BETWEEN LATENT VARIABLES
# Specify a three-factor model where lying is predicted by neuroticism
epi.model <- 'extraversion =~ V1 + V3 + V5 + V8
neuroticism =~ V2 + V4 + V7 + V9
lying =~ V6 + V12 + V18 + V24
lying ~ neuroticism'

# Run the model
epi.fit <- cfa(model = epi.model, data = epi)

# Examine the output
summary(epi.fit, standardized = TRUE, fit.measures = TRUE)

# Calculate the variance of V1 in the epi data
var(epi$V1, na.rm = TRUE)


# Examine the modification indices
modificationIndices(epi.fit, sort = TRUE)

# Edit the model specification
epi.model1 <- 'extraversion =~ V1 + V3 + V5 + V8
neuroticism =~ V2 + V4 + V7 + V9
lying =~ V6 + V12 + V18 + V24
neuroticism =~ V3'

# Reanalyze the model
epi.fit1 <- cfa(model = epi.model2, data = epi)

# Summarize the updated model
summary(epi.fit1, standardized = TRUE, fit.measures = TRUE)

# Compare those models
anova(epi.fit, epi.fit1)

# Find the fit indices for the original model
fitmeasures(epi.fit)
# Find the fit indices for the updated model
fitmeasures(epi.fit1)

# Find the fit indices for the original model
fitmeasures(epi.fit, c('aic', 'ecvi'))
# Find the fit indices for the updated model
fitmeasures(epi.fit1, c('aic', 'ecvi'))
