library (lavaan)
data(HolzingerSwineford1939)

head(HolzingerSwineford1939[7:15])

# (1) ONE-FACTOR MODEL
# Define your model specification
text.model <- 'textspeed =~ x4 +x5 + x6 + x7 + x8 + x9'

# Analyze the model with cfa()
text.fit <- cfa(model = text.model, data=HolzingerSwineford1939)

# Summarize the model
summary(text.fit)

# Standardized loadings
summary(text.fit, standardized = TRUE)

# Fit indices
summary(text.fit, standardized = TRUE, fit.measures = TRUE)


# (2) SPLIT THE MODEL INTO SMALLER MODELS

# (2.1) ZERO DF MODEL
# Create your text model specification
text.model <- 'text =~ x4 + x5 + x6'

# Analyze the model and include data argument
text.fit <- cfa(model = text.model, data = HolzingerSwineford1939)

# Summarize the model
summary(text.fit, standardized = TRUE, fit.measures = TRUE)

# (2.2) FIX THE ZERO DF MODEL
# Update the model specification by setting two paths to the label a
text.model <- 'text =~ x4 + a*x5 + a*x6'

# Analyze the model
text.fit <- cfa(model = text.model, data = HolzingerSwineford1939)

# Summarize the model
summary(text.fit, standardized = TRUE, fit.measures = TRUE)

# (3) MULTI-FACTOR MODEL
# Create a two-factor model of text and speed variables
twofactor.model <- 'text =~ x4 + x5 + x6
                    speed =~ x7 + x8 + x9'

# Use cfa() to analyze the model and include data argument
twofactor.fit <- cfa(model=twofactor.model, data=HolzingerSwineford1939)

# Use summary() to view the fitted model
summary(twofactor.fit, standardized=TRUE, fit.measures=TRUE)
