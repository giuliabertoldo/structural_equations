# Load the lavaan library
library(lavaan)

# Look at the dataset
data(PoliticalDemocracy)
head(PoliticalDemocracy)

# Define your model specification
politics.model <- 'poldemo60 =~ y1 + y2 + y3 + y4'

# Analyze the model with cfa()
politics.fit <- cfa(model = politics.model, data=PoliticalDemocracy)

# Summarize the model
summary(politics.fit, standardized=TRUE, fit.measures=TRUE)

