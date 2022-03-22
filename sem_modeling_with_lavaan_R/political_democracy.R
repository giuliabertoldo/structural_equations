# Load the lavaan library
library(lavaan)

# Look at the dataset
data(PoliticalDemocracy)
head(PoliticalDemocracy)

# Define your model specification
politics.model <- 'poldemo60 =~ y1 + y2 + y3 + y4'
