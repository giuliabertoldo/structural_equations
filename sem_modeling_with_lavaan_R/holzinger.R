library (lavaan)
data(HolzingerSwineford1939)

head(HolzingerSwineford1939[7:15])

# Define your model specification
text.model <- 'textspeed =~ x4 +x5 + x6 + x7 + x8 + x9'
