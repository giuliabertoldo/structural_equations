# Source: https://psu-psychology.github.io/r-bootcamp-2018/talks/lavaan_tutorial.html

library(mlbench)  # Package with Bostron Housing dataset
library(tidyverse)
library(lavaan)
library(tidySEM)
library(corrplot)

# 2 Use lavaan for simple multiple regression -----------------------------
data(BostonHousing2)
glimpse(BostonHousing2)

BostonSmall <- BostonHousing2 %>% dplyr::select(
  cmedv, #median value of home in 1000s
  crim, #per capita crime by town
  nox, #nitric oxide concentration
  lstat, #proportion of lower status
  rad #proximity to radial highways
) %>% mutate(log_crim = log2(crim))

# Use lavaan
lavaan_m <- 'cmedv ~ log_crim'
mlav <- sem(lavaan_m,
            data=BostonSmall)
summary(mlav)

# Compare output of lm()
summary(lm(cmedv ~ log_crim, BostonSmall))

# Ask to include the mean (intercept) in the model

mlav_w_intercept <- sem(lavaan_m,
                        data=BostonSmall,
                        meanstructure=TRUE)
summary(mlav_w_intercept)

# Summary of parameters: free vs constrained
parTable(mlav)

# Obtain standardized solution
standardizedSolution(mlav,
                     type="std.all")



# 3 Path analysis on housing data -----------------------------------------
# model
lavaan_m2 <- '
nox ~ rad
cmedv ~ nox + log_crim
'
# fit model
mlav2 <- sem(lavaan_m2,
             data = BostonSmall)

# graph
graph_sem(model = mlav2)

# see results
summary(mlav2)


# * 3.1 Ill conditioning --------------------------------------------------
# Ill conditioning
varTable(mlav2)

# Solution
BostonSmall <- BostonSmall %>%
  mutate(nox = nox*100) #not parts per 100,000 not 10 million
mlav2 <- sem(lavaan_m2, data=BostonSmall)
summary(mlav2)

# * 3.2 Model fit indices -------------------------------------------------

summary(mlav2,
        fit.measures=TRUE)

fitmeasures(mlav2)

# * 3.3 Model diagnostics -------------------------------------------------
# Model-implied covariance structure
fitted(mlav2)

# Model-implied correlation matrix
inspect(mlav2, what = "cor.all")

# Observed correlation structure
lavCor(mlav2)

# Plot observed correlation structure
plot_matrix <- function(matrix_toplot){
  corrplot::corrplot(matrix_toplot, is.corr = FALSE,
                     type = 'lower',
                     order = "original",
                     tl.col='black', tl.cex=.75)
}
plot_matrix(residuals(mlav2, type="cor")$cov)

# * 3.4 Modification indices ----------------------------------------------

modificationindices(mlav2, minimum.value = 20) #only print MIs > 20

#we can use the add parameter to add a path, while leaving all other model elements the same
mlav3 <- update(mlav2, add="nox ~ log_crim")
summary(mlav3, fit.measures=TRUE)


# 4 Testing mediation -----------------------------------------------------
# Model
m_update <- '
cmedv ~ log_crim + b1*nox #crime and nox predict lower home prices
nox ~ a1*rad + a2*log_crim #proximity to highways predicts nox

i_1 := a1*b1
i_2 := a2*b1
'
# Fit sem: no bootstrap  (not correct)
mlav4 <- sem(m_update, BostonSmall)
summary(mlav4)

# Fit sem: bootstrap (correct!)
mlav4_boot <- sem(m_update, BostonSmall,
                  se = "bootstrap")
summary(mlav4_boot)


# 5 SEM with latent variables ---------------------------------------------

# Specify the model
model <- '
  visual =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
      speed =~ x7 + x8 + x9
'
# Fit cfa
fit <- cfa(model = model,
           data = HolzingerSwineford1939)
summary(fit, fit.measures=TRUE)

# Modification indices for CFA
modificationIndices(fit, minimum.value = 10)

# New model
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9

x7 ~~ x9 #this specifies a variance or covariance parameter
'

fit2 <- cfa(HS.model, data=HolzingerSwineford1939)
summary(fit2, fit.measures=TRUE)

anova(fit, fit2)

# See the model specification in detail
inspect(fit)

# Parameter estimates in matrix form
inspect(fit, "est")

# Structural model
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9

visual ~ grade
textual ~ grade
speed ~ grade
'

fit_structural <- cfa(HS.model, data=HolzingerSwineford1939)
summary(fit_structural, fit.measures=TRUE)


# 6 Categorical data ------------------------------------------------------

hist(HSbinary$x1)
data(HSBinary)


# 7 Estimators ------------------------------------------------------------


# 8 Missing data ----------------------------------------------------------


