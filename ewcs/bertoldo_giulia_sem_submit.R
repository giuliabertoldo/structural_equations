# Load packages -----------------------------------------------------------
library(haven)
library(psych)
library(tidyverse)
library(mice)
library(lavaan)
library(corrplot)
library(purrr)
library(tidySEM)
library(lavaanPlot)
library(semPlot)

# Functions ---------------------------------------------------------------
# Residual correlation matrix
plot_matrix <- function(matrix_toplot) {
  corrplot::corrplot(matrix_toplot,
                     is.corr = FALSE,
                     type = 'upper',
                     order = 'original',
                     tl.col = 'black',
                     tl.cex = .75)
}

# Import data -------------------------------------------------------------
df <- read_sav("data/ewcs_2015.sav")

# Dataset: Germany,  employed, variables of interest ----------------------
df_ge <- df %>%
  filter(Country == 11, # Only Germany
         Q7 == 1) %>%   # Only employed
  select(# Q2a, # gender
    # Q2b, # age
    Q61n,
    Q61d,
    Q61c,
    Q61i,
    Q61e,
    Q61h,
    Q61j,
    Q61k,
    Q61g,
    Q61l,
    Q61a,
    Q61b,
    Q87a,
    Q87b,
    Q87c,
    Q87d,
    Q87e,
    Q88)

# Exploratory Data Analysis -----------------------------------------------
glimpse(df_ge)
# Sample size
(dimen <- dim(df_ge))
# Describe
describe(df_ge)


# * Observed correlation matrix -------------------------------------------
# Observed covariance matrix
obs_cov <- cov(df_ge,
               use = "pairwise.complete.obs"
)

# Observed correlation matrix
obs_cor <- cov2cor(obs_cov)
corrplot::corrplot(obs_cor,
                   is.corr = FALSE,       # whether is a correlation matrix
                   method = "circle",     # magnitude of covariances as circles
                   type = "upper",        # remove the bottom of the covariance matrix
                   addCoef.col = "black"  # add to the plot the coefficients
)

# * Missing data pattern --------------------------------------------------
(pattern <- md.pattern(df_ge))
## 1560 subjects have no missing data

## Percentage of missing data per variable
purrr::map(df_ge, ~(mean(is.na(.))*100))

# * Multivariate normality assumption -------------------------------------
# Henze-Zirkler’s multivariate normality test.
# remove NAs
df_ge_not_na <- na.omit(df_ge)
(mvn_test <- mvnTest::HZ.test(data = df_ge_not_na))


# Models -------------------------------------------------------------------

# * CFA free --------------------------------------------------------------
# This CFA model corresponds to the measurement part of the proposed SEM
# All covariances between latent variables are estimated

# Calculate sample standard deviation for Q88
(var_q88 <- var(df_ge$Q88, na.rm = T))

# Error variance for job satisfaction indicator
## Reliability = 0.60
ev_job_sat <- (1-0.60)*var_q88

# Model specification
model_cfa_free <- '
autonomy =~ Q61c + Q61d + Q61e + Q61i + Q61n
competence =~ Q61g + Q61h + Q61j + Q61k
relatedness =~ Q61a + Q61b + Q61l
psych_wellbeing =~ Q87a + Q87b + Q87c + Q87d + Q87e
job_sat =~ Q88
job_sat ~~ ev_job_sat*job_sat
'
# Model estimation
fit_cfa_free <- cfa(model_cfa_free,
                    data = df_ge,
                    std.lv = FALSE,
                    estimator = 'ml',
                    missing = 'fiml',
                    se = "bootstrap")
summary(fit_cfa_free,
        standardized = TRUE,
        fit.measures = TRUE)

# Understand free parameters
inspect(fit_cfa_free)
lavInspect(fit_cfa_free, what = "list")

# ** Path diagram ---------------------------------------------------------
labels <- list(autonomy = "Autonomy",
               competence = "Competence",
               relatedness = "Relatedness",
               psych_wellbeing = "Psychological Well-being",
               job_sat = 'Job Satisfaction')

lavaanPlot(model = fit_cfa_free,
           labels = labels,
           edge_options = list(color = "grey"),
           coefs = TRUE,
           sig = .05,
           stand = TRUE,
           covs = TRUE,
           # stars = 'latent',
           graph_options = list(rankdir = "RL"))

# ** Residual correlation matrix ------------------------------------------
plot_matrix(residuals(fit_cfa_free, type='cor')$cov)

# ** Modification indices -------------------------------------------------
modificationIndices(fit_cfa_free, minimum.value=20)

# * Full SEM: Original Mediation ---------------------------------------------------
## This is the final SEM model and also the model hypothesized before looking at
## modification indices.
# Model specification
med_model_original <- '
autonomy =~ Q61c + Q61d + Q61e + Q61i + Q61n
competence =~ Q61g + Q61h + Q61j + Q61k
relatedness =~ Q61a + Q61b + Q61l
psych_wellbeing =~ Q87a + Q87b + Q87c + Q87d + Q87e
job_sat =~ Q88
job_sat ~~ ev_job_sat*job_sat

# Direct effect
psych_wellbeing ~ c1*autonomy + c2*competence + c3*relatedness

# Mediator
## Path A
job_sat ~ a1*autonomy
job_sat ~ a2*competence
job_sat ~ a3*relatedness
## Path B
psych_wellbeing ~ b1*job_sat

## Indirect effect
a1b1 := a1*b1
a2b1 := a2*b1
a3b1 := a3*b1

## Total effect
total1 := c1 + (a1*b1)
total2 := c2 + (a2*b1)
total3 := c3 + (a3*b1)
'
# Model estimation
med_fit_original <- sem(med_model_original,
                        data = df_ge,
                        std.lv = FALSE,
                        estimator = 'ml',
                        missing = 'fiml',
                        se = "bootstrap")
summary(med_fit_original,
        standardized = TRUE,
        fit.measures = TRUE)

# Understand free parameters
inspect(med_fit_original)
lavInspect(med_fit_original, what = "list")


# ** Observed covariance matrix -------------------------------------------
lavInspect(med_fit_original, "sampstat")

# ** Model estimated covariance matrix ------------------------------------
fitted(med_fit_original)

# ** Difference between observed and estimated covariance matrix ----------
residuals(med_fit_original)


# ** Parameter estimates with confidence intervals ------------------------
parameterEstimates(med_fit_original, standardized=TRUE)

# ** Path diagram: SEM --------------------------------------------------
labels <- list(autonomy = "Autonomy",
               competence = "Competence",
               relatedness = "Relatedness",
               psych_wellbeing = "Psychological Well-being",
               job_sat = 'Job Satisfaction')

lavaanPlot(model = med_fit_original,
           labels = labels,
           edge_options = list(color = "grey"),
           coefs = TRUE,
           sig = .05,
           stand = TRUE,
           covs = TRUE,
           # stars = 'latent',
           graph_options = list(rankdir = "LR"))

# **  Modification indices --------------------------------------------------
modificationIndices(med_fit_original, minimum.value=20)

