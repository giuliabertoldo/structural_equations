# Load packages -----------------------------------------------------------
library(haven)
library(tidyverse)
library(mice)
library(lavaan)
library(corrplot)

# Import data -------------------------------------------------------------
df <- read_sav("data/ewcs_2015.sav")

# Dataset: Germany,  employed, variables of interest ----------------------
df_ge <- df %>%
  filter(Country == 11, # Only Germany
         Q7 == 1) %>%   # Only employed
  select(Q61n,
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
# Demographics


# * Observed correlation matrix -------------------------------------------
measaurement_model_data  <- df_ge %>%
  select(Q61n,
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
         Q87e)

# Observed covariance matrix
measaurement_model_cov <- cov(measaurement_model_data,
                              use = "pairwise.complete.obs"
)

# Observed correlation matrix
measaurement_model_cor <- cov2cor(measaurement_model_cov)
corrplot::corrplot(measaurement_model_cor,
                   is.corr = FALSE,       # whether is a correlation matrix
                   method = "circle",     # magnitude of covariances as circles
                   type = "upper",        # remove the bottom of the covariance matrix
                   addCoef.col = "black"  # add to the plot the coefficients
)

# * Missing data pattern --------------------------------------------------

(pattern <- md.pattern(df_ge))
## 1546 subjects have no missing data
## The variables with the highest amount of missing data (28) are Q61e and Q61c


## Percentage of missing data per variable
purrr::map(df_ge, ~(mean(is.na(.))*100))


# * Multivariate normality assumption -------------------------------------

# Henze-Zirkler’s multivariate normality test.
# remove NAs
df_ge_not_na <- na.omit(df_ge)

(mvn_test <- mvnTest::HZ.test(data = df_ge_not_na))


mvn_test$multivariateNormality


# Model -------------------------------------------------------------------
# * CFA -------------------------------------------------------------------
# Model specification
model_pulled <- '
autonomy =~ Q61c + Q61d + Q61e + Q61i + Q61n
competence =~ Q61g + Q61h + Q61j + Q61k
relatedness =~ Q61a + Q61b + Q61l
psych_wellbeing =~ Q87a + Q87b + Q87c + Q87d + Q87e
psych_wellbeing ~~ 0*autonomy
psych_wellbeing ~~ 0*competence
psych_wellbeing ~~ 0*relatedness
'
# Model estimation
fit_pulled <- cfa(model_pulled,
                data = df_ge,
                std.lv = TRUE,
                estimator = 'ml',
                missing = 'fiml',
                se = "bootstrap")
summary(fit_pulled,
        standardized = TRUE,
        fit.measures = TRUE)

# * Full SEM: Mediation ---------------------------------------------------

med_model <- '
autonomy =~ Q61c + Q61d + Q61e + Q61i + Q61n
competence =~ Q61g + Q61h + Q61j + Q61k
relatedness =~ Q61a + Q61b + Q61l
psych_wellbeing =~ Q87a + Q87b + Q87c + Q87d + Q87e
psych_wellbeing ~~ 0*autonomy
psych_wellbeing ~~ 0*competence
psych_wellbeing ~~ 0*relatedness
Q88 ~ a1*autonomy + a2*competence + a3*relatedness
psych_wellbeing ~ autonomy + competence + relatedness + b1*Q88
i_1 := a1*b1
i_2 := a2*b1
i_3 := a3*b1
'
med_fit_marker <- sem(med_model,
               data = df_ge,
               std.lv = FALSE,
               # estimator = 'ml',
               missing = 'fiml',
               se = "bootstrap")
summary(med_model,
        standardized = TRUE,
        fit.measures = TRUE)

modificationIndices(med_model, minimum.value=20)

# Understand free parameters
inspect(med_model)
lavInspect(med_model, what = "list")

