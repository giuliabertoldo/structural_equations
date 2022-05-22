# Load packages -----------------------------------------------------------
library(haven)
library(tidyverse)
library(mice)
library(lavaan)
library(corrplot)
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
  select(Q2a, # gender
         Q2b, # age
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
# Demographics

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
## 1546 subjects have no missing data
## The variables with the highest amount of missing data (28) are Q61e and Q61c

## Percentage of missing data per variable
purrr::map(df_ge, ~(mean(is.na(.))*100))

# * Multivariate normality assumption -------------------------------------
# Henze-Zirkler’s multivariate normality test.
# remove NAs
df_ge_not_na <- na.omit(df_ge)
(mvn_test <- mvnTest::HZ.test(data = df_ge_not_na))


# Models -------------------------------------------------------------------

# * CFA free --------------------------------------------------------------
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


# * CFA modified ----------------------------------------------------------
# Model specification
## See: https://lavaan.ugent.be/tutorial/sem.html
model_cfa_free_mod <- '
autonomy =~ Q61c + Q61d + Q61e + Q61i + Q61n
competence =~ Q61g + Q61h + Q61j + Q61k
relatedness =~ Q61a + Q61b + Q61l
psych_wellbeing =~ Q87a + Q87b + Q87c + Q87d + Q87e
job_sat =~ Q88
job_sat ~~ ev_job_sat*job_sat
Q61a ~~ Q61b
'
# Model estimation
fit_cfa_free_mod <- cfa(model_cfa_free_mod,
                        data = df_ge,
                        std.lv = FALSE,
                        estimator = 'ml',
                        missing = 'fiml',
                        se = "bootstrap")
summary(fit_cfa_free_mod,
        standardized = TRUE,
        fit.measures = TRUE)

# Understand free parameters
inspect(fit_cfa_free_mod)
lavInspect(fit_cfa_free_mod, what = "list")


# ** Path diagram ---------------------------------------------------------
labels <- list(autonomy = "Autonomy",
               competence = "Competence",
               relatedness = "Relatedness",
               psych_wellbeing = "Psychological Well-being",
               job_sat = 'Job Satisfaction')

lavaanPlot(model = fit_cfa_free_mod,
           labels = labels,
           edge_options = list(color = "grey"),
           coefs = TRUE,
           sig = .05,
           stand = TRUE,
           covs = TRUE,
           # stars = 'latent',
           graph_options = list(rankdir = "RL"))

# ** Residual correlation matrix ------------------------------------------
plot_matrix(residuals(fit_cfa_free_mod, type='cor')$cov)

# ** Modification indices -------------------------------------------------
modificationIndices(fit_cfa_free_mod, minimum.value=20)


# * Full SEM: Mediation (a) ---------------------------------------------------
# Model specification
med_model <- '
autonomy =~ Q61c + Q61d + Q61e + Q61i + Q61n
competence =~ Q61g + Q61h + Q61j + Q61k
relatedness =~ Q61a + Q61b + Q61l
psych_wellbeing =~ Q87a + Q87b + Q87c + Q87d + Q87e
job_sat =~ Q88
job_sat ~~ ev_job_sat*job_sat
job_sat ~ a1*autonomy + a2*competence + a3*relatedness
psych_wellbeing ~ autonomy + competence + relatedness + b1*job_sat
i_1 := a1*b1
i_2 := a2*b1
i_3 := a3*b1
'
# Model estimation
med_fit <- sem(med_model,
               data = df_ge,
               std.lv = FALSE,
               estimator = 'ml',
               missing = 'fiml',
               se = "bootstrap")
summary(med_fit,
        standardized = TRUE,
        fit.measures = TRUE)

# Understand free parameters
inspect(med_fit)
lavInspect(med_fit, what = "list")


# * Path diagram: SEM --------------------------------------------------


# * Modification indices --------------------------------------------------
modificationIndices(med_fit, minimum.value=20)

# * Full SEM: Mediation (b) ---------------------------------------------------
# Model specification
med_model <- '
autonomy =~ Q61c + Q61d + Q61e + Q61i + Q61n
competence =~ Q61g + Q61h + Q61j + Q61k
relatedness =~ Q61a + Q61b + Q61l
psych_wellbeing =~ Q87a + Q87b + Q87c + Q87d + Q87e
job_sat =~ Q88
job_sat ~~ ev_job_sat*job_sat
Q61a ~~ Q61b
job_sat ~ a1*autonomy + a2*competence + a3*relatedness
psych_wellbeing ~ autonomy + competence + relatedness + b1*job_sat
i_1 := a1*b1
i_2 := a2*b1
i_3 := a3*b1
'
# Model estimation
med_fit <- sem(med_model,
               data = df_ge,
               std.lv = FALSE,
               estimator = 'ml',
               missing = 'fiml',
               se = "bootstrap")
summary(med_fit,
        standardized = TRUE,
        fit.measures = TRUE)

# Understand free parameters
inspect(med_fit)
lavInspect(med_fit, what = "list")


# * Path diagram: SEM --------------------------------------------------


# * Modification indices --------------------------------------------------
modificationIndices(med_fit, minimum.value=20)
