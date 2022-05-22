# Load packages -----------------------------------------------------------
library(haven)
library(tidyverse)
library(mice)
library(lavaan)
library(corrplot)
library(tidySEM)
library(lavaanPlot)
library(semPlot)

# Import data -------------------------------------------------------------
df <- read_sav("data/ewcs_2015.sav")

# Dataset: Germany,  employed, variables of interest ----------------------
df_ge <- df %>%
  filter(Country == 11, # Only Germany
         Q7 == 1) %>%   # Only employed
  select(#Q2a, # gender
         #Q2b, # age
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
## 1560 subjects have no missing data
## The variables with the highest amount of missing data (28) are Q61e and Q61c


## Percentage of missing data per variable
purrr::map(df_ge, ~(mean(is.na(.))*100))

# * Multivariate normality assumption -------------------------------------

# Henze-Zirkler’s multivariate normality test.
# remove NAs
df_ge_not_na <- na.omit(df_ge)
(mvn_test <- mvnTest::HZ.test(data = df_ge_not_na))


# Model -------------------------------------------------------------------

# * CFA: Basic psychological needs ----------------------------------------
# Model specification
model_bpn <- '
autonomy =~ Q61c + Q61d + Q61e + Q61i + Q61n
competence =~ Q61g + Q61h + Q61j + Q61k
relatedness =~ Q61a + Q61b + Q61l
'
# Model estimation
fit_bp <- cfa(model_bpn,
               data = df_ge,
               std.lv = FALSE,
               estimator = 'ml',
               missing = 'fiml',
               se = "bootstrap")
summary(fit_bp,
        standardized = TRUE,
        fit.measures = TRUE)

# Understand free parameters
inspect(fit_bp)
lavInspect(fit_bp, what = "list")


# ** Path diagram: CFA ----------------------------------------------------
# lavaanPlot
labels <- list(autonomy = "Autonomy",
               competence = "Competence",
               relatedness = "Relatedness")

lavaanPlot(model = fit_bp,
           labels = labels,
           edge_options = list(color = "grey"),
           coefs = TRUE,
           sig = .05,
           stand = TRUE,
           covs = TRUE,
           # stars = 'latent',
           graph_options = list(rankdir = "RL"))

# ** * Residual correlation matrix ----------------------------------------
plot_matrix <- function(matrix_toplot) {
  corrplot::corrplot(matrix_toplot,
                     is.corr = FALSE,
                     type = 'upper',
                     order = 'original',
                     tl.col = 'black',
                     tl.cex = .75)
}
plot_matrix(residuals(fit_bp, type='cor')$cov)


# ** Modification indices -------------------------------------------------
modificationIndices(fit_bp, minimum.value=20)


# * CFA: Psychological well-being -----------------------------------------
# Model specification
model_pw <- '
psych_wellbeing =~ Q87a + Q87b + Q87c + Q87d + Q87e
'
# Model estimation
fit_pw <- cfa(model_pw,
              data = df_ge,
              std.lv = FALSE,
              estimator = 'ml',
              missing = 'fiml',
              se = "bootstrap")
summary(fit_pw,
        standardized = TRUE,
        fit.measures = TRUE)

# Understand free parameters
inspect(fit_pw)
lavInspect(fit_pw, what = "list")


# ** Path diagram: CFA ----------------------------------------------------
# lavaanPlot
labels <- list(psych_wellbeing = "Psychological Well-being")

lavaanPlot(model = fit_pw,
           labels = labels,
           edge_options = list(color = "grey"),
           coefs = TRUE,
           sig = .05,
           stand = TRUE,
           covs = TRUE,
           # stars = 'latent',
           graph_options = list(rankdir = "RL"))

# ** Residual correlation matrix ----------------------------------------
plot_matrix(residuals(fit_pw, type='cor')$cov)

# ** Modification indices -------------------------------------------------
modificationIndices(fit_pw, minimum.value=20)


# * Full SEM: Mediation ---------------------------------------------------
# Model specification
model_med <- '
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
# Model estimation
fit_med <- sem(model_med,
               data = df_ge,
               std.lv = FALSE,
               estimator = 'ml',
               missing = 'fiml',
               se = "bootstrap")
summary(fit_med,
        standardized = TRUE,
        fit.measures = TRUE)

# Understand free parameters
inspect(fit_med)
lavInspect(fit_med, what = "list")


# ** Path diagram: SEM  ---------------------------------------------------
# lavaanPlot
labels <- list(psych_wellbeing = "Psychological Well-being",
               autonomy = "Autonomy",
               competence = "Competence",
               relatedness = "Relatedness",
               Q88 = "Job Satisfaction")

lavaanPlot(model = fit_med,
           labels = labels,
           edge_options = list(color = "grey"),
           coefs = TRUE,
           sig = .05,
           stand = TRUE,
           covs = TRUE,
           # stars = 'latent',
           graph_options = list(rankdir = "RL"))

# ** Residual correlation matrix ----------------------------------------
plot_matrix(residuals(fit_med, type='cor')$cov)

# ** Modification indices --------------------------------------------------
modificationIndices(fit_med, minimum.value=20)
