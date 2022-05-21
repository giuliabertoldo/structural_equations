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
df <- read_sav("../data/ewcs_2015.sav")

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


# Model -------------------------------------------------------------------
# * CFA -------------------------------------------------------------------


# * CFA Pulled  ------------------------------------------
# Model specification
model_cfa <- '
autonomy =~ Q61c + Q61d + Q61e + Q61i + Q61n
competence =~ Q61g + Q61h + Q61j + Q61k
relatedness =~ Q61a + Q61b + Q61l
psych_wellbeing =~ Q87a + Q87b + Q87c + Q87d + Q87e
psych_wellbeing ~~ 0*autonomy
psych_wellbeing ~~ 0*competence
psych_wellbeing ~~ 0*relatedness
'
# Model estimation
fit_cfa <- cfa(model_cfa,
               data = df_ge,
               std.lv = FALSE,
               estimator = 'ml',
               missing = 'fiml',
               se = "bootstrap")
summary(fit_cfa,
        standardized = TRUE,
        fit.measures = TRUE)

# Understand free parameters
inspect(fit_cfa)
lavInspect(fit_cfa, what = "list")

# * Path diagram: CFA --------------------------------------------------
lay <- get_layout("Q61c", NA, NA, NA,
                  "Q61d", "autonomy", NA, NA,
                  "Q61e", NA, NA, NA,
                  "Q61i", NA, NA, "Q87a",
                  "Q61n", NA, NA, "Q87b",
                  "Q61g", NA, NA, "Q87c",
                  "Q61h", "competence", "psych_wellbeing", "Q87d",
                  "Q61j", NA, NA, "Q87e",
                  "Q61k", NA, NA,  NA,
                  "Q61a", NA, NA,  NA,
                  "Q61b", "relatedness", NA, NA,
                  "Q61l", NA, NA, NA,
                  rows = 12)
diagram <- graph_sem(model = fit_cfa,
                     layout = lay)
diagram


# lavaanPlot --------------------------------------------------------------
lavaanPlot(model = fit_cfa,
           labels = labels,
           node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "grey"), coefs = T,covs = T,stars = "latent")
lavaanPlot(model = fit,
           node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stars = "covs")

# Modification indices
modificationIndices(fit_cfa, minimum.value=20)


# * Full SEM: Mediation ---------------------------------------------------
# Model specification
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
# Specify layout
lay <- get_layout("autonomy", NA, NA,
                  "competence", "Q88", "psych_wellbeing",
                  "relatedness", NA, NA,
                  rows = 3)

diagram <- graph_sem(model = med_fit,
                     layout = lay)
diagram

graph_data <- prepare_graph(med_fit)
graph_data

plot(graph_data,
     layout = lay,        # layout
     label = "est_std",   # get standardized results (not rounded)
     angle = 170)         # adjust the arrows




# * Modification indices --------------------------------------------------
modificationIndices(med_fit, minimum.value=20)
