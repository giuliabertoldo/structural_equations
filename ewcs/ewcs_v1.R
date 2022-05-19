# Load packages -----------------------------------------------------------
library(haven)
library(tidyverse)
library(mice)

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
         Q78a,
         Q78b,
         Q78c,
         Q78d,
         Q78e,
         Q78f,
         Q78g,
         Q78h,
         Q78i,
         Q78j,
         Q88)

# Exploratory Data Analysis -----------------------------------------------
glimpse(df_ge)
# Sample size
(dimen <- dim(df_ge))
# Demographics



# * Missing data pattern --------------------------------------------------

(pattern <- md.pattern(df_ge))
## 1546 subjects have no missing data
## The variables with the highest amount of missing data (28) are Q61e and Q61c


## Percentage of missing data per variable
purrr::map(df_ge, ~(mean(is.na(.))*100))



# MODEL -------------------------------------------------------------------


# * CFA Psychological needs -----------------------------------------------
model_needs <- '
autonomy =~ Q61c + Q61d + Q61e + Q61i + Q61n
competence =~ Q61g + Q61h + Q61j + Q61k
relatedness =~ Q61a + Q61b + Q61l
'

fit_needs <- cfa(model_needs,
                 data = df_ge,
                 std.lv = TRUE,
                 #estimator = 'MLM',
                 missing = 'fiml')
summary(fit_needs,
        standardized = TRUE,
        fit.measures = TRUE)


# * CFA Psychological wellbeing  ------------------------------------------

model_well <- '
psych_wellbeing =~ Q87a + Q87b + Q87c + Q87d + Q87e
'

fit_well <- cfa(model_well,
                data = df_ge,
                std.lv = TRUE,
                # estimator = 'MLM',
                missing = 'fiml')
summary(fit_needs,
        standardized = TRUE,
        fit.measures = TRUE)

# * CFA Pulled  ------------------------------------------

model_pulled <- '
autonomy =~ Q61c + Q61d + Q61e + Q61i + Q61n
competence =~ Q61g + Q61h + Q61j + Q61k
relatedness =~ Q61a + Q61b + Q61l
psych_wellbeing =~ Q87a + Q87b + Q87c + Q87d + Q87e
psych_wellbeing ~~ 0*autonomy
psych_wellbeing ~~ 0*competence
psych_wellbeing ~~ 0*relatedness
'

fit_pulled <- cfa(model_pulled,
                data = df_ge,
                std.lv = TRUE,
                # estimator = 'MLM',
                missing = 'fiml')
summary(fit_pulled,
        standardized = TRUE,
        fit.measures = TRUE)

# * Full SEM: Mediation ---------------------------------------------------

med_model <- '
autonomy =~ Q61c + Q61d + Q61e + Q61i + Q61n
competence =~ Q61g + Q61h + Q61j + Q61k
relatedness =~ Q61a + Q61b + Q61l
psych_wellbeing =~ Q87a + Q87b + Q87c + Q87d + Q87e
Q88 ~ a1*autonomy + a2*competence + a3*relatedness
psych_wellbeing ~ autonomy + competence + relatedness + b1*Q88
i_1 := a1*b1
i_2 := a2*b1
i_3 := a3*b1
'
med_fit <- sem(med_model,
               data = df_ge,
               std.lv = TRUE,
               # estimator = 'MLM',
               missing = 'fiml',
               se = "bootstrap")
summary(med_fit,
        standardized = TRUE,
        fit.measures = TRUE)
modificationIndices(med_fit, minimum.value=20)
