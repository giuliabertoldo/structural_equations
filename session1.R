# Load packages
library(haven)
library(dplyr)
library(corrplot)

# Import data
ess_df <- haven::read_sav("data/ESS4_belgium.sav")

# EFA
str(ess_df)
View(ess_df)
head(ess_df)

# number of subjects
nrow(ess_df)

# number of variables
ncol(ess_df)

# names of variables
names(ess_df)

# In this first lab, we are exploring two concepts:
# welfare support and welfare criticism. Let’s take a closer look at our items

# Select only variables of interest
ess_df_selected <- ess_df %>% select(
  ## Welfare support items ##
  gvslvol, # the old
  gvslvue, # the unemployed
  gvhlthc, # the sick
  gvcldcr, # working parents
  gvjbevn, # job for everyone
  gvpdlwk, # paid sick leave
  ##    Economic criticism items ##
  sbstrec, # strain on economy
  sbbsntx, # too much taxes
  ##    Social criticism items ##
  sbprvpv, # poverty
  sbeqsoc, # more equal society
  sbcwkfm, # work and family
  ##    Moral criticism items ##
  sblazy,  # people lazy
  sblwcoa, # care for others
  sblwlka  # look after others
)

# Descriptives
descriptive_ess <- as.data.frame(psych::describe(ess_df_selected))
(descriptive_ess <- dplyr::select(descriptive_ess,
                                  n,
                                  mean,
                                  sd,
                                  median,
                                  min,
                                  max,
                                  skew,
                                  kurtosis))

# Calculate variance-covariance matrix
# Welfare support items only
ess_df_welfare_supp <- ess_df %>% select(
  ## Welfare support items ##
  gvslvol, # the old
  gvslvue, # the unemployed
  gvhlthc, # the sick
  gvcldcr, # working parents
  gvjbevn, # job for everyone
  gvpdlwk  # paid sick leave
)
# Sample implied covariance matrix
(welfare_supp_cov <- cov(ess_df_welfare_supp,          # data frame
                        use = "pairwise.complete.obs" # remove NAs
))
# Visualize
(welfare_supp_cor <- cov2cor(welfare_supp_cov))
corrplot::corrplot(welfare_supp_cor,
                   is.corr = FALSE,       # whether is a correlation matrix
                   method = "circle",     # magnitude of covariances as circles
                   type = "upper",        # remove the bottom of the covariance matrix
                   addCoef.col = "black"  # add to the plot the coefficients
)
