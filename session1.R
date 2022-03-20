# Load packages

### DATA MANIPULATION ###
library("haven")      # data import from spss
library("dplyr")      # data manipulation
library("psych")      # descriptives
library('stringr')    # string manipulation

### MODELING ###
library("lavaan")     # SEM modelling

### VISUALIZATION ###
library("corrplot")   # plotting SEM models
library("tidySEM")    # correlation/covariance plots

# Import data
ess_df <- haven::read_sav("data/ESS4_belgium.sav")

#### EDA ####
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

#### 1-FACTOR CFA MODEL ####

# 3 INDICATORS #
# (1) Default: first factor loading = 1
model_ws_3 <-'welf_supp =~ gvslvol + gvslvue + gvhlthc'

fit_ws_3 <- cfa(model_ws_3,             # model formula
                data = ess_df_selected  # data frame
)

summary(fit_ws_3,
        standardized = TRUE)


# (2) Set factor variance = 1
model_ws_3_alt <- '
welf_supp =~ NA*gvslvol + gvslvue + gvhlthc
welf_supp~~1*welf_supp
'

fit_ws_3_alt <- cfa(model_ws_3_alt,            # model formula
                    data = ess_df_selected     # data frame
)

summary(fit_ws_3_alt,
        standardized = TRUE)

# 6 INDICATORS #
model_ws_6 <-'welf_supp =~ gvslvol + gvslvue + gvhlthc + gvcldcr + gvjbevn + gvpdlwk'


fit_ws_6 <- cfa(model_ws_6,             # model formula
                data = ess_df_selected  # data frame
)

summary(fit_ws_6,
        standardized = TRUE)


# MODEL DIAGNOSTICS #

# Returns the observed covariance matrix.
lavInspect(fit_ws_6, "sampstat")

# Returns the model estimated covariance matrix.
fitted(fit_ws_6)

# Returns the difference between the observed and estimated covariance matrix
residuals(fit_ws_6)

# Returns the standard errors for the free parameters in the model.
lavTech(fit_ws_6, "se")

# Returns the parameter estimates with confidence intervals.
parameterEstimates(fit_ws_6, standardized=TRUE)

# Model fit and statistics
summary(fit_ws_6,            # fitted model
        fit.measures = TRUE, # returns commonly used fit measures
        standardized = TRUE  # indicates that we want standardized results
)

# we can also just extract the factor loadings.
# using lavaan terminology, the factors loadings are the lambdas

inspect(fit_ws_6,           # fitted model
        what="std")$lambda  # standardized factors loadings

# more info on the factors loading can be obtained using the tidy SEM package
# first we extract all the estimated parameters

tidy_results <- table_results(fit_ws_6,
                              columns = c("label",
                                          "est_sig",
                                          "se",
                                          "confint"),
                              digits = 2
)

tidy_results %>% filter(str_detect(label, "welf_supp."))

# we can also take a look at residual variances
# using lavaan terminology, the residual variances are the thetas
theta <- round(inspect(fit_ws_6, "est")$theta,3)
theta.std <- round(inspect(fit_ws_6, "std")$theta,3)
r2 <- round(inspect(fit_ws_6, "r2"),3)

data.frame(row.names = c(),                       # empty the columns names
           Variables = colnames(theta),           # variable names
           "Residuals" = diag(theta),             # diagonal theta
           "Std. Residuals" = diag(theta.std),    # diagonal std. theta
           "R Squared" = r2                       # R-squared
)

## Global fit measures ##
# we can select which global fit measures to extract
fitMeasures(fit_ws_6, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")

## Local fit measures: modification indices ##
mi <- inspect(fit_ws_6,"mi")
mi.sorted <- mi[order(-mi$mi),] # sort from high to low mi.sorted[1:5,] # only display some large MI values
mi.sorted[1:5,] # only display some large MI values

# let's plot the modification indices
plot(mi.sorted$mi) # plot the MI values
abline(h=3.84) # add a horizontal reference line (chisq value for 1 df where p=0.05)

#### 3-FACTOR CFA MODEL ####

# Create dataset
ess_df_selected <- ess_df %>% select(
  ## Economic criticism items ##
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

# Check the sample implied covariance matrix

welfare_crit_cov <- cov(ess_df_selected, use = "pairwise.complete.obs")

welfare_crit_cor <- cov2cor(welfare_crit_cov)

corrplot::corrplot(welfare_crit_cor,
                   is.corr = FALSE,       # whether is a correlation matrix
                   method = "circle",     # magnitude of covariance as circles
                   type = "upper",        # remove the bottom of the covariance matrix
                   addCoef.col = "black"  # add to the plot the coefficients
)

# Fit 3-factor model
model_wc <-'
## Economic criticism ##
wc_econo =~ sbstrec + sbbsntx
## Social criticism ##
wc_socia =~ sbprvpv + sbeqsoc + sbcwkfm
##  Moral criticism ##
wc_moral =~ sblazy + sblwcoa + sblwlka
'

fit_wc <- cfa(model_wc,              # model formula
              data = ess_df_selected  # data frame
)

summary(fit_wc, standardized=TRUE)

#### PLOTTING ####

# first, let's define our plot layout
lay <- get_layout("wc_econo", "", "", "wc_socia","", "","wc_moral", "",
                  "sbstrec", "sbbsntx", "sbprvpv", "sbeqsoc", "sbcwkfm", "sblazy", "sblwcoa", "sblwlka", rows = 2)

# let's take a look at our plot layout.
lay

# let's plot our results
plot_wc <- graph_sem(model = fit_wc,      # model fit
                     layout = lay,         # layout
                     angle = 170           # adjust the arrows
                     #label = "est_std",  # get standardized results (not rounded)
)

plot_wc

# Customize the plot
graph_data <- prepare_graph(fit_wc)

edges(graph_data) <- graph_data %>%
  edges() %>%
  mutate(colour = "black") %>%
  mutate(colour = replace(colour, from == "wc_socia" & to == "sbcwkfm", "red"))

plot(graph_data,
     layout = lay,        # layout
     #label = "est_std",   # get standardized results (not rounded)
     angle = 170          # adjust the arrows
)
