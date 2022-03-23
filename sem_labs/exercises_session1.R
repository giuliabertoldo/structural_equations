# EXERCISE SESSION 1

# Load the packages

### DATA MANIPULATION ###
library("haven")
library("dplyr")
library("psych")
library('stringr')

### MODELING ###
library("lavaan")

### VISUALIZATION ###
library("tidySEM")
library("corrplot")

# Load dataset
df <- haven::read_sav("data/ESS4_belgium.sav")

#### EXERCISE 1: LOCAL MODEL FIT ####

# Rename variables
df <- rename(df,
             gvOldStLiv = gvslvol,
             gvUnempStLiv = gvslvue,
             gvHealthSick = gvhlthc,
             gvChildCare = gvcldcr,
             gvJob = gvjbevn,
             gvWorkLeave = gvpdlwk,
             socBenEcoStrain = sbstrec,
             socBenCostBusi = sbbsntx,
             socBenPrevPov = sbprvpv,
             socBenEquSoc = sbeqsoc,
             socBenWorkLife = sbcwkfm,
             socBenLazy = sblazy,
             socBenLessCare = sblwcoa,
             socBenLessLookAfter = sblwlka,
             age = agea,
             eduYears = eduyrs,
             gender = gndr,
             income = hinctnta,
             gvReduceIncDiff = gincdif,
             IncDiffAccept = dfincac,
             fairSocSmallDiff = smdfslv
)

#### QUESTION 1 ####
# Fit the welfare support model with 6 items

# Create a dataframe containing only the 6 items of interest
df1<- df %>% select(
  gvOldStLiv,
  gvUnempStLiv,
  gvHealthSick,
  gvChildCare,
  gvJob,
  gvWorkLeave
)

# See the correlation between the 6 items
cov(df1, use = "pairwise.complete.obs")
cor1 <- cor(df1, use = "pairwise.complete.obs")
corrplot::corrplot(cor1, is.corr = TRUE, type = "upper", addCoef.col = "black")

# Specify the model you would like to fit
model1 <- 'welf_supp =~ gvOldStLiv + gvUnempStLiv + gvHealthSick +
           gvChildCare + gvJob + gvWorkLeave'

# Fit the model
fit1 <- cfa(model1, data = df1)
summary(fit1, standardized = TRUE, fit.measures=TRUE)

### QUESTION 2 ####
# Modify the model by allowing error correlations and refit the model

# Specify the model
model2 <- '
          welf_supp =~ gvOldStLiv + gvUnempStLiv + gvHealthSick +
           gvChildCare + gvJob + gvWorkLeave
          gvOldStLiv ~~ gvHealthSick
          gvChildCare ~~ gvWorkLeave
'
# Fit the model
fit2 <- cfa(model2, data = df1)
summary(fit2, standardized = TRUE, fit.measures = TRUE)


#### QUESTION 3 ####

#### QUESTION 4 ####
fit_measures1<-  fitMeasures(fit1, c("logl","AIC","BIC","chisq","df","pvalue",
                                      "cfi","tli","rmsea"), output = "matrix")

fit_measures2<-  fitMeasures(fit2, c("logl","AIC","BIC","chisq","df","pvalue",
                                     "cfi","tli","rmsea"), output = "matrix")


data.frame(Fit=rownames(fit_measures1),
           "model1" = round(fit_measures1[,1],2),
           "model2" = round(fit_measures2[,1],2)
)


#### QUESTION 5 ####
anova(fit1,fit2)

#### EXERCISE 2 ####
# model-implied var-covariance matrix for observed variables
inspect(fit1, "cov.ov")
inspect(fit2, "cov.ov")
# same thing, but calculated from model parameters
inspect(fit1, "est")$lambda %*% inspect(fit1, "est")$psi %*% t(inspect(fit1, "est")$lambda) + inspect(fit1, "est")$theta
# differences between the two matrices
inspect(fit2, "cov.ov") - inspect(fit1, "cov.ov")


#### EXERCISE 3 ####
# Using the welfare criticism items, fit a 1-factor model.
model1_wc <- '
welf_crit =~ socBenEcoStrain + socBenCostBusi + socBenPrevPov + socBenEquSoc +
             socBenWorkLife + socBenLazy + socBenLessCare + socBenLessLookAfter

'
fit1_wc <- cfa(model1_wc, data = df)
summary(fit1_wc, standardized=TRUE, fit.measures=TRUE)

# Using the welfare criticism items, fit a 3-factor model.
model2_wc <- '
## Economic criticism ##
econ_critic =~ socBenEcoStrain + socBenCostBusi
## Social criticism ##
soc_critic =~ socBenPrevPov + socBenEquSoc + socBenWorkLife
## Moral criticism ##
mor_critic =~ socBenLazy + socBenLessCare + socBenLessLookAfter
'

fit2_wc <- cfa(model2_wc, data = df)
summary(fit2_wc, standardized=TRUE, fit.measures=TRUE)

# Verify whether a 1-factor CFA model is appropriate.
fit_measures_wc1<-  fitMeasures(fit1_wc, c("logl","AIC","BIC","chisq","df","pvalue",
                                     "cfi","tli","rmsea"), output = "matrix")

fit_measures_wc2<-  fitMeasures(fit2_wc, c("logl","AIC","BIC","chisq","df","pvalue",
                                           "cfi","tli","rmsea"), output = "matrix")

data.frame("Fit Indices" = rownames(fit_measures_wc1),
           "model1_wc" = round(fit_measures_wc1[, 1], 2),
           "model2_wc" = round(fit_measures_wc2[, 1], 2)
)

# Modify the 3-factor
model3_wc <-'
## Economic criticism ##
econ_critic =~ socBenEcoStrain + socBenCostBusi
## Social criticism ##
soc_critic =~ socBenPrevPov + socBenEquSoc + socBenWorkLife
##  Moral criticism ##
mor_critic =~ socBenLazy + socBenLessCare + socBenLessLookAfter
socBenLessCare ~~ socBenLessLookAfter
'

fit3_wc <- cfa(model3_wc, data = df)

summary(fit3_wc, standardized=TRUE)

fit_measures_wc3<-  fitMeasures(fit3_wc, c("logl","AIC","BIC","chisq","df","pvalue",
                                           "cfi","tli","rmsea"), output = "matrix")

data.frame("Fit Indices" = rownames(fit_measures_wc1),
           "model1_wc" = round(fit_measures_wc1[, 1], 2),
           "model2_wc" = round(fit_measures_wc2[, 1], 2),
           "model3_wc" = round(fit_measures_wc3[, 1], 2)

)

#### EXERCISE 4 ####

model1_wc_second <- '
## Economic criticism ##
econ_critic =~ socBenEcoStrain + socBenCostBusi
## Social criticism ##
soc_critic =~ socBenPrevPov + socBenEquSoc + socBenWorkLife
##  Moral criticism ##
mor_critic =~ socBenLazy + socBenLessCare + socBenLessLookAfter
## Second order welfare criticism ##
welf_critic =~ econ_critic + soc_critic + mor_critic
'
fit1_wc_second <- cfa(model1_wc_second, data = df)
summary(fit1_wc_second, standardized=TRUE)

inspect(fit1_wc_second, "r2")

mi<-inspect(fit1_wc_second, "mi")
# sort from high to low mi.sorted[1:5,]
mi.sorted<-mi[order(-mi$mi), ]
# # only display some large MI values
head(mi.sorted, 5)

# Remove low loadings and see if model improves
model2_wc_second <-'
## Economic criticism ##
econ_critic =~ socBenEcoStrain + socBenCostBusi
## Social criticism ##
soc_critic =~ socBenPrevPov + socBenEquSoc
##  Moral criticism ##
mor_critic =~ socBenLazy + socBenLessCare + socBenLessLookAfter
## Second order welfare criticism ##
welf_critic =~ econ_critic + soc_critic + mor_critic
'

fit2_wc_second  <- cfa(model2_wc_second, data = df)

summary(fit2_wc_second, standardized=TRUE)

#### EXERCISE 5 #####
summary(fit_wc_3_factor)
summary(fit_wc_2_order)
