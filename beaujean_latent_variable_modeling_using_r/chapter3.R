
# 3.3 Example: Latent Variable Model with One Latent Variable -------------
library(lavaan)

# convert vector of correlations into matrix
wisc4.cor <- lav_matrix_lower2full(c(1,0.72,1,0.64,0.63,1,0.51,0.48,0.37,1,0.37,0.38,0.38,0.38,1))
# name the variables in the matrix
colnames(wisc4.cor) <- rownames(wisc4.cor) <- c("Information", "Similarities",
                                                "Word.Reasoning", "Matrix.Reasoning", "Picture.Concepts")

# enter the SDs
wisc4.sd <- c(3.01 , 3.03 , 2.99 , 2.89 , 2.98)
names(wisc4.sd) <-  c("Information", "Similarities", "Word.Reasoning", "Matrix.Reasoning",
                      "Picture.Concepts")

# convert correlations and SDs to covarainces
wisc4.cov <- cor2cov(wisc4.cor, wisc4.sd)
wisc4.model<-'
g =~ a*Information + b*Similarities + c*Word.Reasoning + d*Matrix.Reasoning +
e*Picture.Concepts
'

# estimate parameters
wisc4.fit <- cfa(model=wisc4.model,
                 sample.cov=wisc4.cov,
                 sample.nobs=550,
                 std.lv=FALSE)
summary(wisc4.fit, standardized = TRUE)
parameterEstimates(wisc4.fit,
                   standardized = TRUE,
                   ci = FALSE)

# model-implied covariances
fitted(wisc4.fit)

# transform model-implied covariances to correlations
wisc4Fit.cov <- fitted(wisc4.fit)$cov
(wisc4Fit.cor <- cov2cor(wisc4Fit.cov))

# original correlations
wisc4.cor

# residual correlations
residuals(wisc4.fit, type = "cor")

# fit measures
# residual correlations
fitMeasures(wisc4.fit)

# modification indices
modificationIndices(wisc4.fit)


# * Use marker variable ---------------------------------------------------
# Option 1: In model specification
wisc4.model.Std<-'
g =~ NA*Information + a*Information + b*Similarities + c*Word.Reasoning +
d*Matrix.Reasoning + e*Picture.Concepts
# constrain the LV variance to 1
g~~1*g
'
wisc4.fit.Std <- cfa(wisc4.model.Std, sample.cov=wisc4.cor, sample.nobs=550)
summary(wisc4.fit.Std)

# Option 2: use the std.lv = TRUE in cfa()
wisc4.fit.Std <- cfa(wisc4.model,
                     sample.cov=wisc4.cor,
                     sample.nobs=550,
                     std.lv=TRUE)





# 3.3.2 Example: Latent variable model with two latent variables ------------
# two-factor model of the WISC-IV data
wisc4.model2<-'
V =~ a*Information + b*Similarities + c*Word.Reasoning
F =~ d*Matrix.Reasoning + e*Picture.Concepts
V~~f*F
'
wisc4.fit2 <- cfa(wisc4.model2, sample.cov=wisc4.cov, sample.nobs=550)

summary(wisc4.fit2,
        standardized = TRUE)



# 3.4 Example: Structural Equation Model ----------------------------------
# structural equation model
wisc4SEM.model <- '
# define latent variables
V =~ a*Information + b*Similarities + c*Word.Reasoning
F =~ d*Matrix.Reasoning + e*Picture.Concepts
# define structural relations
V~k*F
'
wisc4SEM.fit <- cfa(wisc4SEM.model,
                    sample.cov=wisc4.cov,
                    sample.nobs=550)
summary(wisc4SEM.fit,
        standardized = TRUE)







# Exercise 3.1: CFA -------------------------------------------------------
# 3.1.a Enter the covariance matrix into R.
psychSoc.cov <- c(0.77, 0.38, 0.65, 0.39, 0.39, 0.62, -0.25, -0.32, -0.27, 6.09)
psychSoc.cov <- lav_matrix_lower2full(psychSoc.cov)
rownames(psychSoc.cov) <- colnames(psychSoc.cov) <- c("Dep.1", "Dep.2", "Dep.3", "SocActivity")
psychSoc.cov

# 3.1.b Fit the model using the marker variable, standardized latent variable
## For the marker variable method, use Depression 1 as the marker variable.

psychSoc.model <- '
psych_health =~ a*Dep.1 + b*Dep.2 + c*Dep.3 + d*SocActivity
'
# marker variable
psychSoc.fit <- cfa(psychSoc.model,
                    sample.cov=psychSoc.cov,
                    sample.nobs=6053,
                    std.lv = FALSE)
summary(psychSoc.fit,
        standardized = TRUE,
        fit.measures = TRUE)
# standardized latent variable
psychSoc.fit2 <- cfa(psychSoc.model,
                    sample.cov=psychSoc.cov,
                    sample.nobs=6053,
                    std.lv = TRUE)
summary(psychSoc.fit2,
        standardized = TRUE,
        fit.measures = TRUE)



# Exercise 3.2: SEM -------------------------------------------------------
# 3.2.a Enter the covariance matrix into R.
mobility.cov <- c(0.77 , 0.38 , 0.65 , 0.39 , 0.39 , 0.62,  -0.25,  -0.32,  -0.27 , 6.09, 0.31,
                  0.29 , 0.26,  -0.36 , 7.67 , 0.24 , 0.25 , 0.19, -0.18, 0.51, 1.69, -3.16,  -3.56,  -2.63 , 6.09,
                  -3.12,  -4.58, 204.79,  -0.92,  -0.88, -0.72 , 0.88,  -1.49,  -1.41,  16.53, 7.24)
mobility.cov <- lav_matrix_lower2full(mobility.cov)
rownames(mobility.cov) <- colnames(mobility.cov) <- c("Dep.1", "Dep.2", "Dep.3", "SocActivity",
                                                      "Falls", "Chronic", "TotActivity", "PersMobility")
mobility.cov

# 3.2.b Fit the SEM model to the data. Are both psychosocial and physical health
## predictive of personal mobility?

mobility.model <- '
psych_health =~ Dep.1 + Dep.2 + Dep.3 + SocActivity
phys_health =~ Chronic + TotActivity + Falls
PersMobility ~ psych_health+phys_health
'

mobility.fit <- cfa(mobility.model,
                    sample.cov=mobility.cov,
                    sample.nobs=6053,
                    std.lv = TRUE)
summary(mobility.fit,
        fit.measures = TRUE,
        standardized = TRUE)

# Physical health (-0.914) appears to be a stronger predictor of
## personal mobility than psychosocial health (0.101).

# Extra for me: check that you obtain the same coefficients by fitting CFA separately
(subset1 <- mobility.cov[1:7,1:7])
cfa.model <- '
psych_health =~ Dep.1 + Dep.2 + Dep.3 + SocActivity
phys_health =~ Chronic + TotActivity + Falls
'

cfa.fit <- cfa(cfa.model,
               sample.cov=subset1,
               sample.nobs=6053,
               std.lv = TRUE)
summary(cfa.fit,
        fit.measures = TRUE,
        standardized = TRUE)


# Exercise 3.3: Descriptive discriminant analysis  ------------------------

# 3.3.a Import the covariance matrix and mean vector shown in Figure 3.6 into R.
dda.cov <- lav_matrix_lower2full(c(59.66, 11.18, 22.3, 2.63, 0.41, 1), diagonal = TRUE)
rownames(dda.cov) <- colnames(dda.cov) <- c("DV1", "DV2", "Cat1")
dda.cov

# 3.3.b Write lavaan syntax for Figure 3.12.
dda.model<-'
F1 <~ 1*DV1 + a*DV1 + b*DV2
Cat1 ~ c*F1
Cat1~~d*Cat1
'
dda.fit <- sem(dda.model, sample.cov = dda.cov, sample.nobs = 288)


# 3.3.c Obtain the function coefficients (standardized coefficients for a & b),
# canonical correlation (standardized coefficient for c),
# and R2 (standardized coefficient for d) values.
parameterEstimates(dda.fit, standardized = TRUE)

# The function coefficients are 1.016 (a) and -0.057 (b),
# the canonical correlation is 0.341 (c), and R2 is 0.116 (i.e., 1 − 0.884)

