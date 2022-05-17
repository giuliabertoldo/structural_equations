# Example: Path Analysis using lavaan (pg. 29) ----------------------------

# create a correlation matrix
library(lavaan)
regression.cor <- lav_matrix_lower2full(c(1.0,0.20,1,0.24,0.30,1,0.70,0.80,0.30,1))

# name the variables in the matrix
colnames(regression.cor) <- rownames(regression.cor) <- c("X1", "X2", "X3", "Y")
regression.cor

# specify the path model
# Figure 2.2 (a)
regression.model <- '
# structural model for Y
Y ~ a*X1 + b*X2 + c*X3
# label the residual variance of Y
Y ~~ z*Y
'

# estimate the parameters
regression.fit <- sem(regression.model,
                      sample.cov = regression.cor,
                      sample.nobs = 1000)
summary(regression.fit,
        rsquare = TRUE)

# Example: Indirect effects (pg. 32) --------------------------------------
# input the covarianes and name the rows/columns
beaujean.cov <- lav_matrix_lower2full(c(648.07, 30.05, 8.64, 140.18, 25.57, 233.21))
colnames(beaujean.cov) <- rownames(beaujean.cov) <- c("salary", "school", "iq")

# specify the path model
beaujean.model <- '
salary ~ a*school + c*iq
school ~ b*iq
ind:= b*c
'

# estimate parameters
beaujean.fit <- sem(beaujean.model,
                    sample.cov=beaujean.cov,
                    sample.nobs=300)
summary(beaujean.fit)


# Exercise 2.1 ------------------------------------------------------------

# (a) Import the data into R, naming the object MathHmwk.data
MathHmwk.data  <- read.table('beaujean_latent_variable_modeling_using_r/data/MathHmwk.txt',
                             header = TRUE)
str(MathHmwk.data)

# (b) Do a typical regression using the lm() function.
lm1 <- lm(MathAchievement ~ MathHomework,
          data = MathHmwk.data)
summary(lm1)

# (c) Draw a standardized and unstandardized path model of the regression.

# (d) Do the analysis as a path model in lavaan using the sem() function.
## Obtain both the standardized and unstandardized path coefficients

library(lavaan)

# path model
math.model <- '
MathAchievement ~~ a*MathHomework
'

# unstandardized results with an intercept
math.fit <- sem(math.model,
                data = MathHmwk.data,
                meanstructure = TRUE)
summary(math.fit)

# standardizes results without an intercept
# (standardized results are in the Std.all column)
math2.fit <- sem(math.model,
                 data = MathHmwk.data)
summary(math2.fit,
        standardized = TRUE)

#(e) How are the results in Exercise 2.1.d related to the results from Exercise 2.1.b?



# Exercise 2.2 ------------------------------------------------------------

# 2.2.a Input the correlation matrix into R
privSchool.cor <- c(1, 0.178, 0.23, 0.106, 0.195,
                    1, 0.327, 0.245, 0.356,
                    1, 0.183, 0.721,
                    1, 0.178,
                    1)

privSchool.cor <- lav_matrix_upper2full(privSchool.cor)
dimnames(privSchool.cor) <- list(c("Race", "SES", "CogAbil", "SchTyp", "AcadAch"),
                                 c("Race", "SES", "CogAbil", "SchTyp", "AcadAch"))
privSchool.cor

# 2.2.b Create the path model using lavaan syntax.

privSchool.model <- '
SES ~~ a*Race
CogAbil ~~ c*Race + e*SES
SchTyp ~~ f*SES + b*Race + h*CogAbil
AcadAch ~~ i*CogAbil + g*SES + d*Race + j*SchTyp
'
# 2.2.c What is the relationship between school type and academic achievement
## (i.e., path j)?

privSchool_sem <- sem(privSchool.model,
                      sample.cov = privSchool.cor,
                      sample.nobs = 18058)
summary(privSchool_sem)

parameterEstimates(privSchool_sem)


# Exercise 2.3 ------------------------------------------------------------
# 2.3.a Input the covariances into R.
ex3_covariance  <- lav_matrix_lower2full(c(84.85,71.28,140.34,
                                           18.83,-6.25,72.92,
                                           60.05,84.54,
                                           37.18,
                              139.48))
ex3_cov <- lav_matrix_lower2full(ex3_covariance)
ex3_cov
dimnames(ex3_cov) <- list(c("TeachExp", "SocClim", "MatCov", "StAch"),
                       c("TeachExp", "SocClim", "MatCov", "StAch"))
ex3_cov

# 2.3.b Write the syntax for the model.
## Use the := operator to define both indirect effects
## from teacher expectancies to student achievement (a1 × b1 and a2 × b2).

ex3_model <- '
SocClim ~~ a1*TeachExp
MatCov ~~ a2*TeachExp
StAch ~~ c*TeachExp + b1*SocClim + b2*MatCov
ind1 := a1*b1
ind2 := a2*b2
'
ex3_fit <- sem(ex3_model,
               sample.cov = ex3_cov,
               sample.nobs = 40)
