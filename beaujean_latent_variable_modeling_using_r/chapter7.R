# Chapter 7


# 7.3 Example: Missing Data -----------------------------------------------
# Load packages
library(mice)
library(semTools)
library(lavaan)

# Load data
mcar.data <- read.csv("beaujean_latent_variable_modeling_using_r/data/mcar.dat",
                        header=TRUE)

# Pattern
md.pattern(mcar.data)

# Test if missing values are MCAR
# package ‘MissMech’ is not available (for R version 4.0.2)
# package ‘BaylorEdPsych’ is not available (for R version 4.0.2)

# FIML
complete.model <- '
read =~ a*z1 + b*z2 + c*z3
read ~ g*x1
# label error variances
z1~~d*z1
z2~~e*z2
z3~~f*z3
read~~h*read
'
mcarFIML.fit <- sem(complete.model, data=mcar.data, missing="fiml")
summary(mcarFIML.fit, rsquare=TRUE, standardized=TRUE)

# MI
mcarMI.fit <- runMI(complete.model,
                    data = mcar.data,
                    m = 20,
                    miPackage = "mice",
                    fun = "sem")
summary(mcarMI.fit)
