#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("nnet", "MASS", "stargazer", "survival", "eha", "tidyverse", "ggfortify", "stargazer", "MASS", "VGAM", "ggplot2", "survminer"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################

# load data

data(child)

# Creating a survival object using the "Surv()" function
child_surv <- with(child, Surv(enter, exit, event))

# Fitting a Cox Proportional Hazard model using the mother's age and infant's sex 
# as additive covariates
cox <- coxph(child_surv ~ sex + m.age, data = child)
summary(cox)
drop1(cox, test = "Chisq")
stargazer(cox)

cox_binned <- coxph(child_surv ~ sex + m.age_bin, data = child)
summary(cox_binned)
drop1(cox_binned, test = "Chisq")
stargazer(cox_binned)

anova(cox_binned, test = "Chisq")

cox_fit <- survfit(cox)
autoplot(cox_fit)

# Presenting results in graphic form
fit <- survfit(Surv(enter, exit, event) ~ sex, data = child)
fit
autoplot(fit)

min_age
max_age

# Define custom breakpoints for age bins
breakpoints <- c(15, 25, 35, 45, 55)

# Create bins using cut() function
child$m.age_bin <- cut(child$m.age, breaks = breakpoints, labels = c("15-25", "25-35", "35-45", "45-55"), include.lowest = TRUE)

fit_age <- survfit(Surv(enter, exit, event) ~ m.age_bin, data = child)
fit_age
autoplot(fit_age, )

autoplot(aareg(Surv(enter, exit, event) ~ sex + m.age, data = child))

autoplot(survfit(Surv(enter, exit, event) ~ sex + m.age, data = child), fun = 'event')
