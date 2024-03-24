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

lapply(c("nnet", "MASS", "stargazer"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################

### Question 1

# load data
gdp_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/gdpChange.csv", stringsAsFactors = F)

summary(gdp_data)

breaks <- c(-Inf, -0.000001, 0, Inf)

# Define labels for the intervals
labels <- c("negative","no change", "positive")

# Create a new column with categorical data
gdp_data$GDPWdiff_cat <- cut(gdp_data$GDPWdiff, breaks = breaks, labels = labels, include.lowest = TRUE)

# Replace NA values (if any) with "No Change"

# Print first few rows to verify
head(gdp_data)

gdp_data$GDPWdiff_cat <- relevel(gdp_data$GDPWdiff_cat, ref = "no change")


multinom_model1 <- multinom(GDPWdiff_cat ~ REG + OIL, data = gdp_data)
summary(multinom_model1)
exp(coef(multinom_model1))

stargazer(multinom_model1)

### Question 2

polr_model1 <- polr(GDPWdiff_cat ~ REG + OIL, data = gdp_data, Hess= TRUE)
summary(polr_model1)
exp(coef(polr_model1))

stargazer(polr_model1)

#####################
# Problem 2
#####################

# load data
mexico_elections <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/MexicoMuniData.csv")

### Question 1

poisson_model1 <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, data = mexico_elections, family = poisson)
summary(poisson_model1)

stargazer(poisson_model1)

exp(-2.080)
exp(-0.312)


lambda <- exp(-3.81023+(-0.08135)*1+(-2.08014)*0+(-0.31158)*1) 
lambda
