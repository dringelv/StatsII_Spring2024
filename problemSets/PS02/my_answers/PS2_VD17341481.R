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

lapply(c("stargazer", "tidyverse", "ggplot2"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
#####################
# Problem 1
#####################

# load data
data <- load(url("https://github.com/ASDS-TCD/StatsII_Spring2024/blob/main/datasets/climateSupport.RData?raw=true"))
df <- climateSupport
summary(df)
head(df)

# 1.
df$countries <- factor(df$countries, levels = c("20 of 192", "80 of 192", "160 of 192"), ordered = FALSE)
df$sanctions <- factor(df$sanctions, levels = c("None", "5%", "15%", "20%"), ordered = FALSE)

model <- glm(choice ~ countries + sanctions, 
             data = df, 
             family = binomial(link = "logit"))


model_summary <- summary(model)
model_summary

stargazer(model)

exp(-0.27266)


# 2. 

-0.27266 + (0.33636 * 0) + (0.64835 * 1) + (0.19186 * 1) - (0.13325 * 0) - (0.30356 * 0)
-0.27266 + (0.33636 * 0) + (0.64835 * 1) + (0.19186 * 0) - (0.13325 * 1) - (0.30356 * 0)
0.24244 - 0.56755

-0.27266 + (0.33636 * 1) + (0.64835 * 0) + (0.19186 * 0) - (0.13325 * 0) - (0.30356 * 0)


model2 <- glm(choice ~ countries * sanctions, 
                 data = df, 
                 family = binomial(link = "logit"))
summary(model2)

lrt <- anova(model, model2, test = "LRT") 
stargazer(lrt)
