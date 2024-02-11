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
lapply(c("stargazer"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################

ks_test <- function(data) {
  
  ECDF <- ecdf(data)
  empiricalCDF <- ECDF(data)
  
  D <- max(abs(empiricalCDF - pnorm(data)))
  
  # calculate p-value
  n <- length(data)
  p_value = (sqrt(2 * pi)) / D * sum(exp(-((2 * (1:1000) - 1)^2 * pi^2) / (8 * D^2)))
  
  result <- list(
    t_stat = D,
    p_value = p_value
  )
  
  return(result)
} 

# 
set.seed(123)
cauchy_data <- (rcauchy(1000, location = 0, scale = 1))

result <- ks_test(cauchy_data)
print(result)

#####################
# Problem 2
#####################

set.seed (123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)


ols_function <- function(beta, x, y) {
  residuals <- y - (beta[1]+beta[2]* x)
  sum(residuals^2)
}


ols_bfgs <- optim(fn = ols_function, par = c(0,0), hessian = TRUE, x = data$x, y = data$y, method = "BFGS")


ols_bfgs$par

# Compare with lm
lm_results <- lm(y ~ x, data = data)
summary(lm_results) 

stargazer(lm_results)
