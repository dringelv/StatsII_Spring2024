data <- read.csv("long97.csv")

with(data,
     hist(job))
summary(data)
view(data)

# OLS on original data
summary(m1 <- lm(job ~ ., data[,-2])) # effect of being a woman is negative

# OLS on censored data (tau y = 1) 
summary(m2 <- lm(jobcen ~ ., data[,-1])) # effect of being a woman is negative but small and not significant

# OLS on truncated data
summary(m3 <- lm(jobcen ~ ., data = data[data$jobcen > 1, -1])) # effect of being a woman is positive and not significant 

# Tobit 
summary(m.tobit <- vglm(job ~ ., tobit(Lower = 1), data[,-2])) # effect of being a woman is significant and negative
# normal intercept and intercept for the log-standard deviation of the latent variable
