# Justifying Distribution
# Step 02: Make tables and figures
# Last updated: 08/15/2023


# Load data ---------------------------------------------------------------
library(tidyverse)
library(ggfittext)
library(ggthemes)
library(estimatr)
library(texreg)
library(stargazer)

load("cleaned_exp1.RData")
load("cleaned_expR1.RData")
load("cleaned_expR2.RData")
load("cleaned_exp2.RData")

# This function makes regression tables into a Latex format.
source("make_texreg.R")

# Set path here to save your table and figure output to.
# Default is that it creates an output folder in working directory.
dir.create("output")
save_path <- "output/"

# Table 1 -------------------------------------------------------------------

lm1incum <- lm_robust(eval_incumbent~ distatorabovezero +
                           distabovezero + cityatorabovezero + cityabovezero +
                           distworsecity + city_pc + dist_pc + task,
                         cluster = ResponseId, data = df1)
lm1vote <- lm_robust(vote~ distatorabovezero +
                          distabovezero + cityatorabovezero + cityabovezero +
                          distworsecity + city_pc + dist_pc + task,
                        cluster = ResponseId, data = df1)
lm1proj <- lm_robust(eval_project~ distatorabovezero +
                          distabovezero + cityatorabovezero + cityabovezero +
                          distworsecity + city_pc + dist_pc + task,
                        cluster = ResponseId, data = df1)

# Checking if assumptions are violated -------------------------------------------------------------

df1$residuals <- (lm1incum$fitted.values - df1$eval_incumbent)
hist(df1$residuals, breaks = 20, main = "Histogram of Residuals", xlab = "Residuals")

# Get predicted values from the robust linear regression model
predicted_values <- predict(lm1incum, df1)

# Plot residuals against predicted values
plot(predicted_values, df1$residuals, 
     xlab = "Predicted Values", ylab = "Residuals",
     main = "Residuals vs. Predicted Values")
abline(h = 0, col = "red")  # Add horizontal line at y = 0



make_texreg(name = "table1",
            model = list(lm1incum, lm1vote, lm1proj),
            custom.model.names = c("\\makecell{Incumbent\\\\Evaluations\\\\(-1 to 1)}", 
                                   "\\makecell{Vote for Incumbent\\\\vs. Challenger\\\\(-1 to 1)}",
                                   "\\makecell{Project\\\\Evaluation\\\\(-1 to 1)}"),
            custom.coef.map = list("distatorabovezero" = "District At Least Breaks Even (District \\geq 0)",
                                   "distabovezero" = "District Benefits (District > 0)",
                                   "dist_pc" = "District Returns Per Capita",
                                   "distworsecity" = "District Worse Off than City",
                                   "cityatorabovezero" = "City At Least Breaks Even (City \\geq 0)",
                                   "cityabovezero" = "City Benefits (City > 0)",
                                   "city_pc" = "City Returns Per Capita",
                                   "task2" = "Vignette 2",
                                   "task3" = "Vignette 3",
                                   "(Intercept)" = "Constant"),
            custom.note = paste("\\item[\\hspace{-5mm}] %stars.",
                                "\\item[\\hspace{-5mm}] Dependent variables are listed in each column. 
       Models estimated using ordinary least squares regression, with standard errors clustered by respondent."),
            caption = "Effect of District and City-Wide Returns on 
            Evaluations, Experiment 1",
            multiple.tasks = "clustered")


# Table 2 (appendix) -----------------------------------------------------------------
# In appendix tables, after Table A2 because it relies on models from Table A2.

# Table 3 -----------------------------------------------------------------

lm3proj <- lm_robust(project_thermometer ~ distatorabovezero + distabovezero + distworsecity +
                        city_pc + dist_pc + fielding + T_generic +
                        T_baddealdist_notgermane + T_baddealdist_germane +
                        T_notfairshare_notgermane + T_notfairshare_germane, data = df2)

lm3approve <- lm_robust(approve_num ~ distatorabovezero + distabovezero + distworsecity +
                           city_pc + dist_pc + fielding + T_generic +
                           T_baddealdist_notgermane + T_baddealdist_germane +
                           T_notfairshare_notgermane + T_notfairshare_germane, data = df2)

lm3incum <- lm_robust(councilor_thermometer ~ distatorabovezero + distabovezero + distworsecity +
                         city_pc + dist_pc + fielding + T_generic +
                         T_baddealdist_notgermane + T_baddealdist_germane +
                         T_notfairshare_notgermane + T_notfairshare_germane, data = df2)

lm3vote <- lm_robust(vote_num ~ distatorabovezero + distabovezero + distworsecity +
                       city_pc + dist_pc + fielding + T_generic +
                       T_baddealdist_notgermane + T_baddealdist_germane +
                       T_notfairshare_notgermane + T_notfairshare_germane, data = df2)

make_texreg(name = "table3",
            model = list(lm3proj, lm3approve, lm3incum, lm3vote),
            custom.model.names = c("\\makecell{Project\\\\Evaluation\\\\(0 to 100)}", 
                                   "\\makecell{Approval of\\\\Project\\\\(1 to 5)}", 
                                   "\\makecell{Incumbent\\\\Evaluation\\\\(0 to 100)}", 
                                   "\\makecell{Vote for Incumbent\\\\vs. Challenger\\\\(1 to 5)}"),
            custom.coef.map = list("distatorabovezero" = "District At Least Breaks Even (District \\geq 0)}",
                                   "distabovezero" = "District Benefits (District > 0)",
                                   "dist_pc" = "District Returns Per Capita",
                                   "distworsecity" = "District Worse Off than City",
                                   "city_pc" = "City Returns Per Capita",
                                   "fielding" = "Second Sample",
                                   "T_generic" = "Generic Critique",
                                   "T_baddealdist_notgermane" = "District Performance Critique (Not Germane)",
                                   "T_baddealdist_germane" = "District Performance Critique (Germane)",
                                   "T_notfairshare_notgermane" = "Fairness Critique (Not Germane)",
                                   "T_notfairshare_germane" = "Fairness Critique (Germane)",
                                   "(Intercept)" = "Constant"),
            custom.note = paste("\\item[\\hspace{-5mm}] %stars.",
                                "\\item[\\hspace{-5mm}] Dependent variables are listed in each column. Models estimated using ordinary least squares regression, 
       with standard errors clustered by respondent."),
            caption = "Effect of Challenger Criticisms on Evaluations, Experiment 2",
            multiple.tasks = "normal")

# Figure 1 ----------------------------------------------------------------


xlabels_cat3city <- c("-$50", "-$25", "-$10", "-$5", "-$1",
                      "$0", "$1", "$5", "$10", "$25", "$50")

breaks = c("-1250000","-625000", "-250000", "-125000", 
           "-25000", "0", "25000", "125000", "250000",
           "625000", "1250000")

# Step 2 - Visualize New Data Frame
df_cat3city_count <- df1 %>% 
  mutate(cat3city = case_when(city > 0 ~ "Net Gain",
                              city == 0 ~ "Break Even",
                              city < 0 ~ "Net Loss")) %>% 
  group_by(cat3city, dist_pc) %>% 
  count()

df_cat3city <- df1 %>% 
  mutate(cat3city = case_when(city > 0 ~ "Net Gain",
                              city == 0 ~ "Break Even",
                              city < 0 ~ "Net Loss")) %>% 
  group_by(cat3city, dist_pc) %>% 
  summarize(meaneval = mean(eval_incumbent, na.rm = TRUE)) %>% 
  left_join(df_cat3city_count) %>% 
  mutate(n = n^2,
         cat3city = factor(cat3city,
                           levels = c("Net Gain", "Break Even", "Net Loss")),
         dist_pc = factor(dist_pc))%>% 
  rename(sizing = n,
         `City Return` = cat3city)

options(ggplot2.discrete.colour= c("blue", "black", "red"))


gf1 <- ggplot(df_cat3city, aes(x = dist_pc, y = meaneval, col = `City Return`)) +
  geom_line(aes(group = `City Return`)) +
  geom_point(aes(size = sizing, shape = `City Return`)) +
  scale_shape_manual(values = c("\u002b","\u0030",  "\u002d")) + #change values to plus, zero, minus
  ylim(-1,1) + 
  scale_size(range = c(0, 20), guide = 'none') +
  scale_x_discrete(breaks = c(-50, -25, -10, -5, -1, 0, 1, 5, 10, 25, 50),
                   labels = xlabels_cat3city) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "District Per Capita Return (Non-Linear Scale)", 
       y = "Evaluation of Incumbent") +
  theme_few() +
  theme(
    legend.position = c(0.9, 0.12),
    plot.title = element_text(hjust = 0.5)) +
  annotate("rect", xmin=5, xmax=6, ymin=-Inf, ymax=Inf, alpha=0.5, fill="gray") +
  geom_fit_text(
    data = data.frame(xmin = 5, xmax = 6, 
                      label = "Expected\nDiscontinuity\nInterval"),
    aes(xmin = xmin, xmax = xmax, ymin = 0.5, ymax = 0.8, label = label,
        fontface = "bold"),
    inherit.aes = FALSE)
gf1


ggsave(paste0(save_path, "3categorycity.png"),
       plot = gf1, width = 8, height = 6)



# Fact Check p-values in paper --------------------------------------------------------------

#3

mean(df1$eval_incumbent[df1$city_pc > 0 & df1$dist_pc == 0], na.rm = TRUE) -
  mean(df1$eval_incumbent[df1$city_pc > 0 & df1$dist_pc == -1], na.rm = TRUE)

t.test(df1$eval_incumbent[df1$city_pc > 0 & df1$dist_pc == -1],
      df1$eval_incumbent[df1$city_pc > 0 & df1$dist_pc == 0])

#4   

mean(df1$eval_incumbent[df1$city_pc > 0 & df1$dist_pc == 25], na.rm = TRUE) -
  mean(df1$eval_incumbent[df1$city_pc > 0 & df1$dist_pc == 0], na.rm = TRUE)

t.test(df1$eval_incumbent[df1$city_pc > 0 & df1$dist_pc == 25],
       df1$eval_incumbent[df1$city_pc > 0 & df1$dist_pc == 0])

# Attention check percentages in paper --------------------------------------------------------

prop_table <- function(table){
  round(prop.table(table(table, exclude = NULL)), digits = 2)
}

# Experiment 1
prop_table(df1$dist_correct)
prop_table(df1$city_correct)

# Replication 1
prop_table(df1R1$method)
prop_table(df1R1$money)
prop_table(df1R1$dist_correct)
prop_table(df1R1$city_correct)

# Replication 2

prop_table(df1R2$method)
prop_table(df1R2$money)
prop_table(df1R2$dist_correct)
prop_table(df1R2$city_correct)

length(table(df1R2$ResponseId))

# Experiment 2

prop_table(df2$dist_correct)
prop_table(df2$city_correct)

prop_table(df1$city_pc)
prop_table(df1$dist_pc)

prop_table(df1R1$city_pc)
prop_table(df1R1$dist_pc)

prop_table(df1R2$city_pc)
prop_table(df1R2$dist_pc)

prop_table(df2$city_pc)
prop_table(df2$dist_pc)


# Logit ------------------------------------------------------------------------------------------

lm1incum_edu <- lm_robust(eval_incumbent ~ distatorabovezero +
                        distabovezero + cityatorabovezero + cityabovezero +
                        distworsecity + city_pc + dist_pc + task + education,
                      cluster = ResponseId, data = df1)
lm1vote_edu <- lm_robust(vote ~ distatorabovezero +
                       distabovezero + cityatorabovezero + cityabovezero +
                       distworsecity + city_pc + dist_pc + task + education,
                     cluster = ResponseId, data = df1)
lm1proj_edu <- lm_robust(eval_project ~ distatorabovezero +
                       distabovezero + cityatorabovezero + cityabovezero +
                       distworsecity + city_pc + dist_pc + task + education,
                     cluster = ResponseId, data = df1)
summary(lm1incum_edu)
summary(lm1proj_edu)
summary(lm1vote_edu)


# Include interaction terms in the regression models
lm1incum_interaction <- lm_robust(eval_incumbent ~ distatorabovezero +
                                    distabovezero + cityatorabovezero + cityabovezero +
                                    distworsecity + city_pc + dist_pc + task + education +
                                    education * distatorabovezero + education * distabovezero +
                                    education * cityatorabovezero + education * cityabovezero +
                                    education * distworsecity + education * city_pc + 
                                    education * dist_pc + education * task,
                                  cluster = ResponseId, data = df1)

lm1vote_interaction <- lm_robust(vote ~ distatorabovezero +
                                   distabovezero + cityatorabovezero + cityabovezero +
                                   distworsecity + city_pc + dist_pc + task + education +
                                   education * distatorabovezero + education * distabovezero +
                                   education * cityatorabovezero + education * cityabovezero +
                                   education * distworsecity + education * city_pc + 
                                   education * dist_pc + education * task,
                                 cluster = ResponseId, data = df1)

lm1proj_interaction <- lm_robust(eval_project ~ distatorabovezero +
                                   distabovezero + cityatorabovezero + cityabovezero +
                                   distworsecity + city_pc + dist_pc + task + education +
                                   education * distatorabovezero + education * distabovezero +
                                   education * cityatorabovezero + education * cityabovezero +
                                   education * distworsecity + education * city_pc + 
                                   education * dist_pc + education * task,
                                 cluster = ResponseId, data = df1)

summary(lm1proj_interaction)
summary(lm1vote_interaction)
summary(lm1incum_interaction)

texreg(lm1incum_interaction, include.ci = FALSE)

# Load the texreg package
library(texreg)

# Create a list of your regression models
models_list <- list(lm1incum_interaction, lm1proj_interaction, lm1vote_interaction)

texreg(models_list, include.ci = FALSE, pvalues = TRUE)

# Extract coefficients from the first model
coefficients1 <- coef(lm1incum_interaction)

# Check the names of the coefficients
names(coefficients1)

coefficients2 <- coef(lm1proj_interaction)

# Check the names of the coefficients
names(coefficients2)



make_texreg(name = "table4",
            model = list(lm1incum_interaction, lm1vote_interaction, lm1proj_interaction),
                        custom.model.names = c("\\makecell{Incumbent\\\\Evaluations\\\\(-1 to 1)}", 
                                   "\\makecell{Vote for Incumbent\\\\vs. Challenger\\\\(-1 to 1)}",
                                   "\\makecell{Project\\\\Evaluation\\\\(-1 to 1)}"),
            custom.coef.map = list("distatorabovezero:education" = "District At Least Breaks Even × Education",
                                   "distabovezero:education" = "District Benefits × Education",
                                   "dist_pc:education" = "District Returns Per Capita × Education",
                                   "distworsecity:education" = "District Worse Off than City × Education",
                                   "cityatorabovezero:education" = "City At Least Breaks Even × Education",
                                   "cityabovezero:education" = "City Benefits × Education",
                                   "city_pc:education" = "City Returns Per Capita × Education",
                                   "task2" = "Vignette 2",
                                   "task3" = "Vignette 3",
                                   "(Intercept)" = "Constant"),
            custom.note = paste("\\item[\\hspace{-5mm}] %stars.",
                                "\\item[\\hspace{-5mm}] This table presents the results of linear regression models 
                                estimating the effect of district and city-wide returns interacting with education 
                                on evaluations. The dependent variables are listed in each column. Models were 
                                estimated using ordinary least squares regression, with standard errors clustered 
                                by respondent"),
            caption = "Effect of District and City-Wide Returns Interacting with Education on Evaluations",
            multiple.tasks = "clustered")


make_texreg(name = "table4",
            model = list(lm1incum_interaction, lm1vote_interaction, lm1proj_interaction),
            custom.model.names = c("\\makecell{Incumbent\\\\Evaluations\\\\(-1 to 1)}", 
                                   "\\makecell{Vote for Incumbent\\\\vs. Challenger\\\\(-1 to 1)}",
                                   "\\makecell{Project\\\\Evaluation\\\\(-1 to 1)}"),
            custom.coef.map = list("distatorabovezero:education" = "District At Least Breaks Even (District \\geq 0)",
                                   "distabovezero:education" = "District Benefits (District > 0)",
                                   "dist_pc:education" = "District Returns Per Capita",
                                   "distworsecity:education" = "District Worse Off than City",
                                   "cityatorabovezero:education" = "City At Least Breaks Even (City \\geq 0)",
                                   "cityabovezero:education" = "City Benefits (City > 0)",
                                   "city_pc:education" = "City Returns Per Capita",
                                   "task2" = "Vignette 2",
                                   "task3" = "Vignette 3",
                                   "(Intercept)" = "Constant"),
            custom.note = paste("\\item[\\hspace{-5mm}] %stars.",
                                "\\item[\\hspace{-5mm}] Dependent variables are listed in each column. 
       Models estimated using ordinary least squares regression, with standard errors clustered by respondent."),
            caption = "Effect of District and City-Wide Returns on 
            Evaluations, Interacting with education",
            multiple.tasks = "clustered")




library(xtable)
xtable(lm1incum_interaction)
lm1incum_interaction %>% tidy %>% xtable()
lm1vote_interaction %>% tidy %>% xtable()
lm1proj_interaction %>% tidy %>% xtable()
residuals.lm(lm1incum)

# Get residuals from the linear model
residuals_lm1incum <- residuals(lm1incum)
residuals_lm1incum
# Plot histogram of residuals
hist(residuals_lm1incum, breaks = 20, main = "Histogram of Residuals", xlab = "Residuals")

levels(df1$eval_incumbent)


# Logit -------------------------------------------------------------------------------------------
# Load required library for ordinal logistic regression
library(MASS)

# Fit ordinal logistic regression model for incumbent evaluation
ordinal_logit_incumbent <- polr(eval_incumbent ~ distatorabovezero + distabovezero + 
                                  cityatorabovezero + cityabovezero + distworsecity + 
                                  city_pc + dist_pc + task,
                                data = df1, method = "logistic")

# Print summary of the ordinal logistic regression model for incumbent evaluation
summary(ordinal_logit_incumbent)

# Fit ordinal logistic regression model for likelihood of voting
ordinal_logit_vote <- polr(vote ~ distatorabovezero + distabovezero + 
                             cityatorabovezero + cityabovezero + distworsecity + 
                             city_pc + dist_pc + task,
                           data = df1, method = "logistic")

# Print summary of the ordinal logistic regression model for likelihood of voting
summary(ordinal_logit_vote)

# Fit ordinal logistic regression model for project evaluation
ordinal_logit_project <- polr(eval_project ~ distatorabovezero + distabovezero + 
                                cityatorabovezero + cityabovezero + distworsecity + 
                                city_pc + dist_pc + task,
                              data = df1, method = "logistic")

# Print summary of the ordinal logistic regression model for project evaluation
summary(ordinal_logit_project)





#############################################
# Convert eval_incumbent to ordered factor with custom levels
df1$eval_incumbent <- factor(df1$eval_incumbent, ordered = TRUE,
                             levels = c(-1, -0.5, 0, 0.5, 1))

# Fit ordinal logistic regression model for incumbent evaluation
ordinal_logit_incumbent <- polr(eval_incumbent ~ distatorabovezero + distabovezero + 
                                  cityatorabovezero + cityabovezero + distworsecity + 
                                  city_pc + dist_pc + task,
                                data = df1, method = "logistic")

# Print summary of the ordinal logistic regression model for incumbent evaluation
summary(ordinal_logit_incumbent)
ctable_incumbent <- coef(summary(ordinal_logit_incumbent))
## calculate and store p values
p_incumbent <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable_incumbent <- cbind(ctable_incumbent, "p value" = p_incumbent))

###

# Convert vote to ordered factor with custom levels
df1$vote <- factor(df1$vote, ordered = TRUE,
                             levels = c(-1, -0.5, 0, 0.5, 1))

# Fit ordinal logistic regression model for likelihood of voting
ordinal_logit_vote <- polr(vote ~ distatorabovezero + distabovezero + 
                             cityatorabovezero + cityabovezero + distworsecity + 
                             city_pc + dist_pc + task,
                           data = df1, method = "logistic")

# Print summary of the ordinal logistic regression model for likelihood of voting
summary(ordinal_logit_vote)
ctable_vote <- coef(summary(ordinal_logit_vote))

## calculate and store p values
p_vote <- pnorm(abs(ctable_vote[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable_vote <- cbind(ctable_vote, "p value" = p_vote))

###

# Convert eval_project to ordered factor with custom levels
df1$eval_project <- factor(df1$eval_project, ordered = TRUE,
                             levels = c(-1, -0.5, 0, 0.5, 1))

# Fit ordinal logistic regression model for project evaluation
ordinal_logit_project <- polr(eval_project ~ distatorabovezero + distabovezero + 
                                cityatorabovezero + cityabovezero + distworsecity + 
                                city_pc + dist_pc + task,
                              data = df1, method = "logistic")

# Print summary of the ordinal logistic regression model for project evaluation
summary(ordinal_logit_project)
ctable_project <- coef(summary(ordinal_logit_project))

## calculate and store p values
p_project <- pnorm(abs(ctable_project[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable_project <- cbind(ctable_project, "p value" = p_vote))


### Table

model_list <- list(ordinal_logit_incumbent, ordinal_logit_vote, ordinal_logit_project)

# Create a table using screenreg


make_texreg(name = "table4",
            model = list(ordinal_logit_incumbent, ordinal_logit_vote, ordinal_logit_project),
            custom.model.names = c("\\makecell{Incumbent\\\\Evaluations\\\\(-1 to 1)}", 
                                   "\\makecell{Vote for Incumbent\\\\vs. Challenger\\\\(-1 to 1)}",
                                   "\\makecell{Project\\\\Evaluation\\\\(-1 to 1)}"))
stargazer(model_list)