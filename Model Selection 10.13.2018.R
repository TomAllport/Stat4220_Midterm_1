# This is pretty much the same thing as Model Selection 
# 10.10.2018, just with a few extra comments after my meeting
# with Maria. The Model Selection 10.10.2018 code should 
# work just fine though! 

###########################################################

# Hey guys, here's what I got for the modeling. I'll try to 
# explain this as best I can with the comments. I'm not 100%
# sure that this is quality so...please bear with me

###########################################################

# Load packages
library(ggplot2)
library(GGally)

# Remove Weight_num since it is the same as Weight
data1 <- data[-6]

###########################################################

# Correlation plot
ggcorr(data1, label=TRUE, name="Color \nScale") +
  ggtitle("Correlation Color Matrix") +
  theme(plot.title=element_text(hjust=0.5))
# The correlation between Weight and Inches is 0.8, suggesting
# possible multicollinearity. 

###########################################################
# Least squares regression
###########################################################

# Least squares model with ALL the explanatory variables
fit1 <- lm(Price_euros ~ ., data=data1)
anova(fit1)
# Touchscreen is not significant.

# Least squares model with all variables EXCEPT Touchscreen
fit2 <- lm(Price_euros ~ .-Touchscreen, data=data1)
anova(fit2)
# Everything (except Touchscreen) is significant now. 

###########################################################
# Hybrid selection
###########################################################

# Create a null model
null <- lm(Price_euros~1, data=data1)
null

# Create the full model
full <- lm(Price_euros~., data=data1)
full

# Hybrid selection
step(null, scope=list(upper=full), direction="both")

# Final model from hybrid selection
lm(formula = Price_euros ~ Ram + TypeName + Memory.Size + Resolution + 
     Company + Opsys + Gpu.Brand + Weight + Cpu.Cores + Inches + 
     Touchscreen + Cpu.Brand, data = data1)
# All the variables are included in the model. The selection criteria
# was AIC. 

###########################################################
# 4 possible models so far. These models were generated from
# least squares and hybrid selection.
###########################################################

# I named the models "fit1", "fit2", "fit3", "fit4". 

# fit1: NA's are not removed, use all variables
# fit2: NA's are not removed, use all variables EXCEPT Touchscreen

# Create summaries of the models 

# t test for touchscreen is better. 
# F test= 2+ dummy variables
fit1_sum <- summary(fit1)
anova(fit1)
fit2_sum <- summary(fit2)

# Which model is the best?

# For some reason Fit 1 and Fit 3 always had the same numbers.
# Similarly Fit 2 and Fit 4 always had the same numbers. 

# R-squared values 
fit1_sum$r.squared
fit2_sum$r.squared
# fit 1: 0.7737066
# fit 2: 0.7728616

# Adjusted R-squared (adjusts for the # of independent variables)
fit1_sum$adj.r.squared
fit2_sum$adj.r.squared
# fit 1: 0.7640964
# fit 2: 0.7634051

# F-statistic (the larger the better)
fit1_sum$fstatistic
fit2_sum$fstatistic
# fit 1: 80.50875
# fit 2: 81.7279

anova(fit2, fit1)
# the added variable is useful despite not being significant

# Residual standard error (the smaller the better)
fit1_sum$sigma
fit2_sum$sigma
# fit 1: 339.5808
# fit 2: 340.078

# Fit 1 has slightly higher R squared, adjusted R squared, and
# residual standard error. Therefore we should use Fit 1

# Look at fit1
summary(fit1)
anova(fit1)

# However in the ANOVA, Touchscreen is still insignificant.
# Maybe we should use fit2 instead?
# Fit 2 is the same thing as fit 1, except Touchscreen is removed
# in fit 2. 

###########################################################
# Check assumptions for Fit 1

# Make a data frame of the residuals 
outputdata <- data.frame(fitted = fit1$fitted.values,
                         residuals = fit1$residuals)
# Residual plot
ggplot(outputdata, aes(x=fitted, y=residuals)) +
  geom_point() + 
  geom_hline(yintercept = 0, color="red", size=2)
# POSSIBLE ISSUE: non-constant variance. Transformation
# of y (dependent variable) may be required. 

# histogram of residuals
ggplot(outputdata, aes(x=residuals, y=..density..)) +
  geom_histogram(color="black", fill=NA) + 
  geom_density(color="red", size=2)
# centered around 0, relatively normal shape. 
# These assumptions look good!

###########################################################
# Check assumptions for Fit 2

# Make the residuals 
outputdata2 <- data.frame(fitted = fit2$fitted.values,
                         residuals = fit2$residuals)
# Residual plot
ggplot(outputdata2, aes(x=fitted, y=residuals)) +
  geom_point() + 
  geom_hline(yintercept = 0, color="red", size=2)
# possible issues: non-constant variance. Again, a transformation
# may be needed

# histogram of residuals
ggplot(outputdata2, aes(x=residuals, y=..density..)) +
  geom_histogram(color="black", fill=NA) + 
  geom_density(color="red", size=2)
# centered around 0, relatively normal shape.
# These assumptions look good!

# Fit 1 and Fit 2 are really similar in all their characteristics.
# The only difference is that Fit 2 doesn't have a Touchscreen
# variable. Since it's better to have less variables than more,
# we should consider using Fit 2 instead.

# In conclusion, Fit 1 and Fit 2 were the models obtained
# from ordinary least squares and hybrid selection. 

############################################################
# However, we still have the issue of multicollinearity.
# There is a strong relationship between Weight and Inches.
# We should try nonlinear regression (LASSO). 
# LASSO can shrink coefficients to zero, whereas Ridge cannot.
# Therefore LASSO is better for model selection. 
###########################################################

# Load packages
library(MASS)
library(glmnet)

# Create matrix of x values
x1 <- model.matrix(Price_euros~., data1)[,-1]
# Create y value (dependent variable)
y1 <- data1$Price_euros

# lasso regression
lasso_laptop <- glmnet(x1, y1, alpha=1, nlambda=150)

# LASSO with k-fold CV
lasso_cv <- cv.glmnet(x1, y1, type.measure="mse", alpha=1)

# plot MSE vs. log(Lambda)
plot(lasso_cv)

# coefficient plot as function of lambda
plot(lasso_cv$glmnet.fit, xvar="lambda", label=TRUE)

# select lambda with smallest prediction error
(lambda_lasso <- lasso_cv$lambda.min)

# final LASSO model
final_lasso <- glmnet(x1, y1, alpha=1, lambda = lambda_lasso)

# coefficients for final model
coef(final_lasso)

###########################################################

# Final list of models obtained:
# fit1
    # uses data with NA's removed, uses all variables
    # obtained from ordinary least squares
# fit2
    # uses data with NA's removed, uses all variables EXCEPT Touchscreen
    # obtained from ordinary least squares
# final_lasso
    # obtained from LASSO regression

###########################################################

# To do list:

# Look at all 3 models and decide which one is the best.
  # Criteria: R-squred, adjusted R-squared, prediction error
  # Other criteria if you can think of any
  # Also check the assumptions for LASSO regression 
  # (I forgot what those assumptions were and I'm too tired
  # to look them up sorry)

# Run significance testing on the model we pick.

# If least squares is picked: run cross-validation.
# If LASSO is picked: no need to run CV because I already did.

# Make predictions on final laptop (the one that we choose)
# Confidence interval (gives estimates for the mean price)
# Prediction interval (gives estimates for the individual price)


