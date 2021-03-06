# Load packages
library(ggplot2)
library(GGally)
library(car)
library(MASS)
library(glmnet)
library(plotmo)

data1 = data
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
fit1 <- lm(log(Price_euros) ~ ., data=data1)
anova(fit1)
# Touchscreen is not significant.

# Least squares model with all variables EXCEPT Touchscreen
fit2 <- lm(log(Price_euros) ~ .-Touchscreen, data=data1)
anova(fit2)
# Everything (except Touchscreen) is significant now. 

###########################################################
# Hybrid selection
###########################################################

# Create a null model
null <- lm(log(Price_euros)~1, data=data1)

# Create the full model
full <- lm(log(Price_euros)~., data=data1)

# Hybrid selection
fit3 <- step(null, scope=list(upper=full), direction="both", trace = 0)
anova(fit3)
# All the variables are included in the model. The selection criteria
# was AIC. 

ggplot(data1, aes(x=Weight, y=Inches)) + geom_point()
cor(data1$Weight, data1$Inches)

###########################################################
# 3 possible models so far. These models were generated from
# least squares and hybrid selection.
###########################################################

# I named the models "fit1", "fit2", "fit3".

# fit1: Use all variables
# fit2: Use all variables EXCEPT Touchscreen
# fit3: Stepwise selection (Uses all variables ie same as fit1)

# Create summaries of the models 
fit1_sum <- summary(fit1)
fit2_sum <- summary(fit2)

# Which model is the best?

# R-squared values 
fit1_sum$r.squared
fit2_sum$r.squared
# fit 1: 0.8251415 (was 0.7737066)
# fit 2: 0.824597 (was 0.7728616)

# Adjusted R-squared (adjusts for the # of independent variables)
fit1_sum$adj.r.squared
fit2_sum$adj.r.squared
# fit 1: 0.8177157 (was 0.7640964)
# fit 2: 0.8172944 (was 0.7634051)

# F-statistic (the larger the better)
fit1_sum$fstatistic
fit2_sum$fstatistic
# fit 1: 111.117 (was 80.50875)
# fit 2: 112.9128 (was 81.7279)

# Residual standard error (the smaller the better)
fit1_sum$sigma
fit2_sum$sigma
# fit 1: 0.2664177 (was 339.5808)
# fit 2: 0.2667254 (was 340.078)

# Fit 1 has slightly higher R squared, adjusted R squared, and
# residual standard error. Therefore we should use Fit 1

# Look at fit1
summary(fit1)
anova(fit1)

# However in the ANOVA, Touchscreen is still insignificant.
# Maybe we should use fit2 instead?
# Fit 2 is the same thing as fit 1, except Touchscreen is removed
# in fit 2. 

# This returns an error?
vif(fit1, singular.ok=T)

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


# Create matrix of x values
x1 <- model.matrix(log(Price_euros)~., data1)[,-1]
# Create y value (dependent variable)
y1 <- log(data1$Price_euros)

# lasso regression
lasso_laptop <- glmnet(x1, y1, alpha=1, nlambda=150)

# LASSO with k-fold CV
lasso_cv <- cv.glmnet(x1, y1, type.measure="mse", alpha=1)

# plot MSE vs. log(Lambda)
plot(lasso_cv)

# coefficient plot as function of lambda
plot(lasso_cv$glmnet.fit, xvar="lambda", label=TRUE)

# select lambda with smallest prediction error 0.3586016
(lambda_lasso <- lasso_cv$lambda.min)

# CV LASSO model
final_lasso <- glmnet(x1, y1, alpha=1, lambda = lambda_lasso)

# coefficients for final model
coef(final_lasso, s=lambda_lasso)

# pseudo R squared
final_lasso$dev.ratio
# 0.7736444

plotres(final_lasso, w1.xvar="lambda")

summary(final_lasso)

# why doing this again
outputdata <- data.frame(fitted = fit1$fitted.values,
                         residuals = fit1$residuals)
# Residual plot
ggplot(outputdata, aes(x=fitted, y=residuals)) +
  geom_point() + 
  geom_hline(yintercept = 0, color="red", size=2)

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