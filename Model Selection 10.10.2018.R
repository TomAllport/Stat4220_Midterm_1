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

# There were NA errors in the rest of the code so I decided
# to remove NA's. This reduces the sample size to 1276.
data2<-na.omit(data1)
nrow(data2)

# Create a null model
null <- lm(Price_euros~1, data=data2)
null

# Create the full model
full <- lm(Price_euros~., data=data2)
full

# Hybrid selection
step(null, scope=list(upper=full), direction="both")

# Final model from hybrid selection
lm(formula = Price_euros ~ Ram + TypeName + Resolution + Company + 
     Memory.Size + Opsys + Gpu.Brand + Weight + Cpu.Cores + Inches + 
     Touchscreen + Cpu.Brand, data = data2)
# All the variables are included in the model. The selection criteria
# was AIC. 

###########################################################
# 4 possible models so far. These models were generated from
# least squares and hybrid selection.
###########################################################

# I named the models "fit1", "fit2", "fit3", "fit4". 

# fit1: NA's are not removed, use all variables
# fit2: NA's are not removed, use all variables EXCEPT Touchscreen
# fit 3: NA's removed, use all variables
fit3 <- lm(Price_euros ~ ., data=data2)
# fit 4: NA's removed, use all variables except Touchscreen
fit4 <- lm(Price_euros ~.-Touchscreen, data=data2)

# Create summaries of the models 
fit1_sum <- summary(fit1)
fit2_sum <- summary(fit2)
fit3_sum <- summary(fit3)
fit4_sum <- summary(fit4)

# Which model is the best?

# For some reason Fit 1 and Fit 3 always had the same numbers.
# Similarly Fit 2 and Fit 4 always had the same numbers. 

# R-squared values 
fit1_sum$r.squared
fit2_sum$r.squared
fit3_sum$r.squared
fit4_sum$r.squared
# fit 1,3: 0.7699
# fit 2,4: 0.7691

# Adjusted R-squared (adjusts for the # of independent variables)
fit1_sum$adj.r.squared
fit2_sum$adj.r.squared
fit3_sum$adj.r.squared
fit4_sum$adj.r.squared
# 1,3: 0.7605
# 2,4: 0.7599

# F-statistic (the larger the better)
fit1_sum$fstatistic
fit2_sum$fstatistic
fit3_sum$fstatistic
fit4_sum$fstatistic
# 1,3: 81.99
# 2,4: 83.35

# Residual standard error (the smaller the better)
fit1_sum$sigma
fit2_sum$sigma
fit3_sum$sigma
fit4_sum$sigma
# 1,3: 341.6
# 2,4: 342.1

# 1,3 have lower standard error and higher R-squared than 2,4.

# I decided to use Fit 3 because all NA's are removed, and 
# Fit 3 has a lower standard error, higher R-squared, and higher
# adjusted R-squared. 

# Look at fit3
summary(fit3)
anova(fit3)

# However in the ANOVA, Touchscreen is still insignificant.
# Maybe we should use fit4 instead?
# Fit 4 is the same thing as fit3, except Touchscreen is removed
# in fit4. 

###########################################################
# Check assumptions for Fit 3

# Make a data frame of the residuals 
outputdata <- data.frame(fitted = fit3$fitted.values,
                         residuals = fit3$residuals)
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
# Check assumptions for Fit 4

# Make the residuals 
outputdata2 <- data.frame(fitted = fit4$fitted.values,
                         residuals = fit4$residuals)
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

# Fit 3 and Fit 4 are really similar in all their characteristics.
# The only difference is that Fit 4 doesn't have a Touchscreen
# variable. Since it's better to have less variables than more,
# we should consider using Fit 4 instead.

# In conclusion, Fit 3 and Fit 4 were the models obtained
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
x1 <- model.matrix(Price_euros~., data2)[,-1]
# Create y value (dependent variable)
y1 <- data2$Price_euros

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
# fit3
    # uses data with NA's removed, uses all variables
    # obtained from ordinary least squares
# fit4
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


