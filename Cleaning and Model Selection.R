# Load packages
library(ggplot2)
library(GGally)
library(car)
library(MASS)
library(glmnet)
library(plotmo)

#### Data Cleaning ####
setwd("~/set your working directory where laptops.csv is stored")
data = data.frame(read.csv("laptops.csv"))

#change to factor etc
summary(data)

### Split Screen Size to ratio and touchscreen ###
for(i in 1:length(data$ScreenResolution))
{
  data$Touchscreen[i] = grepl("Touchscreen", toString(data$ScreenResolution[i]))
  
  if(grepl("1920x", toString(data$ScreenResolution[i])))
  {
    data$Resolution[i] = "1920"
  }
  else if(grepl("1366x", toString(data$ScreenResolution[i])))
  {
    data$Resolution[i] = "1366"
  }
  else if(grepl("1440x", toString(data$ScreenResolution[i])))
  {
    data$Resolution[i] = "1440"
  }
  else if(grepl("1600x", toString(data$ScreenResolution[i])))
  {
    data$Resolution[i] = "1600"
  }
  else if(grepl("2560x", toString(data$ScreenResolution[i])))
  {
    data$Resolution[i] = "2560"
  }
  else if(grepl("3840x", toString(data$ScreenResolution[i])))
  {
    data$Resolution[i] = "3840"
  }
  else if(grepl("3200x", toString(data$ScreenResolution[i])))
  {
    data$Resolution[i] = "3200"
  }
  else if(grepl("2304x", toString(data$ScreenResolution[i])))
  {
    data$Resolution[i] = "2304"
  }
  else if(grepl("2736x", toString(data$ScreenResolution[i])))
  {
    data$Resolution[i] = "2736"
  }
  else if(grepl("2880x", toString(data$ScreenResolution[i])))
  {
    data$Resolution[i] = "2880"
  }
  else if(grepl("2256x", toString(data$ScreenResolution[i])))
  {
    data$Resolution[i] = "2256"
  }
  else if(grepl("2400x", toString(data$ScreenResolution[i])))
  {
    data$Resolution[i] = "2400"
  }
  else if(grepl("2160x", toString(data$ScreenResolution[i])))
  {
    data$Resolution[i] = "2160"
  }
  else
  {
    data$Resolution[i] = NaN
  }
}

for(i in 1:length(data$Resolution))
{
  if(as.double(data$Resolution[i]) > 2000)
  {
    data$Resolution[i] = "UHD"
  }
  else if(as.double(data$Resolution[i]) < 1280)
  {
    #None of the Screens are < 1280x720
    data$Resolution[i] = "SD"
  }
  else
  {
    data$Resolution[i] = "HD"
  }
}

### Set the levels of resolution ###
data$Resolution = factor(data$Resolution, c("UHD", "HD", "SD"))

### Split up Cpu into brand and number of cores ###
data$Cpu.Cores <- 0
for(i in 1:length(data$Cpu))
{
  if(grepl("Intel", toString(data$Cpu[i])))
  {
    data$Cpu.Brand[i] = "Intel"
    
    if(grepl("Atom", toString(data$Cpu[i])) || grepl("Quad", toString(data$Cpu[i])) || 
       grepl("i5", toString(data$Cpu[i])) || grepl("Xeon E3", toString(data$Cpu[i])))
    {
      data$Cpu.Cores[i] = "4"
    }
    else if(grepl("Dual", toString(data$Cpu[i])) || grepl("i3", toString(data$Cpu[i])) || 
            grepl("Core M", toString(data$Cpu[i])))
    {
      data$Cpu.Cores[i] = "2"
    }
    else if (grepl("i7", toString(data$Cpu[i])))
    {
      if (grepl("i7-8", toString(data$Cpu[i])) || grepl("i7-78", toString(data$Cpu[i])) ||
          grepl("i7-77", toString(data$Cpu[i])) || grepl("i7 8", toString(data$Cpu[i])) || grepl("i7 78", toString(data$Cpu[i])) ||
          grepl("i7 77", toString(data$Cpu[i])))
      {
        data$Cpu.Cores[i] = "4"
      }
      else
      {
        data$Cpu.Cores[i] = "2"
      }
    }
    else
    {
      data$Cpu.Cores[i] = NaN
    }
  }
  else if(grepl("AMD", toString(data$Cpu[i])))
  {
    data$Cpu.Brand[i] = "AMD"
    if(grepl("A10", toString(data$Cpu[i])) || grepl("A12", toString(data$Cpu[i])) ||
       grepl("A8", toString(data$Cpu[i])) || grepl("A9", toString(data$Cpu[i])) )
    {
      data$Cpu.Cores[i] = "4"
    }
    else
    {
      data$Cpu.Cores[i] = "2"
    }
  }
  else
  {
    #Should we remove the laptop with samsung processor as the only one?
    data$Cpu.Brand[i] = NaN
    data$Cpu.Cores[i] = "00"
  }
}

# Reorder Ram to be in ascending order
levels(data$Ram) = c("2GB", "4GB", "6GB", "8GB", "12GB", 
                     "16GB", "24GB", "32GB", "64GB")

### Split Memory into Size (Factor) and Type (Binary) ###
data$Memory.Size = 0
for(i in 1:length(data$Memory))
{
  if(grepl("TB", toString(data$Memory[i])))
  {
    if(grepl(" 1TB", toString(data$Memory[i])) || grepl(" 2TB", toString(data$Memory[i])))
    {
      data$Memory.Size[i] = "Hybrid"
    }
    else if(grepl("2TB", toString(data$Memory[i])))
    {
      data$Memory.Size[i] = "2TB"
    }
    else if(grepl("1TB", toString(data$Memory[i])) || grepl("1.0TB", toString(data$Memory[i])))
    {
      data$Memory.Size[i] = "1TB"
    }
    else
    {
      data$Memory.Size[i] = NaN
    }
  }
  else if(grepl("GB", toString(data$Memory[i])))
  {
    if(grepl("8GB", toString(data$Memory[i])))
    {
      data$Memory.Size[i] = "8GB"
    }
    else if(grepl("16GB", toString(data$Memory[i])))
    {
      data$Memory.Size[i] = "16GB"
    }
    else if(grepl("32GB", toString(data$Memory[i])))
    {
      data$Memory.Size[i] = "32GB"
    }
    else if(grepl("64GB", toString(data$Memory[i])))
    {
      data$Memory.Size[i] = "64GB"
    }
    else if(grepl("128GB", toString(data$Memory[i])))
    {
      data$Memory.Size[i] = "128GB"
    }
    else if(grepl("180GB", toString(data$Memory[i])))
    {
      data$Memory.Size[i] = "180GB"
    }
    else if(grepl("128GB", toString(data$Memory[i])))
    {
      data$Memory.Size[i] = "128GB"
    }
    else if(grepl("240GB", toString(data$Memory[i])))
    {
      data$Memory.Size[i] = "240GB"
    }
    else if(grepl("500GB", toString(data$Memory[i])))
    {
      data$Memory.Size[i] = "500GB"
    }
    else if(grepl("512GB", toString(data$Memory[i])))
    {
      data$Memory.Size[i] = "512GB"
    }
    else if(grepl("GB +", toString(data$Memory[i])))
    {
      data$Memory.Size[i] = "Hybrid"
    }
  }
  else
  {
    #To see if there are any missing laptops
    data$Memory.Size[i] = NaN
  }
}

### Reorder Memory.Size to ascending order ###
data$Memory.Size = factor(data$Memory.Size, c("8GB", "16GB", "32GB", "64GB", "128GB", "180GB", "240GB",
                                              "256GB", "500GB", "512GB", "1TB", "2TB", "Hybrid"))

### Split GPU into brand ###
for(i in 1:length(data$Gpu))
{
  if(grepl("AMD", data$Gpu[i]))
  {
    data$Gpu.Brand[i] = "AMD"
  }
  else if(grepl("Intel", data$Gpu[i]))
  {
    data$Gpu.Brand[i] = "Intel"
  }
  else if(grepl("Nvidia", data$Gpu[i]))
  {
    data$Gpu.Brand[i] = "Nvidia"
  }
  else
  {
    data$Gpu.Brand[i] = "ARM"
  }
}

### Change and Reorder OpSys ###
for(i in 1:length(data$OpSys))
{
  if(grepl("Windows", data$OpSys[i]))
  {
    data$Opsys[i] = "Windows"
  }
  else if(grepl("Chrome OS", data$OpSys[i]))
  {
    data$Opsys[i] = "Chrome"
  }
  else if(grepl("Mac", data$OpSys[i]) || grepl("OS X", data$OpSys[i]) || grepl("mac", data$OpSys[i]))
  {
    data$Opsys[i] = "Mac"
  }
  else
  {
    data$Opsys[i] = levels(data$OpSys)[data$OpSys[i]]
  }
}

### Remove kg from weight and change to continuous variable ###
data$Weight = as.double(substr(data$Weight, 1, nchar(as.character(data$Weight))-2))

### Remove old variables ###
data = subset(data, select = -c(X, Product, ScreenResolution, Cpu, Memory, OpSys, Gpu))

#Remove the NAN values and ones with samsung/ARM?
data = data[-1192,]

#Convert Variables to factors
data$Cpu.Cores = factor(data$Cpu.Cores, c("2", "4"))
data$Cpu.Brand = factor(data$Cpu.Brand, c("Intel", "AMD"))
data$Gpu.Brand = factor(data$Gpu.Brand, c("Intel", "AMD", "Nvidia"))
data$Opsys = factor(data$Opsys, c("Windows", "Linux", "Chrome", "Mac", "Android", "No OS"))

#### Modelling Section ####

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

###########################################################
# 3 possible models so far. These models were generated from
# least squares and hybrid selection.
###########################################################

# I named the models "fit1", "fit2", "fit3".

# fit1: Use all variables
# fit2: Use all variables EXCEPT Touchscreen
# fit3: Stepwise selection (Selects all variables ie same as fit1)

###########################################################

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

# Histogram of residuals
ggplot(outputdata, aes(x=residuals, y=..density..)) +
  geom_histogram(color="black", fill=NA) + 
  geom_density(color="red", size=2)
# Centered around 0, normal shape. 

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

# Lasso regression
lasso_laptop <- glmnet(x1, y1, alpha=1, nlambda=150)

# LASSO with k-fold CV
lasso_cv <- cv.glmnet(x1, y1, type.measure="mse", alpha=1)

# Plot MSE vs. log(Lambda)
plot(lasso_cv)

# Coefficient plot as function of lambda
plot(lasso_cv$glmnet.fit, xvar="lambda", label=TRUE)

# Select lambda with smallest prediction error 0.3586016
lambda_lasso <- lasso_cv$lambda.min

# CV LASSO model
final_lasso <- glmnet(x1, y1, alpha=1, lambda = lambda_lasso)

# Coefficients for final model
coef(final_lasso, s=lambda_lasso)

# Pseudo R squared
final_lasso$dev.ratio
# 0.8251043

# Check Residual plot
plotres(final_lasso, w1.xvar="lambda")

summary(final_lasso)

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

# Prediction of new laptop price using Lasso Model
newdata = data[0,]

newdata[1,] = 0
newdata$Company[1] = "HP"
newdata$TypeName = "Notebook"
newdata$Inches = 15.6
newdata$Ram = "16GB"
newdata$Weight = 2.14
newdata$Touchscreen = T
newdata$Resolution = "UHD"
newdata$Cpu.Cores = "4"
newdata$Cpu.Brand = "Intel"
newdata$Memory.Size = "512GB"
newdata$Gpu.Brand = "Nvidia"
newdata$Opsys = "Windows"
newdata$Price_euros = 0

price = predict.glmnet(final_lasso, newx = newdata, s=lambda_lasso, type = "response")
