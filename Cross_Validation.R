#install.packages("Metrics")
#install.packages("vtreat")
library(Metrics)
library(vtreat)
# Keep whilst testing to remove randomness
set.seed(123)

################## "fit1" ###########################
#Mak3 divisible by 10
data = data[-c(1301,1302),]
# Get the number of rows
nRows <- nrow(data)

# Implement the 3-fold cross-fold plan with vtreat
splitPlan <- kWayCrossValidation(nRows, 3, NULL, NULL)

# Examine the split plan
str(splitPlan)

# Initialize a column of the appropriate length
data.p$pred.cv <- 0 

# k is the number of folds
# splitPlan is the cross validation plan
k <- 2
for(i in 1:k) {
  # Get the ith split
  split <- splitPlan[[i]]
  
  # Build a model on the training data 
  # from this split 
  # (lm, in this case)
  model <- lm(log(Price_euros) ~., data[split$train,])
  
  # make predictions on the 
  # application data from this split
  data$pred.cv[split$app] <- predict(model, 
                                       newdata = data[split$app,])
}

# Predict from a full model
data$pred <- predict(lm(log(Price_euros) ~., data = data))

# Get the rmse of the full model's predictions
rmse(data$pred, log(data$Price_euros))

# Get the rmse of the cross-validation predictions
rmse(data$pred.cv, log(data$Price_euros))