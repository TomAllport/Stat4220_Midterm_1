newdata <- matrix(rep(0, 1302*57), ncol=57, nrow=1302)
newdata[1:11, 1] <- c(1, 8, 22, 25, 30, 34, 35, 37, 44, 50, 55)
newdata[1:11, 2] <- rep(1,11)
newdata[1:11, 3] <- c(1,1,1,15.6,1,2.14,1,1,1,1,0)

predict.glmnet(final_lasso, newx=newdata, s=lambda_lasso, type="response")