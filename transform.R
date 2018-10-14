log_price <- log(data1$Price_euros)
data2 <- cbind(data1, log_price)
names(data2)
x2 <- model.matrix(log_price~., data2)[,-1]
y2 <- data2$log_price
lasso_cv2 <- cv.glmnet(x2, y2, type.measure="mse", alpha=1)
(lambda_lasso2 <- lasso_cv2$lambda.min)
final_lasso2 <- glmnet(x2, y2, alpha=1, lambda = lambda_lasso2)
plotres(final_lasso2, w1.xvar="lambda")

price_inv <- 1/(data1$Price_euros)
data2 <- cbind(data1, price_inv)
names(data2)
x2 <- model.matrix(price_inv~., data2)[,-1]
y2 <- data2$price_inv
lasso_cv2 <- cv.glmnet(x2, y2, type.measure="mse", alpha=1)
(lambda_lasso2 <- lasso_cv2$lambda.min)
final_lasso2 <- glmnet(x2, y2, alpha=1, lambda = lambda_lasso2)
plotres(final_lasso2, w1.xvar="lambda")

price_2 <- (data1$Price_euros)^2
data2 <- cbind(data1, price_2)
names(data2)
x2 <- model.matrix(price_2~., data2)[,-1]
y2 <- data2$price_2
lasso_cv2 <- cv.glmnet(x2, y2, type.measure="mse", alpha=1)
(lambda_lasso2 <- lasso_cv2$lambda.min)
final_lasso2 <- glmnet(x2, y2, alpha=1, lambda = lambda_lasso2)
plotres(final_lasso2, w1.xvar="lambda")


price_sqrt <- (data1$Price_euros)^(1/2)
data2 <- cbind(data1, price_sqrt)
x2 <- model.matrix(price_sqrt~., data2)[,-1]
y2 <- data2$price_sqrt
lasso_cv2 <- cv.glmnet(x2, y2, type.measure="mse", alpha=1)
(lambda_lasso2 <- lasso_cv2$lambda.min)
final_lasso2 <- glmnet(x2, y2, alpha=1, lambda = lambda_lasso2)
plotres(final_lasso2, w1.xvar="lambda")

price_is <- (data1$Price_euros)^(-1/2)
data2 <- cbind(data1, price_is)
names(data2)
x2 <- model.matrix(price_is~., data2)[,-1]
y2 <- data2$price_is
lasso_cv2 <- cv.glmnet(x2, y2, type.measure="mse", alpha=1)
(lambda_lasso2 <- lasso_cv2$lambda.min)
final_lasso2 <- glmnet(x2, y2, alpha=1, lambda = lambda_lasso2)
plotres(final_lasso2, w1.xvar="lambda")
