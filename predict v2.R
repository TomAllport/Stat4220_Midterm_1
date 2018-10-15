# x1: the original set of x values that the model was built on
head(x1)

# x2: make a new matrix
x2 <- x1

# x2: make the 1st row the laptop we care about
x2[1:58] <- c(rep(0, 6), 1, rep(0, 11), 
              0, 0, 1, 0, 0, 
              15.6,
              rep(0, 4), 1, rep(0, 3),
              2.14,
              2.14, 
              TRUE, 
              0, 0,
              1,
              0,
              rep(0,8), 1, rep(0, 3),
              0, 1,
              rep(0,5))

# make all the other values zeros
matrix2 <- matrix(rep(0, 1301*58), nrow=1301)
x2[2:1302,] <- matrix2

# predict and extract the 1st row (the laptop we care about)
predict(object = final_lasso, newx = x2, s = lambda_lasso)[1,]
