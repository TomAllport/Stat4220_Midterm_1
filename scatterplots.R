library(ggplot2)
names(data)
class(data$Inches)
class(data$Weight)

ggplot(data, aes(x=Inches, y=Price_euros)) + geom_point()

ggplot(data, aes(x=Weight, y=Price_euros)) + geom_point()

