library(ggplot2)
library(GGally)
data$Ram1 = as.double(substr(data$Ram, 1, nchar(as.character(data$Ram))-2))   
names(data)
data1 <- data[-6]
names(data1)

# Correlation plot
ggcorr(data1, label=TRUE, name="Color \nScale") +
  ggtitle("Correlation Color Matrix") +
  theme(plot.title=element_text(hjust=0.5))

# Preliminary model
fit1 <- lm(Price_euros ~ ., data=data1)
summary(fit1)
# Everything except operating system and Ram is significant

fit2 <- lm(Price_euros ~ .-Ram1 -Opsys, data=data1)
summary(fit2)

fit3 <- lm(Price_euros ~ .-Ram -Opsys, data=data1)
summary(fit3)

# Hybrid selection

# Create a null model
null <- lm(Price_euros~1, data=data1)
null

# Create the full model
full <- lm(Price_euros~., data=data1)
full

step(null, scope=list(upper=full), direction="both")

lm(formula = Price_euros ~ Ram + TypeName + Memory.Size + Resolution + 
     Company + Opsys + Gpu.Brand + Weight + Cpu.Cores + Inches + 
     Touchscreen + Cpu.Brand, data = data1)
# All the variables


