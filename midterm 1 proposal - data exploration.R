setwd("/Users/emilylin/Downloads")
laptop <- read.csv("laptops.csv", header=T)
# Some exploratory stuff
names(laptop)
class(laptop$Company)
class(laptop$Product)
class(laptop$TypeName)
class(laptop$Inches)
class(laptop$ScreenResolution)
class(laptop$Cpu)
class(laptop$Ram)
class(laptop$Memory)
class(laptop$Gpu)
class(laptop$OpSys)
class(laptop$Weight)
class(laptop$Price_euros)

# Coerce weight to numeric
class(laptop$Weight_num)

levels(laptop$Company)

# Company 
library(ggplot2)
class(laptop$Company)
ggplot(laptop, aes(x=Company)) + geom_bar(fill="white", col="black") +
  theme(axis.text.x = element_text(angle=45, hjust=1))

# Product
class(laptop$Product)

# Type Name
# Inches
ggplot(laptop, aes(x=Inches, y=..density..)) + 
  geom_histogram(fill="white", col="black",bins=15) 
# number of bins in a histogram?
# is this really continuous?
ggplot(laptop, aes(x=Inches)) + geom_bar()

# Screen Resolution

# CPU
# RAM
# Memory
# GPU
# Operating system
# Weight_num
laptop$Weight_num
ggplot(laptop, aes(x=Weight_num, y=..density..)) +
  geom_histogram(fill="white", col="black", bins=30) # number of bins?
# Price_euros
ggplot(laptop, aes(x=Price_euros, y=..density..)) +
  geom_histogram(fill="white", col="black", bins=30)


# center

mean(laptop$Weight_num)
sd(laptop$Weight_num)
median(laptop$Weight_num)

mean(laptop$Inches)
sd(laptop$Inches)
median(laptop$Inches)

mean(laptop$Price_euros)
sd(laptop$Price_euros)
median(laptop$Price_euros)

# relationships

# missing values
names(laptop)
sum(is.na(laptop$Company))
sum(is.na(laptop$Product))
sum(is.na(laptop$TypeName))
sum(is.na(laptop$Inches))
sum(is.na(laptop$ScreenResolution))
sum(is.na(laptop$Cpu))
sum(is.na(laptop$Ram))
sum(is.na(laptop$Memory))
sum(is.na(laptop$Gpu))
sum(is.na(laptop$OpSys))
sum(is.na(laptop$Weight))
sum(is.na(laptop$Weight_num))
sum(is.na(laptop$Price_euros))

names_laptop <- names(laptop)

i <- 0
for (i in names_laptop){
  print(
    sum(is.na(laptop[,names_laptop==i]))
  )
}
laptop[,names_laptop==i]
price_euros <- laptop$Price_euros
sum(laptop[,names_laptop==i] == price_euros)
