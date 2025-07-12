load("C:\\Users\\15612\\Downloads\\sleep75.RData")

ls()

head(data)
str(data$male) # male is already numeric
names(data)

summary(data)

data$male <- as.numeric (data$male) - 1

mod1 <- lm( sleep ~ male + totwrk + educ + age, data = data)
summary(mod1)

n = 3730.45984 + 87.99325
n 



































