engine <- read.csv("/home/dell/Desktop/R_probability/table_7_3.csv",sep=",",head=TRUE)
summary(engine)
#qqnorm(engine$co,main="Carbon Monoxide")
#qqline(engine$co)
boxplot(engine$co,main="Carbon Monoxide")
#hist(engine$co,main="Carbon Monoxide")
#qqnorm(engine$co,main="Carbon Monoxide")
#qqline(engine$co)
lengine <- log(engine$co)
boxplot(lengine,main="Carbon Monoxide")
#hist(lengine,main="Carbon Monoxide")
#qqnorm(lengine,main="QQ Plot for the Log of the Carbon Monoxide")
#qqline(lengine)
m <- mean(lengine)
s <- sd(lengine)
n <- length(lengine)
se <- s/sqrt(n)
error <- se*qt(0.975,df=n-1)
left <- m-error
right <- m + error
lNull <- log(5.4) - error
rNull <- log(5.4) + error