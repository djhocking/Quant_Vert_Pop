
library(AICcmodavg)

data(iris)
str(iris)

# get rid of all NA in the dataset
iris_complete <- iris[which(complete.cases(iris)), ]

lm1 <- lm(Petal.Length ~ Petal.Width, data = iris)

lm2 <- lm(Petal.Length ~ Sepal.Width, data = iris)

lm3 <- lm(Petal.Length ~ Petal.Width + Sepal.Width, data = iris)

# AICc from AICcmodavg package
AICc(lm1)

# Make AIC table
aictab(list(lm1, lm2, lm3), c("m1", "m2", "m3"))





       