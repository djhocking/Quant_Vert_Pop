# install.packages("ggplot2")
library(ggplot2)

voles <- read.csv("C:/Users/djhocking/Documents/Quant/Data/voles_imaginary.csv",
                  stringsAsFactors = FALSE)

str(voles)

ggplot(voles, aes(grass, survival)) + geom_point() + geom_smooth(method = "lm")

lm_voles <- lm(formula = survival ~ grass, data = voles)
summary(lm_voles)
