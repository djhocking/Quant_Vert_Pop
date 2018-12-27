
# Make our deterministic model

b0 <- 0.5
b1 <- 1
b2 <- 0.7

moist <- seq(0, 1, length.out = 50)
depth <- seq(0, 10, length.out = 50)

mass <- rep(NA, times = 50)
mass[1] <- 0.5 + 1 * moist[1] + 0.7 * depth[1]

mass <- b0 + b1 * moist + b2 * depth

hist(mass)

plot(x = moist, y = mass)

# Make out statistical model (simulate real data)

eps <- rnorm(50, 0, 0.5)

mass <- b0 + b1 * moist + b2 * depth + eps

plot(moist, mass)

## make more noise

eps2 <- rnorm(50, 0, 2)

mass2 <- b0 + b1 * moist + b2 * depth + eps2

plot(moist, mass)
points(moist, mass2, col="red")

## Make a linear model

lm1 <- lm(mass ~ 1 + moist + depth)

summary(lm1)

## Read some data

sal <- read.table("Data/Salamander_Demographics.csv", 
                  sep = "",
                  header = TRUE,
                  stringsAsFactors = FALSE)

# look at the structure
str(sal)

# look at a summary of the data
summary(sal)

# Linear Model

lm1 <- lm(sal$mass ~ 1 + sal$svl)
summary(lm1)

