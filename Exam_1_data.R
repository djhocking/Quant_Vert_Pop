
# counts of eastern meadowlark

# sample size
N <- 50

# size of the grassland
grass <- runif(N, 1, 20)

# feral cat density
cats <- runif(N, 0, 10)

# dummy variables
observer <- sample(c("obs1", "obs2", "obs3"), size = N, replace = TRUE)
obs2 <- ifelse(observer == "obs2", 1, 0)
obs3 <- ifelse(observer == "obs3", 1, 0)

fire <- sample(1:5, N, replace = TRUE)

pesticides <- runif(N, 0, 5)

# Coefficients
b0 <- log(10)
b1 <- 0.15
b2 <- -0.35
b3 <- 0.05
b4 <- -1.5
b5 <- -0.1
b6 <- -0.15

lam <- exp(b0 + b1 * grass + b2 * cats + b3 * obs2 + b4 * obs3 + b5 * fire + b6 * pesticides)

count <- rpois(N, lam)

psi <- exp(b0 + b1 * grass + b2 * cats + b3 * obs2 + b4 * obs3) / (1 + exp(b0 + b1 * grass + b2 * cats + b3 * obs2 + b4 * obs3))

presence <- rbinom(N, 1, psi)

# presence <- ifelse(count > 0, 1, 0)

df_larks <- data.frame(count, presence, grass, cats, observer, fire, pesticides, stringsAsFactors = FALSE)

write.csv(df_larks, file = "Data/meadowlarks.csv", row.names = FALSE)

lm1 <- lm(count ~ grass + cats + observer, data = df_larks)
summary(lm1)
# plot(lm1)

glm1 <- glm(count ~ grass + cats + observer, data = df_larks, family = poisson)
summary(glm1)
# plot(glm1)

# prediction

glm2 <- glm(presence ~ grass + cats + observer, data = df_larks, family = binomial)
summary(glm2)

# prediction

# someone tells you that meadowlarks are hit hard by the prescribed burns that are done to maintain the grasslands. The frequency of burns ranges from every 1 to 5 years. But then your boss says that no, she read that pesticides from the surrounding farms are actually most important for regulating meadowlark populations

# AIC models compare these different hypotheses for the count data
