

library(lme4)
library(car)

algae <- read.csv("Lecture/Data/AlgaeProject.csv", header = TRUE, stringsAsFactors = FALSE)

str(algae)

m1 <- lmer(Invasive.Algae.Mass ~ 1 + Native.Algae.Mass + Treatment + (1 | Location), data = algae)

summary(m1)
Anova(m1)


m2 <- lmer(Invasive.Algae.Mass ~ 1 + Native.Algae.Mass + Treatment + (1 + Treatment | Location), data = algae)
summary(m2)


AIC(m1)
AIC(m2)
