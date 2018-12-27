
# 6.6 Study design, and bias and precision of the N-mixture estimator
# -------------------------------------------------------------------

library(AHMbook)
library(unmarked)

# Define simulation settings and arrays for sim results
simreps <- 1000                    # Simulate and analyse 1000 data sets
nsites <- c(20, 100, 200)          # Levels for nsites factor
nreps <- c(2, 4, 6)               # 2, 5, 10, Levels of nrep factor
estimates <- array(NA, dim = c(2, simreps, length(nsites), length(nreps)))
lambda_mean <- 5                  # Mean abundance across sites

# Fill p with random numbers between 0.01 and 0.99
p <- array(runif(n=simreps*length(nsites)*length(nreps), 0.01, 0.99), dim = c(simreps, length(nsites), length(nreps)))

# Launch simulation (takes about 6.3 hours)
for(s in 1:length(nsites)){                     # Loop over levels of nsites factor
  for(r in 1:length(nreps)){                   # Loop over levels of nreps factor
    for(i in 1:simreps){           # Simulate and analyse 1000 data sets
      cat("*** Simrep number", i, "***\n")
      data <- simNmix(nsite=nsites[s], nvisit=nreps[r], mean.lam = lambda_mean,
                      mean.p=p[i,s,r], show.plot = F)        # Generate data set
      umf <- unmarkedFramePCount(y = data$C) # Bundle data for unmarked
      fm <- pcount(~1 ~1, umf)               # Fit model
      estimates[,i,s,r] <- coef(fm)          # Save estimates
    }
  }
}

# Visualisation
par(mfrow = c(length(nsites), length(nreps)), mar = c(4.5,4.5,2,2), cex.lab = 1.5, cex.axis = 1.3)
for(s in 1:length(nsites)){                     # Loop over nsites
  for(r in 1:length(nreps)){                   # Loop over nreps
    plot(p[,s,r], exp(estimates[1,,s,r]), xlab = "Detection probability",
         ylab = "lambda_hat", main = "", ylim = c(0, 75), frame = F)
    text(0.7, 60, paste("M = ", nsites[s], ", J = ", nreps[r], sep = ""),
         cex = 1.5)
    abline(h = 5, col = "red", lwd = 2)
    lines(smooth.spline(exp(estimates[1,,s,r])~p[,s,r]), col="blue", lwd=2)
  }
}

# 6.7 Study of some assumption violations using function simNmix
# --------------------------------------------------------------


simreps <- 1000                   # Number of data sets created/analysed
MLE <- array(dim = c(5, simreps)) # Array to hold MLEs

for(i in 1:simreps){              # Create and analyse 1000 data sets
  cat("*** Simrep number", i, "***\n")
  # Create data set with some extra (here: open populations)
  data <- simNmix(mean.lam=exp(1), mean.p=0.5, beta2.lam=1,
                  beta3.p=1, beta.p.survey=1, open.N=TRUE, show.plot=F)
  # Analyse data set with standard model (here: assuming closure)
  umf <- unmarkedFramePCount(y=data$C, siteCovs =
                               data.frame(cov2=data$site.cov[,2], cov3=data$site.cov[,3]),
                             obsCovs = list(survey.cov = data$survey.cov))
  fm <- pcount(~cov3+survey.cov ~cov2, umf, se = F)
  # Save MLEs
  MLE[,i] <- coef(fm)
}
```
