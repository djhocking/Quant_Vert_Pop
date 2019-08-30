

library(AICcmodavg)

vignette("AICcmodavg")
help("AICcmodavg")

##anuran larvae example from Mazerolle (2006) - Poisson GLM with offset
data(min.trap)
?min.trap

str(min.trap)

##assign "UPLAND" as the reference level as in Mazerolle (2006)          
min.trap$Type <- relevel(min.trap$Type, ref = "UPLAND") 

##set up candidate models          
Cand.mod <- list()

##global model          
Cand.mod[[1]] <- glm(Num_anura ~ Type + log.Perimeter + Num_ranatra,
                     family = poisson, offset = log(Effort),
                     data = min.trap) 
Cand.mod[[2]] <- glm(Num_anura ~ Type + log.Perimeter, family = poisson,
                     offset = log(Effort), data = min.trap) 
Cand.mod[[3]] <- glm(Num_anura ~ Type + Num_ranatra, family = poisson,
                     offset = log(Effort), data = min.trap) 
Cand.mod[[4]] <- glm(Num_anura ~ Type, family = poisson,
                     offset = log(Effort), data = min.trap) 
Cand.mod[[5]] <- glm(Num_anura ~ log.Perimeter + Num_ranatra,
                     family = poisson, offset = log(Effort),
                     data = min.trap) 
Cand.mod[[6]] <- glm(Num_anura ~ log.Perimeter, family = poisson,
                     offset = log(Effort), data = min.trap) 
Cand.mod[[7]] <- glm(Num_anura ~ Num_ranatra, family = poisson,
                     offset = log(Effort), data = min.trap) 
Cand.mod[[8]] <- glm(Num_anura ~ 1, family = poisson,
                     offset = log(Effort), data = min.trap) 

##check c-hat for global model
c_hat(Cand.mod[[1]], method = "pearson") #uses Pearson's chi-square/df
##note the very low overdispersion: in this case, the analysis could be
##conducted without correcting for c-hat as its value is reasonably close
##to 1  

##assign names to each model
Modnames <- c("type + logperim + invertpred", "type + logperim",
              "type + invertpred", "type", "logperim + invertpred",
              "logperim", "invertpred", "intercept only") 

##model selection table based on AICc
aictab(cand.set = Cand.mod, modnames = Modnames)

# get model results
summary(Cand.Mod[[1]])
