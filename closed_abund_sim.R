# code from Michael J. Conroy

require(RMark)

#parametric bootstrap 
#function to create capture histories from simulated data
pasty<-function(x) 
{
k<-ncol(x)
n<-nrow(x)
out<-array(dim=n)
for (i in 1:n)
{
out[i]<-paste(x[i,],collapse="")
}
return(out)
}

#simulate capture histories from assumed model and estimated parameter values

#example is for homogeneous p; but you could read this in as a (random)vector!

sim.data<-function(N=100,p,k=5)
{

#simulate capture histories
y<-array(dim=c(N,k))
ind<-array(dim=N)

  for(i in 1:N)
    {
     y[i,1:k]<-rbinom(k,1,p)
     ind[i]<-sum(y[i,])>0
    }

#capture history data frame 
capt.hist<-data.frame(ch=pasty(y[,1:k]),ind=ind)
capt.hist<-subset(capt.hist,ind==T,select=c(ch))
#end of function
return(capt.hist)
}
#simulate data from homogeneous p

capt.hist<-sim.data(N=100,p=0.3,k=5)

#run a simple model

pdotshared=list(formula=~1,share=TRUE)
m0<-mark(capt.hist,model="Closed",model.parameters=list(p=pdotshared),output=FALSE)
summary(m0)

#simulate data under heterogeneous p
p<-rbeta(100,.3,.7)
hist(p)
capt.hist<-sim.data(N=100,p=p,k=5)
#run the null model again
m0<-mark(capt.hist,model="Closed",model.parameters=list(p=pdotshared),output=FALSE)
summary(m0)

#same mean but lower variance
p<-rbeta(100,3,7)
hist(p)
capt.hist<-sim.data(N=100,p=p,k=5)
#run the null model again
m0<-mark(capt.hist,model="Closed",model.parameters=list(p=pdotshared),output=FALSE)
summary(m0)

#still lower
p<-rbeta(100,30,70)
hist(p)
capt.hist<-sim.data(N=100,p=p,k=5)
#run the null model again
m0<-mark(capt.hist,model="Closed",model.parameters=list(p=pdotshared),output=FALSE)
summary(m0)







