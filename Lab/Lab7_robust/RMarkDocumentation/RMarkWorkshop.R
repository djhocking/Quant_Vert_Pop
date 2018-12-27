# This is code used to create the example data. It requires the marked package.
# so it is commented out.
## ----1,echo=FALSE,eval=FALSE---------------------------------------------
## # simulate data with 300 females and 300 males; 100 of each sex released at 3 occasions
## library(marked)
## set.seed(1235)
## data=data.frame(ch=rep(c(rep("1000",100),rep("0100",100),rep("0010",100)),2),sex=c(rep("Female",300),rep("Male",300)),
##   u=floor(100*(rnorm(600,0,1)+5))/100,freq=rep(1,600),stringsAsFactors=FALSE)
## data$sex=factor(data$sex)
## df=simHMM(data, model.parameters=list(Phi=list(formula=~sex+u),p=list(formula=~time)),
##    initial=list(Phi=c(0,1,.5),p=c(0,2,1)))
## df$ch=sapply(strsplit(df$ch,","),paste,collapse="")
## detach("package:marked")
## library(RMark)
## dp=process.data(df,groups="sex")
## mod=mark(dp)
## # creates example.inp
## if(file.exists("example.inp"))file.remove("example.inp")
## export.MARK(dp,"example",mod,replace=TRUE)
## # creates example.txt
## names(df)=c("ch","sex","weight")
## write.table(df,file="example.txt",sep="\t",row.names=FALSE)
## 

## ----2-------------------------------------------------------------------
MARKData=read.table(file="Lab/Lab7_robust/RMarkDocumentation/example.inp",header=FALSE,
  colClasses=c("character",rep("numeric",2),"character"),
  col.names=c("ch","Females","Males","Weight"))
head(MARKData)
RMarkData=read.table(file="example.txt",header=TRUE,
  colClasses=c("character","factor","numeric"))
head(RMarkData)  

## ----3-------------------------------------------------------------------
library(RMark)
# read in the data; header=TRUE means first row is column names; 
# colClasses specify that the fields are of type character (ch), 
# factor (sex) and numeric (weight)
RMarkData=read.table(file="example.txt",header=TRUE,
             colClasses=c("character","factor","numeric"))
head(RMarkData) 
# use import.chdata to read in the data; ch is first field and is character
# field.types f and n are for sex and weight fields
# Here I'll use the name df so typing will be easier later
df=import.chdata("example.txt",field.types=c("f","n"))
head(df)


## ----4-------------------------------------------------------------------
str(df)
summary(df)


## ----5-------------------------------------------------------------------
dp=process.data(df,model="CJS",groups="sex")
str(dp)


## ----6-------------------------------------------------------------------
ddl=make.design.data(dp)
str(ddl)


## ----7-------------------------------------------------------------------
model=mark(dp,ddl)

## ----8,tidy=TRUE---------------------------------------------------------
str(model)

## ----9-------------------------------------------------------------------
coef(model)

## ----10------------------------------------------------------------------
summary(model)

## ----11------------------------------------------------------------------
str(summary(model))
summary(model)$AICc

## ------------------------------------------------------------------------
data=expand.grid(sex=c("Female","Male"),age=c("0","1","2","3"))
set.seed(98215)
data$height=rnorm(8,10,3)
data
str(data)

## ------------------------------------------------------------------------
model.matrix(~sex,data)
model.matrix(~age,data)

## ------------------------------------------------------------------------
model.matrix(~-1+sex,data)
model.matrix(~-1+age,data)

## ------------------------------------------------------------------------
model.matrix(~sex+age,data)

## ------------------------------------------------------------------------
model.matrix(~-1+sex+age,data)

## ------------------------------------------------------------------------
model.matrix(~sex*age,data)

## ------------------------------------------------------------------------
model.matrix(~-1+sex:age,data)

## ----fig.height=4--------------------------------------------------------
X=model.matrix(~sex+age,data)
beta=1:5
Xbeta=X%*%beta
barplot(Xbeta,beside=TRUE,names.arg=paste(data$sex,data$age,sep="-"),cex.names=.75)

## ----fig.height=4--------------------------------------------------------
X=model.matrix(~height,data)
X
beta=c(1,2.5)
Xbeta=X%*%beta
plot(data$height,Xbeta,xlab="Height",type="b",xlim=c(7,17),ylim=c(15,60))

## ----fig.height=4--------------------------------------------------------
X=model.matrix(~sex+height,data)
X
beta=c(1,3,2)
Xbeta=X%*%beta
plot(data$height[data$sex=="Female"],Xbeta[data$sex=="Female"],
          xlab="Height",type="b",pch="F",ylab="Xbeta",xlim=c(7,17),ylim=c(15,60))
lines(data$height[data$sex=="Male"],Xbeta[data$sex=="Male"],xlab="Height",
            type="b",pch="M",lty=2)

## ----fig.height=4--------------------------------------------------------
X=model.matrix(~sex*height,data)
X
beta=c(1,3,2,1)
Xbeta=X%*%beta
plot(data$height[data$sex=="Female"],Xbeta[data$sex=="Female"],
          xlab="Height",type="b",pch="F",ylab="Xbeta",xlim=c(7,17),ylim=c(15,60))
lines(data$height[data$sex=="Male"],Xbeta[data$sex=="Male"],xlab="Height",
            type="b",pch="M",lty=2)

## ----fig.height=4--------------------------------------------------------
X=model.matrix(~sex:height,data)
X
beta=c(1,2,3)
Xbeta=X%*%beta
plot(data$height[data$sex=="Female"],Xbeta[data$sex=="Female"],
          xlab="Height",type="b",pch="F",ylab="Xbeta",xlim=c(7,17),ylim=c(15,60))
lines(data$height[data$sex=="Male"],Xbeta[data$sex=="Male"],xlab="Height",
           type="b",pch="M",lty=2)

## ----12------------------------------------------------------------------
model.matrix(~sex,ddl$p)

## ----13------------------------------------------------------------------
# add the fields intercept and male to p design data
ddl$p$intercept=1
ddl$p$male=ifelse(ddl$p$sex=="Male",1,0)
# create the DM using an explicit dummy variable for males
model.matrix(~male,ddl$p)
# create the DM using an explicit dummy variable for the intercept and males
model.matrix(~-1+intercept+male,ddl$p)

## ----14------------------------------------------------------------------
pdm=model.matrix(~sex,ddl$p)
Phidm=model.matrix(~sex,ddl$Phi)
dm=cbind(Phidm,matrix(0,nrow=12,ncol=2))
dm=rbind(dm,cbind(matrix(0,nrow=12,ncol=2),pdm))
colnames(dm)=c(paste("Phi:",colnames(Phidm),sep=""),paste("p:",colnames(pdm),sep=""))
rownames(dm)=1:nrow(dm)
dm

## ----15------------------------------------------------------------------
Phi.sex=list(formula=~sex)
p.sex=list(formula=~sex)

## ----16------------------------------------------------------------------
sex.model=mark(dp,ddl,model.parameters=list(Phi=Phi.sex,p=p.sex))
# Note in this case we could have done the same thing with: 
# sex=list(formula=~sex)
# sex.model=mark(dp,ddl,model.parameters=list(Phi=sex,p=sex))
# But usually the models will differ and the reason for different
# specifications will become clear later.

## ----17------------------------------------------------------------------
Phi.sex.weight=list(formula=~sex+weight)
p.sex.time=list(formula=~sex+time)

## ----18------------------------------------------------------------------
example.model=mark(dp,ddl,model.parameters=list(Phi=Phi.sex.weight,p=p.sex.time))

## ----19------------------------------------------------------------------
time=1:24
year=floor((time-1)/12)
season=floor((time-year*12-1)/3) 
year=factor(year)
season=factor(season,labels=c("Jan-Mar","Apr-June","July-Sept","Oct-Dec"))
year
season

## ----20------------------------------------------------------------------
floor(ddl$Phi$time/3)

## ----21------------------------------------------------------------------
floor((ddl$Phi$Time+1)/3)
floor(as.numeric(as.character(ddl$Phi$time))/3)

## ----22------------------------------------------------------------------
ddl$Phi$flood=ifelse(ddl$Phi$time%in%c(1,3),1,0)
ddl$Phi[,-(1:2)]

## ----<23-----------------------------------------------------------------
env.data=data.frame(time=1:3,temp=c(49,36,42),Flood=c(1,0,1))
ddl$Phi=merge_design.covariates(ddl$Phi,env.data)
ddl$Phi

## ----24------------------------------------------------------------------
set.seed(9481)
df$ageclass=factor(ifelse(runif(600)<0.5,0,1),labels=c("Hatch-Year","Adult"))
dp=process.data(df,model="CJS",groups=c("sex","ageclass"),age.var=2,initial.age=c(0,1))
ddl=make.design.data(dp)
ddl$Phi[,-c(1,2,6)]

## ----25------------------------------------------------------------------
ddl=make.design.data(dp,parameters=list(Phi=list(age.bins=c(0,1,4))),right=FALSE)
levels(ddl$Phi$age)=c("0","1Plus")
ddl$Phi[,-c(1,2,6)]

## ----26------------------------------------------------------------------
Phi.age=list(formula=~age)
p.time=list(formula=~time)
model=mark(dp,ddl,model.parameters=list(Phi=Phi.age,p=p.time))

## ----27------------------------------------------------------------------
# to avoid taking up space I'm turning off the output but showing the beta estimates
Phi.sex1=list(formula=~-1+sex)
Phi.sex2=list(formula=~0+sex)
mod0=mark(dp,ddl,model.parameters=list(Phi=Phi.sex),output=FALSE)
mod0$results$AICc
summary(mod0)$beta
mod1=mark(dp,ddl,model.parameters=list(Phi=Phi.sex1),output=FALSE)
mod1$results$AICc
summary(mod1)$beta
mod2=mark(dp,ddl,model.parameters=list(Phi=Phi.sex2),output=FALSE)
mod2$results$AICc
summary(mod2)$beta

## ----28------------------------------------------------------------------
Phi.sex.age=list(formula=~sex+age)
mod0=mark(dp,ddl,model.parameters=list(Phi=Phi.sex.age),output=FALSE)
summary(mod0)$beta

## ----echo=FALSE----------------------------------------------------------
options(width=95)

## ----29------------------------------------------------------------------
mod0$design.matrix

## ----echo=FALSE----------------------------------------------------------
options(width=85)

## ----30------------------------------------------------------------------
Phi.age.x.sex=list(formula=~age*sex)
mod0=mark(dp,ddl,model.parameters=list(Phi=Phi.age.x.sex),output=FALSE)

## ----31------------------------------------------------------------------
summary(mod0)$beta

## ----32------------------------------------------------------------------
mod0$design.matrix

## ----33------------------------------------------------------------------
Phi.age.x.sex=list(formula=~-1+age:sex)
mod0=mark(dp,ddl,model.parameters=list(Phi=Phi.age.x.sex),output=FALSE)
summary(mod0)$beta
mod0$results$AICc
# Note that the DM is an identity matrix
mod0$design.matrix

## ----34------------------------------------------------------------------
Phi.age.x.sex=list(formula=~-1+age:sex,link="sin")
mod0=mark(dp,ddl,model.parameters=list(Phi=Phi.age.x.sex),output=FALSE)
summary(mod0)$beta
mod0$results$AICc

## ----35------------------------------------------------------------------
Phi.age.x.sex=list(formula=~age:sex)
mod0=mark(dp,ddl,model.parameters=list(Phi=Phi.age.x.sex),output=FALSE)
summary(mod0)$beta
mod0$design.matrix

## ----36------------------------------------------------------------------
summary(mod0)$beta

## ----37------------------------------------------------------------------
mod0$design.matrix

## ----38------------------------------------------------------------------
dp=process.data(df,model="CJS",groups="sex")
ddl=make.design.data(dp,parameters=list(Phi=list(age.bins=c(0,1,4))),right=FALSE)
levels(ddl$Phi$age)=c("0","1Plus")

## ----39------------------------------------------------------------------
Phi.sex.time.x.age=list(formula=~sex+time*age)
mod0=mark(dp,ddl,model.parameters=list(Phi=Phi.sex.time.x.age),output=FALSE)
summary(mod0)$beta

## ----40------------------------------------------------------------------
Phi.sex.time.x.age=list(formula=~sex+time:age,remove.intercept=TRUE)
mod0=mark(dp,ddl,model.parameters=list(Phi=Phi.sex.time.x.age),output=FALSE)
summary(mod0)$beta

## ----41------------------------------------------------------------------
Phi.sex.time.x.age=list(formula=~time:age,remove.intercept=TRUE,link="logit")
mod0=mark(dp,ddl,model.parameters=list(Phi=Phi.sex.time.x.age),output=FALSE)
summary(mod0)$beta
mod0$results$npar
mod0$results$AICc
Phi.sex.time.x.age=list(formula=~time:age,remove.intercept=TRUE,link="sin")
mod0=mark(dp,ddl,model.parameters=list(Phi=Phi.sex.time.x.age),output=FALSE)
summary(mod0)$beta
mod0$results$npar
mod0$results$AICc

## ----42------------------------------------------------------------------
ddl$Phi$male=ifelse(ddl$Phi$sex=="Female",0,1)
Phi.sex.time.x.age=list(formula=~-1 + male+time:age)
mod0=mark(dp,ddl,model.parameters=list(Phi=Phi.sex.time.x.age),output=FALSE)
summary(mod0)$beta

## ----43------------------------------------------------------------------
Phi.male.x.age=list(formula=~male:age)
mod0=mark(dp,ddl,model.parameters=list(Phi=Phi.male.x.age),output=FALSE)
summary(mod0)$beta

## ----44------------------------------------------------------------------
# notice that I'm now using df rather than dp which was processed
# with groups
df$male=ifelse(df$sex=="Male",1,0)
model=mark(df,model.parameters=list(Phi=list(formula=~male)))

## ----45------------------------------------------------------------------
df$temp1=rep(10.1,600)
df$temp2=rep(13.5,600)
df$temp3=rep(8.5,600)
temp.model=mark(df,model.parameters=list(Phi=list(formula=~temp)))

## ----46------------------------------------------------------------------
phi=coef(temp.model)[1:2,]
plogis(phi$estimate[1]+c(10.1,13.5,8.5)*phi$estimate[2])

## ----47,fig.height=4-----------------------------------------------------
logit.values=phi$estimate[1]+(5:15)*phi$estimate[2]
deriv=matrix(1,ncol=2,nrow=11)
deriv[,2]=5:15
std.errors=sqrt(diag(deriv%*%temp.model$results$beta.vcv[1:2,1:2]%*%t(deriv)))
lcl.logit=logit.values-1.96*std.errors
ucl.logit=logit.values+1.96*std.errors
plot(5:15,plogis(logit.values),type="l",xlab="Temperature",ylab="Survival")
lines(5:15,plogis(lcl.logit),lty=2)
lines(5:15,plogis(ucl.logit),lty=2)

## ----48------------------------------------------------------------------
data(edwards.eberhardt)
dp.ee=process.data(edwards.eberhardt,model="Closed")
ddl.ee=make.design.data(dp.ee)
# first fit the default model with constant but
# different p and c
model=mark(dp.ee,ddl.ee,output=FALSE)
summary(model)$beta
# next fit constant p=c
p.c=list(formula=~1,share=TRUE)
model=mark(dp.ee,ddl.ee,model.parameters=list(p=p.c),output=FALSE)
summary(model)$beta
# next fit constant p(Time) c(Time) where logit(c(Time))=logit(p(Time))+beta_c
ddl.ee$c$Time=ddl.ee$c$Time+1
p.Time.c=list(formula=~Time+c,share=TRUE)
model=mark(dp.ee,ddl.ee,model.parameters=list(p=p.Time.c))

## ----49-----------------------------------------------------------------
p.Time=list(formula=~Time)
c.Time=list(formula=~Time)
model=mark(dp.ee,ddl.ee,model.parameters=list(p=p.Time,c=c.Time))

## ----50,echo=FALSE-------------------------------------------------------
par=read.delim("SharedParameters.txt",colClasses=rep("character",2))


## ----51------------------------------------------------------------------
model=mark(dp,ddl,output=FALSE)
model$design.matrix
# show the PIM translation vector
model$simplify$pim.translation

## ----52------------------------------------------------------------------
PIMS(model,"p")
PIMS(model,"p",simplified=FALSE)


## ----53------------------------------------------------------------------
ddl$Phi$fix=ifelse(ddl$Phi$time==2,1,NA)
model=mark(dp,ddl)

## ----54------------------------------------------------------------------
summary(model,show.fixed=TRUE)

## ----55------------------------------------------------------------------
#using time
ddl$Phi$fix=NULL
p.time=list(formula=~time,fixed=list(time=3,value=1))
model=mark(dp,ddl,model.parameters=list(p=p.time),output=FALSE)
summary(model,show.fixed=TRUE)
# using index; many of the examples are shown using something like
# time3=as.numeric(rownames(ddl$p[ddl$p$time==3,]))
# because those examples were written before I added par.index field 
# to code
time3=ddl$p$par.index[ddl$p$time==3]
p.time=list(formula=~time,fixed=list(index=time3,value=1))
model=mark(dp,ddl,model.parameters=list(p=p.time),output=FALSE)
summary(model,show.fixed=TRUE)

## ----56------------------------------------------------------------------
# read in data
df=import.chdata("example.txt",field.types=c("f","n"))
# create fake age data and make it a factor variable 
set.seed(9481)
df$ageclass=factor(ifelse(runif(600)<0.5,0,1),labels=c("Hatch-Year","Adult"))
# process data for CJS model with sex and age groups
dp=process.data(df,model="CJS",groups=c("sex","ageclass"),age.var=2,initial.age=c(0,1))
# create design data with age bins for Phi
ddl=make.design.data(dp,parameters=list(Phi=list(age.bins=c(0,1,4))),right=FALSE)
levels(ddl$Phi$age)=c("0","1Plus")
# create analysis function
do_analysis=function()
{
  # create formulas for Phi
  Phi.dot=list(formula=~1)
  Phi.sex=list(formula=~sex)
  Phi.sex.age=list(formula=~sex+age)
  Phi.sex.weight=list(formula=~sex+weight)
  #create formulas for p
  p.dot=list(formula=~1)
  p.time=list(formula=~time)
  # create all combinations (8 models)
  cml=create.model.list("CJS")
  # run all all 8 models and return as a list with class marklist
  results=mark.wrapper(cml,data=dp,ddl=ddl,output=FALSE,silent=TRUE)
  return(results)
}
# Now call the function to run the models and return the results
# stored into example.results
example.results=do_analysis()
# Show the model selection table
example.results

## ----57------------------------------------------------------------------
example.results$model.table=model.table(example.results,use.lnl=TRUE)
example.results

## ----58------------------------------------------------------------------
do_analysis=function()
{
  # create formulas for Phi
  Phi.1=list(formula=~1)
  Phi.2=list(formula=~sex)
  Phi.3=list(formula=~sex+age)
  Phi.4=list(formula=~sex+weight)
  #create formulas for p
  p.1=list(formula=~1)
  p.2=list(formula=~time)
  # create all combinations (8 models)
  cml=create.model.list("CJS")
  # run all all 8 models and return as a list with class marklist
  results=mark.wrapper(cml,data=dp,ddl=ddl,output=FALSE,silent=TRUE)
  return(results)
}
# Now call the function to run the models and return the results
# stored into example.results
example.results=do_analysis()
# reset model names and use -2lnL
example.results$model.table=model.table(example.results,use.lnl=TRUE,model.name=FALSE)
# Show the model selection table
example.results

## ----59------------------------------------------------------------------
# summarize the best model
best.model.number=as.numeric(row.names(example.results$model.table)[1])
summary(example.results[[best.model.number]])

## ----60------------------------------------------------------------------
# get a dataframe with model average estimates, standard errors and related data
mavg=model.average(example.results,parameter="p")
# display the first 5 columns and the time column
mavg[,c(1:5,9)]
# display the p estimates with the standard error from top model
summary(example.results[[best.model.number]],se=TRUE)$reals$p[,1:4]
# show structure if vcv=TRUE
str(model.average(example.results,parameter="p",vcv=TRUE))

## ----61------------------------------------------------------------------
mavg=model.average(example.results,indices=25:26,vcv=TRUE)
mavg

## ----62------------------------------------------------------------------
release.gof(dp)

## ----echo=FALSE,results='hide'-------------------------------------------
if(file.exists("myexample.Rinp"))file.remove("myexample.Rinp")
if(file.exists("myexample.inp"))file.remove("myexample.inp")

## ----63------------------------------------------------------------------
export.MARK(dp,project="myexample",model=example.results)

## ----64------------------------------------------------------------------
Phi.weight.predictions=covariate.predictions(example.results[[best.model.number]], 
                       data=data.frame(index=rep(1:2,each=10),weight=rep(1:10,2)))
# see results excluding first 2 columns so it fits
Phi.weight.predictions$estimates[,-(1:2)]

## ----65,fig.height=4-----------------------------------------------------
with(Phi.weight.predictions$estimates[1:10,],{
     plot(weight, estimate,type="l",lwd=2,xlab="Weight(g)", ylab="Survival",ylim=c(0,1))
     lines(weight,lcl,lty=2)
     lines(weight,ucl,lty=2) 
}) 

## ----66------------------------------------------------------------------
create.cohort=function(x) { 
# split the capture histories into a list with each list element 
# being a vector of the occasion values (0/1). 1001 becomes 
# "1","0","0","1" 
split.ch=sapply(x$ch,strsplit,split="") 
# combine these all into a matrix representation for the ch 
# rows are animals and columns are occasions 
chmat=do.call("rbind",split.ch) 
# use the defined function on the rows (apply(chmat,1...) of the 
# matrix. The defined function figures out the column containing 
# the first 1 (its initial release column). 
return(factor(apply(chmat,1,function(x) min(which(x!="0"))))) }

## ----67------------------------------------------------------------------
RMarkData$cohort=create.cohort(RMarkData)
summary(RMarkData)

## ----68------------------------------------------------------------------
mean.wts=with(RMarkData, tapply(weight, list(cohort,sex), mean))
mean.wts

## ----69------------------------------------------------------------------
mean.wts=as.vector(mean.wts) 
mean.wts

## ----70------------------------------------------------------------------
Phi.by.wt=covariate.predictions(example.results, 
           data=data.frame(index=rep(c(1,7),each=3),weight=mean.wts)) 

## ----71------------------------------------------------------------------
names(Phi.by.wt)
Phi.by.wt$estimates

## ----echo=FALSE----------------------------------------------------------
suppressPackageStartupMessages(library(Hmisc)) 

## ----72------------------------------------------------------------------
par(mfrow=c(2,1)) 
library(Hmisc)
# use ?errbar to learn more
with(Phi.by.wt$estimates,errbar(1:3,estimate[1:3],lcl[1:3],ucl[1:3],
                           ylim=c(.8,1),xlab="Cohort",ylab="Female survival"))
with(Phi.by.wt$estimates, errbar(1:3,estimate[4:6],lcl[4:6],ucl[4:6], 
                           ylim=c(.8,1),xlab="Cohort",ylab=" Male survival"))

## ----73------------------------------------------------------------------
top2=as.numeric(row.names(example.results$model.table)[1:2])
Phi.by.wt$reals[top2]

## ----74------------------------------------------------------------------
cov.data=data.frame(temp1=5:15,temp2=5:15,temp3=5:15)
covariate.predictions(temp.model,data=cov.data,indices=1:3)$estimates[,4:10]

## ----75------------------------------------------------------------------
names(example.results$model.table)

## ----76------------------------------------------------------------------
with(example.results$model.table, tapply(AICc, list(Phi,p), unique))

## ----77------------------------------------------------------------------
with(example.results$model.table, tapply(Neg2LnL, list(Phi,p), unique))

## ----78------------------------------------------------------------------
# fit initial constant model
null=mark(dp,ddl,output=FALSE)
summary(null)$beta
# use null as initial for starting values
model=mark(dp,ddl,model.parameters=list(Phi=list(formula=~sex+age+weight)),
           initial=null,output=FALSE)
summary(model)$beta

## ----79------------------------------------------------------------------
model=mark(dp,ddl,model.parameters=list(Phi=list(formula=~time),p=list(formula=~time)))

## ----80------------------------------------------------------------------
summary(ddl$p$time)
ddl$p$timebin=cut(ddl$p$Time,c(-2,-1,0,1,2))
levels(ddl$p$timebin)
model=mark(dp,ddl, model.parameters=list(p=list(formula=~timebin)))

## ----81------------------------------------------------------------------
ddl$p$timebin=relevel(ddl$p$timebin,"(-1,0]")
model=mark(dp,ddl, model.parameters=list(p=list(formula=~timebin)),output=FALSE)
summary(model)$beta

## ----82------------------------------------------------------------------
cleanup(ask=FALSE) 

## ----83------------------------------------------------------------------
# compute real parameters at average weight value using hessian
model=mark(dp,ddl,model.parameters=list(Phi=list(formula=~sex+age)),
              output=FALSE)
summary(model,se=TRUE)$reals$Phi[1:12,c(3,5:6)]
# compute real parameters at average weight value using profile CI
model=mark(dp,ddl,model.parameters=list(Phi=list(formula=~sex+age)),
              output=FALSE,profile.int=TRUE)
summary(model,se=TRUE)$reals$Phi[1:12,c(3,5:6)]

## ----84------------------------------------------------------------------
data(dipper)
model=mark(dipper,model="POPAN",output=FALSE)
str(model$results$derived)
str(model$results$derived.vcv)

