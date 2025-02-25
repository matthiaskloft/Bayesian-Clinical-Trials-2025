#-------------------------------------------------------------------------#
#                           Practical II Solution                         #
#-------------------------------------------------------------------------#

#(a) Function to compute the type I error and power constraints
error_power <- function(n,u,p0,p1){
  Error <- pbinom(u-1,size=n,prob=p0,lower.tail=F)
  Power <- pbinom(u-1,size=n,prob=p1,lower.tail=F)
  return(data.frame(Error,Power))
}

pbinom(16-1,size=40,prob=0.5,lower.tail=F)
sum(dbinom(16:40,40,0.5))

error_power(40,16,0.3,0.5)




#(b)(i) SS calculation
alpha <- 0.05 
power <- 0.8
maxN <- 50 #Maximum SS
combo <- data.frame(nb=rep(1:maxN,1:maxN),ub=sequence(1:maxN)) #All plausible combinations of n and u 
Myprobs <- array(0,c(nrow(combo),2)) 

for(i in 1:nrow(combo)){
  Myprobs[i,1] <- error_power(combo[i,1],combo[i,2],0.3,0.5)$error 
  Myprobs[i,2] <- error_power(combo[i,1],combo[i,2],0.3,0.5)$power
}

data.frame(
  combo[which(Myprobs[,1] < alpha & Myprobs[,2] > power),],
  pprob = Myprobs[which(Myprobs[,1] < alpha & Myprobs[,2] > power),]) #Find values that satisfy the alpha and beta constraints





#(c)(i) Function to compute the Bayesian constraints
BayesianConstraints <- function(n,u,p0,p1,a,b){
  C1 <- pbeta(p0,shape1=a+u,shape2=b+n-u,lower.tail=F)
  C2 <- pbeta(p1,shape1=a+u-1,shape2=b+n-(u-1))
  return(data.frame(C1,C2))
}

#(c)(ii) SS Calculations
eta <- 0.95
zeta <- 0.8
maxN <- 50
combo <- data.frame(nb=rep(1:maxN,1:maxN),ub=sequence(1:maxN))

#p~Beta(1,1)
Myprobs11 <- array(0,c(nrow(combo),2))
for(i in 1:nrow(combo)){
  Myprobs11[i,1] <- BayesianConstraints(combo[i,1],combo[i,2],0.3,0.5,a=1,b=1)$C1
  Myprobs11[i,2] <- BayesianConstraints(combo[i,1],combo[i,2],0.3,0.5,a=1,b=1)$C2
}

data.frame(
  combo[which(Myprobs11[,1] >= eta & Myprobs11[,2] >= zeta),],
  pprob = Myprobs11[which(Myprobs11[,1] >= eta & Myprobs11[,2] >= zeta),])[1,]

#p~Beta(10,10)
Myprobs1010 <- array(0,c(nrow(combo),2))
for(i in 1:nrow(combo)){
  Myprobs1010[i,1] <- BayesianConstraints(combo[i,1],combo[i,2],0.3,0.5,a=10,b=10)$C1
  Myprobs1010[i,2] <- BayesianConstraints(combo[i,1],combo[i,2],0.3,0.5,a=10,b=10)$C2
}

data.frame(
  combo[which(Myprobs1010[,1] >= eta & Myprobs1010[,2] >= zeta),],
  pprob = Myprobs1010[which(Myprobs1010[,1] >= eta & Myprobs1010[,2] >= zeta),])[1,]


#p~Beta(15,15)
Myprobs1515 <- array(0,c(nrow(combo),2))
for(i in 1:nrow(combo)){
  Myprobs1515[i,1] <- BayesianConstraints(combo[i,1],combo[i,2],0.3,0.5,a=15,b=15)$C1
  Myprobs1515[i,2] <- BayesianConstraints(combo[i,1],combo[i,2],0.3,0.5,a=15,b=15)$C2
}

data.frame(
  combo[which(Myprobs1515[,1] >= eta & Myprobs1515[,2] >= zeta),],
  pprob = Myprobs1515[which(Myprobs1515[,1] >= eta & Myprobs1515[,2] >= zeta),])[1,]


#p~Beta(5,15)
Myprobs515 <- array(0,c(nrow(combo),2))
for(i in 1:nrow(combo)){
  Myprobs515[i,1] <- BayesianConstraints(combo[i,1],combo[i,2],0.3,0.5,a=5,b=15)$C1
  Myprobs515[i,2] <- BayesianConstraints(combo[i,1],combo[i,2],0.3,0.5,a=5,b=15)$C2
}

data.frame(
  combo[which(Myprobs515[,1] >= eta & Myprobs515[,2] >= zeta),],
  pprob = Myprobs515[which(Myprobs515[,1] >= eta & Myprobs515[,2] >= zeta),])[1,]





#(d)(i)
library(rjags)

set.seed(100)
MyDat1 <- rbinom(n = 49, size = 1, prob = 0.5)

model1 <- "model{
  for(i in 1:length(Y)){
      Y[i] ~ dbern(p)
  }
  # prior
  p ~ dunif(0, 1)
}"

data_list <- list(Y = MyDat1)

iter <- 100000
model1.spec <- textConnection(model1)
jags <- jags.model(model1.spec, data = data_list, 
                   n.chains = 1,  n.adapt = iter)

update(jags, iter, progress.bar = "none")
mcmc.sampling <- jags.samples(jags, c("p"), iter, progress.bar = "none")

samples.p <- mcmc.sampling$p[1,,]

data_list_prior <- list(Y = NA) #To obtain the prior distribution feed no data into the model.

model1.spec <- textConnection(model1)
jags_prior <- jags.model(model1.spec, data = data_list_prior, 
                         n.chains = 1,  n.adapt = iter)

update(jags_prior, iter, progress.bar = "none")
mcmc.sampling_prior <- jags.samples(jags_prior, c("p"), iter, progress.bar = "none")

samples.p_prior <- mcmc.sampling_prior$p[1,,]

#Plot the prior and posterior distributions
plot(density(samples.p,bw=0.01),xlim=c(0,1),main='Prior vs Posterior Density')
lines(density(samples.p_prior),col='red')
legend('topright',legend=c('Prior','Posterior'),col=c('red','black'),lty=rep(1,2))


#(d)(ii)

model1 <- "model{
for(i in 1:length(Y)){
Y[i] ~ dbern(p)
}
pCat <- step(p - p0)
# prior
p ~ dunif(0, 1)
}"

data_list <- list(
  Y = MyDat1,
  p0 = 0.3
)

iter <- 100000
model1.spec <- textConnection(model1)
jags <- jags.model(model1.spec, data = data_list,
                   n.chains = 1, n.adapt = iter)
update(jags, iter, progress.bar = "none")
mcmc.sampling <- jags.samples(jags, c("p", "pCat"), iter, progress.bar = "none")
samples.p <- mcmc.sampling$p[1,,]
samples.pCat <- mcmc.sampling$pCat[1,,]

mean(samples.p); sd(samples.p) #Posterior mean and sd
mean(samples.pCat) #Posterior probability


#(d)(iii)
#Need to specify the two data sets seperately to avoid a hierarchy
model1 <- "model{
Y1 ~ dbinom(p_eta,n)
Y2 ~ dbinom(p_zeta,n)

pCateta <- step(p_eta - p0)
pCatzeta <- step(p1-p_zeta)

# prior
p_eta ~ dunif(c, d)
p_zeta ~ dunif(c , d)

}"

JAGSssUNIF <- function(n,u,p0,p1,c,d){
  iter <- 100000
  data_list1 <- list(
    Y1 = u,
    Y2=u-1,
    n=n,
    p0 = p0,
    p1 = p1,
    c=c,
    d=d
  )
  model1.spec <- textConnection(model1)
  jags <- jags.model(model1.spec, data = data_list1,
                     n.chains = 1, n.adapt = iter)
  update(jags, iter,progress.bar='none')
  mcmc.sampling <- jags.samples(jags, c("pCateta","pCatzeta"), iter, progress.bar = "none")
  
  samples.pCateta <- mcmc.sampling$pCateta[1,,]
  P1 <- mean(samples.pCateta) #Eta constraint
  samples.pCatzeta <- mcmc.sampling$pCatzeta[1,,]
  P2 <- mean(samples.pCatzeta) #Zeta constraint
  return(list('P1'=P1,'P2'=P2))
}

#(d)(iv)

maxN <- 50
eta <- 0.95
zeta <- 0.8
combo <- data.frame(nb=rep(1:maxN,1:maxN),ub=sequence(1:maxN))

Myprobs01 <- array(0,c(nrow(combo),2))
for(i in 1:nrow(combo)){
  Myprobs01[i,1] <- JAGSssUNIF(combo[i,1],combo[i,2],0.3,0.5,c=0,d=1)$P1
  Myprobs01[i,2] <- JAGSssUNIF(combo[i,1],combo[i,2],0.3,0.5,c=0,d=1)$P2
}

data.frame(
  combo[which(Myprobs01[,1] >= eta & Myprobs01[,2] >= zeta),],
  pprob = Myprobs01[which(Myprobs01[,1] >= eta & Myprobs01[,2] >= zeta),])[1:5,]


Myprobs025075 <- array(0,c(nrow(combo),2))
for(i in 1:nrow(combo)){
  Myprobs025075[i,1] <- JAGSssUNIF(combo[i,1],combo[i,2],0.3,0.5,c=0,d=1)$P1
  Myprobs025075[i,2] <- JAGSssUNIF(combo[i,1],combo[i,2],0.3,0.5,c=0,d=1)$P2
}

data.frame(
  combo[which(Myprobs025075[,1] >= eta & Myprobs025075[,2] >= zeta),],
  pprob = Myprobs025075[which(Myprobs025075[,1] >= eta & Myprobs025075[,2] >= zeta),])[1:5,]

################################################################################
# P3
################################################################################










