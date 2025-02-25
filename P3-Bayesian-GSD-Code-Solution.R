N<-40
p0<-0.30
p1<-0.50

nsims<-100000


###############   Point (a)  #################### 
X<-rbinom(n=nsims,size=N,prob=p0)

post.prob<-pbeta(p0,shape1=X+1,shape2=N-X+1,lower.tail=F)

c<-0.959
mean(post.prob>c)

c<-0.958
mean(post.prob>c)


X<-rbinom(n=nsims,size=N,prob=p1)

post.prob<-pbeta(p0,shape1=X+1,shape2=N-X+1,lower.tail=F)

c<-0.959
mean(post.prob>c)


###############   Point (b)  #################### 
X1<-rbinom(n=nsims,size=N/2,prob=p0)
X2<-rbinom(n=nsims,size=N/2,prob=p0)
post.prob.1<-pbeta(p0,shape1=X1+1,shape2=N/2-X1+1,lower.tail=F)
post.prob.2<-pbeta(p0,shape1=X1+X2+1,shape2=N-X1-X2+1,lower.tail=F)
c<-0.959
mean(post.prob.1>c | post.prob.2>c)

c<-0.974
mean(post.prob.1>c | post.prob.2>c)


X1<-rbinom(n=nsims,size=N/2,prob=p1)
X2<-rbinom(n=nsims,size=N/2,prob=p1)
post.prob.1<-pbeta(p0,shape1=X1+1,shape2=N/2-X1+1,lower.tail=F)
post.prob.2<-pbeta(p0,shape1=X1+X2+1,shape2=N-X1-X2+1,lower.tail=F)
c<-0.974
mean(post.prob.1>c | post.prob.2>c)


###############   Point (c)  #################### 

# 75%
X1<-rbinom(n=nsims,size=N/2,prob=p0)
X2<-rbinom(n=nsims,size=N/2,prob=p0)
post.prob.futility<-pbeta(p0,shape1=X1+1,shape2=N/2-X1+1,lower.tail=T)
post.prob.efficacy<-pbeta(p0,shape1=X1+X2+1,shape2=N-X1-X2+1,lower.tail=F)
c<-0.959
t<-0.75
mean(post.prob.futility<t & post.prob.efficacy>c)
mean(post.prob.futility>t)


X1<-rbinom(n=nsims,size=N/2,prob=p1)
X2<-rbinom(n=nsims,size=N/2,prob=p1)
post.prob.futility<-pbeta(p0,shape1=X1+1,shape2=N/2-X1+1,lower.tail=T)
post.prob.efficacy<-pbeta(p0,shape1=X1+X2+1,shape2=N-X1-X2+1,lower.tail=F)
mean(post.prob.futility<t & post.prob.efficacy>c)
mean(post.prob.futility>t)


# 50%
X1<-rbinom(n=nsims,size=N/2,prob=p0)
X2<-rbinom(n=nsims,size=N/2,prob=p0)
post.prob.futility<-pbeta(p0,shape1=X1+1,shape2=N/2-X1+1,lower.tail=T)
post.prob.efficacy<-pbeta(p0,shape1=X1+X2+1,shape2=N-X1-X2+1,lower.tail=F)
c<-0.959
t<-0.50
mean(post.prob.futility<t & post.prob.efficacy>c)
mean(post.prob.futility>t)


X1<-rbinom(n=nsims,size=N/2,prob=p1)
X2<-rbinom(n=nsims,size=N/2,prob=p1)
post.prob.futility<-pbeta(p0,shape1=X1+1,shape2=N/2-X1+1,lower.tail=T)
post.prob.efficacy<-pbeta(p0,shape1=X1+X2+1,shape2=N-X1-X2+1,lower.tail=F)
mean(post.prob.futility<t & post.prob.efficacy>c)
mean(post.prob.futility>t)


# 25%
X1<-rbinom(n=nsims,size=N/2,prob=p0)
X2<-rbinom(n=nsims,size=N/2,prob=p0)
post.prob.futility<-pbeta(p0,shape1=X1+1,shape2=N/2-X1+1,lower.tail=T)
post.prob.efficacy<-pbeta(p0,shape1=X1+X2+1,shape2=N-X1-X2+1,lower.tail=F)
c<-0.959
t<-0.25
mean(post.prob.futility<t & post.prob.efficacy>c)
mean(post.prob.futility>t)


X1<-rbinom(n=nsims,size=N/2,prob=p1)
X2<-rbinom(n=nsims,size=N/2,prob=p1)
post.prob.futility<-pbeta(p0,shape1=X1+1,shape2=N/2-X1+1,lower.tail=T)
post.prob.efficacy<-pbeta(p0,shape1=X1+X2+1,shape2=N-X1-X2+1,lower.tail=F)
mean(post.prob.futility<t & post.prob.efficacy>c)
mean(post.prob.futility>t)



###############   Point (d)  #################### 
nsims<-10^5
X1<-rbinom(n=nsims,size=N/2,prob=p0)
X2<-rbinom(n=nsims,size=N/2,prob=p0)
CP<-c()
for(i in 1:nsims){
  CP[i]<-sum(dbinom(x=(18-X1[i]):20,size=20,prob=0.5))
}
post.prob.efficacy<-pbeta(p0,shape1=X1+X2+1,shape2=N-X1-X2+1,lower.tail=F)
c<-0.959
l<-0.20
mean(CP>l & post.prob.efficacy>c)
mean(CP<l)

X1<-rbinom(n=nsims,size=N/2,prob=p1)
X2<-rbinom(n=nsims,size=N/2,prob=p1)
CP<-c()
for(i in 1:nsims){
  CP[i]<-sum(dbinom(x=(18-X1[i]):20,size=20,prob=0.5))
}
post.prob.efficacy<-pbeta(p0,shape1=X1+X2+1,shape2=N-X1-X2+1,lower.tail=F)
mean(CP>l & post.prob.efficacy>c)
mean(CP<l)



###############   Point (e)  #################### 
X1<-rbinom(n=nsims,size=N/2,prob=p0)
X2<-rbinom(n=nsims,size=N/2,prob=p0)
post.prob.futility<-pbeta(p0,shape1=X1+3.2,shape2=N/2-X1+(7.5-3.2),lower.tail=T)
post.prob.efficacy<-pbeta(p0,shape1=X1+X2+3.2,shape2=N-X1-X2+(7.5-3.2),lower.tail=F)
c<-0.959
t<-0.50
mean(post.prob.futility<t & post.prob.efficacy>c)
mean(post.prob.futility>t)


X1<-rbinom(n=nsims,size=N/2,prob=p1)
X2<-rbinom(n=nsims,size=N/2,prob=p1)
post.prob.futility<-pbeta(p0,shape1=X1+3.2,shape2=N/2-X1+(7.5-3.2),lower.tail=T)
post.prob.efficacy<-pbeta(p0,shape1=X1+X2+3.2,shape2=N-X1-X2+(7.5-3.2),lower.tail=F)
mean(post.prob.futility<t & post.prob.efficacy>c)
mean(post.prob.futility>t)



X1<-rbinom(n=nsims,size=N/2,prob=p0)
X2<-rbinom(n=nsims,size=N/2,prob=p0)
post.prob.futility<-pbeta(p0,shape1=X1+3.2,shape2=N/2-X1+(7.5-3.2),lower.tail=T)
post.prob.efficacy<-pbeta(p0,shape1=X1+X2+3.2,shape2=N-X1-X2+(7.5-3.2),lower.tail=F)
c<-0.965
t<-0.50
mean(post.prob.futility<t & post.prob.efficacy>c)
mean(post.prob.futility>t)


X1<-rbinom(n=nsims,size=N/2,prob=p1)
X2<-rbinom(n=nsims,size=N/2,prob=p1)
post.prob.futility<-pbeta(p0,shape1=X1+3.2,shape2=N/2-X1+(7.5-3.2),lower.tail=T)
post.prob.efficacy<-pbeta(p0,shape1=X1+X2+3.2,shape2=N-X1-X2+(7.5-3.2),lower.tail=F)
mean(post.prob.futility<t & post.prob.efficacy>c)
mean(post.prob.futility>t)
