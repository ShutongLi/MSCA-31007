#Rough transformation from any scale to [0,1] scale
set.seed(1138)
my.samp <- sample(201:1000,size=300)
head(my.samp)
head((my.samp-min(my.samp))/(max(my.samp)-min(my.samp)))


#Explaining graph in Workshop
set.seed(1138)
ran.unifs <- runif(n=1000)
head(ran.unifs)
plot(ran.unifs)
hist(ran.unifs)

mean(ran.unifs)
var(ran.unifs)
plot(ecdf(ran.unifs),main='Empirical CDF of Unif Sample')
lines(x=c(0,1),y=c(0,1),col='blue')
ks.test(ran.unifs,punif)

#Look again at Normal CDF
plot(x=seq(from=-3,to=3,by=0.01),
     y=pnorm(seq(from=-3,to=3,by=0.01)),
     type='l',col='navy',lwd=2,
     main='Normal(0,1) CDF',xlab='x',ylab='F(x)')
cbind(x=seq(from=-3,to=3,by=0.1),
      p.lt.x=pnorm(seq(from=-3,to=3,by=0.1)))

norm.samp <- qnorm(p=ran.unifs,mean=0,sd=1)
head(norm.samp)
hist(norm.samp)
plot(norm.samp)
mean(norm.samp)
var(norm.samp)

plot(ecdf(norm.samp),main='Empirical CDF of Simulated Data')
lines(x=seq(-3,3,0.01),y=pnorm(seq(-3,3,0.01)),col='blue')
ks.test(norm.samp,pnorm)
lines(x=seq(-3,3,0.01),y=pnorm(seq(-3,3,0.01),0.05,0.95),col='green')
ks.test(norm.samp,pnorm,mean=0.05,sd=0.95)
legend(x='bottomright',legend=c('Simulated Normal(0,1)',
  'Theoretical Normal(0,1)','Theoretical(0.05,0.95)'),
  col=c('black','blue','green'),lwd=1)

#Randomness: I(ndependently) I(dentically) D(istributed)

#Independence
set.seed(2010)
ar1 <- arima.sim(list(ar=0.1),n=1000)
hist(ar1)
plot(ar1)
plot(ar1[1:50],type='b')
plot(x=ar1[-1000],y=ar1[-1],
     xlab='Lagged values',ylab='Current values')
install.packages('lmtest')
require(lmtest)
dwtest(ar1~1)
plot(acf(ar1))

#Identicality
set.seed(2061)
mixed <- c(rnorm(600,5,2),rnorm(400,11,2))
plot(density(mixed),main='Empirical PDF of Sample')
plot(mixed)
plot(mixed[sample(1:1000,size=1000)])

set.seed(2046)
bern.samp <- sample(0:1,size=1000,replace=T,prob=c(0.4,0.6))
bern.samp[601:1000] <- 1 - bern.samp[601:1000]
plot(cumsum(bern.samp)/(1:1000),type='l',col='orange',
     ylab='Cumul. Prop.',main='Mixed Bernoulli Samples')

#Distribution
set.seed(3018)
cauchy.samp <- rcauchy(1000)
plot(cauchy.samp)
plot(cauchy.samp,ylim=c(-5,5))
plot(cauchy.samp[11:60])
cauchy.matrix <- matrix(cauchy.samp,ncol=20)
dim(cauchy.matrix)
apply(cauchy.matrix,2,function(z) ks.test(z,pnorm)$p.value)
sum(apply(cauchy.matrix,2,function(z) ks.test(z,pnorm)$p.value)>0.05)