library(rethinking)

data(Howell1)

d <- Howell1[Howell1$age>=18,]
plot(d)

alpha <- 0 #if an individual has 0 height they have 0 weight
beta <- 0.5 # slope 
sigma <- 5 #std
n_individuals <- 100 #N

H <- runif(n_individuals, 130,170) #
mu <- alpha + beta*H
W <- rnorm(n_individuals, mu, sigma) #sample from a random normal distribution
W
plot(H, W)


#simulate the data

alpha <- 70 #if an individual has average height they have 70kg weight pick an intercept 
beta <- 0.5 # slope 
sigma <- 5 #stdev
n_individuals <- 100 #N
H <- runif(n_individuals, 130,170) #
mu <- alpha + beta*H
W <- rnorm(n_individuals, mu, sigma) #sample from a random normal distribution

dat<- list(H=H, W=W, Hbar=mean(H))

m_validate <- quap(
  alist(W ~ dnorm(mu,sigma),
        mu <- a + b * (H-Hbar),
        a ~ dnorm(60,10),
        b ~ dlnorm(0,1),
        sigma ~ dunif(0,10)
  ), data=dat)

precis(m_validate)

#real data
data("Howell1") # load data
d <- Howell1 # load data into new variable
d <- d[d$age >= 18,] #restrict age

dat<- list(
  W = d$weight,
  H = d$height,
  Hbar = mean(d$height))

m_adults <- quap(
  alist(W ~ dnorm(mu,sigma),
        mu <- a + b * (H-Hbar),
        a ~ dnorm(60,10),
        b ~ dlnorm(0,1),
        sigma ~ dunif(0,10)
  ), data=dat)

precis(m_adults)
post <- extract.samples(m_adults)
head(post)

# plot sample
col2 <- col.alpha(2,0.8) #set colour and transparency 
plot(d$height, d$weight, col=col2, lwd=3,
     cex=1.2, xlab= "height (cm)", ylab="weight (kg)") # plot height by weight ##cex is numerical vector for scaling the size of the dots 

#expectation with 99% compatibility interval
xseq <- seq(from=130,to=190,len=50)
mu <- link(m_adults, data = list(H=xseq, Hbar=mean(d$height)))
lines(xseq,apply(mu, 2, mean), lwd=4)
shade(apply(mu, 2, PI,prob=0.99), xseq, col=col.alpha(2,0.5))

#89% prediction interval
W_sim <- sim(m_adults, data = list(H=xseq, Hbar=mean(d$height)))
shade(apply(W_sim, 2, PI, prob=0.89), xseq, col=col.alpha(1,0.3))


#creating priors 

n <- 10 #sample = 10 
alpha <- rnorm(n,60,10) #weight 60 SD 10
beta <- rnorm(n,0,10) # 

Hbar <- 150
Hseq <- seq(from=130,to=170,len=30)
plot(NULL, xlim=c(130,170),ylim=c(10,100),
     xlab= "height (cm)", ylab="weight (kg)")
for (i in 1:n)
  lines(Hseq,alpha[i] + beta[i] * (Hseq-Hbar),
        lwd=3, col=2)
#problem is the lines go in all directions - distribution isnt constrained to positive values

n <- 10 #sample = 10 
alpha <- rnorm(n,60,10) #weight 60 SD 10
beta <- rlnorm(n,0,10) # log normed - only positive

Hbar <- 150
Hseq <- seq(from=130,to=170,len=30)
plot(NULL, xlim=c(130,170),ylim=c(10,100),
     xlab= "height (cm)", ylab="weight (kg)")
for (i in 1:n)
  lines(Hseq,alpha[i] + beta[i] * (Hseq-Hbar),
        lwd=3, col=2)

