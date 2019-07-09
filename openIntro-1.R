#openIntro-1.R
setwd('~/pj/rmsc')
getwd()

round(pnorm(-1.35) * 100, 2)
round((1-pnorm(1.48))*100,2)
round((pnorm(1.5) - pnorm(-0.4))*100, 2)
round((1- (pnorm(2) - pnorm(-2)))*100, 2)

#Ex4.3
round((160-151)/7, 2)
round((157-153)/7.67, 2)
round(pnorm(1.29)*100, 2)
round(pnorm(0.52)*100, 2)
round((1-pnorm(1.29))*100, 2)
round((1-pnorm(0.52))*100, 2)

#Ex4.5
z <- round(qnorm(0.8), 2)
round(z * 7.67 + 153)

mu = 151
s = 7
pnorm(-.52)
z <- round(qnorm(0.7), 2) * -1
round(z * s + mu)

#Ex 4.7
#a)
mu = 77
s <- 5
x <- 83
z <- (x - mu) / s
round((1-pnorm(z))*100, 2)
#b)
z <- qnorm(0.1, lower.tail = TRUE)
round(x <- z * s + mu, 1)

#Ex 4.9
#a)
mu = 77
s <- 5
ftoc <- function(f) {
  c <- (f-32)*5/9
}

muc <- ftoc(mu)
sdc <- round(s*5/9, 2)
#N(25, 2.78)
#b)
z <- (28 - muc)/sdc
round(pnorm(z, lower.tail = FALSE)*100, 2)
#d)
zq3 <- qnorm(0.75)
zq1 <- qnorm(0.25) 
q3 <- round((zq3 * sdc) + muc, 2) 
q1 <- round((zq1 * sdc) + muc, 2) 
q3 - q1

#Ex.4.13
#a)
n <- 3
p <- 0.125
round((1-p)**(n-1) * p * 100, 2)
#b)
p <- 0.125
1/p

#GP.4.33
#a)
p <- 0.3
n <- 4
r <- 0
nCr <- factorial(n)/(factorial(r)* factorial(n-r))
lc0 <- nCr * p**r * (1-p)**(n-r)
#b)
r <- 1
nCr <- factorial(n)/(factorial(r)* factorial(n-r))
lc1 <- nCr * p**r * (1-p)**(n-r)
#c)
lc01 <- lc0+lc1

##GP.4.34
lc02 <- 1 - lc01

#GP.4.35
#a)
p <- 0.3
n <- 7
mu <- n*p
#b)

lc2 <- 0
for(i in 0:2) {
  r <- i
  nCr <- factorial(n)/(factorial(r)* factorial(n-r))
  lc2 <- lc2 + (nCr * p**r * (1-p)**(n-r))
}
round(lc2*100,2)

##Ex.4.38
p <- 0.15
n <- 400
x <- 42
pbinom(x,400,p)

##EM.4.40
n <- 400
p <- 0.15
n*p
n*(1-p)
mu <- n*p
sd <- sqrt(n*p*(1-p))
x <- 42
z <- (x-mu)/sd
pnorm(z)

##Ex.4.17
#b)
p <- 0.697
n <- 10
x <- 6
dbinom(x, n, p)
#c)
dbinom(x, n, p)
#d)
n <- 5
x <- 2
pbinom(x, n, p)
#e)
n <- 5
x <- 0
1-pbinom(x, n, p)


##Ex.4.19
p <- 0.697
n <- 50
#a)
mu = n * p
#b)
x <- 45
mu <- n * p
sd <- round(sqrt(n*p*(1-p)),2)
z <- (x - mu)/sd
pnorm(z, lower.tail = FALSE)
#c)
p <- 0.697
n <- 50
x <- 44.5
mu <- n * p
sd <- round(sqrt(n*p*(1-p)),2)
z <- (x - mu)/sd
pnorm(z, lower.tail = FALSE)

#Ex.4.21
#a)
p <- 0.25
1-(1-p)**3
#b)
p <- 0.25
n <- 3
x <- 2
dbinom(x,n,p)
#c)
3 * p * (1-p)**2
#d)
p <- 0.25
n <- 3
x <- 3
1-dbinom(x,n,p)

##Ex.4.23
pBr <- 0.75
pBl <- 0.125
pGr <- 0.125
#a)
pGr * (1 - pGr)
#b)
n <- 2
p <- pGr
x <- 1
2 * pGr * (1 - pGr)
dbinom(x,n,p)
#c)
n <- 6
p <- pGr
x <- 2
dbinom(x,n,p)
#d)
n <- 6
p <- pGr
x <- 0
1 - pbinom(x,n,p)
#e)
n <- 4
p <- pGr
(1 - p)**(n - 1) * p
#f)
n <- 6
p <- pBr
x <- 2
dbinom(x,n,p)

n * p
n * (1-p)

##Ex.4.25
# A, B, C, D, E
#a) 
1/5 * 1/4 * 1/3 * 1/2










































































