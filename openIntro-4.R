#openIntro-4.R
setwd('~/pj/rmsc')
getwd()

trans_compli <- function() {
  # 1. Create a set of 10, where 10% of them are "complications"
  # and 90% are "not".
  possible_entries <- c(rep("comp", 1), rep("no-comp", 9))
  # 2. Sample 62 entries with replacement.
  samp_size <- 62
  sampled_entries <- sample(possible_entries, size = samp_size, replace=TRUE)
  # 3. Compute p-hat: count the number that are "complications", then divide by
  # the sample size.
#  print(sampled_entries)
  sum(sampled_entries == "comp") / samp_size
}


#Confidence Interval for a proportion
##GP.6.2
CI <- 0.95
n <- 826
ph <- 0.7
#a)
# success-failure cond.
n * ph >= 10
n * (1 - ph) >= 10
#b)
# Std. Err.
se <- round(sqrt(ph * (1 - ph)/n), 3)
#c)
z <- qnorm(0.5 - CI/2) * -1
round(CI, 2) == round((0.5 - pnorm(-z)) * 2, 2)
ME <- z * se
lowerL <- round(ph - ME, 3)
upperL <- round(ph + ME, 3)
c(lowerL, upperL)

#hypothesis testing for a proportion
##G.P.6.5
#H0: p0 == 0.5
#Ha: p0 != 0.5
sig.lvl <- 0.05
p0 <- 0.5
n <- 826
ph <- 0.51
# success-failure cond.
n * p0 >= 10
n * (1 - p0) >= 10
# Std. Err.
se <- round(sqrt(p0 * (1 - p0)/n), 3)
z <- (ph - p0)/se
pval <- 2 * pnorm(z, lower.tail = FALSE)
pval < sig.lvl

#Small sample hypothesis testing for a proportion with Simulation
#H0: p0 >= 0.1
#Ha: p0 < 0.1
sig.lvl <- 0.05
p0 <- 0.1
n <- 63
x <- 3
ph <- round(x/n, 3)
# success-failure cond: CANNOT use normal distribution
n * p0 >= 10
n * (1 - p0) >= 10

set.seed(1234)
print(trans_compli())

simulations <- 10000
left_tails <- 0
for(i in 0:simulations) {
  psim <- trans_compli()
  if(psim <= ph) {
    left_tails = left_tails + 1
  }
}
print(left_tails/simulations)

#Binomial probability
p <- 0.1
n <- 62
x <- 3
pbinom(x,n,p)

# SAMPLE SIZE
##Em.6.7
ME <- 0.04
CI <- 0.95
z <- round(qnorm(0.5 - CI/2) * -1, 3)
p <- 0.5
#se <- sqrt(p * (1 - p)/n)
#ME <- z * se
#ME <- z * sqrt(p * (1 - p)/n)
#ME**2 <- z**2 * p * (1 - p)/n 
#n <- z**2 * p * (1 - p)/ME**2
n <- (z/ME)**2 * p * (1 - p)
n <- round(n + 1)

##GP.6.8
ME <- 0.01
CI <- 0.90
z <- round(qnorm(0.5 - CI/2) * -1, 3)
#a)
p <- .017
#se <- sqrt(p * (1 - p)/n)
#ME <- z * se
#ME <- z * sqrt(p * (1 - p)/n)
#ME**2 <- z**2 * p * (1 - p)/n 
#n <- z**2 * p * (1 - p)/ME**2
n <- (z/ME)**2 * p * (1 - p)
n <- round(n + 1)
#b)
p <- .062
#se <- sqrt(p * (1 - p)/n)
#ME <- z * se
#ME <- z * sqrt(p * (1 - p)/n)
#ME**2 <- z**2 * p * (1 - p)/n 
#n <- z**2 * p * (1 - p)/ME**2
n <- (z/ME)**2 * p * (1 - p)
n <- round(n + 1)
#c)
p <- .013
#se <- sqrt(p * (1 - p)/n)
#ME <- z * se
#ME <- z * sqrt(p * (1 - p)/n)
#ME**2 <- z**2 * p * (1 - p)/n 
#n <- z**2 * p * (1 - p)/ME**2
n <- (z/ME)**2 * p * (1 - p)
n <- round(n + 1)

##GP.6.10
ME <- 0.05
CI <- 0.95
z <- round(qnorm(0.5 - CI/2) * -1, 3)
p <- .7
#se <- sqrt(p * (1 - p)/n)
#ME <- z * se
#ME <- z * sqrt(p * (1 - p)/n)
#ME**2 <- z**2 * p * (1 - p)/n 
#n <- z**2 * p * (1 - p)/ME**2
n <- (z/ME)**2 * p * (1 - p)
n <- round(n + 1)

##Ex.6.1
p <- 0.08
#a)
n <- 60
# success-failure cond.
n * p >= 10
n * (1 - p) >= 10
#b)
n <- 50
# success-failure cond.
n * p >= 10
n * (1 - p) >= 10
#c)
sig.lvl <- 0.05
n <- 125
ph <- 0.12
# success-failure cond.
n * p >= 10
n * (1 - p) >= 10
# Std. Err.
se <- round(sqrt(p * (1 - p)/n), 4)
z <- (ph - p)/se
pval <- 2 * pnorm(z, lower.tail = FALSE)
pval < sig.lvl
#d)
sig.lvl <- 0.05
n <- 250
ph <- 0.12
# success-failure cond.
n * p >= 10
n * (1 - p) >= 10
# Std. Err.
se <- round(sqrt(p * (1 - p)/n), 4)
z <- (ph - p)/se
pval <- 2 * pnorm(z, lower.tail = FALSE)
pval < sig.lvl

#Ex.6.3
p <- 0.9
#a)
n <- 30
# success-failure cond.
n * p >= 10
n * (1 - p) >= 10
#c)
n <- 140
# success-failure cond.
n * p >= 10
n * (1 - p) >= 10

#Ex.6.5
n <- 1390
ph <- 0.82
CI <- 0.95
ME <- 0.02
#a)

#Ex.6.7
n <- 600
ph <- 0.56
CI <- 0.95
# success-failure cond.
n * ph >= 10
n * (1 - ph) >= 10
# Std. Err.
se <- round(sqrt(ph * (1 - ph)/n), 3)
z <- qnorm(0.5 - CI/2) * -1
round(CI, 2) == round((0.5 - pnorm(-z)) * 2, 2)
ME <- z * se

##Ex.6.9
n <- 1509
ph <- 0.55
#b)
CI <- 0.90
# success-failure cond.
n * ph >= 10
n * (1 - ph) >= 10
z <- qnorm(0.5 - CI/2) * -1
round(CI, 2) == round((0.5 - pnorm(-z)) * 2, 2)
se <- round(sqrt(ph * (1 - ph)/n), 4)
ME <- z * se
lowerL <- round(ph - ME, 4)
upperL <- round(ph + ME, 4)

##Ex.6.11
#a)
sig.lvl <- 0.05
n <- 617
ph <- 0.55
# success-failure cond.
n * ph >= 10
n * (1 - ph) >= 10
p <- 0.5
se <- sqrt(p * (1 - p)/n)
z <- (ph - p)/se
pval <- 2 * pnorm(z, lower.tail = FALSE)
pval < sig.lvl

#Ex.6.13
#H0: p0 == 0.5
#Ha: p0 != 0.5
#a)
sig.lvl <- 0.05
n <- 80
x <- 53
ph <- x/n
# success-failure cond.
n * ph >= 10
n * (1 - ph) >= 10
p0 <- 0.5
se <- sqrt(p0 * (1 - p0)/n)
z <- (ph - p0)/se
pval <- 2 * pnorm(z, lower.tail = FALSE)
pval < sig.lvl


#Ex.6.15
ME <- 0.01
CI <- 0.90
z <- round(qnorm(0.5 - CI/2) * -1, 3)
ph <- 0.55
#se <- sqrt(ph * (1 - ph)/n)
#ME <- z * se
#ME <- z * sqrt(ph * (1 - ph)/n)
#ME**2 <- z**2 * ph * (1 - ph)/n 
#n <- z**2 * ph * (1 - ph)/ME**2
n <- (z/ME)**2 * ph * (1 - ph)
n <- round(n + 1)

