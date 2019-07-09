#openIntro-6.R
setwd('~/pj/rmsc')
getwd()

##Em.6.25
round(pchisq(6.25, 3, lower.tail = FALSE), 4)

##Em.6.26
round(pchisq(4.3, 2, lower.tail = FALSE), 4)

##Em.6.27
round(pchisq(5.1, 5, lower.tail = FALSE), 4)

##Em.6.28
round(pchisq(11.7, 7, lower.tail = FALSE), 4)

##Em.6.29
round(pchisq(10, 4, lower.tail = FALSE), 4)

##Em.6.30
round(pchisq(9.21, 3, lower.tail = FALSE), 4)

##Em.6.32
round(pchisq(5.89, 3, lower.tail = FALSE), 4)


# When p = 0.7, find the first success within the first 3 cases?
p <- 0.7
x <- 2
pgeom(x, p)

# When p = 0.545, find the first success within the first n cases, in 1362 streaks?
streaks <- 1362
o <- c(717, 369, 155, 69, 28, 14, 10)

p <- 0.545
n <- 1:6
x <- n - 1
af <- round(dgeom(x, p), 5)
af <- c(af, 1 - sum(af))
af
sum(af)

#d <- round(dgeom(x, p) * streaks)
e <- round(af * streaks)
e

chisq.test(x=o, p=af, correct = FALSE)
chisq.test(x=o, p=e, rescale.p = TRUE,  correct = FALSE)

##Ex.6.33
o <- c(71, 30, 25)
sum(o)
af <- c(.60, 0.25, 0.15)
sum(af)
e <- round(af * sum(o), 1)
sum(e)
chisq.test(x=o, p=af, correct = FALSE)
chisq.test(x=o, p=e, rescale.p = TRUE,  correct = FALSE)

##Em.6.40
observed_table <- matrix(c(2, 71, 23, 50, 36, 37), ncol = 3)
rownames(observed_table) <- c('Disclose Problem', 'Hide Problem')
colnames(observed_table) <- c('General', 'Positive Assumption', 'Negative Assumption')
observed_table

(rs <- chisq.test(observed_table))
rs$expected

##GP.6.42
observed_table <- matrix(c(109, 120, 90, 125, 112, 143), ncol = 2)
rownames(observed_table) <- c('lifestyle', 'Met', 'Rosi')
colnames(observed_table) <- c('Failure', 'Success')
observed_table

(rs <- chisq.test(observed_table))
rs$expected

##Ex.6.35
#a)
#         Support         Solo          Total     
# Quit       40 (35)       30            70
# Nope      110           120 (115)     230
# Total     150           150           300

observed_table <- matrix(c(40,110,30,120), ncol = 2)
rownames(observed_table) <- c('Quit', 'Nope')
colnames(observed_table) <- c('Support', 'Solo')
observed_table

(rs <- chisq.test(observed_table))
rs$expected

##Ex.6.37
observed_table <- matrix(c(154,180,104,132,126,131), ncol = 2)
rownames(observed_table) <- c('Support', 'Oppose', 'Do not know')
colnames(observed_table) <- c('Grad', 'No Grad')
observed_table

(rs <- chisq.test(observed_table))
rs$expected

##Ex.6.41
#a)
#H0: Preferred shipping method is independent of Age group
#H1: Preferred shipping method is affected by Age group

##Ex.6.43
#a)
CI <- 0.95
z <- qnorm(0.5 - CI/2) * -1
round(CI, 2) == round((0.5 - pnorm(-z)) * 2, 2)
n <- 200
x <- 40
ph <- x/n
# success-failure cond.
n * ph >= 10
n * (1 - ph) >= 10
# Std. Err.
se <- round(sqrt(ph * (1 - ph)/n), 4)
ME <- z * se
lowerL <- round(ph - ME, 3)
upperL <- round(ph + ME, 3)
c(lowerL, upperL)
#b)
ME <- 0.02
CI <- 0.95
z <- round(qnorm(0.5 - CI/2) * -1, 3)
#a)
p <- ph
#se <- sqrt(p * (1 - p)/n)
#ME <- z * se
#ME <- z * sqrt(p * (1 - p)/n)
#ME**2 <- z**2 * p * (1 - p)/n 
#n <- z**2 * p * (1 - p)/ME**2
n <- (z/ME)**2 * p * (1 - p)
n <- round(n + 1)


##Ex.6.45
CI <- 0.95
z <- qnorm(0.5 - CI/2) * -1
round(CI, 2) == round((0.5 - pnorm(-z)) * 2, 2)
n <- 400
x <- 348
ph <- x/n
# success-failure cond.
n * ph >= 10
n * (1 - ph) >= 10
# Std. Err.
se <- round(sqrt(ph * (1 - ph)/n), 4)
ME <- z * se
lowerL <- round(ph - ME, 3)
upperL <- round(ph + ME, 3)
c(lowerL, upperL)
#c)
CI <- 0.99
z <- qnorm(0.5 - CI/2) * -1
round(CI, 2) == round((0.5 - pnorm(-z)) * 2, 2)
ME <- z * se
lowerL <- round(ph - ME, 3)
upperL <- round(ph + ME, 3)
c(lowerL, upperL)

##Ex.6.47
o <- c(43, 21, 35)
sum(o)
af <- c(1/3, 1/3, 1/3)
sum(af)
e <- round(af * sum(o), 1)
sum(e)
chisq.test(x=o, p=af, correct = FALSE)
chisq.test(x=o, p=e, rescale.p = TRUE,  correct = FALSE)

#Em.6.49
#H0: p0 == 0.38
#Ha: p0 != 0.38
sig.lvl <- 0.05
p0 <- 0.38
n <- 2254
ph <- 0.17
# success-failure cond.
n * p0 >= 10
n * (1 - p0) >= 10
# Std. Err.
se <- round(sqrt(p0 * (1 - p0)/n), 3)
z <- (ph - p0)/se * -1
pval <- 2 * pnorm(z, lower.tail = FALSE)
pval < sig.lvl
#c)
CI <- 0.95
z <- qnorm(0.5 - CI/2) * -1
round(CI, 2) == round((0.5 - pnorm(-z)) * 2, 2)
# Std. Err.
se <- round(sqrt(ph * (1 - ph)/n), 3)
ME <- z * se
lowerL <- round(ph - ME, 3)
upperL <- round(ph + ME, 3)
c(lowerL, upperL)


























