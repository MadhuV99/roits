#openIntro-7.R
setwd('~/pj/rmsc')
getwd()

##Em.7.2
pt(-2.1, 18)

##Em.7.3
pt(1.65, 20, lower.tail = FALSE)

##Em.7.4
2 * pt(-3, 2)

##Em.7.2
pt(-1.79, 19, lower.tail = FALSE)

##Em.7.7, Em.7.8, Em.7.9
n <- 19
xbar <- 4.4
s <- 2.3
se <- round(s/sqrt(n), 3)
df <- n - 1
CI <- 0.95
t <- qt(0.5 - CI/2, df) * -1
ME <- t * se
lowerL <- round(xbar - ME, 3)
upperL <- round(xbar + ME, 3)
c(lowerL, upperL)

# Confidence Interval for one-mean
##GP.7.10, Em.7.11, GP.7.12
n <- 15
xbar <- 0.287
s <- 0.069
minv <- 0.18
maxv <- 0.41
# check for normality: sample size?
n >= 30
# check for normality: outliers?
zcutoff <- 2.5
zmin <- (minv - xbar)/s
zmax <- (maxv - xbar)/s
abs(zmin) <= zcutoff & abs(zmax) <= zcutoff
#std. err.
se <- round(s/sqrt(n), 4)
df <- n - 1
CI <- 0.90
t <- qt(0.5 - CI/2, df) * -1
ME <- t * se
lowerL <- round(xbar - ME, 3)
upperL <- round(xbar + ME, 3)
c(lowerL, upperL)


# Hypothesis Test for one-mean
##GP.7.14, 7.15
#H0: mu = 93.29
#Ha: mu <> 93.29
sig.lvl <- 0.05
mu <- 93.29
n <- 100
xbar <- 97.32
s <- 16.98
# check for normality: sample size?
n >= 30
# check for normality: outliers?
#std. err.
se <- round(s/sqrt(n), 4)
df <- n - 1
t <- (xbar - mu)/se
pval <- round(2 * pt(t, df, lower.tail = FALSE), 4)
pval < sig.lvl

##Ex.7.1
#a)
n <- 6
CI <- 0.9
df <- n - 1
(t <- round(qt(0.5 - CI/2, df), 2) * -1)
#b)
n <- 21
CI <- 0.98
df <- n - 1
(t <- round(qt(0.5 - CI/2, df), 2) * -1)
#c)
n <- 29
CI <- 0.95
df <- n - 1
(t <- round(qt(0.5 - CI/2, df), 2) * -1)
#d)
n <- 12
CI <- 0.99
df <- n - 1
(t <- round(qt(0.5 - CI/2, df), 2) * -1)

##Ex.7.3
sig.lvl <- 0.05
#a)
n <- 11
t <- 1.91
df <- n - 1
(pval <- round(2 * pt(t, df, lower.tail = FALSE), 3))
pval < sig.lvl
#b)
n <- 17
t <- -3.45
df <- n - 1
(pval <- round(2 * pt(t*-1, df, lower.tail = FALSE), 3))
pval < sig.lvl
#c)
n <- 7
t <- 0.83
df <- n - 1
(pval <- round(2 * pt(t, df, lower.tail = FALSE), 3))
pval < sig.lvl
#d)
n <- 28
t <- 2.13
df <- n - 1
(pval <- round(2 * pt(t, df, lower.tail = FALSE), 3))
pval < sig.lvl


##Ex.7.5
n <- 36
df <- n - 1
CI <- 0.95
lowerL <- 18.985
upperL <- 21.015
ME <- (upperL - lowerL)/2 
xbar <- lowerL + ME
t <- round(qt(0.5 - CI/2, df) * -1, 3)
se <- ME / t 
s <- se * sqrt(n)


##Ex.7.7
#H0: mu = 8
#Ha: mu <> 8
sig.lvl <- 0.05
mu <-8
n <- 25
xbar <- 7.73
s <- 0.77
minv <- 6.17
maxv <- 9.78
# check for normality: sample size?
n >= 30
# check for normality: outliers?
zcutoff <- 3
zmin <- (minv - xbar)/s
zmax <- (maxv - xbar)/s
abs(zmin) <= zcutoff & abs(zmax) <= zcutoff
#std. err.
se <- round(s/sqrt(n), 4)
df <- n - 1
t <- (xbar - mu)/se * -1
pval <- round(2 * pt(t, df, lower.tail = FALSE), 4)
pval < sig.lvl

##Ex.7.9
mu <- 60
s <- 8
n <- 20
df <- n - 1
pval <- 0.05
se <- s/sqrt(n)
t <- qt(pval/2, df) * -1
#t <- (xbar - mu)/se 
xbar <- (t * se ) + mu

##Ex.7.11
#a)
#H0: mu = 5
#Ha: mu <> 5
sig.lvl <- 0.05
mu <- 5
n <- 20
xbar <- 4.6
s <- 2.2
# check for normality: sample size?
n >= 30
# check for normality: outliers?
#std. err.
se <- round(s/sqrt(n), 4)
df <- n - 1
t <- (xbar - mu)/se * -1
pval <- round(2 * pt(t, df, lower.tail = FALSE), 4)
pval < sig.lvl
#b)
CI <- 0.95
t <- qt(0.5 - CI/2, df) * -1
ME <- t * se
lowerL <- round(xbar - ME, 3)
upperL <- round(xbar + ME, 3)
c(lowerL, upperL)

##Ex.7.13
s <- 100
ME <- 10
CI <- 0.95
#n ?
df <- 100
t <- qt(0.5 - CI/2, df) * -1
#ME <- t * se
#ME <- t * s/sqrt(n)
#sqrt(n) <- t * s/ME
n <- round((t * s/ME)**2 + 1)

##Ex.7.17
#H0: mud = 0
#Ha: mud <> 0
sig.lvl <- 0.05
mud <- 0
n <- 68
xbar <- 3.58
s <- 13.42
# check for normality: sample size?
n >= 30
# check for normality: outliers?
#std. err.
se <- round(s/sqrt(n), 2)
df <- n - 1
t <- (xbar - mud)/se 
pval <- round(2 * pt(t, df, lower.tail = FALSE), 4)
pval < sig.lvl
# CI
CI <- 0.95
t <- qt(0.5 - CI/2, df) * -1
ME <- t * se
lowerL <- round(xbar - ME, 3)
upperL <- round(xbar + ME, 3)
c(lowerL, upperL)

##Ex.7.19
#H0: mud = 0
#Ha: mud <> 0
sig.lvl <- 0.05
mud <- 0
n <- 197
xbar <- 2.9
s <- 17.2
# check for normality: sample size?
n >= 30
# check for normality: outliers?
#std. err.
se <- round(s/sqrt(n), 3)
df <- n - 1
t <- (xbar - mud)/se 
pval <- round(2 * pt(t, df, lower.tail = FALSE), 4)
pval < sig.lvl
# CI
CI <- 0.9
t <- qt(0.5 - CI/2, df) * -1
ME <- t * se
lowerL <- round(xbar - ME, 3)
upperL <- round(xbar + ME, 3)
c(lowerL, upperL)







