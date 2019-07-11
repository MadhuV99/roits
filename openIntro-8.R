#openIntro-8.R
setwd('~/pj/rmsc')
getwd()

##Ex.7.21
n1 <- 9
xbar1 <- 3.5
s1 <- 5.17
n2 <- 9
xbar2 <- -4.33
s2 <- 2.76
xbar <- xbar1 - xbar2
# Check normality
#std. err.
se <- sqrt((s1**2/n1) + (s2**2/n2))
df <- min(n1 - 1, n2 - 1)
# CI
CI <- 0.95
t <- qt(0.5 - CI/2, df) * -1
ME <- t * se
lowerL <- round(xbar - ME, 3)
upperL <- round(xbar + ME, 3)
c(lowerL, upperL)
 
##Em.7.23
#H0: mu1 - mu2 = 0
#Ha: mu1 - mu2 <> 0
sig.lvl <- 0.05
mud <- 0
n1 <- 100
xbar1 <- 7.18
s1 <- 1.60
n2 <- 50
xbar2 <- 6.78
s2 <- 1.43
xbar <- xbar1 - xbar2
# Check normality
#std. err.
se <- sqrt((s1**2/n1) + (s2**2/n2))
df <- min(n1 - 1, n2 - 1)
t <- (xbar - mud)/se 
pval <- round(2 * pt(t, df, lower.tail = FALSE), 4)
pval < sig.lvl

##GP.7.28
#H0: mu1 - mu2 = 0
#Ha: mu1 - mu2 <> 0
sig.lvl <- 0.05
mud <- 0
n1 <- 30
xbar1 <- 79.4
s1 <- 14
min1 <- 45
max1 <- 100
n2 <- 27
xbar2 <- 74.1
s2 <- 20
min2 <- 32
max2 <- 100
xbar <- xbar1 - xbar2
# check for normality: sample size?
n1 >= 30 & n2 >= 30
# check for normality: outliers?
zcutoff <- 2.5
zmin1 <- (min1 - xbar1)/s1
zmax1 <- (max1 - xbar1)/s1
abs(zmin1) <= zcutoff & abs(zmax1) <= zcutoff
zmin2 <- (min2 - xbar2)/s2
zmax2 <- (max2 - xbar2)/s2
abs(zmin2) <= zcutoff & abs(zmax2) <= zcutoff
#std. err.
se <- sqrt((s1**2/n1) + (s2**2/n2))
df <- min(n1 - 1, n2 - 1)
t <- (xbar - mud)/se 
pval <- round(2 * pt(t, df, lower.tail = FALSE), 4)
pval < sig.lvl


##Ex.7.23
#H0: mud = 0
#Ha: mud <> 0
sig.lvl <- 0.05
mud <- 0
n <- 10
xbar <- 1835
s <- 1176
# check for normality: sample size?
n >= 30
# check for normality: outliers?
#std. err.
se <- round(s/sqrt(n), 3)
df <- n - 1
t <- (xbar - mud)/se 
pval <- round(2 * pt(t, df, lower.tail = FALSE), 3)
pval < sig.lvl


##Ex.7.25
#H0: mud = 0
#Ha: mud <> 0
sig.lvl <- 0.05
mud <- 0
n <- 6
xbar <- -3.33
s <- 3.01
# check for normality: sample size?
n >= 30
# check for normality: outliers?
#std. err.
se <- round(s/sqrt(n), 3)
df <- n - 1
t <- (xbar - mud)/se * -1
pval <- round(2 * pt(t, df, lower.tail = FALSE), 3)
pval < sig.lvl
# CI
CI <- 0.95
t <- qt(0.5 - CI/2, df) * -1
ME <- t * se
lowerL <- round(xbar - ME, 3)
upperL <- round(xbar + ME, 3)
c(lowerL, upperL)


##Ex.7.27
#H0: mu1 - mu2 = 0
#Ha: mu1 - mu2 <> 0
sig.lvl <- 0.05
mud <- 0
n1 <- 12
xbar1 <- 218.75
s1 <- 52.24
n2 <- 10
xbar2 <- 160.20
s2 <- 38.63
xbar <- xbar1 - xbar2
# Check normality
#std. err.
se <- sqrt((s1**2/n1) + (s2**2/n2))
df <- min(n1 - 1, n2 - 1)
t <- (xbar - mud)/se 
pval <- round(2 * pt(t, df, lower.tail = FALSE), 4)
pval < sig.lvl


##Ex.7.29
#H0: mu1 - mu2 = 0
#Ha: mu1 - mu2 <> 0
sig.lvl <- 0.05
mud <- 0
n1 <- 12
xbar1 <- 323.58
s1 <- 64.43
n2 <- 14
xbar2 <- 246.43
s2 <- 54.13
xbar <- xbar1 - xbar2
# Check normality
#std. err.
se <- sqrt((s1**2/n1) + (s2**2/n2))
df <- min(n1 - 1, n2 - 1)
t <- (xbar - mud)/se 
pval <- round(2 * pt(t, df, lower.tail = FALSE), 4)
pval < sig.lvl

##Ex.7.31
#H0: mu = 0
#Ha: mu <> 0
sig.lvl <- 0.05
mu <- 0
n <- 14
xbar <- 6.21
s <- 12.3
# check for normality: sample size?
n >= 30
# check for normality: outliers?
#std. err.
se <- round(s/sqrt(n), 4)
df <- n - 1
t <- (xbar - mu)/se
pval <- round(2 * pt(t, df, lower.tail = FALSE), 4)
pval < sig.lvl

#H0: mu = 0
#Ha: mu <> 0
sig.lvl <- 0.05
mu <- 0
n <- 14
xbar <- 2.86
s <- 7.94
# check for normality: sample size?
n >= 30
# check for normality: outliers?
#std. err.
se <- round(s/sqrt(n), 4)
df <- n - 1
t <- (xbar - mu)/se
pval <- round(2 * pt(t, df, lower.tail = FALSE), 4)
pval < sig.lvl


#H0: mu = 0
#Ha: mu <> 0
sig.lvl <- 0.05
mu <- 0
n <- 14
xbar <- -3.21
s <- 8.57
# check for normality: sample size?
n >= 30
# check for normality: outliers?
#std. err.
se <- round(s/sqrt(n), 4)
df <- n - 1
t <- (xbar - mu)/se * -1
pval <- round(2 * pt(t, df, lower.tail = FALSE), 4)
pval < sig.lvl

# POWER of a Test
##Em.7.31
#H0: mu1 - mu2 = 0
#Ha: mu1 - mu2 <> 0
sig.lvl <- 0.05
mud <- 0
n1 <- 100
xbar1 <- 10
s1 <- 12
n2 <- 100
xbar2 <- 13
s2 <- 12
xbar <- xbar1 - xbar2
# Check normality
#std. err.
se <- round(sqrt((s1**2/n1) + (s2**2/n2)), 2)
df <- min(n1 - 1, n2 - 1)
t <- (xbar - mud)/se * -1
pval <- round(2 * pt(t, df, lower.tail = FALSE), 4)
pval < sig.lvl

##Em.7.4
sig.lvl <- 0.05
z <- round(qnorm(sig.lvl/2) * -1, 2)
ME <- z * se
lowerTail <- round(mud - ME, 3)
upperTail <- round(mud + ME, 3)

##Em.7.35
effect <- -3
zeffect <- round((lowerTail - effect)/se, 2)
power <- round(pnorm(zeffect), 2)

##GP.7.36
#H0: mu1 - mu2 = 0
#Ha: mu1 - mu2 <> 0
sig.lvl <- 0.05
mud <- 0
n1 <- 500
xbar1 <- 10
s1 <- 12
n2 <- 500
xbar2 <- 13
s2 <- 12
xbar <- xbar1 - xbar2
# Check normality
#std. err.
se <- round(sqrt((s1**2/n1) + (s2**2/n2)), 2)
df <- min(n1 - 1, n2 - 1)
t <- (xbar - mud)/se * -1
pval <- round(2 * pt(t, df, lower.tail = FALSE), 4)
pval < sig.lvl
#Tails
z <- round(qnorm(sig.lvl/2) * -1, 2)
ME <- z * se
lowerTail <- round(mud - ME, 3)
upperTail <- round(mud + ME, 3)
#Power
effect <- -3
zeffect <- round((lowerTail - effect)/se, 2)
power <- round(pnorm(zeffect), 3)

##Em.7.37
power <- 0.8
zpower <- round(qnorm(0.8), 2)
#Tail
ztail <- round(qnorm(sig.lvl/2) * -1, 2)
#effect <- (zpower + ztail) * se
#effect <- (zpower + ztail) * sqrt((s1**2/n1) + (s2**2/n2))
#effect**2 <- (zpower + ztail)**2 * (s1**2/n1) + (s2**2/n2)
#n = n1 = n2
n <- round(((zpower + ztail)/effect)**2 * (s1**2 + s2**2) + 1)

































