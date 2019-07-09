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
 
##Ex.7.23
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




