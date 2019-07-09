#openIntro-5.R
setwd('~/pj/rmsc')
getwd()

# CI for Difference of 2 Proportions
##Em.6.11
n1 <- 50
x1 <- 11
p1 <- x1/n1
n2 <- 40
x2 <- 14
p2 <- x2/n2
# success-failure cond. (group 1)
n1 * p1 >= 10
n1 * (1 - p1) >= 10
# success-failure cond. (group 2)
n2 * p2 >= 10
n2 * (1 - p2) >= 10

#Em.6.12
CI <- 0.90
z <- qnorm(0.5 - CI/2) * -1
round(CI, 2) == round((0.5 - pnorm(-z)) * 2, 2)
# Std. Err.
se <- round(sqrt((p1 * (1 - p1)/n1) + (p2 * (1 - p2)/n2)), 3)
ME <- z * se
ph <- p2 - p1
lowerL <- round(ph - ME, 3)
upperL <- round(ph + ME, 3)
c(lowerL, upperL)

#Em.6.13
CI <- 0.95
z <- round(qnorm(0.5 - CI/2) * -1, 2)
round(CI, 2) == round((0.5 - pnorm(-z)) * 2, 2)
n1 <- 12933
x1 <- 145
p1 <- round(x1/n1, 4)
n2 <- 12938
x2 <- 200
p2 <- round(x2/n2, 4)
# success-failure cond. (group 1)
n1 * p1 >= 10
n1 * (1 - p1) >= 10
# success-failure cond. (group 2)
n2 * p2 >= 10
n2 * (1 - p2) >= 10
# Std. Err.
se <- round(sqrt((p1 * (1 - p1)/n1) + (p2 * (1 - p2)/n2)), 5)
ME <- z * se
ph <- p1 - p2
lowerL <- round(ph - ME, 4)
upperL <- round(ph + ME, 4)
c(lowerL, upperL)

# Hypothesis tests for the difference of two proportions
#Em.6.17
#H0: p1 - p2 == 0
#Ha: p1 - p2 != 0
p0 <- 0
sig.lvl <- 0.05
n1 <- 44425 + 500
x1 <- 500
p1 <- round(x1/n1, 5)
n2 <- 44405 + 505
x2 <- 505
p2 <- round(x2/n2, 5)
pp <- round((x1 + x2)/(n1 + n2), 5)
# success-failure cond. (group 1)
n1 * pp >= 10
n1 * (1 - pp) >= 10
# success-failure cond. (group 2)
n2 * pp >= 10
n2 * (1 - pp) >= 10
ph <- round(p1 - p2, 5)
se <- round(sqrt((pp * (1 - pp)/n1) + (pp * (1 - pp)/n2)), 5)
z <- (ph - p0)/se * -1
pval <- round(2 * pnorm(z, lower.tail = FALSE), 4)
pval < sig.lvl


##Ex.6.20
#H0: p1 - p2 == 0.03
#Ha: p1 - p2 != 0.03
p0 <- 0.03
sig.lvl <- 0.05
n1 <- 1000
x1 <- 958
p1 <- round(x1/n1, 5)
n2 <- 1000
x2 <- 899
p2 <- round(x2/n2, 5)
# success-failure cond. (group 1)
n1 * p1 >= 10
n1 * (1 - p1) >= 10
# success-failure cond. (group 2)
n2 * p2 >= 10
n2 * (1 - p2) >= 10
ph <- round(p1 - p2, 5)
se <- round(sqrt((p1 * (1 - p1)/n1) + (p2 * (1 - p2)/n2)), 4)
z <- (ph - p0)/se
pval <- round(2 * pnorm(z, lower.tail = FALSE), 4)
pval < sig.lvl

##Ex.6.17
n1 <- 20
x1 <- 5
p1 <- round(x1/n1, 5)
n2 <- 25
x2 <- 15
p2 <- round(x2/n2, 5)
# success-failure cond. (group 1)
n1 * p1 >= 10
n1 * (1 - p1) >= 10
# success-failure cond. (group 2)
n2 * p2 >= 10
n2 * (1 - p2) >= 10

##Ex.6.21
CI <- 0.95
z <- round(qnorm(0.5 - CI/2) * -1, 2)
round(CI, 2) == round((0.5 - pnorm(-z)) * 2, 2)
n1 <- 347
p1 <- 0.79
x1 <- round(n1 * p1)
n2 <- 617
p2 <- 0.55
x2 <- round(n2 * p2)
# success-failure cond. (group 1)
n1 * p1 >= 10
n1 * (1 - p1) >= 10
# success-failure cond. (group 2)
n2 * p2 >= 10
n2 * (1 - p2) >= 10
# Std. Err.
se <- round(sqrt((p1 * (1 - p1)/n1) + (p2 * (1 - p2)/n2)), 5)
ME <- z * se
ph <- p1 - p2
lowerL <- round(ph - ME, 4)
upperL <- round(ph + ME, 4)
c(lowerL, upperL)

##Ex.6.23
#H0: p1 - p2 == 0
#Ha: p1 - p2 != 0
p0 <- 0
sig.lvl <- 0.05
n1 <- 438
x1 <- 104
p1 <- round(x1/n1, 3)
n2 <- 389
x2 <- 131
p2 <- round(x2/n2, 3)
pp <- round((x1 + x2)/(n1 + n2), 3)
# success-failure cond. (group 1)
n1 * pp >= 10
n1 * (1 - pp) >= 10
# success-failure cond. (group 2)
n2 * pp >= 10
n2 * (1 - pp) >= 10
ph <- round(p1 - p2, 5)
se <- round(sqrt((pp * (1 - pp)/n1) + (pp * (1 - pp)/n2)), 5)
z <- (ph - p0)/se * -1
pval <- round(2 * pnorm(z, lower.tail = FALSE), 4)
pval < sig.lvl


##Ex.6.25
#H0: p1 - p2 == 0
#Ha: p1 - p2 != 0
p0 <- 0
sig.lvl <- 0.05
n1 <- 438
x1 <- 154
p1 <- round(x1/n1, 3)
n2 <- 389
x2 <- 132
p2 <- round(x2/n2, 3)
pp <- round((x1 + x2)/(n1 + n2), 3)
# success-failure cond. (group 1)
n1 * pp >= 10
n1 * (1 - pp) >= 10
# success-failure cond. (group 2)
n2 * pp >= 10
n2 * (1 - pp) >= 10
ph <- round(p1 - p2, 5)
se <- round(sqrt((pp * (1 - pp)/n1) + (pp * (1 - pp)/n2)), 5)
z <- (ph - p0)/se 
pval <- round(2 * pnorm(z, lower.tail = FALSE), 4)
pval < sig.lvl

##Ex.6.27
#H0: p1 - p2 == 0
#Ha: p1 - p2 != 0
p0 <- 0
sig.lvl <- 0.05
n1 <- 203
x1 <- 35
p1 <- round(x1/n1, 3)
n2 <- 292
x2 <- 35
p2 <- round(x2/n2, 3)
pp <- round((x1 + x2)/(n1 + n2), 3)
# success-failure cond. (group 1)
n1 * pp >= 10
n1 * (1 - pp) >= 10
# success-failure cond. (group 2)
n2 * pp >= 10
n2 * (1 - pp) >= 10
ph <- round(p1 - p2, 5)
se <- round(sqrt((pp * (1 - pp)/n1) + (pp * (1 - pp)/n2)), 5)
z <- (ph - p0)/se 
pval <- round(2 * pnorm(z, lower.tail = FALSE), 4)
pval < sig.lvl

##Ex.6.29
#a)
#             virologic failure     Total
#               Yes       No    
# Nevaripine     26        94       120
# Lopinavir      10       110       120
#-------------------------------------------
#Total           36       204       240
#b)
#H0: p1 - p2 == 0
#Ha: p1 - p2 != 0
#c)
p0 <- 0
sig.lvl <- 0.05
n1 <- 120
x1 <- 26
p1 <- round(x1/n1, 3)
n2 <- 120
x2 <- 10
p2 <- round(x2/n2, 3)
pp <- round((x1 + x2)/(n1 + n2), 3)
# success-failure cond. (group 1)
n1 * pp >= 10
n1 * (1 - pp) >= 10
# success-failure cond. (group 2)
n2 * pp >= 10
n2 * (1 - pp) >= 10
ph <- round(p1 - p2, 5)
se <- round(sqrt((pp * (1 - pp)/n1) + (pp * (1 - pp)/n2)), 5)
z <- (ph - p0)/se 
pval <- round(2 * pnorm(z, lower.tail = FALSE), 4)
pval < sig.lvl









