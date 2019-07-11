#openIntro-8.R
setwd('~/pj/rpj/roits')
getwd()

download.file("http://www.openintro.org/stat/data/mlb11.RData", destfile = "mlb11.RData")
load("mlb11.RData")

cor(mlb11$runs, mlb11$at_bats)

hist(mlb11$runs)
hist(mlb11$at_bats)
plot_ss(x = mlb11$at_bats, y = mlb11$runs)
plot_ss(x = mlb11$at_bats, y = mlb11$runs, showSquares = TRUE)

m1 <- lm(runs ~ at_bats, data = mlb11)
summary(m1)

plot(mlb11$runs ~ mlb11$at_bats)
abline(m1)

hist(m1$residuals)
plot(m1$residuals ~ mlb11$at_bats)
abline(h = 0, lty = 3) # adds a horizontal dashed line at y = 0

qqnorm(m1$residuals)
qqline(m1$residuals)


