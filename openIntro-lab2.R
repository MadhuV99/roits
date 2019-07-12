#openIntro-lab2.R
setwd('~/pj/rpj/roits')
getwd()

source("http://www.openintro.org/stat/data/cdc.R")

str(cdc)
dim(cdc)
nrow(cdc)
length(cdc)
names(cdc)
names(cdc)
head(cdc)
tail(cdc)

summary(cdc$weight)
sum(cdc$weight > 400)
median(cdc[,'weight'])
var(cdc$weight)
sd(cdc[, 'weight'])
with(cdc,{
  print(mean(weight))
  var(weight)
})

boxplot(cdc$weight)

table(cdc$smoke100)
table(cdc$smoke100)/nrow(cdc)

barplot(table(cdc$smoke100))
barplot(table(cdc$smoke100)/nrow(cdc))

str(table(cdc$genhlth))
table(cdc$genhlth)
health <- table(cdc$genhlth)
health
barplot(health)

gendersmokes <- table(cdc$gender,cdc$smoke100)
mosaicplot(gendersmokes)

cdc[567, 6]
names(cdc)
cdc[1:10, 6]
cdc[1:10, ]

cdc$weight[567]
cdc$weight[1:10]

mdata <- subset(cdc, cdc$gender== "m")
m_and_over30 <- subset(cdc, cdc$gender == "m" & cdc$age > 30)
m_or_over30 <- subset(cdc, cdc$gender == "m" | cdc$age > 30)

under23_and_smoke <- subset(cdc, cdc$smoke100 == 1 & cdc$age < 23)
head(under23_and_smoke)

summary(cdc$height)
boxplot(cdc$height)

summary(cdc$height[cdc$gender=='m'])
summary(cdc$height[cdc$gender=='f'])
boxplot(cdc$height ~ cdc$gender)

bmi <- (cdc$weight / cdc$height^2) * 703
boxplot(bmi ~ cdc$genhlth)
boxplot(bmi ~ cdc$gender)

hist(cdc$age)
hist(bmi)
hist(bmi, breaks = 50)

plot(cdc$wtdesire ~ cdc$weight)

wdiff <- cdc$weight - cdc$wtdesire
wdiff <- ifelse((cdc$weight - cdc$wtdesire) < -200, -200, cdc$weight - cdc$wtdesire) 
summary(wdiff)
wdiff <- ifelse(wdiff > 200, 200, wdiff) 
head(wdiff)
summary(wdiff)
boxplot(wdiff)
hist(wdiff)
boxplot(wdiff ~ cdc$gender)
summary(wdiff[cdc$gender=="m"])
summary(wdiff[cdc$gender=="f"])
