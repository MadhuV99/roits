#openIntro-lab1.R
setwd('~/pj/rpj/roits')
getwd()

source("http://www.openintro.org/stat/data/arbuthnot.R")
str(arbuthnot)
dim(arbuthnot)
head(arbuthnot)
names(arbuthnot)
colnames(arbuthnot)
arbuthnot$boys
arbuthnot$girls

?plot
plot(x = arbuthnot$year, y = arbuthnot$boys)
plot(arbuthnot$year, arbuthnot$girls, type = 'l')
plot(arbuthnot$boys + arbuthnot$girls ~ arbuthnot$year)
plot(boys + girls ~ year, data = arbuthnot, type = 'l')

with(arbuthnot, mean(girls/(boys + girls)))

plot(x=arbuthnot$year, y=arbuthnot$girls/(arbuthnot$boys+arbuthnot$girls), type='l')

source("http://www.openintro.org/stat/data/present.R")
str(present)
dim(present)
head(present)
names(present)
colnames(present)
present$boys
present$girls

with(present, mean(girls/(boys + girls)))

with(present, {
  plot(x = year, y = boys)
  plot(year, girls, type = 'l')
  plot(boys + girls ~ year)
})

plot(x=present$year, y=present$girls/(present$boys+present$girls), type='l')

plot(boys + girls ~ year, data = present, type = 'l')



