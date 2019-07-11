#openIntro-8.R
setwd('~/pj/rpj/roits')
getwd()

download.file("http://www.openintro.org/stat/data/evals.RData", destfile = "evals.RData")
load("evals.RData")

hist(evals$score)
plot(evals$score ~ evals$bty_avg)
plot(evals$score ~ jitter(evals$bty_avg))
plot(jitter(evals$score) ~ evals$bty_avg)

m_bty <- lm(score ~ bty_avg, data = evals)
summary(m_bty)

plot(evals$score ~ evals$bty_avg)
abline(m_bty)

hist(m_bty$residuals)
plot(m_bty$residuals ~ evals$bty_avg)
abline(h = 0, lty = 3) # adds a horizontal dashed line at y = 0

plot(evals$bty_avg ~ evals$bty_f1lower)
cor(evals$bty_avg, evals$bty_f1lower)

head(evals[, 13:19])
plot(evals[, 13:19])

m_bty_gen <- lm(score ~ bty_avg + gender, data = evals)
summary(m_bty_gen)

multiLines(m_bty_gen)

m_full <- lm(score ~ rank + ethnicity + gender + language + age + cls_perc_eval 
             + cls_students + cls_level + cls_profs + cls_credits + bty_avg 
             + pic_outfit + pic_color, data = evals)
summary(m_full)





