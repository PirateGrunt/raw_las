## ----echo=FALSE, results='hide'------------------------------------------
library(ggplot2)
op <- par(no.readonly = TRUE)

## ------------------------------------------------------------------------
setwd('c:/home/git/raw_las/')
prem <- read.csv('./data/prem.csv')
prem

## ------------------------------------------------------------------------
plot(x = prem$Revenue, y = prem$Premium)

## ------------------------------------------------------------------------
lm1 <- lm(Premium ~ Revenue, data = prem)
summary(lm1)

## ------------------------------------------------------------------------
plot(x = prem$Revenue, y = prem$Premium)
abline(lm1, lwd = 3, col = 'red', lty = 2)

## ---- results='hide'-----------------------------------------------------
Premium ~ Revenue
Premium ~ Revenue + 0
Premium ~ Revenue - 1
Premium ~ log(Revenue)
Premium ~ log(Revenue) + 0
log(Premium) ~ log(Revenue)
Premium ~ log(Revenue) + Territory

## ------------------------------------------------------------------------
lm2 <- lm(Premium ~ log(Revenue), data = prem)
summary(lm2)

## ------------------------------------------------------------------------
subs <- data.frame(Revenue = c(250e3, 300e3, 350e3))
predict(lm2, newdata = subs)
coef(lm2)
log(300e3) * 126.96 - 61.14

## ------------------------------------------------------------------------
plot(x = log(prem$Revenue), y = prem$Premium)
abline(lm2, lwd = 3, col = 'red', lty = 2)

## ------------------------------------------------------------------------
plot(x = prem$Revenue, y = prem$Premium)
abline(lm1, lwd = 3, col = 'red', lty = 2)
xfit <- seq(from = min(prem$Revenue), to = max(prem$Revenue), length.out = 100)
yfit <- predict(lm2, newdata = data.frame(Revenue = xfit))
points(xfit, yfit, type = 'l', lwd = 3, col = 'blue', lty = 4)

