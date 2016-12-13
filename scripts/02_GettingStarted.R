## ----echo=FALSE, results='hide'------------------------------------------
library(pander)

## ----results='hide'------------------------------------------------------
2 + 5
-2^0.5
-(2^0.5)
abs(-15.4)
a <- 0
a == 0
a = 0

## ------------------------------------------------------------------------
sqrt(4)

## ------------------------------------------------------------------------
sqrt(exp(sin(pi)))

## ----eval=FALSE, echo=FALSE----------------------------------------------
## ?S3groupGeneric

## ----eval=FALSE, echo=TRUE, size='tiny'----------------------------------
## ?plot
## 
## ??cluster

## ----eval=TRUE, echo=TRUE, size='tiny'-----------------------------------
getwd()

## ----eval=FALSE, results='hide', size='tiny'-----------------------------
## setwd("~/SomeNewDirectory/SomeSubfolder")

## ------------------------------------------------------------------------
N <- 100
B0 <- 5
B1 <- 1.5

set.seed(1234)

e <- rnorm(N, mean = 0, sd = 1)
X1 <- rep(seq(1,10),10)

Y <- B0 + B1 * X1 + e

myFit <- lm(Y ~ X1)

## ----eval=FALSE----------------------------------------------------------
## source("SomefileName.R")

## ----eval=FALSE----------------------------------------------------------
## # Take the ratio of loss to premium to determine the loss ratio
## 
## lossRatio <- Losses / Premium

## ----eval=FALSE----------------------------------------------------------
## # Because this is a retrospective view of
## # profitability, these losses have been
## # developed, but not trended to a future
## # point in time
## lossRatio <- Losses / Premium

