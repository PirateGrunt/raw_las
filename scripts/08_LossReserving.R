## ----echo=FALSE, results='hide'------------------------------------------
# Author: Adam L. Rich
# Date:   December 5, 2014
# Description:
#
#   Reserving in R
#   
#   Adapted from content found at
#     http://opensourcesoftware.casact.org/chain-ladder
#

# Set-up environment
suppressPackageStartupMessages(library(ChainLadder))

## ------------------------------------------------------------------------
data(othliab, package = 'raw')
names(othliab)

## ---- eval = FALSE-------------------------------------------------------
## # What GROUPs are here?
## sort(unique(othliab$GRNAME))

## ------------------------------------------------------------------------
# How many groups are there?
length(unique(othliab$GRNAME))

# What is a unique key on the data?
nrow(othliab)
nrow(unique(othliab[, c('GRCODE', 'AccidentYear', 'DevelopmentYear')]))
nrow(unique(othliab[, c('GRCODE', 'AccidentYear', 'DevelopmentLag')]))
table(othliab[, c('AccidentYear', 'DevelopmentYear')])

# How would you calculate the total premium for ASL 17 by accident year?
aggregate(
  data = othliab[othliab$DevelopmentYear == 1997, ],
  EarnedPremDIR_h1 ~ AccidentYear,
  FUN = sum
)

## ------------------------------------------------------------------------
asl17 <- aggregate(
  x = othliab[, c("IncurLoss_h1", "CumPaidLoss_h1", "BulkLoss_h1", "EarnedPremDIR_h1")], 
  by = othliab[, c('AccidentYear', 'DevelopmentYear', 'DevelopmentLag')], 
  FUN = sum
)
names(asl17)
names(asl17) <- c("AY", "DY", "Dev", "UltLoss", "PdLoss", "IBNR", "GPE")
asl17$IncLoss <- asl17$UltLoss - asl17$IBNR

## ------------------------------------------------------------------------
tri.inc <- as.triangle(
  asl17[asl17$DY <= 1997, ], 
  origin = 'AY', 
  dev = 'Dev', 
  value = 'IncLoss'
)
tri.pd <- as.triangle(
  asl17[asl17$DY <= 1997, ], 
  origin = 'AY', 
  dev = 'Dev', 
  value = 'PdLoss'
)
tri.gpe <- as.triangle(
  asl17[asl17$DY <= 1997, ], 
  origin = 'AY', 
  dev = 'Dev', 
  value = 'GPE'
)
tri.os <- tri.inc - tri.pd

## ------------------------------------------------------------------------
tri.os

## ---- eval = FALSE-------------------------------------------------------
## MackChainLadder <- function (
##   Triangle,
##   weights = 1,
##   alpha = 1,
##   est.sigma = "log-linear",
##   tail = FALSE,
##   tail.se = NULL,
##   tail.sigma = NULL,
##   mse.method = "Mack") {...}

## ------------------------------------------------------------------------
MackChainLadder(tri.inc)

## ------------------------------------------------------------------------
plot(MackChainLadder(tri.inc))

## ------------------------------------------------------------------------
plot(tri.inc)

## ------------------------------------------------------------------------
MackChainLadder(tri.inc, alpha = 0)$f   # Simple average of the dev ratios
MackChainLadder(tri.inc, alpha = 1)$f   # Chain Ladder ratio ("loss wtd average")
MackChainLadder(tri.inc, alpha = 2)$f   # Wtd Average of the dev ratios

## ------------------------------------------------------------------------
MackChainLadder(tri.inc)$tail
MackChainLadder(tri.inc, tail = 1.1)$tail
MackChainLadder(tri.inc, tail = TRUE)$tail

## ------------------------------------------------------------------------
w <- matrix(1, nrow = 10, ncol = 10)
w[3, 4] <- 0

MackChainLadder(tri.inc)
MackChainLadder(tri.inc, weights = w)

## ---- eval=FALSE---------------------------------------------------------
## MunichChainLadder <- function(
##   Paid,
##   Incurred,
##   est.sigmaP = "log-linear",
##   est.sigmaI = "log-linear",
##   tailP = FALSE,
##   tailI = FALSE
## ) {...}

## ------------------------------------------------------------------------
MunichChainLadder(tri.pd, tri.inc)

## ---- fig.align----------------------------------------------------------
# [ALR Dec 13, 2016]
# How to get past error "figure margins too large"
#
#   plot(MunichChainLadder(tri.pd, tri.inc))
#

## ---- echo=FALSE---------------------------------------------------------
# # BOOT CHAIN LADDER
# #   
# #   The BootChainLadder is a model that provides a predicted distribution 
# #   for the IBNR values for a claims triangle. However, this model predicts 
# #   IBNR values by a different method than the previous two models. 
# #   First, the development factors are calculated and then 
# #   they are used in a backwards recursion to predict 
# #   values for the past loss triangle. Then the predicted values 
# #   and the actual values are used to calculate Pearson residuals. 
# #   The residuals are adjusted by a formula specified in appendix 3 
# #   in the follow paper 
# #
# #     http://www.actuaries.org.uk/system/files/documents/pdf/sm0201.pdf)
# # 
# #   Using the adjusted residuals and the predicted losses from before, 
# #   the model solves for the actual losses in the Pearson formula 
# #   and forms a new loss triangle. The steps for predicting past losses 
# #   and residuals are then repeated for this new triangle. 
# #   After that, the model uses chain ladder ratios to predict 
# #   the future losses then calculates the ultimate and
# #   IBNR values like in the previous Mack model. 
# #   This cycle is performed R times, depending on the argument values in the
# #   model (default is 999 times). The IBNR for each origin period is calculated 
# #   from each triangle (the default 999) and used to form 
# #   a predictive distribution, from which summary statistics 
# #   are obtained such as mean, prediction error, and quantiles.
# #   
# #
# #
# #   BootChainLadder <- function (
# #     Triangle, 
# #     R = 999, 
# #     process.distr = c("gamma", "od.pois")
# #
# #   Triangle        Data
# #   R               the number of bootstraps(the default is 999)
# #   process.distr   or the way the process error is calculated for each 
# #                   predicted IBNR values with the options of
# #                   "gamma"(default) and 
# #                   "od.pois" (over dispersed Poisson)
# #
# 
# BootChainLadder(tri.inc)
# 
# 
# # The output has some of the same values as the Munich and Mack models did.
# # The Mean and SD IBNR is the average and the standard deviation 
# # of the predictive distribution of the IBNRs for each origin year
# 
# # The output also gives the 75% and 95% quantiles of 
# # the predictive distribution of IBNRs, in other words 95% or 75% of
# # the predicted IBNRs lie at or below the given values.
# 


