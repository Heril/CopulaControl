source('./library.R')
# Set number of workers, i.e. number of threads to give to R.
# Number of Iterations in generator.R is split up by this.
registerDoMC(4)
seed <- registerDoRNG(20200218)

# Test parameters.
# copulaList: Bivariate Copula functions to use to generate simulation data
#   Change not supported in run.R, need to change in mvdGen() call in generator.R
# rhoList: Different correlations to be forced in the bivariate copulas
#   Any value in interval (-1,1) is allowed.
# shiftList: List of shift in means of the bivariate data. Listed is for single
#   and double variable shifts. Change is not supported in run.R, need to change
#   mvdGen() call in generator.R to change.
# size: Number of pairs of points to generate for each iteration of each experiment.
# iterations: Number of repitions for each experiment.
# cARL: ARL to calibrate UCL for analysis
copulaList <- c("Normal", "Frank", "Clayton", "Gumbel")
rhoList <- c(0.6, 0.2, -0.2, -0.6)
shiftList <- c("0/0", "0/0.5", "0/1", "0/2", "0/3", "0.5/0.5", "1/1", "2/2", "3/3")
size <- 1000
iterations <- 1000
cARL <- 300

Nrho <- length(rhoList)
Ncopula <- length(copulaList)
Nshift <- length(shiftList)
