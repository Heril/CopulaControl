library(copula)
library(tidyverse)
library(Hmisc)
library(foreach)
library(doMC)
library(doRNG)

# seed <- registerDoRNG(20200218)

copulaMvdGen <- function(myCopula, shift = c(0,0)) {
  return(mvdc(copula = myCopula, margins = c("norm", "norm"), paramMargins = list(list(mean = shift[1], sd = 1), list(mean = shift[2], sd = 1))))
}

mvdGen <- function (category = normalCopula, rho = 0.6, shiftList = c(0.5, 1, 2, 3)) {
  myCopula <- category(iRho(category(), rho))
  myMvd <- list()
  myMvd[[1]] <- copulaMvdGen(myCopula)
  for(i in 1:length(shiftList)) {
    myMvd[[1 + i]] <- copulaMvdGen(myCopula, c(0,shiftList[i]))
    myMvd[[1 + i + length(shiftList)]] <- copulaMvdGen(myCopula, rep(shiftList[i], 2))
  }
  return(myMvd)
}

# MSQC package
# Functions included in the book:
# Santos-Fernandez, Edgar. Multivariate Statistical Quality Control Using R. Springer. 2013.
# volume 14, isbn 9781461454533, http://www.springer.com/statistics/computational+statistics/book/978-1-4614-5452-6}

# Copyright (C) 2013-2015 Santos-Fernandez, Edgar
# All rights reserved.
# These functions are licensed under the GNU General Public License (GPL-2 and GPL-3).
mult.chart <-
  function(type = c("t2", "mewma", "mcusum"),
           x, Xmv, S, colm, alpha = 0.01, lambda = 0.1, k = 0.5,
           h = 5.5, phase = 1, method = "sw", ...){

    type <- match.arg(type)

    ###Variables
    p <- ncol(x) # quality characteristics
    m <- nrow(x) # number of samples or observations
    if (class(x) == "matrix" || class(x) == "data.frame") (x <- array(data.matrix(x),c(m,p,1)))
    n <- dim(x)[3] # observations or sample size

    if(!missing(Xmv))(phase <- 2)

    x.jk <- matrix(0,m,p)
    t2 <- matrix(0,m,1)

    x.jk <- apply(x,1:2,mean)

    if(missing(Xmv))(Xmv <- colMeans(x.jk))
    if(missing(S))(S <- covariance(x,method = method))
    if(missing(colm))(colm <- nrow(x))

    if (type == "t2") { # Hotelling Control Chart
      for (ii in 1 : m){
        t2[ii,1] <- n * t(x.jk[ii,] - Xmv) %*% solve(S) %*% (x.jk[ii,] - Xmv)
      }
    }

    if (type == "mewma") { # MEWMA Control Chart
      z<-matrix(0, m, p)

      for (i in 1 : m){
        if(i==1){
          z[i,] <- lambda * (x.jk[i,] - Xmv)}
        else{ z[i,] <- lambda * (x.jk[i,] - Xmv) + (1 - lambda) * z[i - 1,]}
        weig <- S * (lambda * (1 - ((1 - lambda) ^ (2 * i))) / (2 - lambda))
        t2[i,1] <- t(z[i,]) %*% solve(weig) %*% z[i,]
      }
    }

    if (type == "mcusum") { # MCUSUM Control Chart according to Crosier(1988)
      dif <- sweep(x.jk,2,Xmv)
      s <- matrix(0,m,p)
      ci <- matrix(0,m,1)

      ci[1] <- sqrt(dif[1,] %*% solve((S / n)) %*% dif[1,])

      if (ci[1] > k) { s[1,] <- (s[1,] + dif[1,]) * (1 - k / ci[1])}
      else(s[1,] = matrix(0,ncol = p)) #compute s

      for (i in 2:m){
        ci[i,]=sqrt((s[i - 1,] + dif[i,]) %*% solve(S / n) %*% (s[i - 1,] + dif[i,]))
        if (ci[i] > k){ s[i,] = (s[i - 1,] + dif[i,]) * (1 - k / ci[i])}
        else {s[i,] = matrix(0,ncol = p)}
      }

      for (i in 1:m){
        t2[i]=sqrt(s[i,]%*%solve((S/n))%*%(s[i,]))
      }
    }

    outList = list (t2 = round(t2,2), Xmv = round(Xmv,2), covariance = signif(S,2))
    return(outList)

  }


covariance <-
  function(x,stat,method,...){
    p <- ncol(x) # quality characteristics
    m <- nrow(x) # sample
    if (class(x) == "matrix" || class(x) == "data.frame") (x <- array(data.matrix(x),c(m,p,1)))
    n <- dim(x)[3] # observations or sample size

    s.jk <- matrix(0,m,p ^ 2) # matrix of the covariances of observations
    SS <- matrix(0,m,1) # matrix of /S/ statistic

    if(n > 1){
      arrays <- expand.grid(1:p,1:p)

      for (i in 1 : m){
        for(j in 1 : p ^ 2){
          s.jk[i,j] <- cov(x[i,arrays[j,1],],x[i,arrays[j,2],])
        }
      }

      S <- matrix(colMeans(s.jk),p,p)

      for (ii in 1 : m){
        SS[ii] <- det(matrix(s.jk[ii,],p,p))
      }

      if(missing(stat)) (return(S))
      else (return(SS))

    }

    if(n == 1){
      if(missing(method))(method="sw")

      if(method == "sw"){
        B <- matrix(0,p,p)
        w <- sweep(x,2,(apply(x,2,mean))) #compute de value minus the mean
        for(i in 1:m){
          B <- B + w[i,,] %*% t(w[i,,])
        }
        S <- s1 <- B/(m - 1)
      }

      if(method == "hm"){
        V <- matrix(0,m-1,p)
        for(i in 1:m-1){
          V[i,] <- x[i+1,,] - x[i,,]
        }
        S <- s2 <- .5 * t(V) %*% V / (m - 1)
      }


      return(S)
    }


  }
