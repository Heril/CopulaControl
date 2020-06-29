source('./parameters.R')
options(warn=-1)

simData <- vector("list", Nrho)
for(i in 1:Nrho) {
  myMvd <- list(mvdGen(normalCopula, rho = rhoList[i]),
                mvdGen(frankCopula, rho = rhoList[i]),
                mvdGen(claytonCopula, rho = rhoList[i]),
                mvdGen(gumbelCopula, rho = rhoList[i]))
  simData[[i]] <- vector("list", Ncopula)
  for(j in 1:Ncopula) {
    simData[[i]][[j]] <- foreach(k=1:iterations, .packages="copula") %dorng% {
      tmpData <- vector("list", Nshift)
      for(l in 1:Nshift) {
        tmpVar <- as.data.frame(rMvdc(size, myMvd[[j]][[l]]))
        if (l == 1) {
          t2chart <- mult.chart(tmpVar, type = "t2")
          mechart <- mult.chart(tmpVar, type = "mewma")
          mcchart <- mult.chart(tmpVar, type = "mcusum")
          t2data <- t2chart$t2
          medata <- mechart$t2
          mcdata <- mcchart$t2
        } else {
          t2data <- mult.chart(tmpVar, type = "t2", Xmv = t2chart$Xmv, S = t2chart$covariance)$t2
          medata <- mult.chart(tmpVar, type = "mewma", Xmv = mechart$Xmv, S = mechart$covariance)$t2
          mcdata <- mult.chart(tmpVar, type = "mcusum", Xmv = mcchart$Xmv, S = mcchart$covariance)$t2
        }
        tmpData[[l]] <- cbind(rep(copulaList[[j]]),
                            rep(rhoList[[i]]),
                            rep(shiftList[[l]]),
                            rep(k, size),
                            tmpVar,
                            1:size,
                            t2data,
                            medata,
                            mcdata
        )
        colnames(tmpData[[l]]) <- c("copula", "rho", "shift", "iteration", "data1", "data2", "N", "t2", "mewma", "mcusum")
      }
      tmpData
    }
  }
}
gc()

tmpData <- vector("list", Nrho)
for(i in 1:Nrho) {
  tmpData[[i]] <- vector("list", Ncopula)
  for(j in 1:Ncopula) {
    tmpData[[i]][[j]] <- bind_rows(simData[[i]][[j]])
  }
}
rm(simData)
gc()

simData <- bind_rows(tmpData)
rm(tmpData)
gc()

simData$copula <- factor(simData$copula, level = copulaList)
simData$rho <- factor(simData$rho, level = rhoList)
simData$shift <- factor(simData$shift, level = shiftList)
options(warn=0)
