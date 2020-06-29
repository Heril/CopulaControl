source('./parameters.R')
options(dplyr.summarise.inform=F, width = 110)
if(!exists("simData")) {simData <- readRDS("Data/simData.rds")}

rhoList <- levels(simData$rho)
shiftList <- levels(simData$shift)
copulaList <- levels(simData$copula)
iterations <- max(simData$iteration)
size <- max(simData$N)

Ncopula <- length(copulaList)
Nshifts <- length(shiftList)
Nrho <- length(rhoList)

t2ucl <- simData %>%
  filter(shift == "0/0") %>%
  group_by(copula, rho) %>%
  summarise(UCLt2 = quantile(t2, probs = (cARL - 1)/cARL)) %>%
  select(copula, rho, UCLt2) %>%
  spread(rho, UCLt2)%>%
  column_to_rownames(var="copula")

mewmaucl <- simData %>%
  filter(shift == "0/0") %>%
  group_by(copula, rho) %>%
  summarise(UCLme = quantile(mewma, probs = (cARL - 1)/cARL)) %>%
  select(copula, rho, UCLme) %>%
  spread(rho, UCLme)%>%
  column_to_rownames(var="copula")

mcusumucl <- simData %>%
  filter(shift == "0/0") %>%
  group_by(copula, rho) %>%
  summarise(UCLmc = quantile(mcusum, probs = (cARL - 1)/cARL)) %>%
  select(copula, rho, UCLmc) %>%
  spread(rho, UCLmc)%>%
  column_to_rownames(var="copula")

splitData <- simData %>%
  group_split(copula,rho)

tmpData <- list()
index <- 1
for(i in 1:Ncopula) {
  for(j in 1:Nrho) {
    tmpData[[index]] <- splitData[[index]] %>%
      group_by(copula, rho, shift, iteration) %>%
      summarise(t2ARL = ifelse(shift == "0/0", size/sum(t2 > t2ucl[i,j]),
                              detect_index(t2, function(z)(z>t2ucl[i,j]))),
                meARL = ifelse(shift == "0/0", size/sum(mewma > mewmaucl[i,j]),
                              detect_index(mewma, function(z)(z>mewmaucl[i,j]))),
                mcARL = ifelse(shift == "0/0", size/sum(mcusum > mcusumucl[i,j]),
                              detect_index(mcusum, function(z)(z>mcusumucl[i,j])))) %>%
      mutate(t2ARL = ifelse(is.infinite(t2ARL), size, t2ARL),
             meARL = ifelse(is.infinite(meARL), size, meARL),
             mcARL = ifelse(is.infinite(mcARL), size, mcARL))
    index <- index + 1
  }
}
rm(splitData)
gc()

ARL <- bind_rows(tmpData) %>%
  group_by(copula, rho, shift) %>%
  summarise(t2ci = list(mean_cl_normal(t2ARL) %>%
                          rename(t2ARLmean=y, t2ARLlwr=ymin, t2ARLupr=ymax)),
            meci = list(mean_cl_normal(meARL) %>%
                          rename(meARLmean=y, meARLlwr=ymin, meARLupr=ymax)),
            mcci = list(mean_cl_normal(mcARL) %>%
                          rename(mcARLmean=y, mcARLlwr=ymin, mcARLupr=ymax))) %>%
  unnest(cols = c(t2ci, meci, mcci))
ARL %>%
  filter(shift == "0/0") %>%
  print(width = Inf)
options(dplyr.summarise.inform=T, width = 80)
