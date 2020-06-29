genStartTime <- Sys.time()
source('./generator.R')
genEndTime <- Sys.time()

analysisStartTime <- Sys.time()
source('./analysis.R')
analysisEndTime <- Sys.time()

cat("Generator ")
print(genEndTime - genStartTime)
cat("Analysis ")
print(analysisEndTime - analysisStartTime)

saveRDS(simData, "Data/simData.rds")
saveRDS(ARL, "Data/ARL.rds")
